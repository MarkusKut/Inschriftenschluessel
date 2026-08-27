importScripts("./precache-core.js");


/* ========================================
   Cache names
   ======================================== */

const PREFIX = "inschriftenschluessel";

const CORE_CACHE =
  `${PREFIX}-core-${self.__BUILD_ID}`;

const RUNTIME_CACHE =
  `${PREFIX}-runtime-${self.__BUILD_ID}`;

const FULL_CACHE =
  `${PREFIX}-full-${self.__BUILD_ID}`;


/* ========================================
   Helpers
   ======================================== */

function scopedUrl(path) {
  return new URL(
    path,
    self.registration.scope
  ).href;
}


async function putInCache(
  cacheName,
  request,
  response
) {
  if (
    !response ||
    !response.ok
  ) {
    return;
  }

  const cache =
    await caches.open(cacheName);

  await cache.put(
    request,
    response.clone()
  );
}


/* ========================================
   Install:
   ONLY cache the small core
   ======================================== */

self.addEventListener(
  "install",
  event => {

    event.waitUntil(
      (async () => {

        const cache =
          await caches.open(
            CORE_CACHE
          );

        const urls =
          self.__CORE_URLS.map(
            scopedUrl
          );

        /*
         * Load separately so one bad file
         * does not break the entire install.
         */
        await Promise.allSettled(
          urls.map(
            url => cache.add(url)
          )
        );

        await self.skipWaiting();

      })()
    );
  }
);


/* ========================================
   Activate
   ======================================== */

self.addEventListener(
  "activate",
  event => {

    event.waitUntil(
      (async () => {

        const keep = new Set([
          CORE_CACHE,
          RUNTIME_CACHE,
          FULL_CACHE
        ]);

        const names =
          await caches.keys();

        await Promise.all(
          names
            .filter(
              name =>
                name.startsWith(PREFIX) &&
                !keep.has(name)
            )
            .map(
              name =>
                caches.delete(name)
            )
        );

        await self.clients.claim();

      })()
    );
  }
);


/* ========================================
   CACHE FIRST
   Glyphs, images, fonts
   ======================================== */

async function cacheFirst(request) {

  const cached =
    await caches.match(request);

  if (cached) {
    return cached;
  }

  try {

    const response =
      await fetch(request);

    await putInCache(
      RUNTIME_CACHE,
      request,
      response
    );

    return response;

  } catch (error) {

    return new Response(
      "",
      {
        status: 504,
        statusText: "Offline"
      }
    );
  }
}


/* ========================================
   STALE WHILE REVALIDATE
   HTML, CSS, JS, JSON
   ======================================== */

async function staleWhileRevalidate(
  event
) {

  const request =
    event.request;

  const cached =
    await caches.match(request);

  const networkPromise =
    fetch(request)
      .then(async response => {

        await putInCache(
          RUNTIME_CACHE,
          request,
          response
        );

        return response;

      })
      .catch(() => null);

  if (cached) {

    /*
     * Return cache immediately,
     * update silently in background.
     */
    event.waitUntil(
      networkPromise
    );

    return cached;
  }

  const network =
    await networkPromise;

  if (network) {
    return network;
  }

  /*
   * Navigation fallback
   */
  if (
    request.mode === "navigate"
  ) {

    const fallback =
      await caches.match(
        scopedUrl(
          "./offline.html"
        )
      );

    if (fallback) {
      return fallback;
    }
  }

  return new Response(
    "Offline",
    {
      status: 503,
      headers: {
        "Content-Type":
          "text/plain; charset=utf-8"
      }
    }
  );
}


/* ========================================
   Fetch routing
   ======================================== */

self.addEventListener(
  "fetch",
  event => {

    const request =
      event.request;

    if (
      request.method !== "GET"
    ) {
      return;
    }

    const url =
      new URL(request.url);

    /*
     * Do not interfere with external sites,
     * e.g. TLA.
     */
    if (
      url.origin !==
      self.location.origin
    ) {
      return;
    }

    const pathname =
      url.pathname.toLowerCase();


    /*
     * Glyphs, images and fonts:
     * once cached, never wait for network.
     */
    if (
      pathname.match(
        /\.(png|jpg|jpeg|webp|svg|woff|woff2)$/i
      )
    ) {

      event.respondWith(
        cacheFirst(request)
      );

      return;
    }


    /*
     * Pages and application assets:
     * cached immediately,
     * updated in background.
     */
    if (
      request.mode === "navigate" ||
      pathname.match(
        /\.(html|css|js|json)$/i
      )
    ) {

      event.respondWith(
        staleWhileRevalidate(
          event
        )
      );

      return;
    }


    /*
     * Everything else:
     * ordinary cache-first.
     */
    event.respondWith(
      cacheFirst(request)
    );
  }
);

/* ========================================
   Full offline download
   Pause / Resume / Cancel
   ======================================== */

let offlineDownload = {
  running: false,
  paused: false,
  cancelled: false,
  done: 0,
  total: 0,
  failed: [],
  controller: null
};


async function broadcast(message) {

  const clients =
    await self.clients.matchAll({
      type: "window",
      includeUncontrolled: true
    });

  clients.forEach(
    client =>
      client.postMessage(message)
  );
}


function sleep(ms) {
  return new Promise(
    resolve => setTimeout(resolve, ms)
  );
}


/*
 * Used before Resume/Cancel so we don't
 * start two download loops at the same time.
 */
async function waitUntilDownloadStopped() {

  while (offlineDownload.running) {
    await sleep(50);
  }
}


/*
 * Read the list of all files.
 */
async function getFullOfflineManifest() {

  const manifestUrl =
    scopedUrl(
      "./offline-full-manifest.txt"
    );

  const response =
    await fetch(
      manifestUrl,
      {
        cache: "no-store"
      }
    );

  if (!response.ok) {
    throw new Error(
      "Offline manifest could not be loaded."
    );
  }

  const text =
    await response.text();

  return text
    .split(/\r?\n/)
    .map(x => x.trim())
    .filter(Boolean);
}


/* ========================================
   Download / Resume
   ======================================== */

async function fetchForOffline(
  url,
  attempts = 2
) {

  let lastError = null;

  for (
    let attempt = 1;
    attempt <= attempts;
    attempt++
  ) {

    try {

      offlineDownload.controller =
        new AbortController();

      const response =
        await fetch(
          url,
          {
            signal:
              offlineDownload
                .controller
                .signal
          }
        );

      if (response.ok) {
        return response;
      }

      lastError =
        new Error(
          `HTTP ${response.status} ${response.statusText}`
        );

    } catch (error) {

      /*
       * Pause / Cancel is intentional.
       * Do not retry it.
       */
      if (
        offlineDownload.paused ||
        offlineDownload.cancelled
      ) {
        throw error;
      }

      lastError = error;
    }

    /*
     * Tiny delay before second attempt
     */
    if (attempt < attempts) {
      await sleep(300);
    }
  }

  throw lastError;
}

async function downloadFullSite() {

  /*
   * Prevent two simultaneous downloads.
   */
  if (offlineDownload.running) {
    return;
  }

  offlineDownload.running = true;
  offlineDownload.paused = false;
  offlineDownload.cancelled = false;
  offlineDownload.done = 0;
  offlineDownload.failed = [];

  try {

    const paths =
      await getFullOfflineManifest();

    offlineDownload.total =
      paths.length;

    const cache =
      await caches.open(
        FULL_CACHE
      );


    await broadcast({
      type: "FULL_OFFLINE_START",
      done: 0,
      total: offlineDownload.total
    });


    for (const path of paths) {

      /*
       * Stop before starting another file.
       */
      if (
        offlineDownload.paused ||
        offlineDownload.cancelled
      ) {
        break;
      }


      const url =
        scopedUrl(path);


      /*
       * IMPORTANT:
       *
       * If this file was already downloaded
       * before Pause, skip downloading it again.
       *
       * This is what makes Resume work.
       */
      const existing =
        await cache.match(url);

      if (existing) {

        offlineDownload.done++;

        await broadcast({
          type: "FULL_OFFLINE_PROGRESS",
          done: offlineDownload.done,
          total: offlineDownload.total
        });

        continue;
      }


      /*
       * AbortController allows Pause/Cancel
       * to stop the current network request.
       */
      offlineDownload.controller =
        new AbortController();


      try {

  const response =
    await fetchForOffline(
      url,
      2
    );

  await cache.put(
    url,
    response.clone()
  );

  offlineDownload.done++;

} catch (error) {

  if (
    offlineDownload.paused ||
    offlineDownload.cancelled
  ) {
    break;
  }

  offlineDownload.failed.push({
    url: url,
    error:
      error?.message ||
      String(error)
  });

  console.error(
    "OFFLINE FILE FAILED:",
    url,
    error
  );

} finally {

  offlineDownload.controller =
    null;
}


      await broadcast({
        type: "FULL_OFFLINE_PROGRESS",
        done: offlineDownload.done,
        total: offlineDownload.total
      });
    }


    /* ----------------------------------------
       Cancelled
       ---------------------------------------- */

    if (offlineDownload.cancelled) {
      return;
    }


    /* ----------------------------------------
       Paused
       ---------------------------------------- */

    if (offlineDownload.paused) {

      await broadcast({
        type: "FULL_OFFLINE_PAUSED",
        done: offlineDownload.done,
        total: offlineDownload.total
      });

      return;
    }


    /* ----------------------------------------
       Successfully completed
       ---------------------------------------- */

    if (
      offlineDownload.done >=
      offlineDownload.total
    ) {

      await cache.put(
        scopedUrl(
          "./__full_offline_ready__"
        ),
        new Response(
          self.__BUILD_ID
        )
      );


      await broadcast({
        type: "FULL_OFFLINE_READY",
        done: offlineDownload.done,
        total: offlineDownload.total
      });

    } else {

      /*
       * Some network requests failed.
       */
      await broadcast({
  type: "FULL_OFFLINE_INCOMPLETE",
  done: offlineDownload.done,
  total: offlineDownload.total,
  failed: offlineDownload.failed
});

    }

  } catch (error) {

    console.error(
      "Full offline download error:",
      error
    );

    await broadcast({
      type: "FULL_OFFLINE_ERROR"
    });

  } finally {

    offlineDownload.controller =
      null;

    offlineDownload.running =
      false;
  }
}


/* ========================================
   Messages from website
   ======================================== */

self.addEventListener(
  "message",
  event => {

    const data =
      event.data || {};


    /* ----------------------------------------
       Start download
       ---------------------------------------- */

    if (
      data.type ===
      "DOWNLOAD_FULL_OFFLINE"
    ) {

      event.waitUntil(
        downloadFullSite()
      );

      return;
    }


    /* ----------------------------------------
       Pause
       ---------------------------------------- */

    if (
      data.type ===
      "PAUSE_FULL_OFFLINE"
    ) {

      offlineDownload.paused = true;

      /*
       * Stop the current fetch immediately.
       */
      if (
        offlineDownload.controller
      ) {
        offlineDownload
          .controller
          .abort();
      }

      return;
    }


    /* ----------------------------------------
       Resume
       ---------------------------------------- */

    if (
      data.type ===
      "RESUME_FULL_OFFLINE"
    ) {

      offlineDownload.paused = false;
      offlineDownload.cancelled = false;

      event.waitUntil(
        (async () => {

          /*
           * Wait until the old paused loop
           * has completely stopped.
           */
          await waitUntilDownloadStopped();

          /*
           * Start again.
           *
           * Already cached files are skipped.
           */
          await downloadFullSite();

        })()
      );

      return;
    }


    /* ----------------------------------------
       Cancel completely
       ---------------------------------------- */

    if (
      data.type ===
      "CANCEL_FULL_OFFLINE"
    ) {

      offlineDownload.cancelled = true;
      offlineDownload.paused = false;


      if (
        offlineDownload.controller
      ) {
        offlineDownload
          .controller
          .abort();
      }


      event.waitUntil(
        (async () => {

          /*
           * Wait for download loop to stop
           * before deleting the partial cache.
           */
          await waitUntilDownloadStopped();


          /*
           * Delete ONLY the optional full
           * offline package.
           *
           * Core/runtime cache stays intact.
           */
          await caches.delete(
            FULL_CACHE
          );


          offlineDownload.done = 0;
          offlineDownload.total = 0;
          offlineDownload.cancelled = false;


          await broadcast({
            type:
              "FULL_OFFLINE_CANCELLED"
          });

        })()
      );

      return;
    }


    /* ----------------------------------------
       Status request
       ---------------------------------------- */

    if (
      data.type ===
      "FULL_OFFLINE_STATUS"
    ) {

      event.waitUntil(
        (async () => {

          const ready =
            await caches.match(
              scopedUrl(
                "./__full_offline_ready__"
              )
            );


          event.source?.postMessage({
  type: "FULL_OFFLINE_STATUS",
  ready: Boolean(ready),
  running: offlineDownload.running,
  paused: offlineDownload.paused,
  done: offlineDownload.done,
  total: offlineDownload.total,
  failed: offlineDownload.failed
});

        })()
      );
    }
  }
);