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
   ======================================== */

let offlineDownload = {
  running: false,
  done: 0,
  total: 0
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


async function downloadFullSite() {

  if (offlineDownload.running) {
    return;
  }

  offlineDownload.running = true;
  offlineDownload.done = 0;

  try {

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

    const text =
      await response.text();

    const paths =
      text
        .split(/\r?\n/)
        .map(x => x.trim())
        .filter(Boolean);

    offlineDownload.total =
      paths.length;

    const cache =
      await caches.open(
        FULL_CACHE
      );

    await broadcast({
      type: "FULL_OFFLINE_START",
      total: offlineDownload.total
    });


    /*
     * Download sequentially.
     * Better for mobile than requesting
     * hundreds of files simultaneously.
     */
    for (const path of paths) {

      const url =
        scopedUrl(path);

      try {

        const res =
          await fetch(url);

        if (res.ok) {
          await cache.put(
            url,
            res.clone()
          );
        }

      } catch (error) {
        /*
         * Continue even if one file fails.
         */
      }

      offlineDownload.done++;

      await broadcast({
        type: "FULL_OFFLINE_PROGRESS",
        done: offlineDownload.done,
        total: offlineDownload.total
      });
    }


    /*
     * Marker showing that the complete
     * offline package has been downloaded.
     */
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
      total: offlineDownload.total
    });

  } finally {

    offlineDownload.running = false;
  }
}


/* ========================================
   Messages from the website
   ======================================== */

self.addEventListener(
  "message",
  event => {

    const data =
      event.data || {};


    /*
     * User pressed:
     * "Alle Inhalte herunterladen"
     */
    if (
      data.type ===
      "DOWNLOAD_FULL_OFFLINE"
    ) {

      event.waitUntil(
        downloadFullSite()
      );

      return;
    }


    /*
     * Website asks whether the complete
     * offline package already exists.
     */
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

            running:
              offlineDownload.running,

            done:
              offlineDownload.done,

            total:
              offlineDownload.total
          });

        })()
      );
    }
  }
);
