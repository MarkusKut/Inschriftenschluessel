importScripts('precache-manifest.js');

const VERSION = 'v3';
const PRECACHE = `precache-${VERSION}`;
const RUNTIME = `runtime-${VERSION}`;
const META = `meta-${VERSION}`;

const SCOPE = new URL(self.registration.scope).pathname;
const OFFLINE_URL = new URL('offline.html', self.registration.scope).href;

const PRECACHE_URLS = (self.__PRECACHE || []).map(p =>
  new URL(p, self.registration.scope).href
);

async function markReady(value) {
  const cache = await caches.open(META);
  await cache.put(
    new Request('__offline_ready__'),
    new Response(JSON.stringify({ ready: value }), {
      headers: { 'Content-Type': 'application/json' }
    })
  );
}

async function precacheIndividually(urls) {
  const cache = await caches.open(PRECACHE);
  const results = await Promise.allSettled(
    urls.map(async (url) => {
      const res = await fetch(url, { cache: 'no-cache' });
      if (!res.ok) throw new Error(`${url} -> ${res.status}`);
      await cache.put(url, res.clone());
      return url;
    })
  );

  const failed = results
    .filter(r => r.status === 'rejected')
    .map(r => String(r.reason));

  if (failed.length) {
    console.error('Precache failures:', failed);
    await markReady(false);
    throw new Error(`Precache failed for ${failed.length} file(s)`);
  }

  await markReady(true);
}

self.addEventListener('install', event => {
  event.waitUntil((async () => {
    await markReady(false);
    await precacheIndividually(PRECACHE_URLS);
    await self.skipWaiting();
  })());
});

self.addEventListener('activate', event => {
  event.waitUntil((async () => {
    const keys = await caches.keys();
    await Promise.all(
      keys
        .filter(k => ![PRECACHE, RUNTIME, META].includes(k))
        .map(k => caches.delete(k))
    );
    await self.clients.claim();
  })());
});

self.addEventListener('fetch', event => {
  const req = event.request;
  const url = new URL(req.url);

  if (req.method !== 'GET') return;
  if (url.origin !== location.origin) return;

  event.respondWith((async () => {
    const cached = await caches.match(req);
    if (cached) return cached;

    try {
      const res = await fetch(req);
      const copy = res.clone();
      const runtime = await caches.open(RUNTIME);
      runtime.put(req, copy);
      return res;
    } catch (err) {
      if (req.mode === 'navigate') {
        const offline = await caches.match(OFFLINE_URL);
        if (offline) return offline;
      }
      throw err;
    }
  })());
});

self.addEventListener('message', event => {
  if (event.data === 'offline-ready?') {
    event.waitUntil((async () => {
      const cached = await caches.match('__offline_ready__');
      let ready = false;
      if (cached) {
        try {
          const data = await cached.json();
          ready = !!data.ready;
        } catch (_) {}
      }
      event.source?.postMessage({ type: 'offline-ready', ready });
    })());
  }
});