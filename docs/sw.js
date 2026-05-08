importScripts('precache-manifest.js');

const VERSION = 'v2';
const PRECACHE = `precache-${VERSION}`;
const RUNTIME = `runtime-${VERSION}`;

const SCOPE = self.registration.scope;
const OFFLINE_URL = new URL('offline.html', SCOPE).href;

const PRECACHE_URLS = (self.__PRECACHE || []).map(p =>
  new URL(p, SCOPE).href
);

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(PRECACHE).then(cache => cache.addAll(PRECACHE_URLS))
  );
  self.skipWaiting();
});

self.addEventListener('activate', event => {
  event.waitUntil(
    caches.keys().then(keys =>
      Promise.all(
        keys
          .filter(k => ![PRECACHE, RUNTIME].includes(k))
          .map(k => caches.delete(k))
      )
    )
  );
  self.clients.claim();
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