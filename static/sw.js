self.addEventListener('install', function (e) {
    e.waitUntil(
        caches.open('video-store').then(function (cache) {
            return cache.addAll([
                '/',
                './index.html',
                './timer.js',
                './style.css',
                './img/arny_thumbs_up.png',
                './sounds/bell.mp3',
                './sounds/alert.mp3',
            ]);
        })
    );
});

self.addEventListener('fetch', function (e) {
    console.log(e.request.url);
    e.respondWith(
        caches.match(e.request).then(function (response) {
            return response || fetch(e.request);
        })
    );
});