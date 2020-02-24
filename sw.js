self.addEventListener('install', function (e) {
    e.waitUntil(
        caches.open('video-store').then(function (cache) {
            return cache.addAll([
                '/timer/',
                '/timer/index.html',
                '/timer/static/timer.js',
                '/timer/static/style.css',
                '/timer/static/img/arny_thumbs_up.jpg',
                '/timer/static/sounds/bell.mp3',
                '/timer/static/sounds/alert.mp3',
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