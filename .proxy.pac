// -*- javascript -*-
(function () {
    var blocked_site = 
        ['youtube.com', 'ytimg.com', 'blogger.com',
         'facebook.com', 'twitter.com', 'foxmarks.com',
         'wordpress.com', 'blogspot.com', 'python.org',
         'fbcdn.net', 'groups.google.com', 'developer.android.com',
         'mail-archive.com', 'picasaweb.google.com', 'imageshack',
         'flickr.com', 'radikal.ru', 'com.py',
         'q=cache:', 'tumblr.com', 'imagevenue.com',
         'skneo2.com', 'dailymotion.com', 'wikipedia.org',
         'wikimedia.org', 'feedburner.com', 'newsgator.com',
         'feedproxy.google.com', 'linuxconsulting.ro', 'android-x86.org',
         'docs.google.com', 'google.com/appsstatus', 'sites.google.com',
         'pdk.android.com', 'photobucket.com', 'www.ccthere.com',
         'zh.wikisource.org', 'wikilivres.info', 'wretch.cc',
         'twbbs.org', 'video.google.com', 'groups.google.',
         'book.kanunu.org', 'markmail.org', 'tolchz.net',
         'blog.varunkumar.me', 'webupd8.org', 'www.emacsblog.org',
         'windows-commandline.com', 'vaig.be', 'picturesofmymother.com',
         'xmages.net', 'iphone-dev.org', 'osdir.com', 'www.scribd.com'];
    
    var regexp = new RegExp(blocked_site.join('|'));
    
    FindProxyForURL = function(url, host) {
        try {
            if (url.search(regexp) != -1) {
                return 'SOCKS5 127.0.0.1:8080';
            }
        } catch (e) {
            return 'DIRECT';
        }
        return 'DIRECT';
    }
}) ();
