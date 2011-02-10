// ==UserScript==
// @name          Fix Offline Wiki Links
// @namespace     http://github.com/baohaojun/windows-config
// @description   Fix offline wikipedia page links
// @include       http://developer.android.com/*
// ==/UserScript==

var re = new RegExp("^http://developer.android.com/");
for (var x in document.links) { 
    var y = document.links[x].href; 
    if (! y) 
        continue; 

    y = y.replace(re, "file:///home/bhj/external/bin/linux/ext/android-sdk-linux_86/docs/");
    document.links[x].href = y;
}
