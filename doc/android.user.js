// ==UserScript==
// @name          Fix android sdk manual too slow
// @namespace     http://github.com/baohaojun/windows-config
// @description   Fix android sdk manual too slow
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
