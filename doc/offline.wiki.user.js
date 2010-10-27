// ==UserScript==
// @name          Fix Offline Wiki Links
// @namespace     http://github.com/baohaojun/windows-config
// @description   Fix offline wikipedia page links
// @include       http://localhost:8000/*
// ==/UserScript==

var re = new RegExp("^http://localhost:8000/wiki/");
for (var x in document.links) { 
    var y = document.links[x].href; 
    if (! y) 
        continue; 

    document.links[x].href = y.replace(re, "http://en.wikipedia.org/wiki/");
}
