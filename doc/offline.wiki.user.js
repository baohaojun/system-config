// ==UserScript==
// @name          Fix Offline Wiki Links
// @namespace     http://github.com/baohaojun/windows-config
// @description   Fix offline wikipedia page links
// @include       http://offline-wiki:34567/*
// ==/UserScript==


var docx = document.location + "";

var lang = docx.replace(new RegExp("^http://offline-wiki:34567/(.*?)/.*"), "$1");

var re = new RegExp("^http://offline-wiki:34567/wiki/");
var file_re = new RegExp("^http://offline-wiki:34567/(.*?)/article/File%3[Aa]");
var slash_re = new RegExp("/*$");
for (var x in document.links) { 
    var y = document.links[x].href; 
    if (! y) 
        continue; 

    y = y.replace(re, "http://" + lang + ".wikipedia.org/wiki/");
    y = y.replace(file_re, "http://" + lang + ".wikipedia.org/wiki/File%3A");

    document.links[x].href = y.replace(slash_re, "");
}
