
// Hello World! example user script
// version 0.1 BETA!
// 2005-04-25
// Copyright (c) 2005, Mark Pilgrim
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// --------------------------------------------------------------------
//
// This is a Greasemonkey user script.  To install it, you need
// Greasemonkey 0.3 or later: http://greasemonkey.mozdev.org/
// Then restart Firefox and revisit this script.
// Under Tools, there will be a new menu item to "Install User Script".
// Accept the default configuration and install.
//
// To uninstall, go to Tools/Manage User Scripts,
// select "Hello World", and click Uninstall.
//
// --------------------------------------------------------------------
//
// ==UserScript==
// @name          Hello World
// @namespace     http://diveintomark.org/projects/greasemonkey/
// @description   example script to alert "Hello world!" on every page
// @include       http://lqqm.net/*
// @exclude       http://diveintogreasemonkey.org/*
// @exclude       http://www.diveintogreasemonkey.org/*
// ==/UserScript==

//

//javascript: function dood(x) {var lqqm_pattern=new RegExp(".*F=([^&]+).attachpos=([^&]+).attachname=(.+)$"); return x.replace(lqqm_pattern, "http://lqqm.net:8080/top10_MM/$1/$2$3")}; dood('http://lqqm.net/Lqqm.Net/attach/bbscon/4.jpg?B=386&F=M.1215593898.A&attachpos=229614&attachname=/4.jpg')

var lqqm_pattern=new RegExp(".*B=(.+).F=(.+).attachpos=(.+).attachname=(.+)");

function lqqm_replace(match, board, a2, a3, a4) {
    var board_str = "";
    /* alert(board + a2 + a3 + a4); */
    if (board == 41) {
        board_str = "PerPhoto";
    } else if (board == 386) {
        board_str = "top10_MM";
    }
    return "http://lqqm.net:8080/" + board_str + "/" + a2 + "/" + a3 + a4;
}

for (var i in document.images) {
    var qMarkPos=document.images[i].src.indexOf('?');
    if (qMarkPos==-1) {
        /* alert(document.images[i].src + "hello ?"); */
        if (document.images[i].src.match("http://lqqm.net:8080")) {
            document.images[i].src="";
        }
    } else {
        /* alert(document.images[i].src + "hello world"); */
        var src=document.images[i].src.substring(qMarkPos);
        /* alert(src + "hello again"); */
        document.images[i].src=src.replace(lqqm_pattern, lqqm_replace);
        /* alert(document.images[i].src); */
    }
}
