
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
// @include       http://bbs.lqqm.net/*
// @exclude       http://diveintogreasemonkey.org/*
// @exclude       http://www.diveintogreasemonkey.org/*
// ==/UserScript==


for (var x in document.images) { 
    var y = document.images[x].src; 
    if (! y) 
        continue; 
    if (! y.match(/mod=attachment&aid=/)) 
        continue; 
    if (y.match(/nothumb/) ) 
        continue; 
    document.images[x].src = y + "&noupdate=yes&nothumb=yes";
}
