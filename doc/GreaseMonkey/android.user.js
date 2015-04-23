// ==UserScript==
// @name          Fix android sdk manual too slow
// @namespace     http://github.com/baohaojun/system-config
// @description   Fix android sdk manual too slow
// @include       http://developer.android.com/*
// ==/UserScript==

var re = new RegExp("^http://developer.android.com/");
var y = document.URL;
y = y.replace(re, "file:///home/bhj/system-config/bin/Linux/ext/android-sdk-linux_86/docs/");
alert(y);
location.replace(y);


