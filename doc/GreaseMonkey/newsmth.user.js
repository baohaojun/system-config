// ==UserScript==
// @name          login-newsmth
// @namespace     http://github.com/baohaojun/system-config
// @description   Script for auto login at newsmth
// @include       http://www.newsmth.net/bbs*
// ==/UserScript==


var docx = document.location + "";
// var lang = docx.replace(new RegExp("^http://www.newsmth.net/bbstcon.php/(.*?)/.*"), "$1");

// var re = new RegExp("^http://www.newsmth.net/bbstcon.php/wiki/");
// var file_re = new RegExp("^http://www.newsmth.net/bbstcon.php/(.*?)/article/File%3[Aa]");
// var slash_re = new RegExp("/*$");
var login_re = new RegExp("bbspst");
var found_login = 0;
for (var x in document.links) { 
    var y = document.links[x].href; 

    if (y && y.match(login_re)) {
	found_login = 1;
	alert("hello link: " + y);
	break;
    }
}

if (found_login == 0) {
    alert("you need login\n");
}
