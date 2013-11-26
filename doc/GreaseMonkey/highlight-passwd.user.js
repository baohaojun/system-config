// ==UserScript==
// @name                        Identify formspassword fieldsPassword Fields
// @namespace           http://blog.monstuff.com/archives/cat_greasemonkey.html
// @description         Decorates passwordspassword fieldspassword fields with a background pattern
// @include                     *
// ==/UserScript==
// based on code by Julien Couvreur
// and included here with his gracious permission
var rule = "input[type='password'] { background-image: "    +
      "url(data:image/gif,GIF89a%04%00%04%00%B3%00%00%FF%FF%FF" +
      "%FF%FF%00%FF%00%FF%FF%00%00%00%FF%FF%00%FF%00%00%00%FF"  +
      "%00%00%00%CC%CC%CC%FF%FF%FF%00%00%00%00%00%00%00%00%00"  +
      "%00%00%00%00%00%00%00%00%00!%F9%04%01%00%00%09%00%2C%00" +
      "%00%00%00%04%00%04%00%00%04%070I4k%A22%02%00%3B) }";

var styleNode = document.createElement("style");
styleNode.type = "text/css";
styleNode.innerHTML = rule;
document.getElementsByTagName('head')[0].appendChild(styleNode);
