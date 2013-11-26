// ==UserScript==
// @name                  Linkify
// @namespace     http://youngpup.net/userscripts
// @description   Turn plain-linkstext URLstext URLs into linkshyperlinks
// @include *
// ==/UserScript==

// based on code by Aaron Boodman
// and included here with his gracious permission

var urlRegex = /\b(https?:\/\/[^\s+\"\<\>]+)/ig;
var snapTextElements = document.evaluate("//text()[not(ancestor::a) " +
                                         "and not(ancestor::script) and not(ancestor::style) and " +
                                         "contains(translate(., 'HTTP', 'http'), 'http')]",
                                         document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null);
for (var i = snapTextElements.snapshotLength - 1; i >= 0; i--) {
  var elmText = snapTextElements.snapshotItem(i);
  if (urlRegex.test(elmText.nodeValue)) {
    var elmSpan = document.createElement("span");
    var sURLText = elmText.nodeValue;
    elmText.parentNode.replaceChild(elmSpan, elmText);
    urlRegex.lastIndex = 0;
    for (var match = null, lastLastIndex = 0;
         (match = urlRegex.exec(sURLText)); ) {
           elmSpan.appendChild(document.createTextNode(
             sURLText.substring(lastLastIndex, match.index)));
           var elmLink = document.createElement("a");
           elmLink.setAttribute("href", match[0]);
           elmLink.appendChild(document.createTextNode(match[0]));
           elmSpan.appendChild(elmLink);
           lastLastIndex = urlRegex.lastIndex;
         }
    elmSpan.appendChild(document.createTextNode(
      sURLText.substring(lastLastIndex)));
    elmSpan.normalize();
  }
}
