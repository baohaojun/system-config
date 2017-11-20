// ==UserScript==
// @name          stop-weibo-keys
// @namespace     http://github.com/baohaojun/system-config
// @description   Script for stop annoying weibo edit keys.
// @include       https://weibo.com/*
// ==/UserScript==


(function () {
  let eventType = ["keydown", "keyup", "keypress"];

  function isEventOnEditor(ev) {
    var elem = ev.originalTarget;
    if (!elem) return false;
    var elementName = (elem.localName || "").toLowerCase();
    return elementName === "input" || elementName === "textarea";
  }

  function preventEvent(ev) {
    if (!isEventOnEditor(ev) && ev.key !== 'Escape') {
      ev.stopPropagation();
      alert("hello: " + ev);
    }
  }

  function start () {
    alert("hello prevented");
    for (let type of eventType)
      window.addEventListener(type, preventEvent, true);
  }
  window.setTimeout(start, 3000);
})();
