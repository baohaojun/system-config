// ==UserScript==
// @name          stop-weibo-keys
// @namespace     http://github.com/baohaojun/system-config
// @description   Script for stop annoying weibo edit keys.
// @include       http://weibo.com/*
// ==/UserScript==


(function () {
  let eventType = ["keydown", "keyup"];

  function isEventOnEditor(ev) {
    var elem = ev.originalTarget;
    if (!elem) return false;
    var elementName = (elem.localName || "").toLowerCase();
    return elementName === "input" || elementName === "textarea";
  }

  function preventEvent(ev) {
    if (!isEventOnEditor(ev)) {
      ev.stopPropagation();
    }
  }

  function start () {
    for (let type of eventType)
      window.addEventListener(type, preventEvent, true);
  }
  window.setTimeout(start, 1000);
})();
