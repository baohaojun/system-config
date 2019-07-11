// ==UserScript==
// @name          emacs-keys
// @namespace     http://github.com/baohaojun/system-config
// @description   Script for using keys like in emacs
// @include       *
// @grant         GM.setClipboard
// ==/UserScript==


(function () {
  let eventType = ["keydown", "keyup"];

  function isEventOnEditor(ev) {
    var elem = ev.originalTarget;
    if (!elem) return false;
    var elementName = (elem.localName || "").toLowerCase();
    return elementName === "input" || elementName === "textarea";
  }

  let plugins_options = {};

  plugins_options["follow-link.nextrel"]     = 'a[rel="next"]';
  plugins_options["follow-link.prevrel"]     = 'a[rel="prev"]';
  plugins_options["follow-link.targets"]     = 'a[href]';
  plugins_options["follow-link.nextpattern"] = "^次へ|進む|^次.*|下一|続|→|\\bnext|>>|≫|\\bnewer";
  plugins_options["follow-link.prevpattern"] = "\\bback|戻る|^前.*|上一|^<前|←|\\bprev|<<|≪|\\bolder";

  plugins_options["follow-link.site-dir-selector-map"] = {
    "learn.jquery.com" : {
      next : 'div.next a[href]',
      prev : 'div.prev a[href]',
    }
  };

  function followRel(doc, dir, rel, pattern) {
    let target  = doc.querySelector(rel);
    if (target) {
      target.click();
      return;
    }

    let regex   = new RegExp(pattern, "i");
    let targets = doc.querySelectorAll(plugins_options["follow-link.targets"]);

    for (let [x, elem] of targets.entries()) {
      if (regex.test(elem.textContent) /*|| regex.test(elem.value) */) {
        elem.click();
        return;
      }
    }

    let selector_map = plugins_options["follow-link.site-dir-selector-map"][doc.location.host];
    if (selector_map) {
      let selector = selector_map[dir];
      target = doc.querySelector(selector);
      if (target) {
        target.click();
        return;
      }
    }
  }

  function handleEmacsKeys(ev) {
    if (!isEventOnEditor(ev)) {
      if (ev.type == "keydown") {
        if (ev.key == 'N' || ev.key == 'n') {
          followRel(window.document,
                    "next",
                    plugins_options["follow-link.nextrel"],
                    plugins_options["follow-link.nextpattern"]);
        } else if (ev.key == 'P' || ev.key == 'p') {
          followRel(window.document,
                    "prev",
                    plugins_options["follow-link.prevrel"],
                    plugins_options["follow-link.prevpattern"]);
        } else if (ev.code == "Backspace") {
          window.scrollByPages(-1);
        }
      }

      if (ev.key !== "Escape") {
        ev.stopPropagation();
      }
    }

    if (ev.type == "keydown" && ev.key == "F11" && ev.shiftKey && ev.ctrlKey) {
      GM.setClipboard(document.location);
    }
  }

  function start () {
    for (let type of eventType)
      window.addEventListener(type, handleEmacsKeys, true);
  }
  window.setTimeout(start, 100);
})();
