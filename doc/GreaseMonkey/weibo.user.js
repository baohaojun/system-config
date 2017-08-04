// ==UserScript==
// @name          login-newsmth
// @namespace     http://github.com/baohaojun/system-config
// @description   Script for auto login at newsmth
// @include       http://weibo.com/*
// ==/UserScript==


function check_keyup() {
  var all = document.getElementsByTagName("*");

  // document.onkeyup = window.onkeyup = function(e) {
  //   e.stopPropagation();
  //   e.preventDefault();
  //   e.returnValue = false;
  //   e.cancelBubble = true;
  //   return false;
  // }

  for (var i=0, max=all.length; i < max; i++) {
    if (all[i].onkeyup) {
      alert(all[i])
    }
  }

  if (window.onkeyup) {
    alert("window")
  }
}

window.setTimeout(check_keyup, 5000)
