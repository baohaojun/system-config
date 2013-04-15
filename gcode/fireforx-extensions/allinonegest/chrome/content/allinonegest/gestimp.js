/*
 * gestimp.js
 * For licence information, read licence.txt
 *
 * code for gesture functions
 *
 */
var aioGestTable;
// aioActionTable's 3rd column denotes rocker multiple operations. 0: Not allowed, 1: Allowed 2: Conditional
// 4th column denotes the buddy action if any 
var aioActionTable = [
      [function(){aioBackForward(true);}, "g.browserBack", 2, "1"], // 0
      [function(){aioBackForward(false);}, "g.browserForward", 2, "0"], // 1
      [function(){BrowserReload();}, "g.browserReload", 0, ""], // 2
      [function(){BrowserReloadSkipCache();}, "g.browserReloadSkipCache", 0, ""], // 3
      [function(){BrowserStop();}, "g.browserStop", 0, ""], // 4
      [function(){aioHomePage();}, "g.browserHome", 0, ""], // 5
      [function(){aioOpenNewWindow(false);}, "g.openNewWindow", 0, ""], // 6
      [function(){aioDupWindow();}, "g.duplicateWindow", 0, ""], // 7
      [function(){aioUpDir();}, "g.upDir", 2, ""], // 8
      [function(){aioOpenInNewTab(false);}, "g.browserOpenTabInFg", 0, ""], // 9
      [function(){aioDupTab();}, "g.duplicateTab", 0, ""], // 10
      [function(){aioContent.mTabContainer.advanceSelectedTab(+1, true);}, "g.nextTab", 1, "12"], // 11
      [function(){aioContent.mTabContainer.advanceSelectedTab(-1, true);}, "g.previousTab", 1, "11"], // 12
      [function(){aioRemoveAllTabsBut();}, "g.closeOther", 0, ""], // 13
      [function(){aioRestMaxWin();}, "g.restMaxWin", 1, ""], // 14
      [function(){window.minimize();}, "g.minWin", 0, ""], // 15
      [function(){BrowserFullScreen();}, "g.fullScreen", 1, ""], // 16
      [function(){aioSelectionAsURL();}, "g.openSelection", 0, ""], // 17
      [function(){aioCloseCurrTab(true);}, "g.closeDoc", 0, ""], // 18
      [function(){aioViewSource(0);}, "g.viewPageSource", 0, ""], // 19
      [function(){aioViewSource(1);}, "g.viewFrameSource", 0, ""], // 20
      [function(){aioViewCookies();}, "g.viewSiteCookies", 0, ""], // 21
      [function(){BrowserPageInfo();}, "g.pageInfo", 0, ""], // 22
      [function(){toJavaScriptConsole();}, "g.jsConsole", 0, ""], // 23
      [function(){openAboutDialog();}, "g.about", 0, ""], // 24
      [function(){aioBookmarkCurrentPage();}, "g.addBookmark", 0, ""], // 25
      [function(){aioDoubleWin();}, "g.doubleStackWin", 0, ""], // 26
      [function(){aioSetImgSize(true,false);}, "g.doubleImageSize", 1, "28"], // 27
      [function(){aioSetImgSize(false,false);}, "g.halveImageSize", 1, "27"], // 28
      [function(){aioNukeAnything();}, "g.hideObject", 0, ""], // 29
      [function(){aioZoomEnlarge();}, "g.zoomIn", 1, "31"], // 30
      [function(){aioZoomReduce();}, "g.zoomOut", 1, "30"], // 31
      [function(){aioZoomReset();}, "g.resetZoom", 1, ""], // 32
      [function(){aioActionOnPage(0);}, "g.w3cValidate", 0, ""], // 33
      [function(){aioLinksInWindows();}, "g.linksInWindows", 0, ""], // 34
      [function(){aioLinksInTabs();}, "g.linksInTabs", 0, ""], // 35
      [function(){aioMetaInfo();}, "g.metaInfo", 0, ""], // 36
      [function(){aioVScrollDocument(true,1);}, "g.scrollDown", 1, "38"], // 37
      [function(){aioVScrollDocument(true,-1);}, "g.scrollUp", 1, "37"], // 38
      [function(){aioVScrollDocument(false,0);}, "g.scrollToTop", 0, ""], // 39
      [function(){aioVScrollDocument(false,1000000);}, "g.scrollToBottom", 0, ""], // 40
      [function(){aioResetImgSize(false);}, "g.resetImage", 1, ""], // 41
      [function(){toggleSidebar('viewBookmarksSidebar');}, "g.bookmarkSidebar", 1, ""], // 42
      [function(){aioNukeFlash();}, "g.hideFlash", 0, ""], // 43
      [function(){aioCopyURLToClipBoard();}, "g.URLToClipboard", 0, ""], // 44
      [function(){getWebNavigation().gotoIndex(0);}, "g.firstPage", 0, ""], // 45
      [function(){aioGesturesPage();}, "g.showGestures", 0, ""], // 46
      [function(){aioCloseCurrTab(false);}, "g.closeTab", 0, ""], // 47
      [function(){aioIncURL(1);}, "g.incURL", 2, "49"], // 48
      [function(){aioIncURL(-1);}, "g.decURL", 2, "48"], // 49
      [function(){aioSchemas={};}, "g.clearDigitFlipper", 0, ""], // 50
      [function(){aioLinksInFiles();}, "g.linksInFiles", 0, ""], // 51
      [function(){aioUndoCloseTab();}, "g.undoCloseTab", 0, ""], // 52
      [function(){PrintUtils.printPreview(onEnterPrintPreview, onExitPrintPreview);}, "g.printPreview", 0, ""], //53
      [function(){aioOpenInNewTab(true);}, "g.browserOpenTabInBg", 0, ""], // 54
      [function(){aioDeleteCookies();}, "g.deleteSiteCookies", 0, ""], // 55
      [function(){aioUndoNukeAnything();}, "g.undoHideObject", 0, ""], // 56
      [function(){aioFavoriteURL('1');}, "g.openFav1", 0, ""], // 57
      [function(){aioFavoriteURL('2');}, "g.openFav2", 0, ""], // 58
      [function(){BrowserOpenTab();}, "g.openBlankTab", 0, ""], // 59
      [function(){aioCloseWindow();}, "g.closeWindow", 0, ""], // 60
      [function(){aioOpenNewWindow(true);}, "g.openWindowInBg", 0, ""], // 61
      [function(){aioFrameInfo();}, "g.frameInfo", 0, ""], // 62
      [function(){aioOpenAioOptions();}, "g.aioOptions", 0, ""], // 63
      [function(){toggleSidebar('viewHistorySidebar');}, "g.historySidebar", 1, ""], // 64
      [function(){aioOpenBookmarksManager();}, "g.bookmarkMgr", 0, ""], // 65
      [function(){aioActionOnPage(1);}, "g.translate", 0, ""], // 66
      [function(){aioOpenDownloadManager();}, "g.downloadMgr", 0, ""], // 67
      [function(){saveDocument(window._content.document);}, "g.savePageAs", 0, ""], // 68
      [function(){aioGoToPreviousSelectedTab();}, "g.prevSelectedTab", 1, ""], // 69
      [function(){aioShowHideStatusBar();}, "g.showHideStatusBar", 1, ""], // 70
      [function(){aioSrcEvent.target.ownerDocument.location.reload();}, "g.reloadFrame", 0, ""], // 71
      [function(){aioSetImgSize(true,true);}, "g.enlargeObject", 1, "73"], // 72
      [function(){aioSetImgSize(false,true);}, "g.reduceObject", 1, "72"], // 73
      [function(){aioResetImgSize(true);}, "g.resetSize", 1, ""], //74
      [function(){aioShowHideFindBar();}, "g.showHideFindBar", 1, ""], // 75
      [function(){aioContent.reloadAllTabs();}, "g.reloadAllTabs", 0, ""], // 76
      [function(){aioNextPrevLink(true);}, "g.nextLink", 0, ""], // 77
      [function(){aioFastForward();}, "g.fastForward", 0, ""], // 78
      [function(){aioSelectionAsSearchTerm();}, "g.searchSelection", 0, ""], // 79
      [function(){aioSaveImageAs();}, "g.saveImageAs", 0, ""], // 80
      [function(){aioNextPrevLink(false);}, "g.prevLink", 0, ""], // 81
      [function(){aioGotoLastTab();}, "g.lastTab", 0, ""], // 82
      [function(){aioCopyClipBoardToURLBar();}, "g.pasteAndGo", 0, ""], // 83
      [function(){aioSmartBackForward(-1, false);}, "g.smartBack1", 1, "86"], // 84
      [function(){aioSmartBackForward(-1, true);}, "g.smartBack2", 1, "87"], // 85
      [function(){aioSmartBackForward(+1, false);}, "g.smartForward1", 1, "84"], // 86
      [function(){aioSmartBackForward(+1, true);}, "g.smartForward2", 1, "85"], // 87
      [function(){PrintUtils.print();}, "g.print", 0, ""] //88
//      [function(){aioCloseRightTabs(true);}, "g.CloseAllRightTab", 0, ""], // 89
//      [function(){aioCloseLeftTabs(true);}, "g.CloseAllLeftTab", 0, ""], // 90
//      [function(){aioCloseRightTabs(false);}, "g.CloseRightTab", 0, ""], // 91
//      [function(){aioCloseLeftTabs(false);}, "g.CloseLeftTabs", 0, ""], // 92
//      [function(){aioCloseAllTabs(false);}, "g.CloseAllTabs", 0, ""], // 93
//      [function(){aioHScrollDocument(false,0);}, "g.scrollToLeft", 0, ""], // 94
//      [function(){aioHScrollDocument(false,1000000);}, "g.scrollToRight", 0, ""], // 95
//      [function(){aioCScrollDocument(1000000,1000000);}, "g.scrollToCenter", 0, ""], // 96
//      [function(){aioFullZoomOperation(1);}, "g.FullZoomEnlarge", 0, ""], // 97
//      [function(){aioFullZoomOperation(2);}, "g.FullZoomReduce", 0, ""], // 98
//      [function(){aioFullZoomOperation(0);}, "g.FullZoomReset", 0, ""], // 99
//      [function(){aioOpenAddonManager();}, "g.addOnMgr", 0, ""] // 100
     ];
var aioSchemas = {};
var aioLastTabInfo = [];
var aioUndoHide = [];
var aioUnique = 0;
var aioOrigBlurTab;
const aioKGestures = aioDir + "aiogest.html";

function aioStatusMessage(msg, timeToClear) {
  if (!aioStatusBar) return;
  aioStatusBar.label = msg;
  if (timeToClear) setTimeout(function(){aioStatusMessage("", 0);}, timeToClear );
}

function aioInitGestTable() {
  var i, func, len;
  len = aioActionTable.length;
  if (aioFirst)
     for (i = 0; i < len; ++i) aioActionTable[i][1] = aioGetStr(aioActionTable[i][1])
  var gestTable = aioActionString.split("|");
  var funcTable = aioFuncString.split("|");
  aioGestTable = [];
  for (i = 0; i < gestTable.length; ++i) {
    func = funcTable[i] - 0;
    if (gestTable[i] && func >= 0 && func < len) aioGestTable[gestTable[i]] = func;
  }
}

function aioFireGesture(aGesture) {
  var index = aioGestTable[aGesture];
  if (index == null) {
     index = aioGestTable["+" + aGesture.substr(-2)];
     if (index == null)
        index = aioGestTable["+" + aGesture.substr(-3)];
  }
  if (index == null) {
     index = aioGestTable["/" + aGesture];
     if (index == null) aioStatusMessage(aioUnknownStr + ": " + aGesture, 2000);
     else aioStatusMessage(aioGetStr("g.disabled") + ": " + aioActionTable[index][1], 2000);
  }
  else
     try {
       aioStatusMessage(aioActionTable[index][1], 2000);
       aioActionTable[index][0]();
     }
     catch(err) {}
  aioKillGestInProgress();
  aioDownButton = aioNoB;
}

function aioSetTabId(node) {
  if (!node) return null;
  var nodeId = node.getAttribute("aioTabId");
  if (!nodeId) {
     nodeId = "t" + aioUnique++;
     node.setAttribute("aioTabId", nodeId);
  }
  return nodeId;
}

function aioSetRemoveTab(e) {
  window.removeEventListener("mousemove", aioSetRemoveTab, true);
  if (typeof(aioContent.undoRemoveTab) == "function") return;
  // This code from tabmix
  if (aioFxV35) {
     aioContent.selectIndexAfterRemove = function (oldTab) {
        var currentIndex = this.mCurrentTab._tPos;
        if (this.mCurrentTab != oldTab) return currentIndex;
        var l = this.mTabs.length;
        if (l == 1) return 0;
        switch (aioFocusMode) {
       case 5: // first tab
            return currentIndex == 0 ? 1 : 0;
       case 1: // left tab
            return currentIndex == 0 ? 1 : currentIndex-1 ;
       case 6: // last tab
            return currentIndex == l - 1 ? currentIndex - 1 : l - 1;
       case 7: // last opened
            var lastTab = document.getAnonymousElementByAttribute(this, "linkedpanel",
                                     this.mPanelContainer.lastChild.id);;
            if (lastTab == oldTab && l > 1) {
              lastTab = document.getAnonymousElementByAttribute(this, "linkedpanel",
                                    this.mPanelContainer.childNodes[l-2].id);
            }
            return lastTab._tPos;
       case 2: // last selected
            var lastTab = aioPreviousSelectedTab();
            // if we don't find last selected we fall back to default
            if (lastTab) return lastTab._tPos;
       case 0: // opener / right  (default )
       case 3: // opener
            if ((aioFocusMode == 3 || this.mPrefs.getBoolPref("browser.tabs.selectOwnerOnClose")) && "owner" in oldTab) {
               var owner = oldTab.owner;
               if (owner && owner.parentNode && owner != oldTab)
                  // oldTab and owner still exist just return its position
                  return owner._tPos;
            }
       case 4: // right tab
            return currentIndex == l - 1 ? currentIndex - 1 : currentIndex + 1; //right tab
        } // end switch
     }

     aioOrigBlurTab = aioContent._blurTab;
     aioContent._blurTab = function(aTab) {
        if (this.mCurrentTab != aTab) return;
        var newIndex = this.selectIndexAfterRemove(aTab);
        if (aioTabFocusHistory.length > 1) aioTabFocusHistory.pop();
        if (newIndex > -1) {
          var tab = this.mTabContainer.childNodes[newIndex];
          if (tab && this._removingTabs.indexOf(tab) == -1) {
            this.selectedTab = tab;
            return;
          }
        }
        aioOrigBlurTab.apply(this, arguments);
     }
  }
  else {  // for releases earlier than 3.5
     aioContent.aioNativeRemoveTab = aioContent.removeTab;
     eval("aioContent.aioNativeRemoveTab = " + aioContent.aioNativeRemoveTab.toString().replace(
          'this.selectedTab',
          'if (aioFocusMode == 1) newIndex = (newIndex == 0 || currentIndex != index ? newIndex : currentIndex - 1); \
           this.selectedTab'));
     aioContent.removeTab = function(aTab) {
       if (aTab.localName != "tab") aTab = aioContent.mCurrentTab;
       var lBrowser = aioContent.getBrowserForTab(aTab);
       var ds = lBrowser.docShell;
       if (ds.contentViewer && !ds.contentViewer.permitUnload()) return;
       var tabObj = {};
       tabObj.next = aioSetTabId(aTab.nextSibling);
       tabObj.myId = aioSetTabId(aTab);
       tabObj.hist = lBrowser.sessionHistory;
       aioLastTabInfo.push(tabObj);
       if (aioLastTabInfo.length > 5) aioLastTabInfo.shift();
       aioContent.aioNativeRemoveTab(aTab);
     }
  }
}

function aioCorrectFocus(e) {
  var tabs = aioContent.mTabContainer.childNodes;
  for (var i = 0; i < tabs.length; ++i)
     if (tabs[i].selected && (aioRendering.selectedIndex != i)) {
        aioRendering.selectedIndex = i;
        aioContent.mTabBox.removeEventListener('select', aioCorrectFocus, true);
        return;
     }
}

function aioTabFocus(e) { // derived from "Focus Last Selected Tab" by Daniel Lindkvist
  var lTab;
  if (e.target.localName == "tabs") return;
  var nbTabs = aioRendering.childNodes.length;
  if (!aioFxV35 && nbTabs < aioTabsNb) { // a tab has been closed
     aioTabsNb = nbTabs;
     while (true) {
       if (aioTabFocusHistory.length < 1) break;
       if (aioTabFocusHistory.length > 1) var lHistory = aioTabFocusHistory.pop();
       else lHistory = aioTabFocusHistory[0];
       if (!aioFocusMode || aioFocusMode == 1) break; // standard or to the left mode
       var tabId = aioFocusMode == 2 ? aioTabFocusHistory[aioTabFocusHistory.length - 1].focused
                      : lHistory.openedBy;
       if (!tabId) break;
       for (var i = 0; i < nbTabs; ++i) {
         lTab = aioContent.mTabContainer.childNodes[i];
         if (lTab.getAttribute("aioTabId") == tabId) {
            if (aioContent.mTabContainer.selectedIndex != i) {
               aioContent.mTabContainer.selectedIndex = i;
               e.preventDefault();
               if (typeof(aioContent.moveTabTo) != "function")
                  aioContent.mTabBox.addEventListener('select', aioCorrectFocus, true);
               return;
            }
            break;
         }
       }
       break;
     }
  }
  if (!aioFxV35) aioTabsNb = nbTabs;
  var activeTab = aioContent.mTabContainer.childNodes[aioContent.mTabContainer.selectedIndex];
  var activeId = activeTab.getAttribute("aioTabId");
  if (activeId)
     if (aioTabFocusHistory[aioTabFocusHistory.length - 1].focused == activeId) return; // already at top
     else {
        var openerId = "";
        for (i = 0; i < aioTabFocusHistory.length; ++i) //search for a duplicated entry
          if (aioTabFocusHistory[i].focused == activeId) {
             openerId = aioTabFocusHistory[i].openedBy;
             aioTabFocusHistory.splice(i, 1); // Found: delete it
          }
        aioTabFocusHistory.push({focused: activeId, openedBy: openerId});
     }
  else { // tab's never been visited
     activeId = "t" + aioUnique++;
     activeTab.setAttribute("aioTabId", activeId);
     openerId = aioTabFocusHistory[aioTabFocusHistory.length - 1].focused;
     aioTabFocusHistory.push({focused: activeId, openedBy: openerId});
  }
}

function aioTabLoad(e) {
  if (aioRendering.childNodes.length > aioTabsNb) aioTabsNb = aioRendering.childNodes.length;
}

/*  Gesture actions */

function aioBackForward(back) {
  back ? BrowserBack() : BrowserForward();
  content.focus();
}

function aioPreviousSelectedTab() {
  var lTab;
  if (aioTabFocusHistory.length < 2) return null;
  var tabId = aioTabFocusHistory[aioTabFocusHistory.length - 2].focused;
  for (var i = 0; i < aioContent.mTabs.length; ++i) {
    lTab = aioContent.mTabContainer.childNodes[i];
    if (lTab.getAttribute("aioTabId") == tabId) return lTab;
  }
  return null;
}

function aioGoToPreviousSelectedTab() {
  var lTab = aioPreviousSelectedTab();
  if (lTab) {
     aioTabFocusHistory.pop();
     aioContent.selectedTab = lTab;
  }
}

function aioFavoriteURL(suffix) {
  var shortcutURL = null;
  if (aioFxV3) {
     var bmsvc = Cc["@mozilla.org/browser/nav-bookmarks-service;1"].getService(Ci.nsINavBookmarksService);
     var shortcutURI = bmsvc.getURIForKeyword(aioGetStr("g.keywordForGesture") + suffix);
     if (shortcutURI) shortcutURL = shortcutURI.spec;
  }
  else shortcutURL = BMSVC.resolveKeyword(aioGetStr("g.keywordForGesture") + suffix, { });
  if (!shortcutURL) return;
  if (shortcutURL.toLowerCase().substr(0, 11) != "javascript:" && !aioOpenInCurrTab)
     aioLinkInTab(shortcutURL, true, false);
  else loadURI(shortcutURL);
}

function aioIncURL(inc) { // derived from MagPie by Ben Goodger
  var currSchema, newValue, newIndex, str, url; 
  var currSpec = aioContent.selectedBrowser.webNavigation.currentURI.spec;
  for (var i in aioSchemas) {
     if (currSpec.substr(0, i.length) != i) continue;
     currSchema = aioSchemas[i];
     newValue = parseInt(currSpec.substring(currSchema.startIndex, currSchema.endIndex), 10);
     inc > 0 ? ++newValue : --newValue;
     if (newValue < 0) {
        alert(aioGetStr("noDecrement"));
        return;
     }
     newIndex = newValue.toString();
     str = "";
     for (var j = newIndex.length; j < currSchema.selectedLength; ++j) str += "0";
     str += newIndex;
     url = currSpec.substr(0, currSchema.startIndex) + str + currSpec.substr(currSchema.endIndex);
     aioSchemas[i].endIndex = currSchema.startIndex + str.length;
     aioContent.selectedBrowser.loadURI(url);
     return;
  }

  if (aioTrustAutoSelect) {
     var RE = /\d+/g;
     var rtn, startIndex = -1, endIndex;
     while ((rtn = RE.exec(currSpec)) != null) {
       startIndex = rtn.index;
       endIndex = startIndex + rtn[0].length;
     }
     if (startIndex == -1) return;
     var key = currSpec.substr(0, startIndex);
     aioSchemas[key] = {startIndex: startIndex, endIndex: endIndex, selectedLength: endIndex - startIndex};
     aioIncURL(inc);
     return;
  }

  var rv = { };
  openDialog(aioDir + "aioSchemaBuilder.xul", "", "chrome,centerscreen,modal=yes", currSpec, rv);
  if ("key" in rv) {
     aioSchemas[rv.key] = {startIndex: rv.startIndex, endIndex: rv.endIndex, selectedLength: rv.endIndex - rv.startIndex};
     aioIncURL(inc);
  }
}

function aioShowLocalizedGestures(doc) {
  var imageName;
  const K2 = '<td class="row1" valign="middle" height="30"><span class="forumlink">';
  const K3 = '</span></td><td class="row2" valign="middle"><span class="forumlink">'
  const K4 = '</span></td><td class="row3" valign="middle" align="center">';
  function localized(aStr) {
    if (!aStr) {
       imageName = "nomov";
       return "&nbsp;";
    }
    var lStr = ""; imageName = "";
    for (var i = 0; i < aStr.length; ++i)
        if (aioShortGest[aStr.charAt(i)] == null) {
           imageName = "nomov"; lStr += aStr.charAt(i);
        }
        else lStr += aioShortGest[aStr.charAt(i)];
    if (!imageName)
       if (aStr.length < 5) imageName = aStr.toLowerCase(); else imageName = "nomov";
    return lStr;
  }
  var locGest = aioGetStr("w.gesture").replace(/\'/g, "&#39;");
  var locFunc = aioGetStr("w.function").replace(/\'/g, "&#39;");
  var locMove = aioGetStr("w.move").replace(/\'/g, "&#39;");
  const K1 = '<th width="100" class="thTop" nowrap="nowrap">&nbsp;' + locGest + '&nbsp;</th>';
  const imgURL = '<img src="http://pagesperso-orange.fr/marc.boullet/ext/';
  var divCode = '<table width="100%" cellpadding="2" cellspacing="1" class="forumline">';
  divCode += '<tr><th class="thCornerL" height="30" nowrap="nowrap">&nbsp;' + locFunc + '&nbsp;</th>';
  divCode += K1 + '<th width="50" class="thTop" nowrap="nowrap">&nbsp;' + locMove + '&nbsp;</th>';
  divCode += '<th class="thTop" height="30" nowrap="nowrap">&nbsp;' + locFunc + '&nbsp;</th>';
  divCode += K1 + '<th width="50" class="thCornerR" nowrap="nowrap">&nbsp;' + locMove + '&nbsp;</th></tr>';
  var gestTable = aioActionString.split("|");
  var funcTable = aioFuncString.split("|");
  var func, actionName, j = 0;
  for (var i = 0; i < gestTable.length; ++i) {
     func = funcTable[i] - 0;
     if (func < 0 || func >= aioActionTable.length) {j++; continue;}
     actionName = aioActionTable[func][1].replace(/\'/g, "&#39;");
     if (!((i - j) & 1)) divCode +='<tr>';
     divCode += K2 + actionName + K3 + localized(gestTable[i]) + K4;
     divCode += imgURL + imageName + '.png"></td>';
     if ((i-j) & 1) divCode += '</tr>'
  }
  if ((gestTable.length - j) & 1) {
     divCode += K2 + "&nbsp;" + K3 + "&nbsp;" + K4;
     divCode += imgURL + 'nomov.png"></td></tr>';
  }
  divCode += '</table>';
  var title = aioGetStr("w.gestTable").replace(/\'/g, "&#39;");
  var str = "(function(){window.addEventListener('load',function(e){document.title='" + title +
       "';document.body.innerHTML='" + divCode + "';},false);})();"
  var script = doc.createElement("script");
  script.appendChild(doc.createTextNode(str));
  doc.body.appendChild(script);
}

function aiogestDOMLoaded(e) {  
  var doc = e.originalTarget.defaultView.document;
  if (doc.location.href == aioKGestures) aioShowLocalizedGestures(doc);
//  aioContent.removeEventListener("DOMContentLoaded", aiogestDOMLoaded, false);
}

function aioGesturesPage() {
  aioContent.addEventListener("DOMContentLoaded", aiogestDOMLoaded, false);
  aioLinkInTab(aioKGestures, true, false);
}

function aioCopyURLToClipBoard() {
  var lstr = "";
  try {
    var str = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
    if (!str) return;
    if (aioOnLink.length) {
       for (var i = 0; i < aioOnLink.length; ++i) {
           if (lstr) lstr += "\r\n";
           lstr += aioOnLink[i].href;
       }
       str.data = lstr;
    }
    else str.data = window._content.document.location.href;
    var trans = Components.classes["@mozilla.org/widget/transferable;1"].createInstance(Components.interfaces.nsITransferable);
    if (!trans) return;
	// Since data from the web content are copied to the clipboard, the privacy context must be set.
	var sourceWindow = aioSrcEvent.target.ownerDocument.defaultView;
    var privacyContext = sourceWindow.QueryInterface(Components.interfaces.nsIInterfaceRequestor).
                                  getInterface(Components.interfaces.nsIWebNavigation).
                                  QueryInterface(Components.interfaces.nsILoadContext);
	if ("init" in trans) trans.init(privacyContext);
    trans.addDataFlavor("text/unicode");
    trans.setTransferData("text/unicode", str, str.data.length * 2);
    var clipId = Components.interfaces.nsIClipboard;
    var clip = Components.classes["@mozilla.org/widget/clipboard;1"].getService(clipId);
    if (!clip) return;
    clip.setData(trans, null, clipId.kGlobalClipboard);
  }
  catch(err) {}
}

function aioCopyClipBoardToURLBar() {
  var clip = Components.classes["@mozilla.org/widget/clipboard;1"].getService(Components.interfaces.nsIClipboard);
  if (!clip) return;
  var trans = Components.classes["@mozilla.org/widget/transferable;1"].createInstance(Components.interfaces.nsITransferable);
  if (!trans) return;
  if ("init" in trans) trans.init(null);
  trans.addDataFlavor("text/unicode");
  clip.getData(trans, clip.kGlobalClipboard);
  var data = {}, dataLen = {};
  try {
    trans.getTransferData("text/unicode", data, dataLen);
  }
  catch(err) {
    alert(aioGetStr("g.clipboardEmpty"));
  }
  if (data) {
     data = data.value.QueryInterface(Components.interfaces.nsISupportsString);
     var url = data.data.substring(0, dataLen.value / 2);
     if (gURLBar) gURLBar.value = url;
     aioLinkInTab(url, true, false);
  }
}

function getElemsByTagNameForAllFrames(frameDoc, tagName) {
  var embeds = [];
  var frames = frameDoc.getElementsByTagName("frame");
  for (var i = 0; i < frames.length; ++ i)
      embeds = embeds.concat(getElemsByTagNameForAllFrames(frames[i].contentDocument, tagName));
  frames = frameDoc.getElementsByTagName("iframe");
  for (i = 0; i < frames.length; ++ i)
      embeds = embeds.concat(getElemsByTagNameForAllFrames(frames[i].contentDocument, tagName));
  var lEmbeds = frameDoc.getElementsByTagName(tagName);
  for (i = 0; i < lEmbeds.length; ++i) embeds.push(lEmbeds[i]);
  return embeds;
}

function aioNukeFlash() {
  var currFlash, height, width, top, next, span, text, view, disp, style;
  var topDocument = aioSrcEvent.target.ownerDocument.defaultView.top.document;
  var embeds = getElemsByTagNameForAllFrames(topDocument, "embed");
  for (var i = 0; i < embeds.length; ++i) {
    currFlash = embeds[i];
    if (currFlash.getAttribute("type") != "application/x-shockwave-flash") continue;
    if (currFlash.parentNode.nodeName.toLowerCase() == "object") {
       top = currFlash.parentNode.parentNode; next = currFlash.parentNode;
    }
    else {
       top = currFlash.parentNode; next = currFlash;
    }
    if (next.previousSibling && next.previousSibling.nodeName.toLowerCase() == "span"
        && next.previousSibling.hasAttribute("aioFlash")) continue;
    view = next.ownerDocument.defaultView;
    width = parseInt(view.getComputedStyle(next, "").getPropertyValue("width"));
    height = parseInt(view.getComputedStyle(next, "").getPropertyValue("height"));
    disp = view.getComputedStyle(next, "").getPropertyValue("display");
    if (height && width) {
       style = next.getAttribute("style") || "";
       next.setAttribute("style", style + "display:none;");
       span = document.createElementNS(xhtmlNS, "span");
       text = document.createTextNode("[" + aioGetStr("g.clickToView") + "]");
       span.appendChild(text);
       top.insertBefore(span, next);
       span.setAttribute("style", "height:" + (height - 2) + "px;width:" + (width - 2) + "px;border:1px solid black;display:-moz-inline-box;");
       span.setAttribute("aioFlash", disp);
       span.addEventListener("click", aioPlayFlash, true);
    }
  }
}

function aioPlayFlash(e) {
  e.currentTarget.removeEventListener("click", aioPlayFlash, true);
  var flashNode = e.currentTarget.nextSibling;
  var disp = e.currentTarget.getAttribute("aioFlash");
  e.currentTarget.parentNode.removeChild(e.currentTarget);
  var style = flashNode.getAttribute("style") || "";
  flashNode.setAttribute("style", style + "display:" + disp + ";");
}

function aioNukeAnything() {
  var node = aioSrcEvent.target;
  if (!node) return;
  var view = node.ownerDocument.defaultView;
  var disp = view.getComputedStyle(node, "").getPropertyValue("display");
  node.setAttribute("aioDisp", "display:" + disp + ";");
  var style = node.getAttribute("style") || "";
  node.setAttribute("style", style + "display:none;");
  aioUndoHide.push(node);
}

function aioUndoNukeAnything() {
  try {
    var node = aioUndoHide.pop();
    if (!node || !node.hasAttribute("aioDisp")) return;
    var style = node.getAttribute("style") || "";
    node.setAttribute("style", style + node.getAttribute("aioDisp"));
  }
  catch(err) {}
}

function aioVScrollDocument(relativeScroll, aValue) {
  var scrollObj = aioFindNodeToScroll(aioSrcEvent.target);
  if (scrollObj.scrollType >= 2) return;
  if (relativeScroll) {
     var val = Math.round(scrollObj.realHeight * 0.8 * aValue)
     if (!val) val = aValue;
     if (scrollObj.isXML) aioSrcEvent.view.scrollBy(0, val)
     else scrollObj.nodeToScroll.scrollTop += val;
  }     
  else
     if (scrollObj.isXML) aioSrcEvent.view.scrollTo(0, aValue)
     else scrollObj.nodeToScroll.scrollTop = aValue;
}

function aioHScrollDocument(relativeScroll, aValue) { // contributed by Ingo Fröhlich aka get-a-byte
  var scrollObj = aioFindNodeToScroll(aioSrcEvent.target);
  if (scrollObj.scrollType & 1) return; // 1 = vertical scroll only  3 = no scrolling
  if (relativeScroll) {
     var val = Math.round(scrollObj.realWidth * 0.8 * aValue);
     if (!val) val = aValue;
     if (scrollObj.isXML) aioSrcEvent.view.scrollBy(val, 0);
     else scrollObj.nodeToScroll.scrollLeft += val;
  }
  else
     if (scrollObj.isXML) aioSrcEvent.view.scrollTo(aValue, 0)
     else scrollObj.nodeToScroll.scrollLeft = aValue;
}

function aioCScrollDocument(aValueH, aValueV) { // contributed by Ingo Fröhlich aka get-a-byte
  var scrollObj = aioFindNodeToScroll(aioSrcEvent.target);
  if (scrollObj.scrollType == 3) return;
  if (scrollObj.isXML)
     aioSrcEvent.view.scrollTo(aValueH, aValueV);
  else {
     scrollObj.nodeToScroll.scrollLeft = aValueH;
     scrollObj.nodeToScroll.scrollTop = aValueV;
  }
  if (scrollObj.isXML)
     aioSrcEvent.view.scrollTo(Math.round(aioSrcEvent.view.scrollLeft / 2),
                               Math.round(aioSrcEvent.view.scrollTop / 2));
  else {
     scrollObj.nodeToScroll.scrollLeft = Math.round(scrollObj.nodeToScroll.scrollLeft / 2);
     scrollObj.nodeToScroll.scrollTop = Math.round(scrollObj.nodeToScroll.scrollTop / 2);
  }
}

function aioSelectionAsURL() {
  var focusedWindow = document.commandDispatcher.focusedWindow;
  var winWrapper = new XPCNativeWrapper(focusedWindow, 'getSelection()');
  var url = winWrapper.getSelection().toString();
  if (!url) return;
  // the following by Ted Mielczarek
  url = url.replace(/\s/g, ""); // Strip any space characters
  url = url.replace(/^[^a-zA-Z0-9]+/, ""); // strip bad leading characters
  url = url.replace(/[\.,\'\"\)\?!>\]]+$/, ""); // strip bad ending characters
  if (!url) return;
  url.replace(/\\/g,"/"); // change \ to /
  if (url.indexOf(".") == -1) return; // can't be valid
  if (url.search(/^\w+:/) == -1) // make sure it has some sort of protocol
     if (url.indexOf("@") == -1) url = "http://" + url;
     else url = "mailto:" + url;
  aioLinkInTab(url, true, false);
}

function aioSelectionAsSearchTerm() {
  var focusedWindow = document.commandDispatcher.focusedWindow;
  var winWrapper = new XPCNativeWrapper(focusedWindow, 'getSelection()');
  var searchStr = winWrapper.getSelection().toString();
  if (!searchStr) return;
  if (typeof(BrowserSearch) == "undefined") {
     var searchBar = document.getElementById("searchbar");
     if (!searchBar) return;
     searchBar = searchBar.mTextbox;
     if (!searchBar) return;
     searchBar.value = searchStr;
     searchBar.mEnterEvent = null;
     if (typeof(SearchButton) == "undefined") BrowserOpenTab();
     searchBar.onTextEntered();
  }
  else
     if (!aioFxV3) {  
        searchBar = BrowserSearch.getSearchBar();
        if (!searchBar) return;
        searchBar.removeAttribute("empty");
        var textBox = searchBar._textbox;
        textBox.value = searchStr;
        textBox._formHistSvc.addEntry(textBox.getAttribute("autocompletesearchparam"), searchStr);
        searchBar.doSearch(searchStr, true);
     }
     else {
        searchBar = BrowserSearch.searchBar;
        if (!searchBar) return;
        searchBar.value = searchStr;
        BrowserSearch.loadSearch(searchStr, true);
     }
}

function aioZoomEnlarge() {
  if (aioFxV3) {
     var toggleZoom = ZoomManager.useFullZoom;
     if (toggleZoom) ZoomManager.useFullZoom = false;
     FullZoom.enlarge();
     ZoomManager.useFullZoom = toggleZoom;
     return;
  }
//  if (TextZoom.enlarge) TextZoom.enlarge();
  else ZoomManager.prototype.getInstance().enlarge();
}

function aioZoomReduce() {
  if (aioFxV3) {
     var toggleZoom = ZoomManager.useFullZoom;
     if (toggleZoom) ZoomManager.useFullZoom = false;
     FullZoom.reduce();
     ZoomManager.useFullZoom = toggleZoom;
     return;
  }   
//  if (TextZoom.reduce) TextZoom.reduce();
  else ZoomManager.prototype.getInstance().reduce();
}

function aioZoomReset() {
  if (aioFxV3) {
     FullZoom.reset();
     return;
  }   
//  if (TextZoom.reset) TextZoom.reset();
  else ZoomManager.prototype.getInstance().reset();
}

function aioFullZoomOperation(aOper) {
  if (!aioFxV3) return;
  if (aOper) {
     var toggleZoom = ZoomManager.useFullZoom;
     if (!toggleZoom) ZoomManager.useFullZoom = true;
     if (aOper == 1) FullZoom.enlarge();
     else FullZoom.reduce();
     ZoomManager.useFullZoom = toggleZoom;
  }
  else FullZoom.reset();
}

function aioImageResize(s) {
  aioOnImage.removeAttribute("width");
  aioOnImage.removeAttribute("height");
  var style = aioOnImage.getAttribute("style") || "";
  aioOnImage.setAttribute("style", style + s);
}

function aioSetImgSize(aEnlarge, aMixed) {
  if (!aioOnImage) {
     if (!aMixed) return;
     if (aEnlarge) aioZoomEnlarge();
     else aioZoomReduce();
     return;
  }
  var imgStr = aioOnImage.getAttribute("aioImgSize");
  if (!imgStr) {
     var view = aioOnImage.ownerDocument.defaultView;
     var w = parseInt(view.getComputedStyle(aioOnImage, "").getPropertyValue("width"));
     var h = parseInt(view.getComputedStyle(aioOnImage, "").getPropertyValue("height"));
     var imgTab = [];
     imgTab[0] = w; imgTab[1] = h; imgTab[2] = 1;
  }
  else imgTab = imgStr.split("|");
  imgTab[2] *= aEnlarge ? 2 : 0.5;
  aioOnImage.setAttribute("aioImgSize", imgTab.join("|"));
  w = Math.round(imgTab[0] * imgTab[2]); h = Math.round(imgTab[1] * imgTab[2]);
  if (w && h) aioImageResize("width:" + w + "px; height:" + h + "px;");
  else aioResetImgSize(false);
}

function aioResetImgSize(aMixed) {
  if (!aioOnImage) {
     if (!aMixed) return;
     aioZoomReset();
     return;
  }
  var imgStr = aioOnImage.getAttribute("aioImgSize");
  if (!imgStr) return;
  var imgTab = imgStr.split("|");
  imgTab[2] = "1";
  aioOnImage.setAttribute("aioImgSize", imgTab.join("|"));
  aioImageResize("width:" + imgTab[0] + "px; height:" + imgTab[1] + "px;");
}

function aioSaveImageAs() {
  if (!aioOnImage) return;
  if (aioFxV18) saveImageURL(aioOnImage.src, null, "SaveImageTitle", false, false,
                         aioOnImage.ownerDocument.documentURIObject, aioOnImage.ownerDocument);
  else 
     if (aioFxV3) saveImageURL(aioOnImage.src, null, "SaveImageTitle", false,
                            false, aioOnImage.ownerDocument.documentURIObject);
     else saveURL(aioOnImage.src, null, "SaveImageTitle", false);

}

function aioCloseCurrTab(lastTabClosesWindow) {
  if (aioRendering.childNodes.length > 1 || !lastTabClosesWindow) aioContent.removeCurrentTab();
  else if (typeof(BrowserCloseWindow) == "function") BrowserCloseWindow();
       else closeWindow(true);
}

function aioUndoCloseTab() {
  if (aioFxV3) {
     undoCloseTab();
     return;
  }
  if (typeof(aioContent.undoRemoveTab) == "function") {
     aioContent.undoRemoveTab();
     return;
  }
  if (!aioLastTabInfo.length) return;
  var nbTabs = aioContent.mTabContainer.childNodes.length;
  var lastTabInfo = aioLastTabInfo.pop();
  var lTab = aioContent.addTab();
  aioContent.selectedTab = lTab;
  if (lastTabInfo.hist.count) {
     var history = aioContent.webNavigation.sessionHistory;
     history.QueryInterface(Components.interfaces.nsISHistoryInternal);
     for (var i = 0; i < lastTabInfo.hist.count; ++i)
        history.addEntry(lastTabInfo.hist.getEntryAtIndex(i, false), true);
     aioContent.webNavigation.gotoIndex(lastTabInfo.hist.index);
  }
  if (typeof(aioContent.moveTabTo) != "function" || !lastTabInfo.next) return;
  for (i = 0; i < nbTabs; ++i)
     if (aioContent.mTabContainer.childNodes[i].getAttribute("aioTabId") == lastTabInfo.next) {
        aioContent.moveTabTo(lTab, i);
        break;
     }
}

function aioGotoLastTab() {
  aioContent.selectedTab = aioContent.mTabContainer.childNodes[aioContent.mTabContainer.childNodes.length - 1];
}

function aioWarnOnCloseMultipleTabs(numToClose) {
     var promptService = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
                                   .getService(Components.interfaces.nsIPromptService);
     var bundle = aioContent.mStringBundle;
     window.focus();
     var button = promptService.confirmEx(
              window,
              bundle.getString("tabs.closeWarningTitle"),
              bundle.getFormattedString("tabs.closeWarningMultipleTabs", [numToClose]),
              (promptService.BUTTON_TITLE_IS_STRING * promptService.BUTTON_POS_0)
              + (promptService.BUTTON_TITLE_CANCEL * promptService.BUTTON_POS_1),
              bundle.getString("tabs.closeButtonMultiple"),
              null, null, null,
              {value : 0});
     return button == 0;
}

function aioRemoveAllTabsBut() {
  var ltab = aioContent.mCurrentTab;
  if (ltab.pinned) return;
  var childNodes = aioContent.mTabContainer.childNodes;
  var numToClose = childNodes.length - 1;
  if (!aioNeverWarnOnCloseOtherTabs && numToClose > 1)
     var reallyClose = aioWarnOnCloseMultipleTabs(numToClose);
  else reallyClose = true;
  if (!reallyClose) return;
  for (var i = childNodes.length - 1; i >= 0; --i)
    if (childNodes[i] != ltab && !childNodes[i].pinned) aioContent.removeTab(childNodes[i]);
}

function aioCloseRightTabs(all) { // contributed by Holger Kratz
  var childNodes = aioContent.mTabContainer.childNodes;
  var tabPos = aioContent.mCurrentTab._tPos;
  if (tabPos >= childNodes.length - 1) return;
  if (all) {
     var numToClose = childNodes.length - tabPos - 1;
     if (!aioNeverWarnOnCloseOtherTabs && numToClose > 1)
        var reallyClose = aioWarnOnCloseMultipleTabs(numToClose);
     else reallyClose = true;
     if (!reallyClose) return;
     for (var i = childNodes.length - 1; i > tabPos; --i)
       aioContent.removeTab(childNodes[i]);
  }
  else aioContent.removeTab(childNodes[tabPos + 1]);
}

function aioCloseLeftTabs(all) { // contributed by Holger Kratz
  var childNodes = aioContent.mTabContainer.childNodes;
  var tabPos = aioContent.mCurrentTab._tPos;
  if (tabPos == 0) return;
  if (all) var end = 0; else end = tabPos - 1;
  var numToClose = tabPos - end;
  if (!aioNeverWarnOnCloseOtherTabs && numToClose > 1)
     var reallyClose = aioWarnOnCloseMultipleTabs(numToClose);
  else reallyClose = true;
  if (!reallyClose) return;
  for (var i = tabPos - 1; i >= end; --i)
    aioContent.removeTab(childNodes[i]);
}

function aioCloseAllTabs() { // contributed by Holger Kratz
  var childNodes = aioContent.mTabContainer.childNodes;
  var numToClose = childNodes.length;
  if (!aioNeverWarnOnCloseOtherTabs && numToClose > 1)
     var reallyClose = aioWarnOnCloseMultipleTabs(numToClose);
  else reallyClose = true;
  if (!reallyClose) return;
  for (var i = childNodes.length - 1; i >= 0; --i)
    aioContent.removeTab(childNodes[i]);
}

function aioGetReferrer() {
  try {
    var refURL = aioSrcEvent.target.ownerDocument.location.href;
    var ioService = Components.classes["@mozilla.org/network/io-service;1"]
                    .getService(Components.interfaces.nsIIOService);
    if (refURL) return ioService.newURI(refURL, null, null);
  }
  catch (e) {}
  return null;
}

function aioLinkInTab(url, usePref, bg) {
  var tab = aioContent.addTab(url, aioGetReferrer());
  var loadInBg = (usePref && (aioPrefRoot.getBoolPref("browser.tabs.loadInBackground") != bg)) || (!usePref && bg);
  if (!loadInBg) aioContent.selectedTab = tab;
}

function aioDupTab() {
  if (aioFxV3) {
     var tab = aioContent.duplicateTab(aioContent.mCurrentTab);
     if (!aioPrefRoot.getBoolPref("browser.tabs.loadInBackground")) aioContent.selectedTab = tab;
  }
  else aioLinkInTab(window._content.document.location.href, true, false);
}

function aioMarkLinkVisited(href, linkNode) {
  if (typeof(markLinkVisited) == "function") markLinkVisited(href, linkNode);
}

function aioOpenInNewTab(bg) {
  if (aioOnLink.length) {
     aioLinkInTab(aioOnLink[0].href, false, bg);
     aioMarkLinkVisited(aioOnLink[0].href, aioOnLink[0].node);
  }
  else
     if (!bg) BrowserOpenTab();
     else aioLinkInTab("about:blank", false, true);
}

function aioLinksInTabs() {
  for (var i = 0; i < aioOnLink.length; ++i) {
    aioContent.addTab(aioOnLink[i].href, aioGetReferrer());
    aioMarkLinkVisited(aioOnLink[i].href, aioOnLink[i].node);
  }
}

function aioLinksInFiles() {
  var hRefLC, dontAskBefore;
  try {dontAskBefore = aioPrefRoot.getBoolPref("browser.download.useDownloadDir");}
  catch(err) {dontAskBefore = false;}
  for (var i = 0; i < aioOnLink.length; ++i) {
    hRefLC = aioOnLink[i].href.toLowerCase();
    if (hRefLC.substr(0, 7) != "mailto:" && hRefLC.substr(0, 11) != "javascript:" &&
        hRefLC.substr(0, 5) != "news:" && hRefLC.substr(0, 6) != "snews:")
       if (aioFxV18) saveURL(aioOnLink[i].href, aioGetTextForTitle(aioOnLink[i].node), null, true, dontAskBefore,
	                      aioOnLink[i].node.ownerDocument.documentURIObject, aioOnLink[i].node.ownerDocument);
	   else saveURL(aioOnLink[i].href, aioGetTextForTitle(aioOnLink[i].node), null, true, dontAskBefore);

  }
}

function aioNewWindowFor1_0(url, flag) {
  var handler = Components.classes["@mozilla.org/commandlinehandler/general-startup;1?type=browser"].getService()
                .QueryInterface(Components.interfaces.nsICmdLineHandler);
  var startpage = url || handler.defaultArgs;
  var charsetArg = null;
  if (window._content && window._content.document)
     charsetArg = "charset=" + window._content.document.characterSet;
  return window.openDialog(getBrowserURL(), "_blank", "chrome,all,dialog=no" + flag, startpage, charsetArg);
}

function aioNewWindowFor1_5(url, flag) {
  var handler = Components.classes["@mozilla.org/browser/clh;1"].getService(Components.interfaces.nsIBrowserHandler);
  var startpage = url || handler.defaultArgs;
  if (window._content && window._content.document) {
     var charsetArg = "charset=" + window._content.document.characterSet;
     return window.openDialog("chrome://browser/content/", "_blank", "chrome,all,dialog=no" + flag, startpage, charsetArg);
  }
  return window.openDialog("chrome://browser/content/", "_blank", "chrome,all,dialog=no" + flag, startpage);
}

function aioNewWindow(url, flag) {
  if (aioFxV1_0) return aioNewWindowFor1_0(url, flag);
  else return aioNewWindowFor1_5(url, flag);
}

function aioLinksInWindows() {
  if (!aioOnLink.length) return;
  if (aioSingleNewWindow) {
     var hRefTable = [];
     for (var i = 0; i < aioOnLink.length; ++i) {
        hRefTable[i] = aioOnLink[i].href;
        aioMarkLinkVisited(aioOnLink[i].href, aioOnLink[i].node);
     }
     aioNewWindow(hRefTable.join("|"), "");
  }
  else
     for (i = 0; i < aioOnLink.length; ++i) {
        aioNewWindow(aioOnLink[i].href, "");
        aioMarkLinkVisited(aioOnLink[i].href, aioOnLink[i].node);
     }
}

function aioSetToNormalZ(aWindow) {
  window.focus();
  var treeowner = aWindow.QueryInterface(Components.interfaces.nsIInterfaceRequestor).getInterface(Components.interfaces.nsIWebNavigation)
                     .QueryInterface(Components.interfaces.nsIDocShellTreeItem).treeOwner;
  var xulwin = treeowner.QueryInterface(Components.interfaces.nsIInterfaceRequestor).getInterface(Components.interfaces.nsIXULWindow);
  xulwin.zLevel = xulwin.normalZ;
}

function aioOpenNewWindow(background) {
  var s = background ? ",alwaysLowered" : "";
  if (aioOnLink.length) {
     var win = aioNewWindow(aioOnLink[0].href, s);
     aioMarkLinkVisited(aioOnLink[0].href, aioOnLink[0].node);
  }
  else
     if (aioOnImage) win = aioNewWindow(aioOnImage.src, s);
     else win = aioNewWindow("", s);
  if (background) setTimeout(function(a){aioSetToNormalZ(a);}, 200, win);
}

function aioDupWindow() {
  aioNewWindow(window._content.document.location.href, "");
}

function aioCloseWindow() {
  var numTabs = aioRendering.childNodes.length;
  if (numTabs <= 1 || !aioPrefRoot.getBoolPref("browser.tabs.warnOnClose"))
     if (typeof(BrowserCloseWindow) == "function") BrowserCloseWindow();
     else closeWindow(true);
  else if (aioContent.warnAboutClosingTabs(true))
          if (typeof(BrowserCloseWindow) == "function") BrowserCloseWindow();
          else closeWindow(true);
}

function aioDoubleWin() {
  if (!aioOnLink.length) return;
  window.moveTo(0, 0);
  window.resizeTo(screen.availWidth / 2, screen.availHeight);
  aioNewWindow(aioOnLink[0].href, "");
  window.moveTo(screen.availWidth / 2, 0);
  aioMarkLinkVisited(aioOnLink[0].href, aioOnLink[0].node);
}

function aioNextPrevLink(next) { // submitted by Christian Kruse
  if (next && document.getElementById("nextPleasePopupMenu")) {
     document.getElementById("nextPleasePopupMenu").doCommand();
     return;
  }
  if (!next && document.getElementById("prevPleasePopupMenu")) {
     document.getElementById("prevPleasePopupMenu").doCommand();
     return;
  }
  var re = [];
  var relStr = next ? "next" : "prev" ;
  var doc = aioSrcEvent.target.ownerDocument;
  var links = doc.getElementsByTagName("link");
  var imgElems;
  for (var i = 0; i < links.length; ++i)
    if (links[i].getAttribute("rel") && links[i].getAttribute("rel").toLowerCase() == relStr)
       if (links[i].href) {loadURI(links[i].href); return;}
  if (!aioNextsString) return;
  var nextArray = next ? aioNextsString.split("|") : aioPrevsString.split("|");
  for (var j = 0; j < nextArray.length; ++j)
     re[j] = new RegExp(nextArray[j], "i");
  links = doc.links;
  for (j = 0; j < re.length; ++j)
    for (i = 0; i < links.length; ++i) // search for exact match
      if (links[i].textContent && links[i].textContent.search(re[j]) != -1 &&
          nextArray[j].length == links[i].textContent.length && links[i].href) {
         loadURI(links[i].href);
         return;
      }
  for (j = 0; j < re.length; ++j)
    for (i = 0; i < links.length; ++i) { // search for partial match
      if (links[i].textContent && links[i].textContent.search(re[j]) != -1 && links[i].href) {
         loadURI(links[i].href);
         return;
      }
      imgElems = links[i].getElementsByTagName("img"); // Is it an image tag?
      if (imgElems.length > 0 && imgElems[0].src && imgElems[0].src.search(re[j]) != -1 && links[i].href) {
         loadURI(links[i].href);
         return;
      }
    }
}

function aioSmartBackForward(aRwnd, aCurrDomainEndPoint) { // derived from SHIMODA "Piro" Hiroshi Rewind/Fastforward buttons
  var webNav = getWebNavigation();
  var sessionH = webNav.sessionHistory;
  var lURI = sessionH.getEntryAtIndex(sessionH.index, false).URI;
  var c_host = lURI ? lURI.host : null ;
  var check = (aRwnd == -1) ? function(aInd) {return aInd >= 0;} : function(aInd) {return aInd < sessionH.count;}
  var start = sessionH.index + aRwnd;
  var t_host;
  for (var i = start; check(i); i += aRwnd) {
     lURI  = sessionH.getEntryAtIndex(i, false).URI;
     t_host = lURI ? lURI.host : null ;
     if ((c_host && !t_host) || (!c_host && t_host) || (c_host != t_host)) {
        if (aCurrDomainEndPoint) {
           if (i == start) {
              c_host = t_host;
              continue;
           }
           i -= aRwnd;
        }
	webNav.gotoIndex(i);
        return;
     }
  }
  webNav.gotoIndex((aRwnd == -1) ? 0 : sessionH.count - 1 );
}

function aioFastForward() {
  var sessionH = getWebNavigation().sessionHistory;
  if (sessionH.index < 0 || sessionH.count <= 0) return; // Firefox bug
  if (sessionH.index + 1 < sessionH.count) BrowserForward();
  else aioNextPrevLink(true);
}

function aioHomePage() {
  var homePage = gHomeButton.getHomePage();
  if (!aioGoUpInNewTab) {
     loadOneOrMoreURIs(homePage);
     return;
  }
  var urls = homePage.split("|");
  var firstTabAdded = aioContent.addTab(urls[0]);
  for (var i = 1; i < urls.length; ++i) aioContent.addTab(urls[i]);
  aioContent.selectedTab = firstTabAdded;
  content.focus();
}

function aioUpDir() { // from Stephen Clavering's GoUp
  function getUp(url) {
    var origUrl = url;
    // trim filename (this makes subdriectory digging easier)
    var matches = url.match(/(^.*\/)(.*)/);
    if (!matches) return ""; //only fails if "url" has no /'s
    url = matches[1];
    if (url != origUrl && !/(index|main)\.(php3?|html?)/i.test(url)) return url;
    // dig through subdirs
    matches = url.match(/^([^\/]*?:\/\/.*\/)[^\/]+?\//);
    if (matches) return matches[1];
    // we've reach (ht|f)tp://foo.com/, climb up through subdomains
    // split into protocol and domain
    matches = url.match(/([^:]*:\/\/)?(.*)/);
    var protocol = matches[1];
    var domain = matches[2];
    matches = domain.match(/^[^\.]*\.(.*)/);
    if (matches) return (protocol + matches[1]);
    // nothing found
    return "";
  }
  var url = getUp(window._content.document.location.href);
  if (!url) return;
  if (aioGoUpInNewTab) aioLinkInTab(url, false, false);
  else loadURI(url);
}

function aioRestMaxWin() {
  if (window.windowState == STATE_MAXIMIZED) window.restore();
  else window.maximize();
}

function aioFrameInfo() {
  var targetDoc = aioSrcEvent.target.ownerDocument;
  if (targetDoc.defaultView.frameElement) BrowserPageInfo(targetDoc); // it's a frame
  else BrowserPageInfo();
}

function aioShowHideStatusBar() {
  var bar = document.getElementById("status-bar");
  if (bar) bar.hidden = !bar.hidden;
}

function aioShowHideFindBar() {
   var focusedWindow = document.commandDispatcher.focusedWindow;
   var winWrapper = new XPCNativeWrapper(focusedWindow, 'getSelection()');
   var selectStr = winWrapper.getSelection().toString();
   var toolbar = document.getElementById("FindToolbar");
   var beforeV2 = typeof(gFindBar) == "undefined";
   if (toolbar.hidden || selectStr) {
      if (toolbar.hidden) {
         if (beforeV2) onFindCmd();
         else if (!aioFxV3) gFindBar.onFindCmd();
              else gFindBar.onFindCommand();
         if (!selectStr) return;
      }
      if (!aioFxV3) var findField = document.getElementById("find-field");
      else findField = gFindBar._findField;
      findField.value = selectStr;
      findField.select(); findField.focus();
      if (beforeV2) find("");
      else if (!aioFxV3) gFindBar.find("");
           else gFindBar._find("")
   }
   else
      if (beforeV2) closeFindBar();
      else if (!aioFxV3) gFindBar.closeFindBar();
           else gFindBar.close();
}

function aioViewSource(frame) {
   if (frame) BrowserViewSourceOfDocument(aioSrcEvent.target.ownerDocument);
   else BrowserViewSourceOfDocument(window._content.document);
}

function aioViewCookies() { //Contributed by Squarefree.com
  if (window._content.document.cookie)
    alert(aioGetStr("cookies") + "\n\n" +
       window._content.document.cookie.replace(/; /g,"\n"));
  else alert(aioGetStr("noCookies"));
}

function aioDeleteCookies() { //Contributed by Squarefree.com
  var d, sl, p, i, cookie;
  var cookieStr = window._content.document.cookie;
  if (!cookieStr) alert(aioGetStr("noCookies"));
  var cookies = cookieStr.split("; ");
  for (d = "." + window._content.document.location.host; d; d = ("" + d).substr(1).match(/\..*$/))
    for (sl = 0; sl < 2; ++sl)
      for (p = "/" + window._content.document.location.pathname; p; p = p.substring(0, p.lastIndexOf('/')))
        for (i in cookies) {
          cookie = cookies[i];
          if (cookie) window._content.document.cookie = cookie + ";domain = " + d.slice(sl) + "; path=" +
                   p.slice(1) + "/" + "; expires=" + new Date((new Date).getTime()-1e11).toGMTString();
        }
}

function aioBookmarkCurrentPage() {
  if (aioFxV3) 
     PlacesCommandHook.bookmarkCurrentPage(true, PlacesUtils.bookmarksMenuFolderId);
  else addBookmarkAs(aioContent);
}

function aioMetaInfo() {
  var metas, metastr, mymeta;
  metas = window._content.document.getElementsByTagName("meta");
  if (metas.length) {
    metastr = aioGetStr("meta") + "\n\n";
    for (var i = 0; i < metas.length; ++i) {
      mymeta = metas.item(i);
      metastr += "<META ";
      if (mymeta.name) metastr += "name=\"" + mymeta.name + "\" ";
      if (mymeta.httpEquiv) metastr+= "http-equiv=\"" + mymeta.httpEquiv + "\" ";
      if (mymeta.content) metastr += "content=\"" + mymeta.content + "\" ";
      if (mymeta.scheme) metastr += "scheme=\"" + mymeta.scheme + "\" ";
      metastr += ">\n";
    }
    alert(metastr);
  }
  else alert(aioGetStr("noMeta"));
}

function aioActionOnPage(caseNb) { // code by Ben Basson aka Cusser
  var service, serviceDomain;
  switch(caseNb) {
    case 0: service = "http://validator.w3.org/check?uri=";
            serviceDomain = "validator.w3.org";
            break;
    case 1: service = aioGetStr("g.translateURL");
            serviceDomain = "translate.google.com";
  }
  var targetURI = getWebNavigation().currentURI.spec; 
  if (targetURI.indexOf(serviceDomain) >= 0) BrowserReload(); // already activated
  else loadURI(encodeURI(service + targetURI));
}

function aioOpenAioOptions() {
  window.openDialog(aioDir + "pref/aioOptions.xul", "", "chrome,dialog,modal,resizable");
}

function aioOpenBookmarksManager() {
  if (aioFxV3) PlacesCommandHook.showPlacesOrganizer('AllBookmarks');
  else toOpenWindowByType("bookmarks:manager",
                          "chrome://browser/content/bookmarks/bookmarksManager.xul");
}

function aioOpenAddonManager() {
  if (aioFxV3) BrowserOpenAddonsMgr();
}

function aioOpenDownloadManager() {
  if (aioFxV3) BrowserDownloadsUI();
  else toOpenWindowByType("Download:Manager",
                          "chrome://mozapps/content/downloads/downloads.xul",
                          "chrome,dialog=no,resizable");
}
