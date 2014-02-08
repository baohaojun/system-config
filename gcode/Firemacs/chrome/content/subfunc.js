Firemacs.SubFunc = {
    initialize: function(version) {
        this._context = {};
        Firemacs.addClassMethods(this, Firemacs.Mixin);
        Firemacs.addClassMethods(this, Firemacs['SubFuncFF' + version]);
        return this;
    },

    ////////////////////////////////////////////////////////////////
    //
    // Mark
    //

    marked: function(e) {
        var o = e.originalTarget;
        if (typeof(o.FMXmarked) == 'number' || typeof(o.FMXmarked) == 'boolean') {
            return true;
        } else {
            return false;
        }
    },

    setMark: function(e) {
        var o = e.originalTarget;
        if (typeof(o.selectionStart) == 'number') {
            o.FMXmarked = o.selectionStart;
        } else {
            o.FMXmarked = true;
        }
        this._displayMessage('Mark set', 1000);
    },

    resetMark: function(e, force) {
        var o = e.originalTarget;
        var mark = o.FMXmarked;
        o.FMXmarked = null;

        try {
            if (o.selectionStart && typeof(o.selectionStart) == 'number') {
                if (mark && (o.selectionStart < mark)) {
                    o.selectionEnd = o.selectionStart;
                } else {
                    o.selectionStart = o.selectionEnd;
                }
            }
            else {
                goDoCommand('cmd_selectNone');
            }
        } catch (error) {
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Buffer
    //

    // if o.selectionStart and o.selectionEnd are the same,
    // a caret becomes identical to a cursor.

    bob: function(e) {
        try {
            var o = e.originalTarget;
            return ((o.selectionStart == o.selectionEnd) &&
                    (o.selectionStart === 0));
        } catch (error) {
            return false;
        }
    },

    eob: function(e) {
        try {
            var o = e.originalTarget;
            return ((o.selectionEnd == o.selectionStart) &&
                    (o.selectionEnd === o.value.length));
        } catch (error) {
            return false;
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Moving cursor
    //

    PreviousLine: function(e) {
        if (this.marked(e)) {
            goDoCommand('cmd_selectLinePrevious');
        } else {
            goDoCommand('cmd_linePrevious');
        }
    },

    NextLine: function(e) {
        if (this.marked(e)) {
            goDoCommand('cmd_selectLineNext');
        } else {
            goDoCommand('cmd_lineNext');
        }
    },

    PreviousChar: function(e) {
        if (this.marked(e)) {
            goDoCommand('cmd_selectCharPrevious');
        } else {
            goDoCommand('cmd_charPrevious');
        }
    },

    NextChar: function(e) {
        if (this.marked(e)) {
            goDoCommand('cmd_selectCharNext');
        } else {
            goDoCommand('cmd_charNext');
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Text insertion
    // See http://kb.mozillazine.org/Inserting_text_at_cursor
    //

    insertText: function(aText) {
        try {
            var command = 'cmd_insertText';
            var controller = document.commandDispatcher.getControllerForCommand(command);
            if (controller && controller.isCommandEnabled(command)) {
                controller = controller.QueryInterface(Components.interfaces.nsICommandController);
                var params = Components.classes['@mozilla.org/embedcomp/command-params;1'];
                params = params.createInstance(Components.interfaces.nsICommandParams);
                params.setStringValue('state_data', aText);
                controller.doCommandWithParams(command, params);
            }
        } catch (error) {
            dump('Cannot do cmd_insertText!');
            dump(error + '\n');
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Generating a key event
    // contributed by Hirano-san
    //

    generateKey: function(e, key) {
        this._generateKey(e.originalTarget, key);
    },

    _generateKey: function(target, key) {
        var new_event = document.createEvent('KeyboardEvent');
        new_event.initKeyEvent('keypress', true, true, null, false, false,
                               false, false, key, 0);
        target.dispatchEvent(new_event);
    },

    ////////////////////////////////////////////////////////////////
    //
    // tabs overview with filter (similar to ido-switch-buffer)
    //

    allTabs: function() {
        if (typeof(allTabs) != 'object') {
            return;
        }

        allTabs.open(); // opens tabs preview and sets focus to filter input
    },

    ////////////////////////////////////////////////////////////////
    //
    // Moving tab
    //

    moveTab: function(move) {
        if (typeof(gBrowser) != 'object') {
            return;
        }
        var tabs;
        if (gBrowser.visibleTabs) {
            tabs = gBrowser.visibleTabs;
        } else {
            tabs = gBrowser.tabContainer.childNodes;
        }
        var len = tabs.length;
        var cTab = gBrowser.selectedTab;

        for (var i = 0; i < len; i++) {
            if (tabs[i] === cTab) {
                var nTab = tabs[(i + move + len) % len];
                if (nTab && nTab !== cTab) {
                    gBrowser.selectedTab = nTab;
                    content.focus();
                }
                break;
            }
        }
    },

    subPageDown: function(e) {
        var doc = e.target.ownerDocument;
        var wnd = doc.defaultView;
        goDoCommand('cmd_scrollPageDown');
    },

    subPageUp: function(e) {
        var doc = e.target.ownerDocument;
        var wnd = doc.defaultView;
        goDoCommand('cmd_scrollPageUp');
    },

    subNextLink: function(e) {
        var doc = e.target.ownerDocument;
        var wnd = doc.defaultView;
        this._followLink(1, wnd, doc);
    },

    subPrevLink: function(e) {
        var doc = e.target.ownerDocument;
        var wnd = doc.defaultView;
        this._followLink(-1, wnd, doc);
    },

    _followLink: function(dir, wnd, doc) {
        var re;
        if (dir > 0) {
            re = /buttonright|next|>|下一|下页|→/i;
          re_ptag = /next-link/; // for https://developer.apple.com/
        } else {
            re = /buttonleft|prev|<|上一|上页|←/i;
          re_ptag = /previous-link/;
        }
        var re_exclude = /<.*>|>.*</;
        var links = doc.links;
        var anchors = doc.getElementsByTagName("a");
      for (i = 0; i < anchors.length; ++i) {
        the_class = anchors[i].className;
        if (the_class &&
            the_class.search(re) != -1 &&
            anchors[i].href) {
          loadURI(anchors[i].href);
          return;
        }
      }
      for (i = 0; i < anchors.length; ++i) {
        the_class = anchors[i].parentNode.className;
        if (the_class &&
            the_class.search(re_ptag) != -1 &&
            anchors[i].href) {
          loadURI(anchors[i].href);
          return;
        }
      }
        for (i = 0; i < links.length; ++i) {
        if (links[i].textContent &&
            links[i].textContent.search(re) != -1 &&
            links[i].textContent.search(re_exclude) == -1 &&
            links[i].href
           ) {
             loadURI(links[i].href);
             return;
           }
        imgElems = links[i].getElementsByTagName("img"); // Is it an image tag?
        if (imgElems.length > 0 &&
            imgElems[0].src &&
            imgElems[0].src.search(re) != -1 &&
            imgElems[0].src.search(re_exclude) == -1 &&
            links[i].href) {
          loadURI(links[i].href);
          return;
        }
      }
      alert("Next/Prev link not found, using default for php manual");
        if (dir > 0) {
            loadURI(links[1].href); //for the php manual, the 2nd link is the next link
        } else {
            loadURI(links[0].href);
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Inputs and buttons
    //

    _walkTreeInit: function() {
        var ctx = this._context;
        if (typeof(getBrowser) != 'function') {
            return;
        }
        var doc = getBrowser().contentDocument;
        ctx.body = doc.body;
        if (ctx.body) {
            this._displayMessage('Parsing HTML...', 1000);
            ctx.body.FMXwalked = true;
            ctx.body.FMXinputs = [];
            ctx.body.FMXsubmits = [];
            ctx.iidx = 0;
            ctx.sidx = 0;
            ctx.tabindex = false;
            this._walkTree(ctx.body, doc);
            this._displayMessage('Parsing HTML...done', 1000);
        }
    },

    _walkTreeInput: function(node) {
        var ctx = this._context;
        ctx.body.FMXinputs[ctx.iidx] = node;
        node.FMXidx = ctx.iidx;
        node.FMXtype = 'input';
        ctx.iidx++;
    },

    _walkTreeSubmit: function(node) {
        var ctx = this._context;
        ctx.body.FMXsubmits[ctx.sidx] = node;
        node.FMXidx = ctx.sidx;
        node.FMXtype = 'submit';
        ctx.sidx++;
    },

    _walkTree: function(node, doc) {
        if (node.nodeType === node.ELEMENT_NODE || node.noteType === node.DOCUMENT_NODE) {
            if (node.style.display === 'none') {
                return;
            }
            if (node.style.visibility === 'hidden') {
                return;
            }
            if (doc.defaultView.getComputedStyle(node, null).display === 'none') {
                return;
            }
            if (doc.defaultView.getComputedStyle(node, null).visibility === 'hidden') {
                return;
            }
            var width = node.style.width;
            var height = node.style.height;
            if (width === '0' || width === '0px' ||
                height === '0' || height === '0px') {
                    return;
            }
            if (this._localNameIs(node, 'textarea') || this._localNameIs(node, 'input')) {
                var type = node.getAttribute('type');
                if ((type === null) || // 'text' or textarea
                    (type === 'text') ||
                    (type === 'password')) {
                    width = doc.defaultView.getComputedStyle(node, null).width;
                    height = doc.defaultView.getComputedStyle(node, null).height;
                    if (width === '0' || width === '0px' ||
                        height === '0' || height === '0px') {
                        return;
                    }
                    this._walkTreeInput(node);
                } else if ((type === 'submit') ||
                           (type === 'file') ||
                           (type === 'image')) {
                    this._walkTreeSubmit(node);
                }
            } else if (this._localNameIs(node, 'button')) {
                this._walkTreeSubmit(node);
            }
            if (this._localNameIs(node, 'frame') || this._localNameIs(node, 'iframe')) {
                node = node.contentDocument;
                doc = node;
            }

            if (node.hasChildNodes()) {
                var children = node.childNodes;
                var length = children.length;
                for (var i = 0; i < length; i++)  {
                    this._walkTree(children[i], doc);
                }
            }
        }
    },

    _localNameIs: function(node, str) {
        var regex = new RegExp('^' + str + '$', 'i');
        return (node.localName.search(regex) != -1);
    },

    ////////////////////////////////////////////////////////////////
    //
    // Commands for moving in text area
    //

    moveFocus: function(e, move) {
        this._moveNode(e, move, 'FMXinputs', 'input', 'No input/text area');
    },

    moveButton: function(e, move) {
        this._moveNode(e, move, 'FMXsubmits', 'submit', 'No submit button');
    },

    focusBody: function() {
        if (typeof(gBrowser) != 'object') {
            return;
        }
        gBrowser.selectedTab.focus();
        content.focus();
        this._displayMessage('The body was focused', 1000);
    },

    _moveNode: function(e, move, key, type, errmsg) {
        if (typeof(getBrowser) != 'function') {
            return;
        }
        var body = getBrowser().contentDocument.body;
        var index = null;
        if ((! body.FMXwalked) ||
            ((typeof(e.originalTarget.FMXidx) == 'undefined') &&
             (typeof(e.originalTarget.parentNode.FMXidx) == 'undefined'))) {
            this._walkTreeInit();
        }
        if (body[key].length === 0) {
            this._displayMessage(errmsg, 1000);
            return;
        }

        if (e.originalTarget.FMXtype == type) {
            index = e.originalTarget.FMXidx;
        }
        // <input type='file'> = input:input
        if ((index === null) && (e.originalTarget.parentNode.FMXtype == type)) {
            index = e.originalTarget.parentNode.FMXidx;
        }
        if (index !== null) { /* 0 is false */
            var length = body[key].length;
            index = (index + move + length) % length;
        } else {
            index = 0;
        }
        body[key][index].focus();
    },

    ////////////////////////////////////////////////////////////////
    //
    // Kill AccessKeys
    //

    // should be recursive?
    killAccesskeys: function(e) {
        var ctx = this._context;
        if (typeof(getBrowser) != 'function') {
            return;
        }
        var doc = getBrowser().contentDocument;
        ctx.body = doc.body;
        if (ctx.body) {
            var nodes = doc.evaluate('//*[@accesskey]', doc, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
            for (var i = 0; i < nodes.snapshotLength; i++) {
                var node = nodes.snapshotItem(i);
                var clone = node.cloneNode(true);
                clone.removeAttribute('accesskey');
                node.parentNode.replaceChild(clone, node);
            }
            this._displayMessage(nodes.snapshotLength + ' accesskeys were canceled', 1000);
        }
    },

    ////////////////////////////////////////////////////////////////
    //
    // Web/Map Search
    //
    _getSelection: function(e) {
        if (e && (e.originalTarget.tagName.toLowerCase() == 'textarea' ||
                  e.originalTarget.tagName.toLowerCase() == 'input')) {
            var el = e.originalTarget;
            if (el.selectionStart == undefined) {
                // not tested
                var r = getBrowser().contentDocument.selection.createRange();
                s = new Array(r.start, r.end);
            } else {
                s = el.value.substring(el.selectionStart, el.selectionEnd);
            }
        } else {
            // don't use contentWindow
            s = getBrowser().contentDocument.getSelection();
        }
        if (s === '' || !s) {
            s = null;
        }
        return s;
    },

    webSearch: function(e) {
        var s = this._getSelection(e);
        if (s === null) {
            return;
        }
        var searchbar = document.getElementById('searchbar');
        if (!searchbar) {
            return;
        }
        searchbar.value = s; searchbar.doSearch(s, 'tab');
    },

    mapSearch: function(e) {
        var s = this._getSelection(e);
        if (s === null) {
            return;
        }
        var func = this._generateKey;
        openNewTabWith('http://maps.google.com/');
        setTimeout(function() {
            var form = getBrowser().contentDocument.getElementById('q_d');
            form.value = s;
            func(form, KeyEvent.DOM_VK_RETURN);
        }, 1000);
    },

   pageSave: function(e) {
       document.getElementById("Browser:SavePage").doCommand();
   }
};

////////////////////////////////////////////////////////////////
//
// Firefox 2
//

Firemacs.SubFuncFF2 = {
    PreviousCompletion: function(e) {
        var target = e.originalTarget.parentNode.parentNode.parentNode;
        this._moveCompletion(target, 'up');
    },

    NextCompletion: function(e) {
        var target = e.originalTarget.parentNode.parentNode.parentNode;
        this._moveCompletion(target, 'down');
    },

    _moveCompletion: function(target, direction) {
        if (target.tabScrolling && target.popup.mPopupOpen) {
            var controller = Components.interfaces.nsIAutoCompleteController;
            var direct;
            switch (direction) {
                case 'up':
                if (controller.KEY_UP) {
                    direct = controller.KEY_UP;
                } else if (KeyEvent.DOM_VK_UP) {
                    direct = KeyEvent.DOM_VK_UP;
                }
                break;
                case 'down':
                if (controller.KEY_DOWN) {
                    direct = controller.KEY_DOWN;
                } else if (KeyEvent.DOM_VK_DOWN) {
                    direct = KeyEvent.DOM_VK_DOWN;
                }
                break;
            }
            target.mController.handleKeyNavigation(direct);
        }
    },

    SearchOpen:     function() { gFindBar.onFindCmd(); },
    SearchClose:    function() { gFindBar.closeFindBar();  },
    SearchField:    function() { return document.getElementById('find-field'); },
    SearchUnhilite: function() { gFindBar.toggleHighlight(false); },
    SearchForward:  function() { gFindBar.onFindAgainCmd(); },
    SearchBackward: function() { gFindBar.onFindPreviousCmd(); }
};

////////////////////////////////////////////////////////////////
//
// Firefox 3
//

Firemacs.SubFuncFF3 = {
    PreviousCompletion: function(e) {
        this._moveCompletion('up');
    },

    NextCompletion: function(e) {
        this._moveCompletion('down');
    },

    _moveCompletion: function(direction) {
        var controller = Components.classes['@mozilla.org/autocomplete/controller;1'].getService(Components.interfaces.nsIAutoCompleteController);
        var direct;
        switch (direction) {
            case 'up':
                if (controller.KEY_UP) {
                    direct = controller.KEY_UP;
                } else if (KeyEvent.DOM_VK_UP) {
                    direct = KeyEvent.DOM_VK_UP;
                }
                break;
            case 'down':
                if (controller.KEY_DOWN) {
                    direct = controller.KEY_DOWN;
                } else if (KeyEvent.DOM_VK_DOWN) {
                    direct = KeyEvent.DOM_VK_DOWN;
                }
                break;
        }
        controller.handleKeyNavigation(direct);
    },

    SearchOpen:     function() {
        if (typeof(gFindBar) != 'object') {
            return;
        }
        gFindBar.onFindCommand();
    },
    SearchClose:    function() {
        if (typeof(gFindBar) != 'object') {
            return;
        }
        gFindBar.close();
    },
    SearchField:    function() {
        if (typeof(gFindBar) != 'object') {
            return null;
        }
        return gFindBar._findField;
    },
    SearchUnhilite: function() {
        if (typeof(gFindBar) != 'object') {
            return;
        }
        gFindBar.toggleHighlight(false);
    },
    SearchForward:  function() {
        if (typeof(gFindBar) != 'object') {
            return;
        }
        /*
        This code does not work.
        var x = document.getAnonymousElementByAttribute(gFindBar, 'anonid', 'find-next');
        var evt = document.createEvent('MouseEvents');
        evt.initMouseEvent('click', true, true, window,
                           0, 0, 0, 0, 0, false, false, false, false, 0, null);
        x.dispatchEvent(evt);
        */
        gFindBar.onFindAgainCommand(false);
    },
    SearchBackward: function() {
        if (typeof(gFindBar) != 'object') {
            return;
        }
        gFindBar.onFindAgainCommand(true);
    },
    copyText: function(copytext) {
        var str = Components.classes['@mozilla.org/supports-string;1'].createInstance(Components.interfaces.nsISupportsString);
        if (!str) {
            return;
        }

        str.data = copytext;

        var trans = Components.classes['@mozilla.org/widget/transferable;1'].createInstance(Components.interfaces.nsITransferable);
        if (!trans) {
            return;
        }

        trans.addDataFlavor('text/unicode');
        trans.setTransferData('text/unicode', str, copytext.length * 2);

        var clipid = Components.interfaces.nsIClipboard;
        var clip = Components.classes['@mozilla.org/widget/clipboard;1'].getService(clipid);
        if (!clip) {
            return;
        }

        clip.setData(trans, null, clipid.kGlobalClipboard);
    }
};
