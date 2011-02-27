////////////////////////////////////////////////////////////////
//
// Key Handler
//

Firemacs.KeyHandler = {
    initialize: function(version, binder, preference, status) {
	this._binder = binder;
	this._preference = preference;
	this._status = status;
	Firemacs.addClassMethods(this, Firemacs.Mixin);
	Firemacs.addClassMethods(this, Firemacs['KeyHandlerFF' + version]);

	Firemacs.addClassMethods(this, (function() {
	    // Using closure to avoid global variables.
	    // We cannot use e.originalTarget.var on FF3, sigh.
	    var isControlXPressed = false;
	    var isEscapePressed = false;
	    return {
		_getControlX: function()    { return isControlXPressed; },
		_setControlX: function(val) { isControlXPressed = val; },
		_getEscape:   function()    { return isEscapePressed; },
		_setEscape:   function(val) { isEscapePressed = val; }
	    };
	})());

	return this;
    },

    // Key Event Handler
    keypress: function(e) {
	if (this._status.get() === false) {
	    return;
	}

	if (this._dispatch(e)) {
	    e.stopPropagation();
	    e.preventDefault();
	}
    },

    _dispatch: function(e) {
	var k = this._getKey(e);

	if (k === true) {
	    return true;
	}

	var binder = this._binder;
	var editOnly = this._preference.getEditOnly();

	// Menu
	if (!editOnly && this._isMenu(e)) {
	    if (k.ctrl) {
		if (binder.doCommand('Menu', k.key, e)) {
		    return true;
		} // else fall through
	    } else {
		return false;
	    }
	}

	var isWritable = this._isWritable();

	// Viewing
	if (!editOnly && !isWritable) {
	    if (binder.doCommand('View', k.key, e)) {
		return true;
	    }
	}

	// Editing
	if (isWritable && (k.ctrl || k.meta || k.prfx || k.arrow)) {
	    if (binder.doCommand('Edit', k.key, e)) {
		return true;
	    }
	}

	// Common
	if (!editOnly && (k.ctrl || k.meta || k.prfx)) {
	    if (binder.doCommand('Common', k.key, e)) {
		return true;
	    }
	}

	return false;
    },

    ////////////////////////////////////////////////////////////////
    //      ctrlKey     altKey       metaKey  <- Firefox mapping
    // Win: Ctrl* (C-)  Alt    (M-)  Windows
    // Mac: Ctrl  (C-)  Option       Apple*  (M-)
    // Uni: Ctrl* (C-)  Alt    (M-)  Meta
    //
    // * is hotkey
    // Windows: To use Alt, set 'ui.key.menuAccessKey' to 0.
    // Mac: Option inserts symbols, sigh!
    //      Firefox 2 does not pass metaKey. Hotkey wins!
    //      Firefox 3 passes metaKey.

    _getKey: function(e) {
	var ctrl    = e.ctrlKey;
	//
	var meta    = false;
	var altKey  = e.altKey;
	var metaKey = e.metaKey;
	var useEscape = this._preference.getUseEscape();
	var useAlt    = this._preference.getUseAlt();
	var useMeta   = this._preference.getUseMeta();
	//
	var xprefix   = this._preference.getXPrefix();
	var ch = String.fromCharCode(e.charCode);
	var prfx = '';
	var key = '';
	var arrow = false;

	////////////////////////////////////////////////////////////////
	//
	// Key Mapping
	//

	if (ctrl && ('C-' + ch) == xprefix) {
	    this._setControlX(true);
	    this._displayMessage(xprefix, 1000);
	    return true;
	}

	if ((ctrl && (ch == '\['))                 || // C-[
	    (e.charCode == KeyEvent.DOM_VK_ESCAPE) || // Mac's C-[
	    (useEscape && (e.keyCode == KeyEvent.DOM_VK_ESCAPE))) { // ESC
	    this._setEscape(true);
	    this._displayMessage('ESC-', 1000);
	    return true;
	}

	if (ch == ' ') {
	    ch = 'SPC';
	}

	switch (e.keyCode) {
	case 28:
	    ch = '\\';
	    break;
	case KeyEvent.DOM_VK_DELETE:
	case KeyEvent.DOM_VK_BACK_SPACE:
	    ch = 'DEL';
	    break;
	case KeyEvent.DOM_VK_RIGHT:
	    ch = 'right';
	    arrow = true;
	    break;
	case KeyEvent.DOM_VK_LEFT:
	    ch = 'left';
	    arrow = true;
	    break;
	case KeyEvent.DOM_VK_UP:
	    ch = 'up';
	    arrow = true;
	    break;
	case KeyEvent.DOM_VK_DOWN:
	    ch = 'down';
	    arrow = true;
	    break;
	}

	////////////////////////////////////////////////////////////////
	//
	// Modifiers
	//

	if (this._getControlX()) {
	    this._setControlX(false);
	    prfx = xprefix;
	}

	if ((!useAlt && altKey) || (!useMeta && metaKey)) {
	    // let hotkey win
	    key = '';
	} else if ((useAlt && altKey) || (useMeta && metaKey) || this._getEscape()) {
	    meta = true;
	    if (ctrl) {
		key = 'C-M-' + ch;
		this._displayMessage('ESC C-' + ch, 300);
	    } else {
		key = 'M-' + ch;
		this._displayMessage('ESC ' + ch, 300);
	    }
	} else if (ctrl) {
	    key = prfx + 'C-' + ch;
	    if (prfx !== '') {
		this._displayMessage(key, 300);
	    }
	} else {
	    key = prfx + ch;
	    if (prfx !== '') {
		this._displayMessage(key, 300);
	    }
	}

	this._setControlX(false);
	this._setEscape(false);

	return {
	    key: key,
	    ctrl: ctrl,
	    meta: meta,
	    prfx: prfx,
	    arrow: arrow
	};
    },

    _isWritable: function() {
	var command = 'cmd_insertText';
	var controller = document.commandDispatcher.getControllerForCommand(command);
	return (controller && controller.isCommandEnabled(command));
    },

    _killAccessKeys: function(e, url) {
	var regstrAccess = this._preference.getAccessRegex();
	if (regstrAccess && regstrAccess !== '') {
	    var regexAccess = new RegExp(regstrAccess);
	    if (regexAccess.test(url)) {
		this._binder.KillAccessKeys(e);
	    }
	}
    },

    _turnOnOff: function(url) {
	var regstrTurnoff = this._preference.getTurnoffRegex();
	if (regstrTurnoff && regstrTurnoff !== '') {
	    var regexTurnoff = new RegExp(regstrTurnoff);
	    if (regexTurnoff.test(url)) {
		this._status.set(false);
		return false;
	    } else {
		this._status.set(true);
		return true;
	    }
	} else {
	    return true;
	}
    },

    // Page-load evnet handler
    contentLoad: function(e) {
	var url = e.originalTarget.URL;
	if (this._turnOnOff(url)) {
	    this._killAccessKeys(e, url);
	}
    },

    contentSelect: function(e) {
	if (typeof(getBrowser) != 'function') {
	    return;
	}
	var url = getBrowser().contentDocument.URL;
	this._turnOnOff(url);
    }
};


////////////////////////////////////////////////////////////////
//
// Firefox 2
//

Firemacs.KeyHandlerFF2 = {
    _isMenu: function(e) {
	// toolkit/content/widgets/autocomplete.xml
	// textbox:hbox:hbox:input
	var o = e.originalTarget;
	return (o.localName == 'input' &&
		o.parentNode &&
		o.parentNode.parentNode &&
		o.parentNode.parentNode.parentNode &&
		o.parentNode.parentNode.parentNode.localName == 'textbox' &&
		o.parentNode.parentNode.parentNode.tabScrolling &&
		o.parentNode.parentNode.parentNode.popup.mPopupOpen);
    }
};

////////////////////////////////////////////////////////////////
//
// Firefox 3
//

Firemacs.KeyHandlerFF3 = {
    _isMenu: function(e) {
	var mController = Components.classes['@mozilla.org/autocomplete/controller;1'].getService(Components.interfaces.nsIAutoCompleteController);
	if (mController.matchCount !== 0) {
	    var open = false;
	    var actpps = document.getElementsByAttribute('autocompletepopup', '*');
	    for (var i=0; i < actpps.length; i++) {
		open = open || document.getElementById(actpps[i].getAttribute('autocompletepopup')).QueryInterface(Components.interfaces.nsIAutoCompletePopup).popupOpen;
	    }
	    return open;
	}
    }
};

