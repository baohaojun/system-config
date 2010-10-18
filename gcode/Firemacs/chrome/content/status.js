////////////////////////////////////////////////////////////////
//
// Status
//

Firemacs.Status = {
    initialize: function(preference) {
	this._status = true;
	this._preference = preference;
	return this;
    },

    get: function() {
	return this._status;
    },

    set: function(onoff) {
	this._status = onoff;
	this._display();
    },

    toggle: function() {
	this._status = !this._status;
	this._display();
    },

    _display: function() {
	var image = document.getElementById('firemacs-image');
	if (!image) {
	    return;
	}
	if (this._status) {
	    image.src = 'chrome://firemacs/skin/icon16.png';
	    image.tooltipText = 'Firemacs enabled';
	} else {
	    image.src = 'chrome://firemacs/skin/icon16gray.png';
	    image.tooltipText = 'Firemacs disabled';
	}
    },

    checked: function() {
	var mitem1 = document.getElementById('firemacs-menu-enabled');
	if (mitem1) {
	    mitem1.setAttribute('checked', this._status);
	}
	var mitem2 = document.getElementById('firemacs-menu-config');
	if (mitem2) {
	    var win = this._getWindow();
	    mitem2.setAttribute('checked', !!win);
	}
	var mitem3 = document.getElementById('firemacs-menu-editonly');
	if (mitem2) {
	    var editonly = this._preference.getEditOnly();
	    mitem3.setAttribute('checked', editonly);
	}
    },

    _getWindow: function() {
	return Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator).getMostRecentWindow('Firemacs:Config');
    },

    openConfig: function() {
	var win = this._getWindow();
	if (win) {
	    win.focus();
	} else {
	    var parentWindow = (!window.opener || window.opener.closed) ? window : window.opener;
	    parentWindow.openDialog('chrome://firemacs/content/config.xul',
				    '_blank', 'resizable,dialog=no,centerscreen');
	}
    },

    toggleEditOnly: function() {
	var editonly = this._preference.getEditOnly();
	this._preference.setBool('EditOnly', !editonly);
    }
};
