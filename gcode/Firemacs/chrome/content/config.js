////////////////////////////////////////////////////////////////
///
/// configration for Firemacs
///

var FmxConfig = {
    _prefix: 'firemacs2',
    _pref: Components.classes['@mozilla.org/preferences;1'].getService(Components.interfaces.nsIPrefBranch),
    save: function() {
	this._save(this._names, this._pref.setCharPref, 'value');
	this._save(this._boolNames, this._pref.setBoolPref, 'checked');
    },
    _save: function(array, func, attr) {
	var length = array.length;
	for (var i = 0; i < length; i++) {
	    var elt = document.getElementById(array[i]);
	    var value = elt[attr]; // must not use elt.getAttribute('value')
	    var prefName = this._prefix + '.'  + array[i];
	    func(prefName, value);
	}
    },
    onload: function() {
	try {
	    this._load(this._names, this._pref.getCharPref, 'value');
	    this._load(this._boolNames, this._pref.getBoolPref, 'checked');
	} catch (error) {
	    // no preference in the first time
	}
    },
    _load: function(array, func, attr) {
	var length = array.length;
	for (var i = 0; i < length; i++) {
	    var prefName = this._prefix + '.' + array[i];
	    var value = func(prefName);
	    var elt = document.getElementById(array[i]);
	    elt.setAttribute(attr, value);
	}
    }
};
