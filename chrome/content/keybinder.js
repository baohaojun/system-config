Firemacs.KeyBinder = {
    initialize: function(preference, config) {
	this._preference = preference;
	this._commandHolderNames = config.commandHolderNames;
	this._commandHolders = {};
	return this;
    },
    init: function(sfun) {
	var length = this._commandHolderNames.length;
	for (var i = 0; i < length; i++) {
	    var cmdHldrNm = this._commandHolderNames[i];
	    var commandHolder = new Firemacs.CommandHolder(sfun, this._preference, Firemacs.Commands[cmdHldrNm], Firemacs.CmdKey[cmdHldrNm]);
	    this._commandHolders[cmdHldrNm] = commandHolder;
	}
	this.rehash();
    },
    rehash: function() {
	for (var cmdHldrNm in this._commandHolders) {
	    this._commandHolders[cmdHldrNm].rehash();
	}
    },
    doCommand: function(cmdHldrNm, key, e) {
	return this._commandHolders[cmdHldrNm].doCommand(key, e);
    },
    KillAccessKeys: function(e) {
	this._commandHolders['Common'].doCommandByName('KillAccessKeys', e);
    }
};

Firemacs.Preference = {
    initialize: function(config) {
	this._prefix = config.prefix;
	this._opts = Firemacs.CmdKey[config.userOptions];
	this._prefer = Components.classes['@mozilla.org/preferences;1'].getService(Components.interfaces.nsIPrefBranch);
	return this;
    },
    getXPrefix: function() {
	return this.getKey('XPrefix', this._opts);
    },
    getUseEscape: function() {
	return this.getBool('UseEscape', this._opts);
    },
    getUseAlt: function() {
	return this.getBool('UseAlt', this._opts);
    },
    getUseMeta: function() {
	return this.getBool('UseMeta', this._opts);
    },
    getAccessRegex: function() {
	return this.getKey('AccessRegex', this._opts);
    },
    getTurnoffRegex: function() {
	return this.getKey('TurnoffRegex', this._opts);
    },
    getWalkForm: function() {
	return this.getBool('WalkForm', this._opts);
    },
    getEditOnly: function() {
	return this.getBool('EditOnly', this._opts);
    },
    getKey: function(cmd, hash) {
	try {
	    return this._prefer.getCharPref(this._prefix + '.' + cmd);
	} catch (error) {
	    return hash[cmd];
	}
    },
    getBool: function(name, hash) {
	try {
	    return this._prefer.getBoolPref(this._prefix + '.' + name);
	} catch (error) {
	    return hash[name];
	}
    },
    setBool: function(name, value) {
	try {
	    this._prefer.setBoolPref(this._prefix + '.' + name, value);
	} catch (error) {
	}
    }
};

Firemacs.CommandHolder = Firemacs.defClass({
    initialize: function(sfun, preference, commands, cmdKey) {
	this._sfun = sfun;
	this._preference = preference;
	this._commands = commands;
	this._cmdKey = cmdKey;
	this._keyCmd = null;
    },
    rehash: function() {
	this._keyCmd = {};
	for (var cmd in this._cmdKey) {
	    var key = this._preference.getKey(cmd, this._cmdKey);
	    this._keyCmd[key] = cmd;
	}
    },
    doCommand: function(key, e) {
	var name = this._keyCmd[key];
	if (name) {
	    this._commands[name].call(this, e);
	    return true;
	} else {
	    return false;
	}
    },
    doCommandByName: function(name, e) {
	var command = this._commands[name];
	if (command) {
	    command.call(this, e);
	    return true;
	} else {
	    return false;
	}
    }
});

Firemacs.Observer = {
    initialize: function(binder, config) {
	this._binder = binder;
	this._prefer = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefService).getBranch(config.prefix + '.');
	return this;
    },
    addObserver: function() {
	this._prefer.QueryInterface(Components.interfaces.nsIPrefBranch2);
	this._prefer.addObserver('', this, false);
    },
    observe: function(aSubject, aTopic, aData) {
	if(aTopic != 'nsPref:changed') {
	    return;
	}
	this._binder.rehash();
    }
};
