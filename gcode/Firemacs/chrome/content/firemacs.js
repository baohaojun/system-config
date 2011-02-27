////////////////////////////////////////////////////////////////
//
// Name holder
//

var Firemacs = {
    defClass: function(obj) {
	function cls() {
	    this.initialize.apply(this, arguments);
	}
	Firemacs.addMethods(cls, obj);
	return cls;
    },
    addMethods: function(cls, mixin) {
	for (var func in mixin) {
	    if (! cls.prototype[func]) {
		cls.prototype[func] = mixin[func];
	    }
	}
    },
    addClassMethods: function(cls, mixin) {
	for (var func in mixin) {
	    if (! cls[func]) {
		cls[func] = mixin[func];
	    }
	}
    }
};

////////////////////////////////////////////////////////////////
//
// Configuration
//

Firemacs.Config = {
    commandHolderNames: ['Menu', 'View', 'Edit', 'Common'],
    prefix: 'firemacs2',
    userOptions: 'Option'
};

////////////////////////////////////////////////////////////////
//
// Mixin
//

Firemacs.Mixin = {
    _displayMessage: function(msg, time) {
	// if we makes statusbar a global variable,
	// icons on the status bar are hidden, sigh.
	var statusbar = document.getElementById('statusbar-display');
	if (!statusbar) {
	    return;
	}
	statusbar.label = msg;
	if (time) {
	    setTimeout(arguments.callee, time, '', 0);
	}
    }
};
