const CID = Components.ID('{768f9b78-70bf-4be6-bb0f-d41897641ebc}');
const CLASS_NAME = 'Firemacs Service';
const CONTRACT_ID = '@mew.org/firemacs;1';

function FiremacsService() {
}

FiremacsService.prototype = {
    handleEvent: function(e) {
	e.currentTarget.removeEventListener('load', this, false);
	e.target.loadOverlay('chrome://firemacs/content/firemacs.xul', null);
    },

    observe: function(subject, topic, data) {
	if (topic == 'app-startup') {
	    var ww = Components.classes['@mozilla.org/embedcomp/window-watcher;1'].getService(Components.interfaces.nsIWindowWatcher);
	    ww.registerNotification(this);
	} else if (topic == 'domwindowopened') {
	    /* loadOverlay on DOMContentLoaded can cause crash */
	    subject.addEventListener('load', this, false);
	}
    },

    QueryInterface: function(iid) {
	if (!iid.equals(Components.interfaces.nsIDOMEventListener) &&
	    !iid.equals(Components.interfaces.nsIObserver) &&
	    !iid.equals(Components.interfaces.nsISupports)) {
	    throw Components.results.NS_ERROR_NO_INTERFACE;
	}
	return this;
    }
};

var factory = {
    createInstance: function (outer, iid) {
	if (outer !== null) {
	    throw Components.results.NS_ERROR_NO_AGGREGATION;
	}
	return (new FiremacsService()).QueryInterface(iid);
    }
};

var module = {
    registerSelf: function(compmgr, spec, location, type) {
	compmgr = compmgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
	compmgr.registerFactoryLocation(CID, 'Firemacs Service', CONTRACT_ID,
					spec, location, type);
	var catmgr = Components.classes['@mozilla.org/categorymanager;1'].getService(Components.interfaces.nsICategoryManager);
	catmgr.addCategoryEntry('app-startup', CLASS_NAME,
				CONTRACT_ID, true, true, null);
    },

    unregisterSelf: function(compmgr, location, type) {
	compmgr = compmgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
	compmgr.unregisterFactoryLocation(CLASS_ID, location);
    },

    getClassObject: function(compmgr, cid, iid) {
	if (!iid.equals(Components.interfaces.nsIFactory)) {
	    throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
	}

	if (!cid.equals(CID)) {
	    throw Components.results.NS_ERROR_NO_INTERFACE;
	}

	return factory;
    },

    canUnload: function(compmgr) {
	return true;
    }
};

function NSGetModule(compmgr, spec) {
    return module;
}
