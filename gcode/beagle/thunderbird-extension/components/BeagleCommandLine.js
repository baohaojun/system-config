//
// beagle.xul: Overlay for basic main window GUI items
//
// Copyright (C) 2007 Pierre Östlund
//

//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

// Original source: http://developer.mozilla.org/en/docs/Chrome:_Command_Line

const nsIAppShellService    = Components.interfaces.nsIAppShellService;
const nsISupports           = Components.interfaces.nsISupports;
const nsICategoryManager    = Components.interfaces.nsICategoryManager;
const nsIComponentRegistrar = Components.interfaces.nsIComponentRegistrar;
const nsICommandLine        = Components.interfaces.nsICommandLine;
const nsICommandLineHandler = Components.interfaces.nsICommandLineHandler;
const nsIFactory            = Components.interfaces.nsIFactory;
const nsIModule             = Components.interfaces.nsIModule;
const nsIWindowWatcher      = Components.interfaces.nsIWindowWatcher;

const clh_contractID = "@mozilla.org/commandlinehandler/general-startup;1?type=beagle";
const clh_CID = Components.ID("{679f520a-b062-45f9-a02c-482cfebf3b77}");
const clh_category = "m-beagle";

var loaded = false;
var last_uri = null;

const startupObserver = {

	Observer: Components.classes['@mozilla.org/observer-service;1']
		.getService(Components.interfaces.nsIObserverService),

	RegisterSelf: function ()
	{
		this.Observer.addObserver (this, 'beagle-loaded', false);
		this.Observer.addObserver (this, 'quit-application', false);
	},
	
	UnregisterSelf: function ()
	{
		this.Observer.removeObserver (this, 'beagle-loaded');
		this.Observer.removeObserver (this, 'quit-application');
	},
	
	notify: function ()
	{
		if (!loaded || (loaded && !last_uri))
			return;
		
		var uri = last_uri;
		last_uri = null;
		this.Observer.notifyObservers (this, 'beagle-open-uri', uri);
	},

	observe: function (subject, topic, data)
	{
		if (topic == 'beagle-loaded') {
			// Each message will send this so we better not respond to all of them
			if (loaded)
				return;
			
			loaded = true;
			this.notify ();
		} else if (topic == 'quit-application')
			this.UnregisterSelf ();
	}
};
 
const myAppHandler = {

	QueryInterface : function clh_QI(iid)
	{
		if (iid.equals(nsICommandLineHandler) || iid.equals(nsIFactory) || iid.equals(nsISupports))
			return this;

		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	handle : function clh_handle(cmdLine)
	{
		try {
			var uristr = cmdLine.handleFlagWithParam("viewbeagle", false);
			if (uristr) {
				last_uri = uristr;
				cmdLine.preventDefault = false;
				startupObserver.notify ();
			}
		} catch (e) {
			Components.utils.reportError("incorrect parameter passed to -viewbeagle on the command line.");
		}
	},

	helpInfo : "  -viewbeagle <uri>       Open specified URI,\n",

	createInstance : function clh_CI(outer, iid)
	{
		if (outer != null)
			throw Components.results.NS_ERROR_NO_AGGREGATION;

		return this.QueryInterface(iid);
	},

	lockFactory : function clh_lock(lock)
	{
	}
};


const myAppHandlerModule = {

	QueryInterface : function mod_QI(iid)
	{
		if (iid.equals(nsIModule) || iid.equals(nsISupports))
			return this;

		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	getClassObject : function mod_gch(compMgr, cid, iid)
	{
		if (cid.equals(clh_CID))
			return myAppHandler.QueryInterface(iid);

		throw Components.results.NS_ERROR_NOT_REGISTERED;
	},

	registerSelf : function mod_regself(compMgr, fileSpec, location, type)
	{
		compMgr.QueryInterface(nsIComponentRegistrar);

		compMgr.registerFactoryLocation(clh_CID,
			"myAppHandler",
			clh_contractID,
			fileSpec,
			location,
			type);

		var catMan = Components.classes["@mozilla.org/categorymanager;1"]
			.getService(nsICategoryManager);
		catMan.addCategoryEntry("command-line-handler",
			clh_category,
			clh_contractID, true, true);
	},

	unregisterSelf : function mod_unreg(compMgr, location, type)
	{
		compMgr.QueryInterface(nsIComponentRegistrar);
		compMgr.unregisterFactoryLocation(clh_CID, location);

		var catMan = Components.classes["@mozilla.org/categorymanager;1"]
			.getService(nsICategoryManager);
		catMan.deleteCategoryEntry("command-line-handler", clh_category);
	},

	canUnload : function (compMgr)
	{
		return true;
	}
};

function NSGetModule(comMgr, fileSpec)
{
	return myAppHandlerModule;
}

startupObserver.RegisterSelf ();


