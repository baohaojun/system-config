//
// BeagleSettings.js: Base code for settings component
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

// Source: http://hyperstruct.net/2006/8/10/your-first-javascript-xpcom-component-in-10-minutes

const CLASS_ID = Components.ID('{bde6ce3b-f79d-4cea-8db2-a26d69616536}');
const CLASS_NAME = 'Beagle settings component';
const CONTRACT_ID = '@beagle-project.org/services/settings;1';
const SOURCE = 'chrome://beagle/content/beagleSettings.js';
const INTERFACE = Components.interfaces.nsISupports;

const Cc = Components.classes;
const Ci = Components.interfaces;
const Cr = Components.results;
const loader = Cc['@mozilla.org/moz/jssubscript-loader;1']
    .getService(Ci.mozIJSSubScriptLoader);

function Component() {
    this.wrappedJSObject = this;
}

function GetJsService (component)
{
	var comp = Components.classes [component].getService (Components.interfaces.nsISupports);
	return comp.wrappedJSObject;
}

loader.loadSubScript(SOURCE, Component.prototype);

var Factory = {
    createInstance: function(aOuter, aIID) {
        if(aOuter != null)
            throw Cr.NS_ERROR_NO_AGGREGATION;
        var component = new Component();
        if(typeof(component.wrappedJSObject.init) == 'function')
            component.init();

        return component.QueryInterface(aIID);
    }
};

var Module = {
    _firstTime: true,

    registerSelf: function(aCompMgr, aFileSpec, aLocation, aType) {
        if (this._firstTime) {
            this._firstTime = false;
            throw Components.results.NS_ERROR_FACTORY_REGISTER_AGAIN;
        };
        aCompMgr = aCompMgr.QueryInterface(Ci.nsIComponentRegistrar);
        aCompMgr.registerFactoryLocation(
            CLASS_ID, CLASS_NAME, CONTRACT_ID, aFileSpec, aLocation, aType);
    },

    unregisterSelf: function(aCompMgr, aLocation, aType) {
        aCompMgr = aCompMgr.QueryInterface(Ci.nsIComponentRegistrar);
        aCompMgr.unregisterFactoryLocation(CLASS_ID, aLocation);
    },

    getClassObject: function(aCompMgr, aCID, aIID) {
        if (!aIID.equals(Ci.nsIFactory))
            throw Cr.NS_ERROR_NOT_IMPLEMENTED;

        if (aCID.equals(CLASS_ID))
            return Factory;

        throw Cr.NS_ERROR_NO_INTERFACE;        
    },

    canUnload: function(aCompMgr) { return true; }
};

function NSGetModule(aCompMgr, aFileSpec) { return Module; }

