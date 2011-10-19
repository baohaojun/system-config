/*** -*- Mode: Javascript; tab-width: 2;

The contents of this file are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

The Original Code is jslib team code.
The Initial Developer of the Original Code is jslib team.

Portions created by jslib team are
Copyright (C) 2000 jslib team.  All
Rights Reserved.

Original Author: Pete Collins <pete@mozdevgroup.com>
Contributor(s): Martin Kutschker <Martin.T.Kutschker@blackbox.net>

***/

/**
 * insure jslib base is not already loaded
 */
if (typeof(JS_LIB_LOADED) != "boolean")
{
  try {

  const JS_LIB_LOADED     = true;

  const JS_LIBRARY        = "jslib";
  const JS_LIB_FILE       = "jslib.js"
  const JS_LIB_PATH       = "chrome://beagle/content/jslib/";
  const JS_LIB_VERSION    = "0.1.184-modified";
  const JS_LIB_AUTHORS    = "\tPete Collins       <pete@mozdevgroup.com>\n"
                          + "\tNeil Deakin        <neil@mozdevgroup.com>\n"
                          + "\tEric Plaster       <plaster@urbanrage.com>\n"
                          + "\tMartin.T.Kutschker <Martin.T.Kutschker@blackbox.net>\n";
  const JS_LIB_BUILD      = "mozilla 1.3+";
  const JS_LIB_ABOUT      = "\tThis is an effort to provide a fully "
                          + "functional js library\n"
                          + "\tfor mozilla package authors to use "
                          + "in their applications\n";
  const JS_LIB_HOME       = "http://jslib.mozdev.org/";

  const ON                = true;
  const OFF               = false;
  const C                 = Components;
  const jslibRes          = C.results;
  const jslibI            = C.interfaces;

  const JS_LIB_OK         = jslibRes.NS_OK;

  // DEPRICATED
  const jslib_results     = jslibRes;

  if (typeof(JS_LIB_DEBUG) != "boolean")
    var JS_LIB_DEBUG      = ON;

  var JS_LIB_DEBUG_ALERT  = OFF;
  var JS_LIB_ERROR        = ON;
  var JS_LIB_ERROR_ALERT  = OFF;

  const JS_LIB_HELP       = "\n\nWelcome to jslib version "+JS_LIB_VERSION+"\n\n"
                          + "Global Constants:\n\n"
                          + "JS_LIBRARY     \n\t"+JS_LIBRARY     +"\n"
                          + "JS_LIB_FILE    \n\t"+JS_LIB_FILE    +"\n"
                          + "JS_LIB_PATH    \n\t"+JS_LIB_PATH    +"\n"
                          + "JS_LIB_VERSION \n\t"+JS_LIB_VERSION +"\n"
                          + "JS_LIB_AUTHORS \n"  +JS_LIB_AUTHORS
                          + "JS_LIB_BUILD   \n\t"+JS_LIB_BUILD   +"\n"
                          + "JS_LIB_ABOUT   \n"  +JS_LIB_ABOUT
                          + "JS_LIB_HOME    \n\t"+JS_LIB_HOME    +"\n\n"
                          + "Global Variables:\n\n"
                          + "  JS_LIB_DEBUG\n  JS_LIB_ERROR\n\n";


  function jslibGetService (aURL, aInterface)
  {
    var rv;
    try {
      // determine how 'aInterface' is passed and handle accordingly
      switch (typeof(aInterface))
      {
        case "object":
          rv = C.classes[aURL].getService(aInterface);
          break;

        case "string":
          rv = C.classes[aURL].getService(C.interfaces[aInterface]);
          break;

        default:
          rv = C.classes[aURL].getService();
          break;
      }
    } catch (e) {
      rv = -1;
    }
    return rv;
  }

  function jslibCreateInstance (aURL, aInterface)
  {
    var rv;
    try {
      rv = C.classes[aURL].createInstance(C.interfaces[aInterface]);
    } catch (e) {
      rv = -1;
    }
    return rv;
  }

  function jslibGetInterface (aInterface)
  {
    var rv;
    try {
      rv = C.interfaces[aInterface];
    } catch (e) {
      rv = -1;
    }
    return rv;
  }

  function jslibQI (aObj, aInterface)
  {
    try {
      return aObj.QueryInterface(C.interfaces[aInterface]);
    } catch (e) {
      throw ("Unable to QI " + aObj + " to " + aInterface);
    }
  }

  /****************************************************************
  * void include(aScriptPath)                                     *
  * aScriptPath is an argument of string lib chrome path          *
  * returns NS_OK on success, 1 if file is already loaded and     *
  * - errorno or throws exception on failure                      *
  *   eg:                                                         *
  *       var path='chrome://jslib/content/io/file.js';           *
  *       include(path);                                          *
  *  Or:                                                          *
  *       include(jslib_file);                                    *
  *                                                               *
  *   outputs: void(null)                                         *
  ****************************************************************/

  function include (aScriptPath)
  {
    if (!aScriptPath) {
      dump("include: Missing file path argument\n");
      throw - jslibRes.NS_ERROR_XPC_NOT_ENOUGH_ARGS;
    }

    if (aScriptPath == JS_LIB_PATH+JS_LIB_FILE) {
      dump("include: "+aScriptPath+" is already loaded!\n");
      throw - jslibRes.NS_ERROR_INVALID_ARG;
    }

    var start   = aScriptPath.lastIndexOf('/') + 1;
    var end     = aScriptPath.lastIndexOf('.');
    var slice   = aScriptPath.length - end;
    var loadID  = aScriptPath.substring(start, (aScriptPath.length - slice));

    if (typeof(this['JS_'+loadID.toUpperCase()+'_LOADED']) == "boolean")
      return jslibRes.NS_OK;

    var rv;
    try {
      if (jslibNeedsPrivs())
        netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");
      jslibGetService("@mozilla.org/moz/jssubscript-loader;1",
                      "mozIJSSubScriptLoader").loadSubScript(aScriptPath);
      rv = jslibRes.NS_OK;
      dump("include: "+aScriptPath+"\n");
    } catch (e) {
      const msg = aScriptPath+" is not a valid path or is already loaded\n";
      dump(e+"\n");
      dump("include: "+msg+"\n");
      rv = - jslibRes.NS_ERROR_INVALID_ARG;
    }
    return rv;
  }

  function jslibNeedsPrivs ()
  {
    var rv;
    if (typeof(this.location) == "object") {
      var proto = this.location.protocol;
      rv = (proto == "file:")
    }
    return rv;
  }

  // include debug methods
  const jslib_debug = JS_LIB_PATH+'debug/debug.js';
  include(jslib_debug);

  function jslibUninstall (aPackage, aCallback)
  {
    if (!aPackage)
      throw - jslibRes.NS_ERROR_INVALID_ARG;

    include (jslib_window);
    var win = new CommonWindow(null, 400, 400);
    win.position = JS_MIDDLE_CENTER;
    win.openUninstallWindow(aPackage, aCallback);
  }

  /*********** Launch JSLIB Splash ***************/
  function jslibLaunchSplash ()
  {
    include (jslib_window);
    var win = new CommonWindow("chrome://jslib/content/splash.xul", 400, 220);
    win.position = JS_MIDDLE_CENTER;
    win.openSplash();
  }

  // DEPRICATED
  function jslib_turnDumpOn () { jslibTurnDumpOn(); }

  function jslibTurnDumpOn ()
  {
    include (jslib_prefs);
    // turn on dump
    var pref = new Prefs();
    const prefStr = "browser.dom.window.dump.enabled"

    // turn dump on if not enabled
    if (!pref.getBool(prefStr)) {
      pref.setBool(prefStr, true);
      pref.save();
    }
    return;
  }

  // DEPRICATED
  function jslib_turnDumpOff () { jslibTurnDumpOff(); }

  function jslibTurnDumpOff ()
  {
    include (jslib_prefs);
    // turn off dump
    var pref = new Prefs();
    const prefStr = "browser.dom.window.dump.enabled"

    // turn dump off if enabled
    if (pref.getBool(prefStr)) {
      pref.setBool(prefStr, false);
      pref.save();
    }
    return;
  }

  const jslib_modules = JS_LIB_PATH+'modules.js';
  include (jslib_modules);

  } catch (e) {}

} // end jslib load test

