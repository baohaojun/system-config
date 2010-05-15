/*

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

Original Author: Pete Collins <pete@mozdev.org>
Contributor(s):
  Henrik Gemal <http://gemal.dk>
*/

/************** DUBUG ******************/
if (typeof(JS_LIB_LOADED) == "boolean") 
{

  const JS_DEBUG_LOADED = true;
  const JS_DEBUG_FILE = "debug.js";

  /****************************************************************
  * void jslibDebug(aOutString)                                   *
  * aOutString is an argument of string debug message             *
  * returns void                                                  *
  *   eg:                                                         * 
  *       var msg="Testing function";                             *
  *       jslibDebug(msg);                                        *
  *                                                               *
  *   outputs: Testing function                                   *
  ****************************************************************/

  // DEPRECATED 
  function jslib_debug(aOutString) { return jslibDebug(aOutString); }

  function jslibDebug(aOutString) 
  {
    if (!JS_LIB_DEBUG)
      return; 

    if (JS_LIB_DEBUG_ALERT)
      alert(aOutString);

    dump(aOutString+'\n');
  }

  // print to stdout
  function jslibPrint(aOutString) 
  {
    dump(aOutString+'\n');
  }

  // print to stdout
  function jslibPrintDebug(aMsg, aOutString) 
  {
    if (!aMsg) aMsg = "JSLIB_DEBUG: ";
    dump(aMsg+aOutString+'\n');
  }

  // print to stdout
  function jslibPrintBracket(aOutString) 
  {
    dump("["+aOutString+']\n');
  }

  // print message
  function jslibPrintMsg(aOutStr1, aOutStr2) 
  {
    dump(aOutStr1+": "+aOutStr2+"\n");
  }


  /****************************************************************
  * void jslibError(e, aType, aResults, aCaller)                  *
  * e        - argument of results exception                      *
  * aType    - argument of string error type message              *
  * aResults - argument of string Components.results name         *
  * aCaller  - argument of string caller filename and func name   *
  * returns void                                                  *
  *   Ex:                                                         * 
  *       jslibError(null, "Missing file path argument\n",        *
  *                 "NS_ERROR_XPC_NOT_ENOUGH_ARGS",               *
  *                 JS_LIB_FILE+": include");                     *
  *                                                               *
  *   outputs:                                                    *
  *       -----======[ ERROR ]=====-----                          *
  *       Error in jslib.js: include:  Missing file path argument *
  *                                                               *
  *       NS_ERROR_NUMBER:   NS_ERROR_XPC_NOT_ENOUGH_ARGS         *
  *       ------------------------------                          *
  *                                                               *
  ****************************************************************/

  function jslibError(e) 
  {
		var rv = null;
    var errMsg="";
    if (typeof(e) == 'object') {
      var m, n, r, l, ln, fn = "";
      try {
        rv = e.result;
        m  = e.message;
        fn = e.filename;
        l  = e.location; 
        ln = l.lineNumber; 
      } catch (e) {}
      errMsg+="FileName:          "+fn+"\n"           +
              "Result:            "+rv+"\n"           +
              "Message:           "+m+"\n"            +
              "LineNumber:        "+ln+"\n";
    }

    errMsg = "\n-----======[ jsLib ERROR ]=====-----\n" + errMsg;
    errMsg += "-------------------------------------\n";

    jslibDebug(errMsg);

		return rv;
  }

	function jslibErrorMsg (e)
	{
		jslibDebug(e);
		return null;
	}

	function jslibErrorWarn (e)
	{
		jslibDebug("jsLib warn: "+e);
		return null;
	}

	function jslibErrorMsg (e)
	{
		jslibDebug(e);
		return jslibRes[e];
	}

  // Welcome message
  jslibDebug('*** load: '+JS_DEBUG_FILE+' OK');
  jslibDebug(JS_LIB_HELP);
  jslibDebug("\n\n*********************\nJS_LIB DEBUG IS ON\n*********************\n\n");

} 
                                                                                                    

