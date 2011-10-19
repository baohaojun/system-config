/*** -*- Mode: Javascript; tab-width: 2; -*-
  
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  
  The Original Code is Collabnet code.
  The Initial Developer of the Original Code is Collabnet.
  
  Portions created by Collabnet are Copyright (C) 2000 Collabnet.
  All Rights Reserved.
  
  Contributor(s): Pete Collins,
                  Doug Turner,
                  Brendan Eich,
                  Warren Harris,
                  Eric Plaster,
                  Martin Kutschker
                  Philip Lindsay
  
  
  JS FileUtils IO API (The purpose of this file is to make it a little easier to do file IO from js) 
  
      fileUtils.js
  
  Function List
  
      chromeToPath(aPath)              // Converts a chrome://bob/content uri to a path.
                                       // NOTE: although this gives you the
                                       // path to a file in the chrome directory, you will
                                       // most likely not have permisions
                                       // to create or write to files there.
      chromeToURL(aPath)               // Converts a chrome://bob/content file:// uri.
      urlToPath(aPath)                 // Converts a file:// url to a path
      exists(aPath);                   // check to see if a file exists
      append(aDirPath, aFileName);     // append is for abstracting platform specific file paths
      remove(aPath);                   // remove a file
      copy(aSource, aDest);            // copy a file from source to destination
      leaf(aPath);                     // leaf is the endmost file string
                                       //  eg: foo.html in /myDir/foo.html
      permissions(aPath);              // returns the files permissions
      dateModified(aPath);             // returns the last modified date in locale string
      size(aPath);                     // returns the file size
      ext(aPath);                      // returns a file extension if there is one
      parent(aPath)                    // returns the dir part of a path
      dirPath(aPath)                   // *Depriciated* use parent 
      spawn(aPath, aArgs)              // spawns another program 
      nsIFile(aPath)                   // returns an nsIFile obj 
      help;                            // currently returns a list of available functions 
  
    Deprecated
  
      chrome_to_path(aPath);           // synonym for chromeToPath
      URL_to_path(aPath)               // synonym for use urlToPath
      rm(aPath);                       // synonym for remove
      extension(aPath);                // synonym for ext
  
  Instructions:
  
    First include this js file 
  
     var file = new FileUtils();
  
    Examples:
  
     var path='/usr/X11R6/bin/Eterm';
     file.spawn(path, ['-e/usr/bin/vi']); 
     *note* all args passed to spawn must be in the form of an array
  
     // to list help
     dump(file.help);
  
    Warning: these API's are not for religious types
  
*******************************************/
  
  // Make sure jslib is loaded
  if (typeof(JS_LIB_LOADED)=='boolean')
  {
  
  /****************** Globals **********************/
  
  const JS_FILEUTILS_FILE                    = "fileUtils.js";
  const JS_FILEUTILS_LOADED                  = true;
  
  const JS_FILEUTILS_LOCAL_CID               = "@mozilla.org/file/local;1";
  const JS_FILEUTILS_FILESPEC_PROGID         = '@mozilla.org/filespec;1';
  const JS_FILEUTILS_NETWORK_STD_CID         = '@mozilla.org/network/standard-url;1';
  const JS_FILEUTILS_SIMPLEURI_PROGID        = "@mozilla.org/network/simple-uri;1";
  const JS_FILEUTILS_CHROME_REG_PROGID       = '@mozilla.org/chrome/chrome-registry;1';
  const JS_FILEUTILS_DR_PROGID               = "@mozilla.org/file/directory_service;1";
  const JS_FILEUTILS_PROCESS_CID             = "@mozilla.org/process/util;1";
  
  const JS_FILEUTILS_I_LOCAL_FILE            = "nsILocalFile";
  const JS_FILEUTILS_INIT_W_PATH             = "initWithPath";
  const JS_FILEUTILS_I_PROPS                 = "nsIProperties";
  
  const JS_FILEUTILS_CHROME_DIR              = "AChrom";
  
  const JS_FILEUTILS_OK                      = true;
  const JS_FILEUTILS_FilePath                = new C.Constructor(JS_FILEUTILS_LOCAL_CID, 
                                                                 JS_FILEUTILS_I_LOCAL_FILE, 
                                                                 JS_FILEUTILS_INIT_W_PATH);
  
  
  /****************** FileUtils Object Class *********************/
  function FileUtils () 
  {
    include (jslib_dirutils);
    this.mDirUtils = new DirUtils();
  } // constructor
  
  FileUtils.prototype  = 
  {
    mFileInst        : null,
    mDirUtils        : null,
  
    /********************* CHROME_TO_PATH ***************************/
    // this is here for backward compatability but is deprecated --pete
    chrome_to_path : function (aPath) { return this.chromeToPath(aPath); },
  
    chromeToPath : function (aPath) 
    {
      if (!aPath)
        return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
      return this.urlToPath(this.chromeToURL(aPath));
    },
  
    chromeToURL : function (aPath) 
    {
      if (!aPath)
        return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
      var uri = jslibCreateInstance(JS_FILEUTILS_SIMPLEURI_PROGID, "nsIURI");
  
      var rv = null;
      if (/^chrome:/.test(aPath)) {
        try {
          var cr        = jslibGetService(JS_FILEUTILS_CHROME_REG_PROGID);
          if (cr) {
            cr          = jslibQI(cr, "nsIChromeRegistry");
            uri.spec    = aPath;
            uri.spec    = cr.convertChromeURL(uri);
            rv          = uri.path;
  
            // deal w/ jar resource files
            if (/.jar!/.test(rv)) {
              rv = rv.replace(/resource:/, "");
              rv =  "file://"+this.mDirUtils.getCurProcDir()+rv;
            }
          }
        } catch (e) {}
  
        if (/^\/|\\|:chrome/.test(rv)) {
          try {
            // prepend the system path to this process dir
            rv = "file:///"+(''+this.mDirUtils.getCurProcDir()+rv)
                  .replace(/\\/g, "\/").replace(/^\s*\/?/, "").replace(/\ /g, "%20");
          } catch (e) {
            rv = jslibError(e);
          }
        }
      } else if (/^file:/.test(aPath)) {
        rv = this.urlToPath(aPath); 
      } 

      return rv;
    },
  
    /********************* URL_TO_PATH ***************************/
    URL_to_path : function (aPath) { return this.urlToPath(aPath); },
  
  urlToPath : function (aPath)
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    // xpcshell doesn't have unescape func
    const hasUnescape = (typeof(unescape)=="function");
    var path = aPath;
    var rv;
  
    if (/^file:/.test(path)) {
      try {
        var uri = jslibCreateInstance(JS_FILEUTILS_NETWORK_STD_CID, "nsIURI");
        uri.spec = path;
        rv = uri.path; 
  
        var file = jslibCreateInstance(JS_FILEUTILS_LOCAL_CID, "nsILocalFile");
  
        // unix and friends
        try {
          file.initWithPath(rv);
          rv = hasUnescape ? unescape(file.path) : file.path;
          return rv;
        } catch (e) {}
  
        // windows
        try {
          file.initWithPath(rv.replace(/^\//,"").replace(/\//g,"\\"));
          rv = hasUnescape ? unescape(file.path) : file.path;
          return rv;
        } catch (e) {}
  
        // FIXME: add checking for Mac
      
      } catch (e) { 
        rv = jslibError(e);
      }
    }
  
    return rv;
  },
  
  /********************* EXISTS ***************************/
  exists : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    var rv;
    try { 
      var file = new JS_FILEUTILS_FilePath(aPath);
      rv = file.exists();
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* RM *******************************/
  rm : function (aPath) { return this.remove(aPath); },
  
  remove : function (aPath) 
  {
    if (!aPath) 
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath))
      return jslibErrorMsg("NS_ERROR_FILE_TARGET_DOES_NOT_EXIST");
  
    var rv;
  
    try { 
      var fileInst = new JS_FILEUTILS_FilePath(aPath);
      if (fileInst.isDirectory())
        return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
      fileInst.remove(false);
      rv = jslibRes.NS_OK;
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* COPY *****************************/
  copy  : function (aSource, aDest) 
  {
    if (!aSource || !aDest) 
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aSource)) 
      return jslibErrorMsg("NS_ERROR_UNEXPECTED");
  
    var rv;
  
    try { 
      var fileInst = new JS_FILEUTILS_FilePath(aSource);
      var dir      = new JS_FILEUTILS_FilePath(aDest);
      var copyName = fileInst.leafName;
  
      if (fileInst.isDirectory())
        return jslibErrorMsg("NS_ERROR_FILE_COPY_OR_MOVE_FAILED");
  
      if (!this.exists(aDest) || !dir.isDirectory()) {
        copyName   = dir.leafName;
        dir        = new JS_FILEUTILS_FilePath(dir.path.replace(copyName,''));
  
        if (!this.exists(dir.path)) 
          return jslibErrorMsg("NS_ERROR_FILE_ALREADY_EXISTS");
  
        if (!dir.isDirectory())
          return jslibErrorMsg("NS_ERROR_FILE_INVALID_PATH");
      }
  
      if (this.exists(this.append(dir.path, copyName))) 
        return jslibError("NS_ERROR_FILE_ALREADY_EXISTS");
  
      rv = fileInst.copyTo(dir, copyName);
      rv = jslibRes.NS_OK;
    } catch (e) { 
      return jslibError(e);
    }
  
    return rv;
  },
  
  /********************* LEAF *****************************/
  leaf  : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      jslibErrorWarn("NS_ERROR_FILE_NOT_FOUND ["+aPath+"]")
  
    var rv;
  
    try {
      var fileInst = new JS_FILEUTILS_FilePath(aPath);
      rv=fileInst.leafName;
    }
  
    catch (e) { 
      return jslibError(e);
    }
  
    return rv;
  },
  
  /********************* APPEND ***************************/
  append : function (aDirPath, aFileName) 
  {
    if (!aDirPath || !aFileName)
      jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aDirPath)) 
      return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var rv;
  
    try { 
      var fileInst = new JS_FILEUTILS_FilePath(aDirPath);
      if (fileInst.exists() && !fileInst.isDirectory()) 
        return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
      fileInst.append(aFileName);
      rv=fileInst.path;
      delete fileInst;
    } catch (e) { 
      return jslibError(e);
    }
  
    return rv;
  },
  
  /********************* VALIDATE PERMISSIONS *************/
  validatePermissions : function(aNum) 
  {
    if ( parseInt(aNum.toString(10).length) < 3 ) 
      return false;
  
    return JS_FILEUTILS_OK;
  },
  
  /********************* PERMISSIONS **********************/
  permissions : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var rv;
  
    try { 
      rv=(new JS_FILEUTILS_FilePath(aPath)).permissions.toString(8);
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* MODIFIED *************************/
  dateModified  : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var rv;
  
    try { 
      var date = new Date((new JS_FILEUTILS_FilePath(aPath)).lastModifiedTime).toLocaleString();
      rv=date;
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* SIZE *****************************/
  size  : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var rv;
  
    try { 
      rv = (new JS_FILEUTILS_FilePath(aPath)).fileSize;
    } catch (e) { 
      jslibError(e);
      rv=0;
    }
  
    return rv;
  },
  
  /********************* EXTENSION ************************/
  extension  : function (aPath) { return this.ext(aPath); },
  
  ext  : function (aPath)
  {
    if (!aPath) 
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var rv;
  
    try { 
      var leafName  = (new JS_FILEUTILS_FilePath(aPath)).leafName;
      var dotIndex  = leafName.lastIndexOf('.'); 
      rv=(dotIndex >= 0) ? leafName.substring(dotIndex+1) : ""; 
    } catch (e) { 
      return jslibError(e);
    }
  
    return rv;
  },
  
  /********************* DIRPATH **************************/
  dirPath   : function (aPath) { return this.parent(aPath); }, 
  
  parent   : function (aPath) 
  {
    if (!aPath)
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    var rv;
  
    try { 
      var fileInst = new JS_FILEUTILS_FilePath(aPath);
  
      if (!fileInst.exists()) 
        return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
      if (fileInst.isFile())
        rv = fileInst.parent.path;
  
      else if (fileInst.isDirectory())
        rv = fileInst.path;
  
      else
        rv = null;
    }
  
    catch (e) { 
      return jslibError(e);
    }
  
    return rv;
  },
  
  /********************* SPAWN ****************************/
  spawn : function (aPath, aArgs) { this.run(aPath, aArgs); },
  run : function (aPath, aArgs) 
  /*
   * Trys to execute the requested file as a separate *non-blocking* process.
   * 
   * Passes the supplied *array* of arguments on the command line if
   * the OS supports it.
   *
   */
  {
    if (!aPath) 
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    if (!this.exists(aPath)) 
      return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");
  
    var len = 0;
    if (aArgs)
      len = aArgs.length;
    else
      aArgs = null;
  
    var rv;
    try { 
      var fileInst = new JS_FILEUTILS_FilePath(aPath);
  
      if (!fileInst.isExecutable()) 
        return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
      if (fileInst.isDirectory())
        return jslibErrorMsg("NS_ERROR_FILE_IS_DIRECTORY");
        // Create and execute the process...
        /*
         * NOTE: The first argument of the process instance's 'run' method
         *       below specifies the blocking state (false = non-blocking).
         *       The last argument, in theory, contains the process ID (PID)
         *       on return if a variable is supplied--not sure how to implement
         *       this with JavaScript though.
         */
        try {
          var theProcess = jslibCreateInstance(JS_FILEUTILS_PROCESS_CID, "nsIProcess");
          
          theProcess.init(fileInst);
  
          rv = theProcess.run(false, aArgs, len);
          jslib_debug("rv="+rv);
        } catch (e) {
          rv = jslibError(e);
        }
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* nsIFILE **************************/
  nsIFile : function (aPath) 
  {
    if (!aPath) 
      return jslibErrorMsg("NS_ERROR_INVALID_ARG");
  
    var rv;
    try {
      rv = new JS_FILEUTILS_FilePath(aPath);
    } catch (e) { 
      rv = jslibError(e);
    }
  
    return rv;
  },
  
  /********************* HELP *****************************/
  get help() 
  {
    var help =
  
      "\n\nFunction List:\n"                  +
      "\n"                                    +
      "   exists(aPath);\n"                   +
      "   chromeToPath(aPath);\n"             +
      "   chromeToURL(aPath);\n"              +
      "   urlToPath(aPath);\n"                +
      "   append(aDirPath, aFileName);\n"     +
      "   remove(aPath);\n"                   +
      "   copy(aSource, aDest);\n"            +
      "   leaf(aPath);\n"                     +
      "   permissions(aPath);\n"              +
      "   dateModified(aPath);\n"             +
      "   size(aPath);\n"                     +
      "   ext(aPath);\n"                      +
      "   parent(aPath);\n"                   + 
      "   run(aPath, aArgs);\n"               + 
      "   nsIFile(aPath);\n"                  + 
      "   help;\n";
  
    return help;
  }
  
};
  
jslibDebug('*** load: '+JS_FILEUTILS_FILE+' OK');
  
} // END BLOCK JS_LIB_LOADED CHECK
  
// If jslib base library is not loaded, dump this error.
else
{
    dump("JS_FILE library not loaded:\n"                                +
         " \tTo load use: chrome://jslib/content/jslib.js\n"            +
         " \tThen: include(jslib_fileutils);\n\n");
}
  
