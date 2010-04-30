/*** -*- Mode: Javascript; tab-width: 2; 
The contents of this file are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

The Original Code is jslib code.
The Initial Developer of the Original Code is jslib team.

Portions created by jslib team are
Copyright (C) 2000 jslib team.  All
Rights Reserved.

Contributor(s): Pete Collins, 
                Doug Turner, 
                Brendan Eich, 
                Warren Harris, 
                Eric Plaster,
                Martin Kutschker

The purpose of this file is to make it a little easier to use 
xpcom nsIFile file IO library from js

 File API 
    file.js

 Base Class:
    FileSystem
      filesystem.js

 Function List:
    // Constructor
    File(aPath)                         creates the File object and sets the file path

    // file stream methods
    open(aMode, aPermissions);          open a file handle for reading,
                                        writing or appending.  permissions are optional.
    read();                             returns the contents of a file
    readline();                         returns the next line in the file.
    EOF;                                boolean check 'end of file' status
    write(aContents);                   writes the contents out to file.
    copy(aDest);                        copy the current file to a aDest
    close();                            closes a file handle
    create();                           creates a new file if one doesn't already exist
    exists();                           check to see if a file exists

    // file attributes
    size;                               read only attribute gets the file size
    ext;                                read only attribute gets a file extension if there is one
    permissions;                        attribute gets or sets the files permissions
    dateModified;                       read only attribute gets last modified date in locale string

    // file path attributes
    leaf;                               read only attribute gets the file leaf
    path;                               read only attribute gets the path
    parent;                             read only attribute gets parent dir part of a path

    // direct manipulation
    nsIFile                             returns an nsIFile obj

    // utils
    remove();                           removes the current file
    append(aLeaf);                      appends a leaf name to the current file
    appendRelativePath(aRelPath);       appends a relitave path the the current file

    // help!
    help;                               currently dumps a list of available functions

 Instructions:

       First include this js file in your xul file.  
       Next, create an File object:

          var file = new File("/path/file.ext");

       To see if the file exists, call the exists() member.  
       This is a good check before going into some
       deep code to try and extract information from a non-existant file.

       To open a file for reading<"r">, writing<"w"> or appending<"a">,
       just call:

          file.open("w", 0644);

       where in this case you will be creating a new file called '/path/file.ext', 
       with a mode of "w" which means you want to write a new file.

       If you want to read from a file, just call:

          file.open(); or
          file.open("r");
          var theFilesContents    = file.read();

          ---- or ----

          while(!file.EOF) {
            var theFileContentsLine = file.readline();
            dump("line: "+theFileContentsLine+"\n");
          }

       The file contents will be returned to the caller so you can do something usefull with it.

          file.close();

       Calling 'close()' destroys any created objects.  If you forget to use file.close() no probs
       all objects are discarded anyway.

       Warning: these API's are not for religious types

************/

// insure jslib base is loaded
if (typeof(JS_LIB_LOADED)=='boolean') {

// test to make sure filesystem base class is loaded
if (typeof(JS_FILESYSTEM_LOADED)!='boolean')
  include(jslib_filesystem);

/****************** Globals **********************/
const JS_FILE_LOADED           = true;
const JS_FILE_FILE             = "file.js";

const JS_FILE_IOSERVICE_CID    = "@mozilla.org/network/io-service;1";
const JS_FILE_I_STREAM_CID     = "@mozilla.org/scriptableinputstream;1";
const JS_FILE_OUTSTREAM_CID    = "@mozilla.org/network/file-output-stream;1";

const JS_FILE_F_TRANSPORT_SERVICE_CID  = "@mozilla.org/network/file-transport-service;1";

const JS_FILE_I_IOSERVICE              = C.interfaces.nsIIOService;
const JS_FILE_I_SCRIPTABLE_IN_STREAM   = "nsIScriptableInputStream";
const JS_FILE_I_FILE_OUT_STREAM        = C.interfaces.nsIFileOutputStream;

const JS_FILE_READ          = 0x01;  // 1
const JS_FILE_WRITE         = 0x08;  // 8
const JS_FILE_APPEND        = 0x10;  // 16

const JS_FILE_READ_MODE     = "r";
const JS_FILE_WRITE_MODE    = "w";
const JS_FILE_APPEND_MODE   = "a";

const JS_FILE_FILE_TYPE     = 0x00;  // 0

const JS_FILE_CHUNK         = 1024;  // buffer for readline => set to 1k

const JS_FILE_DEFAULT_PERMS = 0644;

const JS_FILE_OK            = true;

try {
  const JS_FILE_InputStream  = new C.Constructor
  (JS_FILE_I_STREAM_CID, JS_FILE_I_SCRIPTABLE_IN_STREAM);

  const JS_FILE_IOSERVICE    = C.classes[JS_FILE_IOSERVICE_CID].
  getService(JS_FILE_I_IOSERVICE);

} catch (e) {
  jslibError(e);
}

/***
 * Possible values for the ioFlags parameter 
 * From: 
 * http://lxr.mozilla.org/seamonkey/source/nsprpub/pr/include/prio.h#601
 */


// #define PR_RDONLY       0x01
// #define PR_WRONLY       0x02
// #define PR_RDWR         0x04
// #define PR_CREATE_FILE  0x08
// #define PR_APPEND       0x10
// #define PR_TRUNCATE     0x20
// #define PR_SYNC         0x40
// #define PR_EXCL         0x80

const JS_FILE_NS_RDONLY               = 0x01;
const JS_FILE_NS_WRONLY               = 0x02;
const JS_FILE_NS_RDWR                 = 0x04;
const JS_FILE_NS_CREATE_FILE          = 0x08;
const JS_FILE_NS_APPEND               = 0x10;
const JS_FILE_NS_TRUNCATE             = 0x20;
const JS_FILE_NS_SYNC                 = 0x40;
const JS_FILE_NS_EXCL                 = 0x80;
/****************** Globals **********************/


/****************************************************************
* void File(aPath)                                              *
*                                                               *
* class constructor                                             *
* aPath is an argument of string local file path                *
* returns NS_OK on success, exception upon failure              *
*   Ex:                                                         *
*     var p = '/tmp/foo.dat';                                   *
*     var f = new File(p);                                      *
*                                                               *
*   outputs: void(null)                                         *
****************************************************************/

function File(aPath) {

  if (!aPath)
    return jslibErrorMsg("NS_ERROR_INVALID_ARG");

  return this.initPath(arguments);
} // constructor

File.prototype = new FileSystem();

// member vars
File.prototype.mMode        = null;
File.prototype.mFileChannel = null;
File.prototype.mTransport   = null;
File.prototype.mURI         = null;
File.prototype.mOutStream   = null;
File.prototype.mInputStream = null;
File.prototype.mLineBuffer  = null;
File.prototype.mPosition    = 0;

/********************* OPEN *************************************
* bool open(aMode, aPerms)                                      *
*                                                               *
* opens a file handle to read, write or append                  *
* aMode is an argument of string 'w', 'a', 'r'                  *
* returns true on success, null on failure                      *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*                                                               *
*   outputs: void(null)                                         *
****************************************************************/

File.prototype.open = function (aMode, aPerms) 
{

  // close any existing file handles
  this.close();

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.mPath) 
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (this.exists() && this.mFileInst.isDirectory()) 
      return jslibErrorMsg("NS_ERROR_FILE_IS_DIRECTORY");

  if (this.mMode) 
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.mURI) {
    if (!this.exists())
      this.create();
    this.mURI = JS_FILE_IOSERVICE.newFileURI(this.mFileInst);
  }

  if (!aMode)
    aMode=JS_FILE_READ_MODE;

  this.resetCache();
  var rv;

  switch (aMode) 
  {
    case JS_FILE_WRITE_MODE: 
    case JS_FILE_APPEND_MODE: {
      try {
        if (!this.mFileChannel)
          this.mFileChannel = JS_FILE_IOSERVICE.newChannelFromURI(this.mURI);
      } catch (e)    {
        return jslibError(e);
      }    

      if (aPerms && !this.validatePermissions(aPerms))
        jslibErrorMsg("NS_ERROR_INVALID_ARG");

      if (!aPerms)
        aPerms=JS_FILE_DEFAULT_PERMS;

      try {
        var offSet=0;
        if (aMode == JS_FILE_WRITE_MODE) {
          this.mMode=JS_FILE_WRITE_MODE;
          // create a filestream
          var fs = jslibCreateInstance(JS_FILE_OUTSTREAM_CID, JS_FILE_I_FILE_OUT_STREAM);

          fs.init(this.mFileInst, JS_FILE_NS_TRUNCATE | 
                                  JS_FILE_NS_WRONLY, 00004, null); 
          this.mOutStream = fs;
        } else {
          this.mMode=JS_FILE_APPEND_MODE;
          // create a filestream
          fs = jslibCreateInstance(JS_FILE_OUTSTREAM_CID, JS_FILE_I_FILE_OUT_STREAM);

          fs.init(this.mFileInst, JS_FILE_NS_RDWR | 
                                  JS_FILE_NS_APPEND, 00004, null); 
          this.mOutStream = fs;
        }
      } catch (e) {
        return jslibError(e);
      }
      rv = JS_LIB_OK;
      break;
    }

    case JS_FILE_READ_MODE: {
      if (!this.exists()) 
        jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");

      this.mMode=JS_FILE_READ_MODE;

      try {
        jslibDebug('****** '+this.mURI);
        this.mFileChannel = JS_FILE_IOSERVICE.newChannelFromURI(this.mURI);
        this.mInputStream = new JS_FILE_InputStream();    
        this.mInputStream.init(this.mFileChannel.open());
        this.mLineBuffer  = new Array();
        rv = JS_LIB_OK;
      } catch (e) {
        rv = jslibError(e);
      }
      break;
    }

    default:
      rv = jslibErrorMsg("NS_ERROR_INVALID_ARG");
  }
  return rv;
}

/********************* READ *************************************
* string read()                                                 *
*                                                               *
* reads a file if the file is binary it will                    *
* return type ex: ELF                                           *
* takes no arguments needs an open read mode filehandle         *
* returns string on success, null on failure                    *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.open(p);                                                *
*     f.read();                                                 *
*                                                               *
*   outputs: <string contents of foo.dat>                       *
****************************************************************/

File.prototype.read = function (aSize) 
{

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (this.mMode != JS_FILE_READ_MODE) {
    this.close();
    return jslibErrorMsg("NS_ERROR_NOT_AVAILABLE");
  }

  var rv = null;
  try {
    if (!this.mFileInst || !this.mInputStream) 
      jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

    rv = this.mInputStream.read(aSize != undefined ? aSize : this.mFileInst.fileSize);
    this.mInputStream.close();
  } catch (e) { 
    rv = jslibError(e);
  }
  return rv;
}

/********************* READLINE**********************************
* string readline()                                             *
*                                                               *
* reads a file if the file is binary it will                    *
* return type string                                            *
* takes no arguments needs an open read mode filehandle         *
* returns string on success, null on failure                    *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.open();                                                 *
*     while(!f.EOF)                                             *
*       dump("line: "+f.readline()+"\n");                       *
*                                                               *
*   outputs: <string line of foo.dat>                           *
****************************************************************/

File.prototype.readline = function ()
{

  if (!this.checkInst() || !this.mInputStream)
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  var rv      = null;
  var buf     = null;
  var tmp     = null;
  try {
    if (this.mLineBuffer.length < 2) {
      buf = this.mInputStream.read(JS_FILE_CHUNK);
      this.mPosition = this.mPosition + JS_FILE_CHUNK;
      if (this.mPosition > this.mFileInst.fileSize) 
        this.mPosition  = this.mFileInst.fileSize;
      if (buf) {
        if (this.mLineBuffer.length == 1) {
          tmp = this.mLineBuffer.shift();
          buf = tmp+buf;
        }
        this.mLineBuffer = buf.split(/[\n\r]/);
      }
    }
    rv = this.mLineBuffer.shift();
  } catch (e) {
    rv = jslibError(e);
  }
  return rv;
}

/********************* EOF **************************************
* bool getter EOF()                                             *
*                                                               *
* boolean check 'end of file' status                            *
* return type boolean                                           *
* takes no arguments needs an open read mode filehandle         *
* returns true on eof, false when not at eof                    *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.open();                                                 *
*     while(!f.EOF)                                             *
*       dump("line: "+f.readline()+"\n");                       *
*                                                               *
*   outputs: true or false                                      *
****************************************************************/

File.prototype.__defineGetter__('EOF', 
function ()
{
  if (!this.checkInst() || !this.mInputStream)
    throw jslibErrorThrow("NS_ERROR_NOT_INITIALIZED");

  if ((this.mLineBuffer.length > 0) || (this.mInputStream.available() > 0)) 
    return false;
  
  return true;
})

/********************* WRITE ************************************
* write()                                                       *
*                                                               *
*  Write data to a file                                         *
*                                                               *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.open("w");                                              *
*     f.write();                                                *
*                                                               *
*   outputs: JS_LIB_OK upon success                             *
****************************************************************/

File.prototype.write = function (aBuffer)
{

  if (!aBuffer)
    return jslibErrorMsg("NS_ERROR_INVALID_ARG");

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (this.mMode == JS_FILE_READ_MODE) {
    this.close();
    jslibErrorMsg("NS_ERROR_FILE_READ_ONLY");
  }

  if (!this.mFileInst)
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED")

  var rv = null;
  try {
    this.mOutStream.write(aBuffer, aBuffer.length);
    this.mOutStream.flush();
    rv = JS_LIB_OK;
  } catch (e) { 
    rv = jslibError(e);
  }

  return rv;
}

/********************* COPY *************************************
* void copy(aDest)                                              *
*                                                               *
* void file close                                               *
* return type void(null)                                        *
* takes no arguments closes an open file stream and             *
* deletes member var instances of objects                       *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     fopen();                                                  *
*     f.close();                                                *
*                                                               *
*   outputs: JS_LIB_OK upon success                             *
****************************************************************/

File.prototype.copy = function (aDest)
{

  if (!aDest)
    return jslibErrorMsg("NS_ERROR_INVALID_ARG");

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.exists()) 
    return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");

  var rv = JS_LIB_OK;
  try {
    var dest = new JS_FS_File_Path(aDest);
    var copyName, dir = null;

    if (dest.equals(this.mFileInst)) 
      return jslibErrorMsg("NS_ERROR_FILE_COPY_OR_MOVE_FAILED");

    if (dest.exists()) 
      return jslibErrorMsg("NS_ERROR_FILE_ALREADY_EXISTS");

    if (this.mFileInst.isDirectory()) 
      return jslibErrorMsg("NS_ERROR_FILE_IS_DIRECTORY");

    if (!dest.exists()) {
      copyName = dest.leafName;
      dir = dest.parent;

      if (!dir.exists()) 
        return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");

      if (!dir.isDirectory()) 
        return jslibErrorMsg("NS_ERROR_FILE_DESTINATION_NOT_DIR");
    }

    if (!dir) {
      dir = dest;
      if (dest.equals(this.mFileInst)) 
        return jslibErrorMsg("NS_ERROR_FILE_COPY_OR_MOVE_FAILED");
    }
    this.mFileInst.copyTo(dir, copyName);
    jslibDebug(JS_FILE_FILE+":copy successful!");
  } catch (e) {
    rv = jslibError(e);
  }
  return rv;
}

/********************* CLOSE ************************************
* void close()                                                  *
*                                                               *
* void file close                                               *
* return type void(null)                                        *
* takes no arguments closes an open file stream and             *
* deletes member var instances of objects                       *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     fopen();                                                  *
*     f.close();                                                *
*                                                               *
*   outputs: void(null)                                         *
****************************************************************/

File.prototype.close = function () 
{
  /***************** Destroy Instances *********************/
  if (this.mFileChannel)   delete this.mFileChannel;
  if (this.mInputStream)   delete this.mInputStream;
  if (this.mTransport)     delete this.mTransport;
  if (this.mMode)          this.mMode=null;
  if (this.mOutStream) {
    this.mOutStream.close();
    delete this.mOutStream;
  }
  if (this.mLineBuffer)    this.mLineBuffer=null;
  this.mPosition           = 0;
  /***************** Destroy Instances *********************/

  return void(null);
}

/********************* CREATE *****************************/
File.prototype.create = function ()
{

  // We can probably implement this so that it can create a 
  // file or dir if a long non-existent mPath is present

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (this.exists()) 
    return jslibErrorMsg("NS_ERROR_FILE_ALREADY_EXISTS");

  var rv = null;
  try { 
    rv = this.mFileInst.create(JS_FILE_FILE_TYPE, JS_FILE_DEFAULT_PERMS); 
  } catch (e) {
    rv = jslibError(e);
  }

  return rv;
}

/********************* REMOVE *******************************/
File.prototype.remove = function ()
{
  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.mPath) 
    return jslibErrorMsg("NS_ERROR_FILE_INVALID_PATH");

  this.close();
  var rv;
  try {
    // this is a non recursive remove because we are only dealing w/ files.
    rv = this.mFileInst.remove(false); 
  } catch (e) {
    rv = jslibError(e);
  }

  return rv;
}

/********************* POS **************************************
* int getter POS()                                              *
*                                                               *
* int file position                                             *
* return type int                                               *
* takes no arguments needs an open read mode filehandle         *
* returns current position, default is 0 set when               *
* close is called                                               *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.open();                                                 *
*     while(!f.EOF){                                            *
*       dump("pos: "+f.pos+"\n");                               *
*       dump("line: "+f.readline()+"\n");                       *
*     }                                                         *
*                                                               *
*   outputs: int pos                                            *
****************************************************************/

File.prototype.__defineGetter__('pos', function (){ return this.mPosition; })

/********************* SIZE *************************************
* int getter size()                                             *
*                                                               *
* int file size                                                 *
* return type int                                               *
* takes no arguments a getter only                              *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.size;                                                   *
*                                                               *
*   outputs: int 16                                             *
****************************************************************/

File.prototype.__defineGetter__('size',
function ()
{

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.mPath) 
    return jslibErrorMsg("NS_ERROR_FILE_INVALID_PATH");

  if (!this.exists()) 
    return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");

  var rv = null;
  this.resetCache();
  try { 
    rv=this.mFileInst.fileSize; 
  } catch(e) {
    rv = jslibError(e);
  }

  return rv;
}) // END size Getter

/********************* EXTENSION ********************************
* string getter ext()                                           *
*                                                               *
* string file extension                                         *
* return type string                                            *
* takes no arguments a getter only                              *
*   Ex:                                                         *
*     var p='/tmp/foo.dat';                                     *
*     var f=new File(p);                                        *
*     f.ext;                                                    *
*                                                               *
*   outputs: dat                                                *
****************************************************************/

File.prototype.__defineGetter__('ext', 
function ()
{

  if (!this.checkInst())
    return jslibErrorMsg("NS_ERROR_NOT_INITIALIZED");

  if (!this.mPath) 
    return jslibErrorMsg("NS_ERROR_FILE_INVALID_PATH");
  
  if (!this.exists()) 
    return jslibErrorMsg("NS_ERROR_FILE_NOT_FOUND");

  var rv = null;
  try {
    var leafName  = this.mFileInst.leafName;
    var dotIndex  = leafName.lastIndexOf('.');
    rv=(dotIndex >= 0) ? leafName.substring(dotIndex+1) : "";
  } catch(e) {
    rv = jslibError(e);
  }

  return rv;
}) // END ext Getter

File.prototype.super_help = FileSystem.prototype.help;

/********************* HELP *****************************/
File.prototype.__defineGetter__('help', 
function ()
{
  const help = this.super_help()            +
    "   open(aMode);\n"                     +
    "   read();\n"                          +
    "   readline();\n"                      +
    "   EOF;\n"                             +
    "   write(aContents, aPermissions);\n"  +
    "   copy(aDest);\n"                     +
    "   close();\n"                         +
    "   create();\n"                        +
    "   remove();\n"                        +
    "   size;\n"                            +
    "   ext;\n"                             +
    "   help;\n";

  return help;
})

jslibDebug('*** load: '+JS_FILE_FILE+' OK');

} // END BLOCK JS_LIB_LOADED CHECK

// If jslib base library is not loaded, dump this error.
else
{
    dump("JS_FILE library not loaded:\n"                                +
         " \tTo load use: chrome://jslib/content/jslib.js\n"            +
         " \tThen: include(jslib_file);\n\n");
}
