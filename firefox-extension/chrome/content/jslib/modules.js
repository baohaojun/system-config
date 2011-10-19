/** 
 * jslib module identifiers
 */

const JS_MODULES_LOADED        = true;
const JS_MODULES_FILE          = "modules.js";

// insure jslib base is loaded
if (typeof(JS_LIB_LOADED)=='boolean') 
{

// help identifier
const jslib_help = "need to write some global help docs here\n";

// Library Identifiers

// io library modules
const jslib_io         = JS_LIB_PATH+'io/io.js';
const jslib_filesystem = JS_LIB_PATH+'io/filesystem.js'
const jslib_file       = JS_LIB_PATH+'io/file.js';
const jslib_fileutils  = JS_LIB_PATH+'io/fileUtils.js';
const jslib_dir        = JS_LIB_PATH+'io/dir.js';
const jslib_dirutils   = JS_LIB_PATH+'io/dirUtils.js';

// data structures
const jslib_dictionary       = JS_LIB_PATH+'ds/dictionary.js';
const jslib_chaindictionary  = JS_LIB_PATH+'ds/chainDictionary.js';

// RDF library modules
const jslib_rdf           = JS_LIB_PATH+'rdf/rdf.js';
const jslib_rdffile       = JS_LIB_PATH+'rdf/rdfFile.js';
const jslib_rdfcontainer  = JS_LIB_PATH+'rdf/rdfContainer.js';
const jslib_rdfresource   = JS_LIB_PATH+'rdf/rdfResource.js';
const jslib_rdfmemory     = JS_LIB_PATH+'rdf/inMemoryRDF.js';

// network library modules
const jslib_remotefile  = JS_LIB_PATH+'network/remoteFile.js';
const jslib_socket      = JS_LIB_PATH+'network/socket.js';

// network - http
const jslib_http                = JS_LIB_PATH+'network/http.js';
const jslib_getrequest          = JS_LIB_PATH+'network/getRequest.js';
const jslib_postrequest         = JS_LIB_PATH+'network/postRequest.js';
const jslib_multipartrequest    = JS_LIB_PATH+'network/multipartRequest.js';
const jslib_filepart            = JS_LIB_PATH+'network/parts/filePart.js';
const jslib_textpart            = JS_LIB_PATH+'network/parts/textPart.js';
const jslib_urlparameterspart   = JS_LIB_PATH+'network/parts/urlParametersPart.js';
const jslib_bodyparameterspart  = JS_LIB_PATH+'network/parts/bodyParametersPart.js';

// xul dom library modules
const jslib_dialog      = JS_LIB_PATH+'xul/commonDialog.js';
const jslib_filepicker  = JS_LIB_PATH+'xul/commonFilePicker.js';
const jslib_window      = JS_LIB_PATH+'xul/commonWindow.js';
const jslib_routines    = JS_LIB_PATH+'xul/appRoutines.js';

// sound library modules
const jslib_sound = JS_LIB_PATH+'sound/sound.js';

// utils library modules
const jslib_date     = JS_LIB_PATH+'utils/date.js';
const jslib_prefs    = JS_LIB_PATH+'utils/prefs.js';
const jslib_validate = JS_LIB_PATH+'utils/validate.js';
const jslib_sax      = JS_LIB_PATH+'utils/sax.js';

// zip
const jslib_zip  = JS_LIB_PATH+'zip/zip.js';

// install/uninstall
const jslib_install    = JS_LIB_PATH+'install/install.js';
const jslib_uninstall  = JS_LIB_PATH+'install/uninstall.js';
const jslib_autoupdate = JS_LIB_PATH+'install/autoupdate.js';

}

