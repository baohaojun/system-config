//
// beagleIndexer.js: Indexer component implementation
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

const BEAGLE_INDEX_PROPERTY = 'beagleIndex';
const BEAGLE_SHOULDNOTINDEX_PROPERTY = 'beagleNoIndex';

var object_count = -1;

// This function taks a hashtable as argument with key-value pairs and writes to the next 
// output file. This should be considered an internal function and should only be called
// from within this instance. The type variable should reflect the content and will be the
// document element name (we generate some sort of xml here for easy parsing).
function writeHashTableToNextFile (hashtable, type)
{
	var file = Components.classes ['@mozilla.org/file/local;1']
		.createInstance (Components.interfaces.nsILocalFile);
	file.initWithPath (newOutputFilename ())
	
	// Components.resultseate a stream so that data can be written to file. The various modes were found
	// on this page: http://developer.mozilla.org/en/docs/Code_snippets:File_I/O
	var stream = Components.classes ['@mozilla.org/network/file-output-stream;1']
		.createInstance (Components.interfaces.nsIFileOutputStream);
	try {
		stream.init (file, 0x20 | 0x08 | 0x02, 0664, 0);
	} catch (ex) {
		dump ('Failed to write index file: ' + ex + "\n");
		return;
	}
	
	stream.write ('<' + type + ">\n", type.length + 3);
	for (var key in hashtable) {
		var line = '<' + key + '><![CDATA[' + hashtable [key] + ']]></' + key + ">\n";
		stream.write (line, line.length);
	}
	stream.write ("</" + type + ">\n", type.length + 4);

	stream.close ();
}

function findLastOutputFile ()
{
	var settings = GetJsService ('@beagle-project.org/services/settings;1');
	var dir = Components.classes ['@mozilla.org/file/local;1']
		.getService (Components.interfaces.nsILocalFile);
	
	try {
		dir.initWithPath (settings.getCharPref ('DestinationDirectory') + "/ToIndex");
		
		var enumerator = dir.directoryEntries;
		while (enumerator.hasMoreElements ()) {
			var file = enumerator.getNext ().QueryInterface (Components.interfaces.nsIFile);
			if (!file || !file.isFile ())
				continue;
			
			var number = parseInt (file.leafName);
			if (object_count == -1 || object_count > number)
				object_count = number;
		}
		
		if (object_count == -1)
			object_count = 0;
	} catch (ex) {
		// We default to 0 (zero)
		object_count = 0;
	}
}

// Figure out what our next object name is (we don't overwrite existing files)
function newOutputFilename () 
{
	if (object_count == -1)
		findLastOutputFile ();
		
	var settings = GetJsService ('@beagle-project.org/services/settings;1');
	var file = Components.classes ["@mozilla.org/file/local;1"]
		.createInstance (Components.interfaces.nsILocalFile);
	if (!file)
		throw Components.results.NS_ERROR_FAILURE;
	
	do {
		file.initWithPath (settings.getCharPref ('DestinationDirectory') +
			"/ToIndex/" + object_count++);	
	} while (file.exists ());

	return file.path;
}


Component.prototype = {

	reload: function() {
		loader.loadSubScript(SOURCE, this.__proto__);
	},

	QueryInterface: function(aIID) {
		if(!aIID.equals(INTERFACE) && !aIID.equals(Ci.nsISupports))
			throw Cr.NS_ERROR_NO_INTERFACE;
		return this;
	},

	// Checks if an account should be indexed by pulling its type and checks if that type is enabled
	// for indexing.
	shouldIndexAccount: function (account)
	{
		var settings = GetJsService ('@beagle-project.org/services/settings;1');

		if (account instanceof Components.interfaces.nsIMsgAccount)
			account.QueryInterface (Components.interfaces.nsIMsgAccount);
		else
			return false;
		
		switch (account.incomingServer.type) {
		case 'imap':
			return settings.getBoolPref ('EnableImap');
		case 'pop3':
			return settings.getBoolPref ('EnablePop');
		case 'rss':
			return settings.getBoolPref ('EnableRss');
		case 'nntp':
			return settings.getBoolPref ('Enable.News');
		case 'movemail':
			return settings.getBoolPref ('EnableMailspool');
		case 'none':
			return settings.getBoolPref ('EnableLocal');
		}
		
		return false;
	},

	// A folder or a message should only be indexed in case BEAGLE_SHOULDNOTINDEX_PROPERTY does not
	// exist or if it exists with a value other than '1'
	shouldIndexFolder: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder) {
			var prop = folder.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY);
			return !prop || (prop && prop != '1');
		}
		
		return false;
	},

	shouldIndexHdr: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr) {
			var prop = hdr.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY);
			return !prop || (prop && prop != '1');
		}
		
		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	// A folder or message is indexed if it has the BEAGLE_INDEX_PROPERTY set with value "1".
	isFolderIndexed: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder) 
			return (folder && folder.getStringProperty (BEAGLE_INDEX_PROPERTY) == '1');

		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	isHdrIndexed: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr) 
			return (hdr && hdr.getStringProperty (BEAGLE_INDEX_PROPERTY) == '1');
		
		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	isFolderUserMarked: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder)
			return (folder && folder.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY) == '1');
		
		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	isHdrUserMarked: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr)
			return (hdr && hdr.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY) == '1');
		
		throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	markFolderAsIndexed: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder)
			folder.setStringProperty (BEAGLE_INDEX_PROPERTY, '1');
		else 
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	markHdrAsIndexed: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr)
			hdr.setStringProperty (BEAGLE_INDEX_PROPERTY, '1');
		else	
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	markFolderAsUserMarked: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder) 
			folder.setStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY, '1');
		else
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	markHdrAsUserMarked: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr)
			hdr.setStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY, '1');
		else
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	resetFolderUserMarked: function (folder)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder) { 
			if (folder.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY) != '')
				folder.setStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY, '');
		} else
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	resetHdrUserMarked: function (hdr)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr) {
			if (hdr.getStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY) != '')
				hdr.setStringProperty (BEAGLE_SHOULDNOTINDEX_PROPERTY, '');
		} else
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	resetFolder: function (folder, userMarked, recursive, content)
	{
		if (folder instanceof Components.interfaces.nsIMsgFolder) {
			// We only update with a new value if we already have one
			if (folder.getStringProperty (BEAGLE_INDEX_PROPERTY) != '') 
				folder.setStringProperty (BEAGLE_INDEX_PROPERTY, '');

			if (userMarked)
				this.resetFolderUserMarked (folder);
		} else 
			throw Components.results.NS_ERROR_NO_INTERFACE;
		
		// Should we mark content as well?
		if (content && folder.getTotalMessages (false) > 0) {
			var enumerator = folder.getMessages (null);
			while (enumerator.hasMoreElements ()) {
				var hdr = enumerator.getNext ().QueryInterface (Components.interfaces.nsIMsgDBHdr);
				if (!hdr)
					continue;
				this.resetHdr (hdr);
			}
		}
		
		// Should we go recursive too?
		if (recursive) {
			var allFolders = Components.classes ['@mozilla.org/supports-array;1']
				.createInstance (Components.interfaces.nsISupportsArray);
			folder.ListDescendents (allFolders);
			for (var i = 0; i < allFolders.Count (); i++) {
				var subFolder = allFolders.QueryElementAt (i, Components.interfaces.nsIMsgFolder);
				if (!subFolder) 
					continue;
				
				// Using ListDescendents above will "plain out" the folder list and give us all folders.
				// This is why we should not go recursive from here.
				this.resetFolder (subFolder, userMarked, false, content);
			}
		}
	},

	resetHdr: function (hdr, userMarked)
	{
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr) {
			// We only update with a new value if we already have one
			if (hdr.getStringProperty (BEAGLE_INDEX_PROPERTY) != '')
				hdr.setStringProperty (BEAGLE_INDEX_PROPERTY, '');
			
			if (userMarked) 
				this.resetHdrUserMarked (hdr);
		} else 
			throw Components.results.NS_ERROR_NO_INTERFACE;
	},

	resetEverything: function (userMarked)
	{
		var gAccountManager = Components.classes ['@mozilla.org/messenger/account-manager;1']
			.getService (Components.interfaces.nsIMsgAccountManager);
		var accounts = gAccountManager.accounts;
		
		for (var i = 0; i < accounts.Count (); i++) {
			var account = accounts.QueryElementAt (i, Components.interfaces.nsIMsgAccount)
			if (!account)
				continue;
			
			// We reset everything recursively
			this.resetFolder (account.incomingServer.rootFolder, userMarked, true, true);
		}
	},

	addToIndex: function (hdr)
	{
		try {
			hdr.QueryInterface (Components.interfaces.nsIMsgDBHdr);
		} catch (ex) {
		}
		
		if (!hdr)
			throw Components.interfaces.NS_ERROR_NO_INTERFACE;
			
		var properties = new Array ();
		
		var serverType = hdr.folder.server.type, type = null;
		
		// We must ensure that all elements exist. Some of them might throw an exception in various
		// set-ups, so we have to catch them and default to something.
		properties ['Author'] = hdr.author;
		properties ['Date'] = hdr.dateInSeconds;
		properties ['Folder'] = hdr.folder.name;
		properties ['FolderFile'] = hdr.folder.path.unixStyleFilePath;
		
		try {
			properties ['HasOffline'] = hdr.folder.hasMsgOffline (hdr.messageKey);
		} catch (ex) {
			dump ('Failed to parse HasOffline: ' + ex + "\n");
			properties ['HasOffline'] = 'false';
		}
		
		properties ['MessageId'] = hdr.messageId;
		properties ['MessageSize'] = hdr.messageSize;
		properties ['MessageOffset'] = hdr.messageOffset;
		properties ['OfflineSize'] = hdr.offlineMessageSize;
		properties ['Recipients'] = hdr.recipients;
		properties ['Subject'] = hdr.subject;
		properties ['MessageKey'] = hdr.messageKey;
		
		try {
			properties ['Uri'] = hdr.folder.getUriForMsg (hdr);
		} catch (ex) { 
			properties ['Uri'] = null;
			dump ('Failed to parse uri: ' + ex + "\n");
		}
		
		switch (serverType) {
		case 'none': // local account
		case 'pop3':
		case 'imap':
		case 'nntp':
		case 'movemail':
			type = 'MailMessage';
			break;
		case 'rss':
			// We usually have the content body available but hasMsgOffline still reports false. Just
			// default to true until this mess has been cleared out.
			properties ['HasOffline'] = true;
			
			try {
				var db = hdr.folder.getMsgDatabase (null);
				var urls = db.dBFolderInfo.getCharPtrProperty ('feedUrl');
				properties ['FeedURL'] = urls.substring (urls.lastIndexOf ('|')+1);
			} catch (ex) {
				properties ['FeedURL'] = 'Unknown';
			}
			type = 'FeedItem';
			break;
		}
		
		// Write everything to file
		if (type) {
			dump ('Writing ' + type + ' to file. Subject: ' + properties ['Subject'] + "\n");
			writeHashTableToNextFile (properties, type);
		}
	},

	dropFolderFromIndex: function (folder)
	{
		try {
			folder.QueryInterface (Components.interfaces.nsIMsgFolder);
		} catch (ex) {
		}
		
		if (!folder)
			throw Components.interfaces.NS_ERROR_NO_INTERFACE;

		// We don't even bother if no messages exist
		if (folder.getTotalMessages (false) < 1)
			return;

		var properties = new Array ();
		properties ['FolderFile'] = folder.path.unixStyleFilePath;
		
		writeHashTableToNextFile (properties, 'DeleteFolder');
	},

	dropHdrFromIndex: function (hdr)
	{
		try {
			hdr.QueryInterface (Components.interfaces.nsIMsgDBHdr);
		} catch (ex) {
		}
		
		if (!hdr)
			throw Components.interfaces.NS_ERROR_NO_INTERFACE;
		
		var properties = new Array ();
		properties ['Uri'] = hdr.folder.getUriForMsg (hdr);

		properties ['FolderFile'] = hdr.folder.path.unixStyleFilePath;
		properties ['MessageKey'] = hdr.messageKey;

		writeHashTableToNextFile (properties, 'DeleteHdr');
	}
};

