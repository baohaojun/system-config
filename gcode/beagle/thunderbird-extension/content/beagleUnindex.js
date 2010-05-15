//
// beagleUnindex.js: Unindexing code, used when data needs to be removed from index or unindexed
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

// FIXME: This entire implementation should somehow be threaded (or similar) to update GUI correctly

const FOLDER_MODE_REMOVE = 0;
const FOLDER_MODE_UNINDEX = 1;
const HDR_MODE_REMOVE = 2;
const HDR_MODE_UNINDEX = 3;
const EVERYTHING_MODE_REMOVE = 4;
const EVERYTHING_MODE_UNINDEX = 5;

var gBeagleIndexer = GetJsService ('@beagle-project.org/services/indexer;1');
var gBeagleQueue = GetJsService ('@beagle-project.org/services/queue;1');

//
//	Call one of the following functions to remove or unindex content
//

var gBeagleUnindex = {

	RemoveFolder: function (folder, recursive, askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
		
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							FOLDER_MODE_REMOVE, folder, recursive, userMarked);
	},

	RemoveHdrs: function (hdrs, askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
			
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							HDR_MODE_REMOVE, hdrs, userMarked);
	},

	RemoveEverything: function (askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
		
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							EVERYTHING_MODE_REMOVE, userMarked);
	},

	UnindexFolder: function (folder, recursive, askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
			
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							FOLDER_MODE_UNINDEX, folder, recursive, userMarked);
	},

	UnindexHdrs: function (hdrs, askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
		
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							HDR_MODE_UNINDEX, hdrs, userMarked);
	},

	UnindexEverything: function (askUserMarked)
	{
		var userMarked = false;
		if (askUserMarked) {
			var ret = getUserMarkedChoice ();
			if (ret == 0)
				userMarked = true;
			else if (ret == 2)
				return;
		}
		
		window.openDialog ('chrome://beagle/content/beagleUnindex.xul',
							'UnindexWindow',
							'chrome,modal=yes,resizable=no',
							EVERYTHING_MODE_UNINDEX, userMarked);
	}
};

//
//	Various handlers and functions
//

function onLoad ()
{
	window.setTimeout (function () { startProcessing () }, 0);
}

function startProcessing ()
{
	// Make sure we have arguments (we should always have that)
	if (!window.arguments || window.arguments.length < 1) {
		window.close (); 
		return;
	}

	var remove = false;
	switch (window.arguments [0]) {
	case FOLDER_MODE_REMOVE:
		remove = true;
	case FOLDER_MODE_UNINDEX:
		prepareFolder (window.arguments [1], window.arguments [2], true, window.arguments [3]);
		handleFolder (window.arguments [1], window.arguments [2], remove, window.arguments [3]);
		break;
	case HDR_MODE_REMOVE:
		remove = true;
	case HDR_MODE_UNINDEX:
		prepareHdrs (window.arguments [1], window.arguments [2]);
		handleHdrs (window.arguments [1], window.arguments [2], remove);
		break;
	case EVERYTHING_MODE_REMOVE:
		remove = true;
	case EVERYTHING_MODE_UNINDEX:
		prepareEverything ();
		handleEverything (remove, window.arguments [1]);
		break;
	}
	
	// Force a process
	gBeagleQueue.forceProcess ();

	window.close ();
}

// Returns 0 when true, 1 when false and 2 when cancel
function getUserMarkedChoice ()
{
	var bundle = document.getElementById ('bundle_beagle');
	var prompts = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
		.getService(Components.interfaces.nsIPromptService);
	var flags = prompts.BUTTON_POS_0 * prompts.BUTTON_TITLE_YES + 
				prompts.BUTTON_POS_1 * prompts.BUTTON_TITLE_NO + 
				prompts.BUTTON_POS_2 * prompts.BUTTON_TITLE_CANCEL;
	var check = {value: true}
	var button = prompts.confirmEx (window, 
		bundle.getString ('userMarkedContent'),
		bundle.getString ('removeUserMarkedContent'),
		flags,
		null, null, null, null, check);
		
	return button;
}

var folder_count = 0;
var current_folder = 0;
var message_count = 0;
var current_message = 0;

// Updates window with current statistics
function updateWindow ()
{
	// Everything folder related goes here
	var folderStatus = document.getElementById ('folder-status');
	var folderMeter = document.getElementById ('folder-status-meter');
	folderStatus.value = current_folder + '/' + folder_count;
	folderMeter.value = 100 * (current_folder / folder_count);
	
	// Next are message related things
	var messageStatus = document.getElementById ('message-status');
	var messageMeter = document.getElementById ('message-status-meter');
	messageStatus.value = current_message + '/' + message_count;
	messageMeter.value = 100 * (current_message / message_count);
}

function prepareDone ()
{
	// Take GUI from preparation mode to unindex/remove mode
	var statusElem = document.getElementById ('unindex-mainstatus');
	var prepareElem = document.getElementById ('unindex-preparestatus');
	var currentElem = document.getElementById ('current-item')
	currentElem.value = '';
	prepareElem.setAttribute ('style', 'display: none;');
	statusElem.setAttribute ('style', '');
	updateWindow ();
}

// Preapares the GUI when removing/unindexing a folder
function prepareFolder (folder, recursive, gui)
{
	if (!folder)
		return;
	
	// Update our statistics
	if (recursive) {
		var allFolders = Components.classes ['@mozilla.org/supports-array;1']
			.createInstance (Components.interfaces.nsISupportsArray);
		folder.ListDescendents (allFolders);
		folder_count += allFolders.Count () + 1;
		message_count += folder.getTotalMessages (true);
		current_folder = 0;
		current_message = 0;
	} else {
		folder_count += 1;
		message_count += folder.getTotalMessages (false);
		current_folder = 0;
		current_message = 0;
	}
	
	if (gui)
		prepareDone ();
}

function prepareHdrs (hdrs)
{
	// Pretty easy, huh?
	folder_count = 1;
	message_count = hdrs.Count ();
	current_folder = 0;
	current_message = 0;
	prepareDone ();
}

function prepareEverything ()
{
	var gAccountManager = Components.classes ['@mozilla.org/messenger/account-manager;1']
		.getService (Components.interfaces.nsIMsgAccountManager);
	var accounts = gAccountManager.accounts;
	
	for (var i = 0; i < accounts.Count (); i++) {
		var account = accounts.QueryElementAt (i, Components.interfaces.nsIMsgAccount);
		if (!account)
			continue;
		
		prepareFolder (account.incomingServer.rootFolder, true, false);
	}
	
	prepareDone ();
}

function handleFolder (folder, recursive, remove, userMarked)
{
	if (!folder)
		return;

	var currentItem = document.getElementById ('current-item');
		
	if (recursive) {
		var allFolders = Components.classes ['@mozilla.org/supports-array;1']
			.createInstance (Components.interfaces.nsISupportsArray);
		folder.ListDescendents (allFolders);
		allFolders.AppendElement (folder);
		for (var i = 0; i < allFolders.Count (); i++) {
			var currentFolder = allFolders.QueryElementAt (i, Components.interfaces.nsIMsgFolder);
			if (!currentFolder)
				continue;
		
			handleFolder (currentFolder, false, remove, userMarked);
		}
	} else {
	    try {
			currentItem.value = folder.prettyName;
			
			// Update user interface
			current_folder++;
			updateWindow ();

			gBeagleIndexer.resetFolder (folder, userMarked, false, false);
			if (remove)
				gBeagleQueue.removeFolder (folder);
			
			// Only process content if we have any content (getMessages will throw an exception otherwise)
			if (folder.getTotalMessages (false) > 0) {		
				var enumerator = folder.getMessages (null);
				while (enumerator.hasMoreElements ()) {
					var hdr = enumerator.getNext ().QueryInterface (Components.interfaces.nsIMsgDBHdr);
					if (!hdr)
						continue;
					current_message++;
					updateWindow ();
					gBeagleIndexer.resetHdr (hdr, userMarked);
				}
				folder.getMsgDatabase (null).Commit (1);
			}
		} catch (ex) {
		}
	}
}

function handleHdrs (hdrs, userMarked, remove)
{
	var folder = null;
	
	for (var i = 0; i < hdrs.Count (); i++) {
		var hdr = hdrs.QueryElementAt (i, Components.interfaces.nsIMsgDBHdr);
		if (!hdr)
			continue;
		
		// We save the folder so we can mark it as "unindexed". Otherwise the extension won't try to re-index
		// the content of the folder.
		if (!folder)
			folder = hdr.folder;
		
		// We only remove in case the message is indexed
		var isIndexed = gBeagleIndexer.isHdrIndexed (hdr);
		if (isIndexed) {
			if (remove)
				gBeagleQueue.removeHdr (hdr);
			else
				gBeagleIndexer.resetHdr (hdr, false);
		}
		if (userMarked)
			gBeagleIndexer.resetHdrUserMarked (hdr);
		
		current_message++;
		updateWindow ();
	}
	
	// Reset folder
	if (folder) 
		gBeagleIndexer.resetFolder (folder, false, false, false);
}

function handleEverything (remove, userMarked)
{
	var gAccountManager = Components.classes ['@mozilla.org/messenger/account-manager;1']
		.getService (Components.interfaces.nsIMsgAccountManager);
	var accounts = gAccountManager.accounts;
	
	for (var i = 0; i < accounts.Count (); i++) {
		var account = accounts.QueryElementAt (i, Components.interfaces.nsIMsgAccount);
		if (!account)
			continue;
		handleFolder (account.incomingServer.rootFolder, true, remove, userMarked);
	}
}

