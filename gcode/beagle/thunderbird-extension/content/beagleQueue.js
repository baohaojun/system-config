//
// beagleQueue.js: Queue component implementation
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

var queueAdd = Components.classes ['@mozilla.org/supports-array;1']
	.createInstance (Components.interfaces.nsISupportsArray);
var queueRemove = Components.classes ['@mozilla.org/supports-array;1']
	.createInstance (Components.interfaces.nsISupportsArray);
var observerService = Components.classes ['@mozilla.org/observer-service;1']
	.getService(Components.interfaces.nsIObserverService);

function notify (data)
{
	var self = GetJsService ('@beagle-project.org/services/queue;1');
	observerService.notifyObservers (self, "beagle-queue", data);
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

	totalAdded: 0,
	totalRemoved: 0,

	init: function ()
	{
		observerService.addObserver (gBeagleQueueObserver, 'quit-application', null);
	},

	// obj is either nsIMsgDBHdr or nsIMsgFolder
	add: function (obj)
	{
		if (queueAdd.GetIndexOf (obj) != -1)
			return;
		
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (obj instanceof Components.interfaces.nsIMsgDBHdr)
			indexer.markHdrAsIndexed (obj);
		else if (obj instanceof Components.interfaces.nsIMsgFolder)
			indexer.markFolderAsIndexed (obj);
		else 
			return;
		
		queueAdd.AppendElement (obj);
		this.totalAdded++;
		notify ('add');
	},

	remove: function (obj)
	{
		if (queueRemove.GetIndexOf (obj) != -1)
			return;
		
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (obj instanceof Components.interfaces.nsIMsgDBHdr)
			indexer.resetHdr (obj, false);
		else if (obj instanceof Components.interfaces.nsIMsgFolder)
			indexer.resetFolder (obj, false, false, false);
		else 
			return;
		
		queueRemove.AppendElement (obj);
		this.totalRemoved++;
		notify ('remove');
	},

	// add, remove* and move* all return true if the object was added to the queue. If the object was
	// rejected by a filter, then they will return false. A filter in this sense is if a mail is marked
	// to be indexed or not (by the user).

	// Add a new header for inclusion in the beagle index
	addHdr: function (hdr)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		// Check if we should index this
		if (!indexer.shouldIndexHdr (hdr) || !indexer.shouldIndexFolder (hdr.folder)) 
			return false;

		this.add (hdr);
		
		this.process ();
		
		return true;
	},

	removeHdr: function (hdr)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (hdr instanceof Components.interfaces.nsIMsgDBHdr) {
			this.remove (hdr);

			this.process ();

			return true;
		}
		
		return false;
	},

	// Basic purpose of this function is to make the main loop run which eventually will pick it up
	addFolder: function (folder)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (indexer.isFolderIndexed (folder))
			return;
		
		notify ('add-folder');
	},

	removeFolder: function (folder)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (folder instanceof Components.interfaces.nsIMsgFolder) {
			this.remove (folder);
			this.process ();

			return true;
		}
		
		return false;
	},

	moveHdr: function (oldHdr, newHdr)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (!indexer.shouldIndexHdr (oldHdr) || !indexer.shouldIndexHdr (newHdr))
			return false;
		
		this.remove (oldHdr);
		this.add (newHdr);
		this.processs ();
		
		return true;
	},

	moveFolder: function (oldFolder, newFolder)
	{
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');
		
		if (!indexer.shouldIndexFolder (oldFolder) || !indexer.shouldIndexFolder (newFolder))
			return false;
			
		this.remove (oldFolder);
		this.add (newFolder);
		this.process ();
			
		return true;
	},

	// This process function will make sure that we have enough objects in the queue before processing
	process: function ()
	{
		var settings = GetJsService ('@beagle-project.org/services/settings;1');
		
		if (this.getQueueCount () < settings.getIntPref ('IndexQueueCount'))
			return;
		
		this.forceProcess ();
	},

	// No object count is done here, mainly so that the queue can be processed at any given time
	forceProcess: function ()
	{
		var count = this.getQueueCount ();
		if (count == 0)
			return;
		
		var indexer = GetJsService ('@beagle-project.org/services/indexer;1');

		// Add new items to the beagle database
		for (var i = 0; i < queueAdd.Count (); i++) {
			var msg = queueAdd.GetElementAt (i).QueryInterface (Components.interfaces.nsIMsgDBHdr);
			if (!msg)
				continue;
			indexer.addToIndex (msg);
		}
		
		// Remove old items from the beagle database
		for (var i = 0; i < queueRemove.Count (); i++) {
			var obj = queueRemove.GetElementAt (i);
			
			if (obj instanceof Components.interfaces.nsIMsgDBHdr) {
				obj.QueryInterface (Components.interfaces.nsIMsgDBHdr);
				indexer.dropHdrFromIndex (obj);
			} else if (obj instanceof Components.interfaces.nsIMsgFolder) {
				obj.QueryInterface (Components.interfaces.nsIMsgFolder);
				indexer.dropFolderFromIndex (obj);
			}
		}
		
		queueAdd.Clear ();
		queueRemove.Clear ();
		
		dump ("Done processing " + count + " items\n");
	},

	getQueueCount: function ()
	{
		return queueAdd.Count () + queueRemove.Count ();
	}

};

// This observer will check if the application is about to quit and process any remaining
// items in the queue when it does
var gBeagleQueueObserver = {

	observe: function (subject, topic, data)
	{
		// Just process whatever is left in the queue
		try {
			var queue = GetJsService ('@beagle-project.org/services/queue;1');
			queue.forceProcess ();
			observerService.removeObserver (gBeagleQueueObserver, 'quit-application');
		} catch (ex) {
		}
	}
};

