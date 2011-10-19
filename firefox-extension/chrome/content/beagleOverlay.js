/*
 * Beagle Extension: Index webpages you visit using the Beagle Indexing Engine.
 * An Extension for the Firefox  Browser.
 */

var beagle = {
    //some constant 
    RUN_BEAGLE_NOT_FOUND:-1,
    RUN_INITED: 0,
    RUN_ENABLED : 1,
    RUN_DISABLED : 2,
    RUN_ERROR : 3,
    
    pref : beaglePref,

    ENV:Components.classes["@mozilla.org/process/environment;1"].getService(Components.interfaces.nsIEnvironment),

    FILE_UTILS:new FileUtils(),// js lib  file utils
    
    /**
     *@see http://www.xulplanet.com/references/xpcomref/comps/c_embeddingbrowsernsWebBrowserPersist1.html
     */
    get PersistMask(){
        var comp = Components.classes["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"].createInstance(Components.interfaces.nsIWebBrowserPersist);
        return (comp.PERSIST_FLAGS_FROM_CACHE | 
                comp.PERSIST_FLAGS_REPLACE_EXISTING_FILES |
                comp.PERSIST_FLAGS_NO_BASE_TAG_MODIFICATIONS |
                comp.PERSIST_FLAGS_DONT_FIXUP_LINKS |
                comp.PERSIST_FLAGS_DONT_CHANGE_FILENAMES |
                comp.PERSIST_FLAGS_CLEANUP_ON_FAILURE);
    },

    get EncodeMask(){
        var comp = Components.classes["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"].createInstance(Components.interfaces.nsIWebBrowserPersist);
        return (comp.ENCODE_FLAGS_RAW | comp.ENCODE_FLAGS_ABSOLUTE_LINKS);
    },

    get STATUS_ICON(){ 
        return document.getElementById('beagle-notifier-status');
    },
    /**
     * beagle data path (should be ~/.beagle/ToIndex)
     */
    dataPath : null,
    
    
    /**
     * the path to beagle search ,it is used for search (for link/page/text)
     */
    get beagleSearchPath() { 
        var path = this.ENV.get("PATH");
        if (path) {
            var split = path.split(':');
            var idx = 0;
            while (idx < split.length) 
            {
                var trypath = split[idx++] + '/' + "beagle-search";
                if (this.FILE_UTILS.exists(trypath))
                    return trypath;
            }
        }
        return undefined;
    },
    
    /**
     * save the tasks for the purpose of extra meta data
     */ 
    tasks: [],
   
    /**
     * always call this before any index work begins 
     */
    startTask : function(url,extrameta)
    {
        this.tasks[url] = {meta:extrameta};
    },

    /**
     * get the content file path for a give url and type 
     * type can be "web" or "bookmark"
     */
    getContentPath: function(url,type)
    {
        if(typeof type == "undefined")
            type = "web";
        var hash = hex_md5(url);
        return this.dataPath + "/firefox-beagle-"+ type + "-" + hash;
        
    },

    /**
     * get the meta file path for a give url and type 
     * type can be "web" or "bookmark"
     */
    getMetaPath: function(url,type)
    {
        if(typeof type == "undefined")
            type = "web";
        var hash = hex_md5(url);
        return this.dataPath + "/.firefox-beagle-"+ type + "-" + hash;
    },

    /**
     * init beagle
     * check environment
     * init state
     * init some varible
     * add event listeners 
     * import pref from oldExtension (if beagle.first.run)
     */
    init : function()
    {
        log("init");
        if(!this.checkEnv())
        {
            this.runStatus = this.RUN_BEAGLE_NOT_FOUND;
        }
        else
        {
            if(this.pref.get('beagle.autoindex.active'))
            {
                this.enable();
            }
            else
            {
                this.disable();
            }
            if(!this.FILE_UTILS.exists(this.dataPath))
            {
                var d = new Dir(this.dataPath);
                d.create();
            };
        }
        this.addEventListener();
        if (this.pref.get("beagle.first.run"))
        {
            this.pref.firstRunImport();
            this.pref.set("beagle.first.run",false);
        }
    },
    
    /**
     * add the event listeners
     * 
     */
    addEventListener : function ()
    {
        // Add listener for page loads
        if (this.runStatus != this.RUN_BEAGLE_NOT_FOUND && document.getElementById("appcontent"))
        {
            document.getElementById("appcontent").addEventListener(
                "load",
                Function.bind(this.onPageLoad,this),
                true
            );
            document.getElementById("contentAreaContextMenu").addEventListener(
                "popupshowing", 
                Function.bind(this.initContextMenu,this), 
                false
            );
        }
        if(this.runStatus != this.RUN_BEAGLE_NOT_FOUND)
        {
            window.addEventListener(
                "unload",
                function(){
                    if(beaglePref.get("beagle.bookmark.active"))
                        bookmarkIndexer.indexModified(false);
                },
                false
            );
/*            var observerService =
                 Components.classes["@mozilla.org/observer-service;1"]
                    .getService(Components.interfaces.nsIObserverService)
            var observer = {
                observe: function(subject,topic,data){
                    log("index bookmarks when exit");
                    dump(bookmarkIndexer.indexModified());
                    log(" done r");
                }
            };
            observerService.addObserver(observer,"quit-application",false);
*/
        }
        this.STATUS_ICON.addEventListener(
            'click',
            Function.bind(this.onIconClick,this),
            false
        );
    },

    /**
     * show the beagle context menu
     */
    initContextMenu : function (e)
    {
        if(e.originalTarget.id != "contentAreaContextMenu")
            return;
        //log(" gContextMenu " + gContextMenu );
        gContextMenu.showItem("beagle-context-index-this-link", gContextMenu.onLink && !gContextMenu.onMailtoLink); 
        gContextMenu.showItem("beagle-context-index-this-image", gContextMenu.onImage && gContextMenu.onLoadedImage); 
        gContextMenu.showItem("beagle-context-search-link", gContextMenu.onLink); 
        gContextMenu.showItem("beagle-context-search-text", gContextMenu.isTextSelected);
        document.getElementById("beagle-context-search-text").setAttribute("label",
            _f("beagle_context_search_text",[getBrowserSelection(16)]));
    },

    /**
     *  check enviroment 
     */
    checkEnv : function()
    {   
        var storage_directory = this.pref.get("beagle.storage.directory"); 
        if(storage_directory == "")
            storage_directory = this.ENV.get("HOME") + "/.beagle";
        if (!this.FILE_UTILS.exists (storage_directory)) {
	    var errorMsg = (_f("beagle_check_env_error", [storage_directory]));
	    this.error(errorMsg);
            return false;
        }
        this.dataPath = storage_directory + "/ToIndex";
        return true;
    },

    /**
     * Check the page, 
     * 1. the protocal (We will NOT index about:* file:///* )
     * 2. check is the page  itself 
     * TODO: for file:/// chrome:// and so on Index OR NOT
     */
    checkPage : function(page)
    {
        if (!page)
        {
            log("[checkPage the page doesn't seems to be a page");
            return false;
        } 
        if (!page.location ||
            !page.location.href ||
            page.location.href.indexOf("about:") == 0 ||
            page.location.href.indexOf("file:") == 0 )
        {
            log("checkPage  strage page " + page );
            return false;
        }
        return true;
    },

    /*
     * check weather the url should index
     * return true if it need to index
     */
    shouldIndex : function(page)
    {

        var prefObject = this.pref.load();

	// Don't index any of the custom URLs; only http[s]:// is good for browsing history
	if (page.location.protocol != "http:" && page.location.protocol != "https:")
	{
	    return false;
	}

        //check https
        if (page.location.protocol == "https:" && !prefObject['beagle.security.active'])
        {
            return false;
        }

        var lists = ['beagle.exclude.list','beagle.include.list'];
        var flags = [false,false];
        for(var j = 0; j < 2; j++)
        {
            var list = parseJSON(prefObject[lists[j]]);
            var len = list.length;
            var flag = false;
            for(var i = 0; i < len && !flag; i++)
            {
                switch(list[i]['patternType'])
                {
                case 'domain':
                    //what means a domain matches
                    //www.google.com matches google.com and matches .com
                    //www.agoogle.com NOT matches google.com but matches com
                    //www.com.google. NOT matches .com 
                    var hostname = page.location.hostname;
                    var pattern = list[i]['pattern'];
                    if (pattern[0] != '.')
                        pattern = "." + pattern;
                    flag = hostname.isEndWith(pattern) ||(hostname == list[i]['pattern']);
                    break;
                case 'wildcard':
                    var re =  RegExp(list[i]['pattern'].wilcard2RE());
                    flag = (page.location.href.match(re) != null);
                    break;
                case 'regular expression':
                    var re = RegExp(list[i]['pattern']);
                    flag = (page.location.href.match(re) != null)
                    break;
                default:
                    log("invaild rule" + list[i]); 
                    //something wrong;
                    break;
                }
            }
            flags[j] = flag;
        }
        log("[Should Index ?][exclude=" + flags[0] + "][include=" + flags[1] + "]");
        if(!flags[0] && !flags[1])
            return prefObject['beagle.default.action'] == 1;
        if(flags[0] && flags[1])
            return prefObject['beagle.conflict.action'] == 1;
        return flags[1];

    },
    
    /**
     * just set the status label 
     */
    setStatusLabel : function (msg)
    {
        setTimeout(
            function(){document.getElementById('statusbar-display').label = msg},
            100
        );
    },
    
    /**
     * prompt extra keywords on demand-index
     */
    promptExtraKeywords : function(url)
    {
        if(this.pref.get("beagle.prompt.keywords.active"))
        {
            var prompts = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
                                    .getService(Components.interfaces.nsIPromptService);
            var input = { value: "" };
            var chk = { value:false };
            result = prompts.prompt(window, 
                _("beagle_prompt_keywords_title"),
                _("beagle_prompt_keywords_text"), input, null, chk);
            if (result && input.value != "")
            {
                this.tasks[url]["meta"].push("t:dc:keyword="+ input.value);
            }
        }
    },

    /***************************************************
     *        IO related code 
     **************************************************/

    /**
     * write page content (NOT the HTML source, 
     * the DOM instead, it may include dym contnent created by js)
     */
    writeContent : function(page, tmpfilepath)
    {
        var tmpfile = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
        tmpfile.initWithPath(tmpfilepath);

        var persist = Components.classes["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"].createInstance(Components.interfaces.nsIWebBrowserPersist);
        persist.persistFlags = this.PersistMask;
        persist.saveDocument(page, tmpfile, null, null, this.EncodeMask, 0);
    },

    /**
     * for non-html and non-text file . save it 
     * progressListener is used by index-link. to show the progree.
     */
    saveFile : function(url,path,progressListener)
    {
        var tmpfile = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
        tmpfile.initWithPath(path);
        var cacheKey  = Components.classes['@mozilla.org/supports-string;1'].createInstance(Components.interfaces.nsISupportsString);
        cacheKey.data = url;
        var urifix  = Components.classes['@mozilla.org/docshell/urifixup;1'].getService(Components.interfaces.nsIURIFixup);
        var uri     = urifix.createFixupURI(url, 0);
        var hosturi = null;
        if (uri.host.length > 0)
        {
            hosturi = urifix.createFixupURI(uri.host, 0);
        }
        this.persist = Components.classes['@mozilla.org/embedding/browser/nsWebBrowserPersist;1'].createInstance(Components.interfaces.nsIWebBrowserPersist);
        this.persist.persistFlags = this.PersistMask;
        if(progressListener)
            this.persist.progressListener = progressListener; 
        this.persist.saveURI(uri, cacheKey, hosturi, null, null, tmpfile);
    },

    /**
     * write raw-meatadata 
     */
    writeRawMetadata : function(meta, tmpfilepath)
    {
        var tmpfile = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
        tmpfile.initWithPath(tmpfilepath);

        var stream = Components.classes["@mozilla.org/network/file-output-stream;1"].createInstance(Components.interfaces.nsIFileOutputStream);
        stream.QueryInterface(Components.interfaces.nsIOutputStream);
        stream.init(tmpfile, 0x04 | 0x08 | 0x20, 0600, 0);

        var line;
        log("writing metas ");
        for(var i = 0; i < meta.length; i++)
        {
            line = meta[i] + "\n";
            log(meta[i]);
            stream.write(line, line.length);
        }
        stream.flush();
        stream.close();
    },


    /**
     * write meatadata of page  
     * include URI hittype mimetype characterset referrer 
     * if any extra meta is set in task, write it too.
     */
    writeMetadata : function(page, tmpfilepath)
    {
        var url = page.location.href;
        var meta = [
            url,
            'WebHistory',
            page.contentType,
            "k:_unindexed:encoding="+page.characterSet,
            ];
        if(typeof page.referrer != "undefined" && page.referrer != "")
        {
            meta.push("t:fixme:referrer=" + page.referrer);
        }

	// Tokenize the url
	var loc = page.location;
	var url_tokenized = loc.host + " " + loc.port + " " + loc.pathname + " " + loc.hash + " " + loc.search;
	url_tokenized = url_tokenized.replace(/\./g, " ").replace(/\//g, " ").replace(/&/g, " ").replace (/\+/g, " ").replace (/=/g, " ").replace(/%../g, " ");
	meta.push("t:beagle:inuri=" + url_tokenized);

        meta = meta.concat(this.tasks[url]['meta'])
        beagle.writeRawMetadata(meta,tmpfilepath);
    },

    /**
     * index a page 
     * write content and meta
     */
    indexPage : function(page)
    {
        var url = page.location.href;
        log(" We will index " + url ); 
        
        try {
            this.writeContent(page, this.getContentPath(url));
            this.writeMetadata(page, this.getMetaPath(url));
        } catch (ex) {
            log ("beaglePageLoad: beagleWriteContent/Metadata failed: " + ex );
            if(confirm(_('beagle_write_error_confirm')))
                this.disable();
            return;
        }
        this.setStatusLabel(_f("beagle_statuslabel_indexing",[url]));
    },
    
    /**
     * index a file (non-html and non-text) 
     * we assume the content is saved already.
     * just write meta here
     */
    indexFile : function(url,contentType)
    {
        log(" We will index " + url ); 
        
        var meta = [url,'WebHistory',contentType];
        meta = meta.concat(this.tasks[url]['meta'])
        try {
            this.writeRawMetadata(meta, this.getMetaPath(url));
        } catch (ex) {
            log ("[indexFile] beage write Metadata failed: " + ex + "\n");
            if(confirm(_('beagle_write_error_confirm')))
                this.disable();
            return;
        }
        this.setStatusLabel(_f("beagle_statuslabel_indexing",[url]));
    },
    
    /****************************************************************************
     *                      Event Handlers 
     ***************************************************************************/

    /**
     * index this page (event handler)
     */
    indexThisPage : function()
    {
        var doc = document.getElementById('content').selectedBrowser.contentDocument;
        if(!this.checkPage(doc))
            return;
        var url = doc.location.href;
        this.startTask(url,[]);
        this.promptExtraKeywords(url);
        if(doc.contentType.match(/(text|html|xml)/i))// a document
        {
            this.indexPage(doc);
        }
        else
        {
            this.saveFile(url,this.getContentPath(url),null);
            this.indexFile(url,doc.contentType);
        }
    },

    /**
     * index link (event handler)
     */
    indexLink : function()
    {
        var url = gContextMenu.linkURL; 
        if (!url)
            return;
        var referrer = gBrowser.currentURI.spec;
        this.startTask(url,
            ["t:fixme:referrer=" + referrer]
            );
        window.openDialog("chrome://beagle/content/indexLink.xul",
            "","chrome,centerscreen,all,resizable,dialog=no",url,referrer);
    },
    /**
     * index image (event handler)
     */
    indexImage : function()
    {
        var image = gContextMenu.target; 
        if(image.tagName.toLowerCase() != 'img' || !image.src)
            return;
        var url = image.src;
        var referrer = gBrowser.currentURI.spec;
        this.startTask(url,[
                "t:alttext="+(image.getAttribute('alt')?image.getAttribute('alt'):""),
                "t:fixme:referrer="+referrer]
                );
        window.openDialog("chrome://beagle/content/indexLink.xul",
            "","chrome,centerscreen,all,resizable,dialog=no",url,referrer);
    },

    /**
     * callback for link loaded  (called from indexLink.js)
     * TODO: what if the url is no longer the url we passed to indexLink
     */
    onLinkLoad : function(url,contentType,doc,orginalURL)
    {   
        if(url != orginalURL)
        {
            log(url)
            log(orginalURL);
            this.tasks[url] = this.tasks[orginalURL];
        }
        this.promptExtraKeywords(url);
        if(contentType.match(/(text|html|xml)/i) && doc)// a document
        {
            if(!this.checkPage(doc))
                return;
            this.indexPage(doc);
        }
        else
        {
            this.indexFile(url,contentType);
        }
    },

    /**
     * called when page is loaded (event handler)
     */
    onPageLoad : function(event)
    { 
        log("Page Loaded ");
        //if disabled or error
        if(this.runStatus != this.RUN_ENABLED)
        {
            log(" NOT RUN_ENABLED status .  NO INDEX");
            return;
        }
        var page = event.originalTarget;
        if (!this.checkPage(page))
            return;
        if (!this.shouldIndex(page))
            return;
        var url = page.location.href;
        this.startTask(url,[]);
        if(page.contentType.match(/(text|html|xml)/i))// a document
        {
            this.indexPage(page);
        }
        else
        {
            this.saveFile(url,this.getContentPath(url),null);
            this.indexFile(url,page.contentType);
        }
    },   
    
        
    /**
     * add exclude /include rule
     * the "domain" rule
     */
    quickAddRule : function (page,flag)
    {
        try{
            var domain =  page.location.hostname;
            this.pref.addRule("qa_" + domain,domain,"domain",flag);
        }
        catch(e){
            alert(_("beagle_quick_add_rule_error"));
        }
    },

    /**
     * show preference winodw (event handler)
     */
    showPrefs : function()
    {
        window.openDialog('chrome://beagle/content/beaglePrefs.xul',
                'PrefWindow',
                'chrome,resizable=no',
                'browser');
    },

    /**
     * status icon clicked. (event handler)
     * toggle auto-index
     */
    onIconClick : function(event)
    {
       // Left-click event (also single click, like Mac).
        if (event.button == 0 && event.ctrlKey == 0) {
            switch(this.runStatus)
            {
            case this.RUN_ENABLED:
                // currently enabled. disable 
                this.disable();
                break;
            case this.RUN_DISABLED:
                // currently disabled  enable.
                this.enable();
                break;
	    case this.RUN_BEAGLE_NOT_FOUND:
		alert(this.STATUS_ICON.getAttribute("tooltiptext"));
		break;
            default: // RUN_ERROR
                // last run was an error, show the error
                alert("Error running Beagle Indexer.");
                break;
            }
        }
    },

    /**
     * call beagle search by query 
     */
    search : function(query)
    {
        if(!this.beagleSearchPath)
            return;
        try {
            log("Running beagle search with query: "+ query );
            var retval = this.FILE_UTILS.spawn(this.beagleSearchPath, ["", query]);
            if (retval) 
                alert("Error running beagle search: " + retval);
        } 
        catch(e){
                alert("Caught error from beagle-search: " + e);
        }
    },
    
    /************************************************************
     *  status switch functions
     ************************************************************/
    
    /**
     * disable beagle auto index
     */
    disable : function()
    {
        this.runStatus = this.RUN_DISABLED;
        this.STATUS_ICON.setAttribute("status","00f");
        this.STATUS_ICON.setAttribute("tooltiptext",_("beagle_tooltip_disabled"));
        this.pref.set("beagle.autoindex.active",false);
    },

    /**
     * enable beagle auto index
     */
    enable : function()
    {
        this.runStatus = this.RUN_ENABLED;
        this.STATUS_ICON.setAttribute("status","000");
        this.STATUS_ICON.setAttribute("tooltiptext",_("beagle_tooltip_actived"));
        this.pref.set("beagle.autoindex.active",true);
    },
    
    /**
     * error occours
     */
    error : function(msg)
    {
        this.runStatus = this.RUN_ERROR;
        this.STATUS_ICON.setAttribute("status","f00");
        this.STATUS_ICON.setAttribute("tooltiptext",_f("beagle_tooltip_error",[msg]));
        this.pref.set("beagle.autoindex.active",false);
    },
};

// Create event listener.
window.addEventListener('load', Function.bind(beagle.init,beagle),false); 

