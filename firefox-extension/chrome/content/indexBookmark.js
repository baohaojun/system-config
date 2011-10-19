/**
* index bookmarks.
* Include URL,name,shorcurURL (the keywords), description
* After index, a last-indexed-date is saved.
* Later only index the modified bookmark or new bookmarks.
*/


//Used to include only one time bookmark.js and avoid error message about already specified constant
try{
    if(ADD_BM_DIALOG_FEATURES) {}
} catch(e){
    var loader =  Components.classes["@mozilla.org/moz/jssubscript-loader;1"]
		    .getService(Components.interfaces.mozIJSSubScriptLoader);
    loader.loadSubScript("chrome://browser/content/bookmarks/bookmarks.js");
}
if(!BMDS)
{
    //init bookmark js service
    initServices();
    initBMService();
}

/**
 * a class for Bookmark
 */
function Bookmark(bmRes,path)
{
    this.bmRes = bmRes;
    this.URL = this.getLiteral(this.URLArc);
    this.Name = this.getLiteral(this.NameArc);
    this.ShortcutURL = this.getLiteral(this.ShorcurURLArc);
    this.Description = this.getLiteral(this.DescriptionArc);
    this.LastModifiedDate = this.getDate(this.LastModifiedDateArc);
    this.LastVisitDate = this.getDate(this.LastVisitDateArc);
    this.BookmarkAddDate = this.getDate(this.BookmarkAddDateArc);
    this.type = BookmarksUtils.resolveType(this.bmRes);
    this.path = path; 
}

Bookmark.prototype = {
    URLArc:             RDF.GetResource(gNC_NS + "URL"),
    //FeedURLArc:         RDF.GetResource(gNCNS + "FeedURL",
    NameArc:            RDF.GetResource(gNC_NS + "Name"),
    ShortcutURLArc:     RDF.GetResource(gNC_NS + "ShortcutURL"),
    DescriptionArc:     RDF.GetResource(gNC_NS + "Description"),
    LastModifiedDateArc:RDF.GetResource(gWEB_NS + "LastModifiedDate"),
    LastVisitDateArc:   RDF.GetResource(gWEB_NS + "LastVisitDate"),
    BookmarkAddDateArc:   RDF.GetResource(gNC_NS + "BookmarkAddDate"),
    
    /**
     * is bookmark newer than lastIndexDate
     */
    isModified: function(lastIndexDate)
    {   
        var last_modified = this.LastModifiedDate;
        if (!last_modified)
            last_modified = this.BookmarkAddDate;
        return last_modified && last_modified > lastIndexDate;
    },

    /**
     * folder / seperator / livebookmark  are not "bookmark"
     */
    isBookmark: function()
    {
        var parent = BMSVC.getParent(this.bmRes);
        if (parent) 
            var type = BookmarksUtils.resolveType(parent);
        if (type == "Livemark") 
            return false;
        return !!this.URL;
    },

    getLiteral:function(arc) 
    {
        try{
            var target = BMDS.GetTarget(this.bmRes, arc, true);          
            if (target) {
                   return target.QueryInterface(Components.interfaces.nsIRDFLiteral).Value;
            }
        } catch (e) { /* probably a bad interface */ }
        return null;
    },

    getDate:function(arc) 
    {
        try{
            var target = BMDS.GetTarget(this.bmRes, arc, true);          
            if (target) {
                   return target.QueryInterface(Components.interfaces.nsIRDFDate).Value/1000;
            }
        } catch (e) { /* probably a bad interface */ }
        return null;
    },

    /**
     * get children bookmarks 
     */
    getChildren:function()
    {
        var container = Components.classes["@mozilla.org/rdf/container;1"]
                    .createInstance(Components.interfaces.nsIRDFContainer);
        container.Init(BMDS, this.bmRes);
        var bookmarks = new Array();
        var elements = container.GetElements();
        while (elements.hasMoreElements()) {
            var element = elements.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
            bookmarks.push(new Bookmark(element,this.path + " " + this.Name))
        }
        return bookmarks;
    }

}

var bookmarkIndexer = {
    
    /**
     * get the bookmark  one by one 
     * if filter(bookmark) == true do action(bookmark)
     * return the num of indexed bookmarks
     */
    walk: function(bm,filter,action)
    {
        var num = 0;
        switch(bm.type)
        {
        //folder. walk it's chidren
        case "Folder":
        case "PersonalToolbarFolder":
        case "IEFavoriteFolder":
            var children = bm.getChildren();
            for(var i = 0; i < children.length; i++)
                num += this.walk(children[i],filter,action);
            break;
        default:
            if(filter.call(null,bm))
            {
                action.call(null,bm);
                num ++;
            }
            break;
        }
        return num;
    },
    
    /**
     * Index a bookmark.
     * write meta to metafile and write a empty content file
     */
    indexBookmark: function(bookmark)
    {
        log("index bookmark " + bookmark.URL );
        var meta = [
            bookmark.URL,
            "Bookmark",
            "", //mimetype is null for bookmarks
            "t:dc:title=" + bookmark.Name,
            "t:dc:identifier=" + bookmark.path,
        ];
        if(bookmark.Description)
            meta.push("t:dc:description=" + bookmark.Description);
        if(bookmark.ShortcutURL)
            meta.push("t:fixme:shortcuturl=" + bookmark.ShortcutURL);
        //if(bookmark.LastModifiedDate)
        //    meta.push("k:lastmodifieddate=" + bookmark.LastModifiedDate);
        //if(bookmark.LastVisitDate)
        //    meta.push("k:lastvisitdate=" + bookmark.LastVisitDate);
        beagle.writeRawMetadata(meta,beagle.getMetaPath(bookmark.URL,"bookmark"));
        // a little hack , write empty content to content file
        beagle.writeRawMetadata([],beagle.getContentPath(bookmark.URL,"bookmark"));
    },
    
    /**
     * Index all the bookmarks. 
     * It is not used.
     */
    indexAll:function()
    {
        this.walk(
            function(bookmark){return bookmark.isBookmark();}, 
            this.indexBookmark
        );
        beaglePref.set("beagle.bookmark.last.indexed.date","" + (new Date()).getTime());
    },
    
    /**
     * Index the modifled (or new ) bookmarks.
     * if report is true , alert the num of indexed bookmarks
     */
    indexModified:function(report)
    {
        var root = new Bookmark(RDF.GetResource("NC:BookmarksRoot"),"");
        var lastIndexDate = beaglePref.get("beagle.bookmark.last.indexed.date");
        var num = this.walk(
            root,
            function(bookmark){return bookmark.isBookmark() && bookmark.isModified(lastIndexDate);},
            this.indexBookmark
        );
        beaglePref.set("beagle.bookmark.last.indexed.date","" + (new Date()).getTime());
        if(report)
           alert(_f("beagle_index_bookmark_finish",[num]));
        log(_f("beagle_index_bookmark_finish",[num]));
    }
}


