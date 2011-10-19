/**
* index bookmarks (for firefox 3)
* Include URL,name,shorcurURL (the keywords), description
* After index, a last-indexed-date is saved.
* Later only index the modified bookmark or new bookmarks.
*/

function flat(node)
{
    var list = [];
    var children = node.children;
    for (var i=0; i<children.length; i++) {
        var child = children[i];
        if(child.type == 'bookmark')
        {
            list.push(child);
        }
        else if (child.type == 'folder')
        {
           list.concat(flat(child)); 
        }
    }
    return list;
}


var bookmarkIndexer = {
   
    bmsvc : Components.classes["@mozilla.org/browser/nav-bookmarks-service;1"]
                .getService (Components.interfaces.nsINavBookmarksService),
/*
    init : function()
    {
        this.bmsvc.addObserver(this, false);
    },

    onItemAdded: function(id, folder, index)
    {
        //how to getBookarkById ?
    },
*/
    /**
     * get the bookmark  one by one 
     * if filter(bookmark) == true do action(bookmark)
     * return the num of indexed bookmarks
     */
    walk: function(bms, filter, action)
    {
        var num = 0;
        for (var i = 0; i< bms.length; i++)
        {
            if(filter(bms[i]))
            {
                action(bms[i]);
                num ++;
            }
        }
        return num;
    },
    
    /**
     * Index a bookmark.
     * write meta to metafile and write a empty content file
     */
    indexBookmark: function(bookmark)
    {
        log("index bookmark " + bookmark.uri.spec );
        var meta = [
            bookmark.uri.spec,
            "Bookmark",
            "", //mimetype is null for bookmarks
            "t:dc:title=" + bookmark.title,
            "t:dc:identifier=" + bookmark.path,
            "t:dc:description=" + bookmark.description,
            "t:fixme:keyword=" + bookmark.keyword,
        ];
        beagle.writeRawMetadata(meta,beagle.getMetaPath(bookmark.uri.spec, "bookmark"));
        // a little hack , write empty content to content file
        beagle.writeRawMetadata([],beagle.getContentPath(bookmark.uri.spec, "bookmark"));
    },
   
    /**
     * Get All the bookmarks (return a array of bookmakrs)
     */
    getAllBookmarks:function()
    {
        return flat(Application.bookmarks.menu);
    },
   
    /**
     * check if bookmark is modified since last index
     */
    isModified : function(bookmark, lastIndexDate)
    {
        try{
            var lastModified = this.bmsvc.getItemLastModified(bookmark.id);
            return lastModified > lastIndexDate;
        }
        catch(e){
            alert(e);
            return false;
        }

    },

    /**
     * Index the modifled (or new ) bookmarks.
     * if report is true , alert the num of indexed bookmarks
     */
    indexModified:function(report)
    {
        var _this = this;
        var bms = this.getAllBookmarks(); 
        var lastIndexDate = beaglePref.get("beagle.bookmark.last.indexed.date");
        var num = this.walk(
            bms,
            function(bookmark){return _this.isModified(bookmark, lastIndexDate);},
            _this.indexBookmark
        );
        beaglePref.set("beagle.bookmark.last.indexed.date","" + (new Date()).getTime());
        if(report)
           alert(_f("beagle_index_bookmark_finish",[num]));
        log(_f("beagle_index_bookmark_finish",[num]));
    },

}

