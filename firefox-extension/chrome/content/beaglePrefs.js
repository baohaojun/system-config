/*
 * Beagle Extension: Index webpages you visit using the Beagle Indexing Engine.
 * An Extension for the Firefox Browser.
 */

// Initiate a new preference instance.
var gPrefService = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefBranch);

var beaglePref = {
   
    //some constant
    RULE_INCLUDE : 1,
    RULE_EXCLUDE : 2,

    // Declare Pref Keys and Type.
    prefKeys : { 
      'beagle.security.active':{'type':'bool','default':false},
      'beagle.bookmark.active':{'type':'bool','default':false},
      'beagle.prompt.keywords.active':{'type':'bool','default':false},
      'beagle.default.action':{'type':'int','default':1},
      'beagle.conflict.action':{'type':'int','default':1},
      'beagle.include.list':{'type':'string','default':"[]"},
      'beagle.exclude.list':{'type':'string','default':"[]"},
      'beagle.autoindex.active':{'type':'bool','default':true},
      'beagle.bookmark.last.indexed.date':{'type':'string','default':'0'},
      'beagle.first.run':{'type':'bool','default':true},
      'beagle.storage.directory':{'type':'string','default':""},
    },

   
    //functions used to get/set pref
    func_factory:{
        'get':{
            'bool': Function.bind(gPrefService.getBoolPref,gPrefService),
            'int': Function.bind(gPrefService.getIntPref,gPrefService),
            'string' : Function.bind(gPrefService.getCharPref,gPrefService)
        },
        'set':{
            'bool': Function.bind(gPrefService.setBoolPref,gPrefService),
            'int' : Function.bind(gPrefService.setIntPref,gPrefService),
            'string' : Function.bind(gPrefService.setCharPref,gPrefService)
        }
    },

    prefObject : {},
    
    /**
     * get the pref value by key
     * we will use right type according to prefKeys
     */
    get : function(key)
    {
        if(!this.prefKeys.hasOwnProperty(key))
            return null;
        try{
            return this.func_factory['get'][this.prefKeys[key]['type']].call(null,key);
        }
        catch(ex){
            log("[beaglPref.get " + key + "] " + ex );
            return this.prefKeys[key]['default']; 
        }
    },

    /**
     * set pref value 
     * we will use right type according to prefKeys
     */
    set : function(key,value)
    {
        if(!this.prefKeys.hasOwnProperty(key))
            return false;
        try{
            this.func_factory['set'][this.prefKeys[key]['type']].call(null,key,value);
            return true;
        }
        catch(ex){
            return false; 
        }
        
    },

    /*
     * Load Prefs into a javascript object (this.prefObject)
     *
     */
    load : function()
    {
        //log(toJSONString(this.prefKeys));
        
        for(key in this.prefKeys)
        {
            if(!this.prefKeys.hasOwnProperty(key))
                continue;
            var value = this.get(key);
            if(value != null)
                this.prefObject[key] = value;
            else
                log(key + " is null" );
        }
        return this.prefObject;
    },

    /*
     * Save Prefs (in this.prefObject) into firefox
     */
    save : function()
    {
        for(key in this.prefKeys)
        {
            this.set(key, this.prefObject[key]);
        }
        log("Save Beagle Prefs:" + toJSONString(this.prefObject) );
    },

    /**
     * init beagle pref , load pref, init UI
     */
    init : function ()
    {
        log("beaglePref init");
        this.load(); 
        this.UIInit();
    },

    /**
     *  init the UI
     */
    UIInit : function ()
    {
        log("beaglePref uiinit");
        var checkboxElements = ["beagle.security.active","beagle.bookmark.active","beagle.prompt.keywords.active"]
        for(var i = 0; i < checkboxElements.length; i++)
        {
            var elementID = checkboxElements[i];
            try{
                document.getElementById(elementID).checked = this.prefObject[elementID]
             }
            catch(ex){
                log(ex);
                document.getElementById(elementID).checked = true;
             }
        }

        var radioElements = ["beagle.default.action","beagle.conflict.action"]
        for(var i = 0; i < radioElements.length; i++)
        {
            var elementID = radioElements[i];
            var radios = document.getElementById(elementID).getElementsByTagName('radio');
            try{
                for (var j = 0; j < radios.length; j++)
                {
                    if(radios[j].value == this.prefObject[elementID])
                    {
                        document.getElementById(elementID).selectedItem = radios[j]
                        break;
                    }
                }
            }
            catch(ex){
                log(ex);
            }
        }
     
        //beagle.include.list and beagle.exclude.list
        var listElements = ["beagle.include.list","beagle.exclude.list"];
        for (var i = 0; i < listElements.length; i++)
        {
            var elementID = listElements[i];
            try{
                var items = parseJSON(this.prefObject[elementID]); 
                var listbox = document.getElementById(elementID) ;
                //log("listbox.getRowCount:" + listbox.getRowCount() + '\n');
                var num = listbox.getRowCount();
                for (var j = 0; j < num; j++)
                    listbox.removeItemAt(0);
                
                for (var j = 0; j < items.length; j++){
                    appendRow(listbox,items[j]['name'],items[j]['pattern'],items[j]['patternType']);
                 }
            } catch(ex) {
                log(ex);
                log(this.prefObject[elementID]);
            }
        }
     
    },

    /****************************************************************************
     *                      Event Handlers 
     ***************************************************************************/
    
    /*
     *  This function is called when the save button is clicked
     */
    onSave : function ()
    {
        var prefs = {};
        
        var checkboxElements = ["beagle.security.active","beagle.bookmark.active","beagle.prompt.keywords.active"]
        for(var i = 0; i < checkboxElements.length; i++)
        {
            var elementID = checkboxElements[i];
            try{
                prefs[elementID] = document.getElementById(elementID).checked;
             }
            catch(e){
                prefs[elementID] = false;
             }
        }

        var radioElements = ["beagle.default.action","beagle.conflict.action"]
        for(var i = 0; i < radioElements.length; i++)
        {
            var elementID = radioElements[i];
            try{
                prefs[elementID] = document.getElementById(elementID).value;
             }
            catch(e){
             }
        }
             
        //beagle.include.list and beagle.exclude.list
        var listElementIDs = ["beagle.include.list","beagle.exclude.list"];
        for (var i = 0; i < listElementIDs.length; i++)
        {
            var elementID = listElementIDs[i];
            try {
                var items = new Array() ;
                var listbox = document.getElementById(elementID) ;

                for (var j = 0; j < listbox.getRowCount(); j++){
                    var listitem =  listbox.getItemAtIndex(j);
                    var name = listitem.getElementsByTagName('listcell')[0].getAttribute('value');
                    var pattern = listitem.getElementsByTagName('listcell')[1].getAttribute('value');
                    var patternType = listitem.getElementsByTagName('listcell')[2].getAttribute('value');
                    items.push({'name':name,'pattern':pattern,'patternType':patternType});
                }
                var value = toJSONString(items);
                prefs[elementID] = value;
            } catch(e) {
                // We don't seem to care about this.
            }
        }

        this.prefObject = prefs;
        this.save();   
    },

    /**
     * open a dialog to add a filter 
     */
    onAddFilter : function (type)
    {
        window.openDialog(
            'chrome://beagle/content/beagleAddFilter.xul',
            "add_filter_dialog", 
            'chrome,modal',
            type
        );
    },

    /**
     *remove a filter
     */
    onRemoveFilter : function(type) 
    {
        try{
            var listbox = document.getElementById('beagle.'+type+'.list');
            listbox.removeItemAt(listbox.selectedIndex);
        } catch(e){
            //ignore
        }
    },

    
    /**
     * Add Exclude / Include rule
     * name the rule name
     * pattern the pattern
     * type the pattern type 
     * flag  RULE_INCLUDE or RULE_EXCLUDE
     */
    addRule : function (name,pattern,type,flag)
    {
        switch(flag)
        {
        case this.RULE_INCLUDE:
            key = "beagle.include.list";
            break;
        case this.RULE_EXCLUDE:
            key = "beagle.exclude.list";
            break;
        default:
            //error
            return;
        }
        var rules = parseJSON(this.get(key));
        rules.push({"name":name,"pattern":pattern,"patternType":type});
        this.set(key,toJSONString(rules));
    },

    /**
     * First Run import (from old extension)
     */
    firstRunImport : function()
    {
        try{
            this.set("beagle.autoindex.active",gPrefService.getBoolPref("beagle.enabled"));
            this.set("beagle.security.active",gPrefService.getBoolPref("beagle.security.active"));
            var filters = gPrefService.getCharPref("beagle.security.filters").split(";");
            var excludeList = parseJSON(this.get("beagle.exclude.list"));
            for(var i = 0; i < filters.length; i++)
            {
                if(filters[i] != "")         
                    excludeList.push({"name":"Import_"+i,"pattern":filters[i],"patternType":"domain"});
            }
            this.set("beagle.exclude.list",toJSONString(excludeList));
        }
        catch(ex){
            log("first run import error");
            log(ex);
        }
    },
}

