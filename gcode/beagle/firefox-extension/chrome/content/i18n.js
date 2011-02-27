//check the string budles
var bundles  = new Array();

function gettext(key)
{
    //dump("\ncallled gettext  bundles.length = " + bundles.length + "\n");
    var ret = null;
    for(var i = 0 ; i < bundles.length; i++)
    {
        try{
            ret = bundles[i].getString(key);
        }
        catch(e){dump(e); }
        if (ret != null)
            break;  
    }
    //not found , just return the  orginal string 
    if (ret == null) 
        ret = key;
    //dump(ret + '\n');
    return ret;
}

var _ = gettext;

function getformatedtext(key,subs)
{
    var ret = null;
    for(var i = 0 ; i < bundles.length; i++)
    {
        try{
            ret = bundles[i].getString(key);
            if (ret != null)
            {
                return bundles[i].getFormattedString(key,subs);
            }
        }
        catch(e){}
    }
    //not found , just return the   orginal string 
    if (ret == null) 
        ret = key;
    return ret;
}

var _f = getformatedtext;

function initBundles(){
    bundles.push(document.getElementById('beagleStrings'));
}

window.addEventListener('load', initBundles, false); 


