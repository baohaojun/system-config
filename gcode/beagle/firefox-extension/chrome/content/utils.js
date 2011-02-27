/*
 * Add A Row to a list 
 * Param is string . Might be more than one .
 * Every param is a considered to be a cell .
 */
function appendRow (){
    var list = arguments[0];
    var listitem = document.createElement('listitem');
    for(var i = 1; i< arguments.length; i++)
    {
        var listcell = document.createElement('listcell');
        listcell.setAttribute('label',arguments[i]);
        listcell.setAttribute('value',arguments[i]);
        listitem.appendChild(listcell);
    }
    list.appendChild(listitem);
}


/*
 * check weather a string is end with another 
 * usage: somestirng.endWith(antoherString)
 * @param {string} subfix  
 */
String.prototype.isEndWith = function(subfix)
{
    var index = this.lastIndexOf(subfix);
    return index != -1 && index + subfix.length == this.length;
}

/**
 * convert  a wildcard expression to regular expression
 * usage: wildcard.wildcard2RE
 * @return the re string ( not RegExp Object)
 */
String.prototype.wildcard2RE = function()
{
    return this.replace(/([\\\+\[\]\{\}\^])/g,"\\$1").replace(/\?/g,".?").replace(/\*/g,".*");
}

Function.prototype.bind = function(f,obj) {
    var temp = function() {
        return f.apply(obj, arguments);
    };
    return temp;
}

function log(msg)
{
   dump("[beagle] " + msg + "\n"); 
}
