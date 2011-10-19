/*
    json.js
    2007-04-30

    Public Domain

    This file adds these methods to JavaScript:

        array.toJSONString()
        boolean.toJSONString()
        date.toJSONString()
        number.toJSONString()
        object.toJSONString()
        string.toJSONString()
            These methods produce a JSON text from a JavaScript value.
            It must not contain any cyclical references. Illegal values
            will be excluded.

            The default conversion for dates is to an ISO string. You can
            add a toJSONString method to any date object to get a different
            representation.

        string.parseJSON(filter)
            This method parses a JSON text to produce an object or
            array. It can throw a SyntaxError exception.

            The optional filter parameter is a function which can filter and
            transform the results. It receives each of the keys and values, and
            its return value is used instead of the original value. If it
            returns what it received, then structure is not modified. If it
            returns undefined then the member is deleted.

            Example:

            // Parse the text. If a key contains the string 'date' then
            // convert the value to a date.

            myData = text.parseJSON(function (key, value) {
                return key.indexOf('date') >= 0 ? new Date(value) : value;
            });

    It is expected that these methods will formally become part of the
    JavaScript Programming Language in the Fourth Edition of the
    ECMAScript standard in 2008.

    This file will break programs with improper for..in loops. See
    http://yuiblog.com/blog/2006/09/26/for-in-intrigue/

    This is a reference implementation. You are free to copy, modify, or
    redistribute.

    Use your own copy. It is extremely unwise to load untrusted third party
    code into your pages.
*/

/*jslint evil: true */

/**
rewirite by taofei 2007-8-22
to avoid for-in loop problem.
toJSONString(anything)
parseJSON(anything)
*/



//help function xxxToJSONString


    function arrayToJSON (arrayVal) {
        var a = ['['],  // The array holding the text fragments.
            b,          // A boolean indicating that a comma is required.
            i,          // Loop counter.
            l = arrayVal.length,
            v;          // The value to be stringified.

        function p(s) {

// p accumulates text fragments in an array. It inserts a comma before all
// except the first fragment.

            if (b) {
                a.push(',');
            }
            a.push(s);
            b = true;
        }

// For each value in arrayVal array...

        for (i = 0; i < l; i += 1) {
            v = arrayVal[i];
            p(toJSONString(v));
        }

// Join all of the fragments together and return.

        a.push(']');
        return a.join('');
    };




    function objectToJSON(objectVal) {
        var a = ['{'],  // The array holding the text fragments.
            b,          // A boolean indicating that a comma is required.
            k,          // The current key.
            v;          // The current value.

        function p(s) {

// p accumulates text fragment pairs in an array. It inserts a comma before all
// except the first fragment pair.

            if (b) {
                a.push(',');
            }
            a.push(toJSONString(k), ':', s);
            b = true;
        }

// Iterate through all of the keys in the object, ignoring the proto chain.

        for (k in objectVal) {
            if (objectVal.hasOwnProperty(k)) {
                v = objectVal[k];
                p(toJSONString(v));
            }
        }

// Join all of the fragments together and return.

        a.push('}');
        return a.join('');
    };

function strToJSON(str)
{
         var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };

   // If the string contains no control characters, no quote characters, and no
    // backslash characters, then we can simply slap some quotes around it.
    // Otherwise we must also replace the offending characters with safe
    // sequences.

    if (/["\\\x00-\x1f]/.test(str)) {
        return '"' + str.replace(/([\x00-\x1f\\"])/g, function (a, b) {
            var c = m[b];
            if (c) {
                return c;
            }
            c = b.charCodeAt();
            return '\\u00' +
                Math.floor(c / 16).toString(16) +
                (c % 16).toString(16);
        }) + '"';
    }
    return '"' + str + '"';
}

function boolToJSON(bool)
{
    return String(bool);
}

function dateToJSON(dateVal)
{

// Ultimately, this method will be equivalent to the date.toISOString method.

        function f(n) {

// Format integers to have at least two digits.

            return n < 10 ? '0' + n : n;
        }

        return '"' + dataVal.getFullYear() + '-' +
                f(dataVal.getMonth() + 1) + '-' +
                f(dataVal.getDate()) + 'T' +
                f(dataVal.getHours()) + ':' +
                f(dataVal.getMinutes()) + ':' +
                f(dataVal.getSeconds()) + '"';
};

function numberToJSON(numberVal) 
{

// JSON numbers must be finite. Encode non-finite numbers as null.

        return isFinite(numberVal) ? String(numberVal) : "null";
};

function toJSONString(anything)
{
     switch (typeof anything) {

// Serialize a JavaScript object value. Ignore objects thats lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.
    case 'array':
        return arrayToJSON(anything);
    case 'object':
        if (anything) {
            if (anything instanceof Array)
                return arrayToJSON(anything);
            else
                return objectToJSON(anything);
        } else {
            return "null";
        }
        break;

    case 'string':
        return strToJSON(anything);
    case 'number':
        return numberToJSON(anything);
    case 'boolean':
        return boolToJSON(anything);
    default:
        return String(anything);
    }
}



function parseJSON(str,filter) {
    var j;

    function walk(k, v) {
        var i;
        if (v && typeof v === 'object') {
            for (i in v) {
                if (v.hasOwnProperty(i)) {
                    v[i] = walk(i, v[i]);
                }
            }
        }
        return filter(k, v);
    }


// Parsing happens in three stages. In the first stage, we run the text against
// a regular expression which looks for non-JSON characters. We are especially
// concerned with '()' and 'new' because they can cause invocation, and '='
// because it can cause mutation. But just to be safe, we will reject all
// unexpected characters.

    if (/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.
            test(str)) {

// In the second stage we use the eval function to compile the text into a
// JavaScript structure. The '{' operator is subject to a syntactic ambiguity
// in JavaScript: it can begin a block or an object literal. We wrap the text
// in parens to eliminate the ambiguity.

        try {
            j = eval('(' + str + ')');
        } catch (e) {
            throw new SyntaxError("parseJSON");
        }
    } else {
        throw new SyntaxError("parseJSON");
    }

// In the optional third stage, we recursively walk the new structure, passing
// each name/value pair to a filter function for possible transformation.

    if (typeof filter === 'function') {
        j = walk('', j);
    }
    dump(toJSONString(j));
    return j;
};




