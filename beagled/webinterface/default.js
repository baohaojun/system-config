/*
 * Copyright (2007) Debajyoti Bera
 * Copyright (2007) Nirbheek Chauhan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 * 
 *
 */

/* Generic TODOs/FIXMEs:
 * - set_results_style: instead of showing/hiding results, only search for those that the user has checked
 *   query and append results as he checks the categories
 * - Ev!L stuff like `element.innerHTML` needs to be replaced by nice clean DOM stuff; except where good for performance
 * - Code needs to be made cleaner...
 */


/************ Global state variables *****************/

MaxResultsPerBackend = 30; // arbitrary
InitialExpanded = 5; // some low number

QueryState = {
	'stemmed_str': '',
	'hits': {}
};

function clear_query_state ()
{
	QueryState ['stemmed_str'] = '';
	QueryState ['hits'] = {};
}

/************ Code for 'Current Status' **************/

function get_information ()
{
	var req_string = '<?xml version="1.0" encoding="utf-8"?><RequestWrapper xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><Message xsi:type="DaemonInformationRequest"> <GetVersion>true</GetVersion><GetSchedInfo>true</GetSchedInfo><GetIndexStatus>true</GetIndexStatus> <GetIsIndexing>true</GetIsIndexing></Message></RequestWrapper>';

	xmlhttp.onreadystatechange = function () {
		state_change_info ();
	};
	xmlhttp.open ("POST", "/", true);
	// XHR binary charset opt by mgran 2006 [http://mgran.blogspot.com]
	xmlhttp.overrideMimeType ('text/txt; charset=utf-8'); // if charset is changed, need to handle bom
	//xmlhttp.overrideMimeType('text/txt; charset=x-user-defined');
	xmlhttp.send (req_string);

	document.queryform.querytext.disabled = true;
	document.queryform.querysubmit.disabled = true;
	document.getElementById ('status').style.display = 'block';
	return false;
}

function state_change_info ()
{
	if (xmlhttp.readyState == 4) {
		// FIXME: Should also check for status 200
		var res = xmlhttp.responseText;

		// if charset is x-user-defined split by \uF7FF
		// if charset is utf-8, split by FFFD
		// And dont ask me why!
		var responses = res.split ('\uFFFD'); 

		// Appending without clearing is bad... mmkay?
		reset_document_style ();
		reset_document_content ();

		// There should be only one response in responses
		for (var i = 0; i < responses.length; ++i) {
			if (responses [i].length <= 0)
				continue;

			var response_dom = parser.parseFromString (responses [i], "text/xml");
			var fragment = status_processor.transformToFragment (response_dom, document);
			document.getElementById ('info').appendChild (fragment);
		}

		document.getElementById ('status').style.display = 'none';
		document.queryform.querytext.disabled = false;
		document.queryform.querysubmit.disabled = false;
	}
}

function shutdown_beagle ()
{
	if ( ! window.confirm ("Are you sure you want to Shutdown Beagle?")) {
		return;
	}
	var req_string = '<?xml version="1.0" encoding="utf-8"?><RequestWrapper xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><Message xsi:type="ShutdownRequest"/></RequestWrapper>';

	xmlhttp.onreadystatechange = function () {
		var message_div = document.getElementById ('shutdown-beagle');
		if (xmlhttp.readyState == 4) {
			var message = document.createElement ('i');
			var text = document.createTextNode ('Shutdown request sent to beagle');
			message.appendChild (text);
			message_div.replaceChild (message, message_div.firstChild);
			document.getElementById ('status').style.display = 'none';
			document.queryform.querytext.disabled = false;
			document.queryform.querysubmit.disabled = false;
		}
	};

	xmlhttp.open ("POST", "/", true);
	// XHR binary charset opt by mgran 2006 [http://mgran.blogspot.com]
	xmlhttp.overrideMimeType ('text/txt; charset=utf-8'); // if charset is changed, need to handle bom
	//xmlhttp.overrideMimeType('text/txt; charset=x-user-defined');
	xmlhttp.send (req_string);

	document.queryform.querytext.disabled = true;
	document.queryform.querysubmit.disabled = true;
	document.getElementById ('status').style.display = 'block';
	return false;
}

/******** Code to handle searching of properties names ******/

function search_property (search_property_node)
{
	// <tr><td><a>Search</a></td><td key="{@Key}">Key</td><td type="{@Type}">Value</td></tr>
	//
	var property_key  = search_property_node.parentNode.nextSibling.getAttribute ("key");
	var property_type = search_property_node.parentNode.nextSibling.nextSibling.getAttribute ("type");
	var property_val  = search_property_node.parentNode.nextSibling.nextSibling.textContent;
	//dump ("type = " + property_type + " key = " + property_key + " val = " + property_val + "\n");

	var prefix = "";
	if (property_type == "Text")
		prefix = "property:";
	else if (property_type == "Keyword")
		prefix = "keyword:";
	else
		return;

	document.queryform.querytext.value = (prefix + '"' + property_key + '=' + property_val + '"');
	search ();
}

/*************** Main code to handle search ****************/

function search ()
{
	// get the search string
	var query_str = document.queryform.querytext.value;
	// FIXME: Escape query_str
	// What kind of escaping? I couldn't do any code injection :-/
	if (query_str.length == 0) {
		return;
	} else if (query_str == '42') {
		window.location = "http://en.wikipedia.org/wiki/The_Answer_to_Life,_the_Universe,_and_Everything";
		return;
	} else if (query_str == '4u7h0rz') {
		window.location = "http://svn.gnome.org/viewvc/beagle/trunk/beagle/AUTHORS?view=markup";
		return;
	}

	var req_string = '<?xml version="1.0" encoding="utf-8"?> <RequestWrapper xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"> <Message xsi:type="Query"> <IsIndexListener>false</IsIndexListener> <Parts> <Part xsi:type="QueryPart_Human"> <Logic>Required</Logic> <QueryString>' + query_str + '</QueryString> </Part> </Parts> <QueryDomain>Local System</QueryDomain> <MaxHits>' + MaxResultsPerBackend + '</MaxHits> </Message> </RequestWrapper> ';

	var begin_date = Date.now ();

	xmlhttp.onreadystatechange = function () {
		state_change_search (begin_date);
	};
	xmlhttp.open ("POST", "/", true);
	//XHR binary charset opt by mgran 2006 [http://mgran.blogspot.com]
	xmlhttp.overrideMimeType ('text/txt; charset=utf-8'); // if charset is changed, need to handle bom
	//xmlhttp.overrideMimeType('text/txt; charset=x-user-defined');
	xmlhttp.send (req_string);

	// https://bugzilla.mozilla.org/show_bug.cgi?id=167801
	// The focus would have moved to some hidden element and would be
	// lost foreveh! Instead ... we cheat and set the focus explicitly
	// to a harmless element.
	document.queryform.querysubmit.focus ();

	document.queryform.querytext.disabled = true;
	document.queryform.querysubmit.disabled = true;
	document.getElementById ('status').style.display = 'block';
	return false;
}

function state_change_search (begin_date)
{
	if (xmlhttp.readyState == 4) {
		// FIXME: Should also check for status 200

		var end_date = Date.now ();
		var elapsed = (end_date - begin_date)/1000;

		//dump("Response:\n");
		//dump(xmlhttp.responseText);
		//dump("\n");
		res = xmlhttp.responseText;

		// if charset is x-user-defined split by \uF7FF
		// if charset is utf-8, split by FFFD
		// And dont ask me why!
		var responses = res.split ('\uFFFD'); 

		// Appending without clearing is bad... mmkay?
		reset_document_style ();
		reset_document_content ();
		document.getElementById ('timetaken').textContent = elapsed + ' secs';
		var num_matches = 0;

		//var _start = Date.now ();

		// Use document fragment to temporarily collect the hits in memory
		var doc_fragments = {};
		var category_count = {};
		for (var f = 0; f < category_funcs.length; ++ f) {
			doc_fragments [(category_funcs [f])['name']] = document.createDocumentFragment ();
			category_count [(category_funcs [f])['name']] = 0;
		}

		// Process hit xml nodes with xsl and append with javascript
		for (var i = 0; i < responses.length; ++i) {
			if (responses [i].length <= 0)  {
				continue;
			}

			var response_dom = parser.parseFromString (responses [i], "text/xml");

			// Read SearchTermResponse message and store the stemmed string for snippet purpose
			var msg_node = response_dom.getElementsByTagName ('Message') [0];
			if (msg_node.getAttributeNS ('http://www.w3.org/2001/XMLSchema-instance', 'type') == 'SearchTermResponse') {
				var query_terms = msg_node.getElementsByTagName ('Stemmed') [0];
				query_terms = query_terms.getElementsByTagName ('Text');
				var stemmed_str = '';
				for (var q_j = 0; q_j < query_terms.length; ++ q_j)
					stemmed_str += ('<string>' + query_terms [q_j].textContent + '</string>');

				QueryState ['stemmed_str'] = stemmed_str;
				continue;
			}

			// FIXME: ignoring all other messages
			if (msg_node.getAttributeNS ('http://www.w3.org/2001/XMLSchema-instance', 'type') != 'HitsAddedResponse')
				continue;

			var hits = msg_node.getElementsByTagName ('Hit');
			for (var j = 0; j < hits.length; ++j) {
				// Copy and store the hit node for snippet purposes before it is modified
				(QueryState ['hits']) [hits [j].getAttribute ("Uri")] = hit_serializer.serializeToString (hits [j]);

				// Get timestamp and process it
				var timestamp = (hits [j]).getAttribute ('Timestamp');
				(hits [j]).setAttribute ('Timestamp', humanise_timestamp (timestamp));

				var div_id = process_hit (hits [j]);

				category_count [div_id] = category_count [div_id] + 1;
				if (category_count [div_id] > InitialExpanded)
					hits [j].setAttribute ('style', 'display: none');
				else
					hits [j].setAttribute ('style', 'display: block');

				// Process Hit using hitresult.xsl and append to `div`
				var hit = hit_processor.transformToFragment (hits [j], document);

				// If more than InitialExpanded hits in this category, collapse them
				//if (category_count [div_id] > InitialExpanded)
				//	hit.style.display = 'none';

				doc_fragments [div_id].appendChild (hit);
			}

			var num_matches_elems = response_dom.getElementsByTagName ('NumMatches');
			if (num_matches_elems.length > 0) {
				var n = parseInt (num_matches_elems [0].textContent);
				if (n > 0) {
					num_matches += n;
					document.getElementById ('numhits').textContent = num_matches;
				}
			}
		}

		// Blast all the generated hits into the document
		for (var f = 0; f < category_funcs.length; ++ f) {
			var div = document.getElementById (category_funcs [f].name);
			div.appendChild (doc_fragments [category_funcs [f].name]);
		}

		//alert (Date.now () - _start);

		set_results_style ();
		if (num_matches  == 0) {
			document.getElementById ('NoResults').style.display = 'block';
		} else {
			document.getElementById ('NoResults').style.display = 'none';
		}
		document.getElementById ('topbar').style.display = 'block';
		document.getElementById ('status').style.display = 'none';

		document.queryform.querytext.disabled = false;
		document.queryform.querytext.focus ();
		document.queryform.querysubmit.disabled = false;
	}
}

/************ Snippet handling ******************/

function get_snippet (div_link, uri, fulltext)
{
	var snippet_div = div_link.parentNode;
	var hit_xml = (QueryState ['hits']) [uri];
	if (hit_xml == null) {
		alert ('Error! Cannot fetch snippet for unregistered hit');
		return;
	}

	//var query_str_stemmed = document.getElementById ('query_str').getAttribute ('stemmed');
	var req_string = '<?xml version="1.0" encoding="utf-8"?> <RequestWrapper xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"> <Message xsi:type="SnippetRequest">' + hit_xml + '<QueryTerms>' + QueryState ['stemmed_str'] + '</QueryTerms> <FullText>' + fulltext + '</FullText> </Message> </RequestWrapper>';

	xmlhttp.onreadystatechange = function () {
		state_change_snippet (snippet_div, uri, fulltext);
	};
	
	xmlhttp.open ("POST", "/", true);
	// XHR binary charset opt by mgran 2006 [http://mgran.blogspot.com]
	xmlhttp.overrideMimeType ('text/txt; charset=utf-8'); // if charset is changed, need to handle bom
	//xmlhttp.overrideMimeType('text/txt; charset=x-user-defined');
	xmlhttp.send (req_string);

	// do fancy js dom stuff
	return false;
}

function state_change_snippet (snippet_div, uri, fulltext)
{
	if (xmlhttp.readyState == 4) {
		// FIXME: Should also check for status 200
		// Help the GC
		(QueryState ['hits']) [uri] = '';

		// if charset is x-user-defined split by \uF7FF
		// if charset is utf-8, split by FFFD
		// And dont ask me why!
		var response = xmlhttp.responseText.split ('\uFFFD') [0];
		var snippet_xml = parser.parseFromString (response, "text/xml");

		var snippet = snippet_xml.getElementsByTagName ('Snippets') [0];

		if (fulltext)
			show_fulltext (snippet_div, snippet);
		else
			show_snippet (snippet_div, snippet);
	}
}

function show_snippet (snippet_div, snippet)
{
	var snippet_lines = snippet.getElementsByTagName ('SnippetLine');
	if (snippet_lines.length == 0) {
		snippet_div.innerHTML = '<i>Search terms not found in text content</i>';
		return;
	}

	//dump (hit_serializer.serializeToString (snippet) + "\n");

	var fragment = hit_processor.transformToFragment (snippet, document);
	while (snippet_div.childNodes.length != 0)
		snippet_div.removeChild (snippet_div.childNodes [0]);

	snippet_div.appendChild (fragment);
}

function show_fulltext (snippet_div, snippet)
{
	var snippet_lines = snippet.getElementsByTagName ('SnippetLine');
	if (snippet_lines.length == 0) {
		snippet_div.innerHTML = '<i>No text content</i>';
		return;
	}

	// Only one snippetline with one fragment
	var fragment = snippet_lines [0].getElementsByTagName ('Fragment');
	if (fragment == null) {
		snippet_div.innerHTML = '<i>No text content</i>';
		return;
	}

	var text = fragment [0].textContent;

	var snippet_str = text.replace (/\n+/g, '<br/>');
	snippet_div.innerHTML = snippet_str;
}

/************ Hit categorization ****************/

// The following logic is taken mappings.xml;
// ideally, these functions should be auto-generated
// from mappings.xml

function is_document (properties)
{
	var hit_type = properties ['beagle:HitType'];
	if (hit_type == null)
		hit_type = '';

	var file_type = properties ['beagle:FileType'];
	if (file_type == null)
		file_type = '';

	return (hit_type != 'MailMessage' &&
		hit_type != 'WebHistory' && (
			file_type == 'document' ||
			file_type == 'archive' ||
			file_type == 'source' ||
			hit_type == 'note'));
}

function is_image (properties)
{
	var file_type = properties ['beagle:FileType'];
	if (file_type == null)
		file_type = '';

	return (file_type == 'image');
}

function is_media (properties)
{
	var file_type = properties ['beagle:FileType'];
	if (file_type == null)
		file_type = '';

	return (file_type == 'audio' ||
		file_type == 'video');
}

function is_mail (properties)
{
	var hit_type = properties ['beagle:HitType'];
	if (hit_type == null)
		hit_type = '';

	return (hit_type == 'MailMessage');
}

function is_imlog (properties)
{
	var hit_type = properties ['beagle:HitType'];
	if (hit_type == null)
		hit_type = '';

	return (hit_type == 'IMLog');
}

function is_website (properties)
{
	var hit_type = properties ['beagle:HitType'];
	if (hit_type == null)
		hit_type = '';

	return (hit_type == 'WebHistory' ||
		hit_type == 'Bookmark' ||
		hit_type == 'FeedItem');
}

function is_other (properties)
{
	var file_type = properties ['beagle:FileType'];
	if (file_type == null)
		file_type = '';

	return (file_type == 'directory');
}

// The order is important
// The 'name's are the names of the Categories in mapping.xml
var category_funcs = new Array (
	{'name': 'Mail'	    , 'func': is_mail	    },
	{'name': 'Documents', 'func': is_document   },
	{'name': 'Images'   , 'func': is_image	    },
	{'name': 'Media'    , 'func': is_media	    },
	{'name': 'IM Logs'  , 'func': is_imlog	    },
	{'name': 'Websites' , 'func': is_website    },
	{'name': 'Others'   , 'func': is_other	    }
);

function classify_hit (properties)
{
	for (var i = 0; i < category_funcs.length; ++ i) {
		if ((category_funcs [i]).func (properties))
			return (category_funcs [i]).name;
	}

	return "Others";
}

/* Processes the hit and return the category */
function process_hit (hit)
{
	var properties = hit.getElementsByTagName ('Property');
	var property_table = {};

	// Read on the internet and tested: going down is faster than going up
	for (var k = properties.length - 1; k >= 0; -- k) {
		var key = properties [k].getAttribute ('Key');
		var value = properties [k].getAttribute ('Value');

		property_table [key] = value;

		// Change the property names to more human friendly ones
		var property_name = PropertyNameTable [key];
		// Search for parent properties
		if (property_name == null && (key.indexOf ('parent:') == 0))
			property_name = PropertyNameTable [key.slice (7)];
		if (property_name == null)
			property_name = key;
		properties [k].setAttribute ('Name', property_name);

		// Change the value of date properties
		// Date properties are never used in mappings.xml
		if (properties [k].getAttribute ('Type') == 'Date')
			properties [k].setAttribute ('Value', humanise_timestamp (value));
	}

	return classify_hit (property_table);
}

/************* Datetime parsing routines ***************/

// We're putting these here so they're reused. Much faster this way.
var regexp = /^(.{4})(.{2})(.{2})(.{2})(.{2})(.{2})/;
function humanise_timestamp (ts) 
{
	var array = regexp.exec (ts);
	// Erm. Months are counted from 0 in javascript for some reason <_<
	var timestamp = new Date (Date.UTC (array [1], array [2] - 1, array [3], array [4], array [5], array [6]));

	var now = new Date ();
	var today = new Date (now.getFullYear (), now.getMonth (), now.getDate ());

	if (timestamp >= today ) {
		return "Today " + timestamp.toLocaleFormat ('%I:%M %p');
	}

	// 1 day = 86,400,000 msecs
	var yesterday = new Date (today.getTime () - 86400000);
	if (timestamp >= yesterday) {
		return "Yesterday " + timestamp.toLocaleFormat ('%I:%M %p');
	}
	
	// This week (ignoring the seconds part)
	var week_begin = new Date (today.getTime () - (86400000 * today.getDay ()));
	if (timestamp >= week_begin) {
		return timestamp.toLocaleFormat ('%A') + " " + timestamp.toLocaleFormat ('%I:%M %p');
	}

	// This year
	var year_begin = new Date (today.getFullYear, 0, 1);
	if (timestamp >= year_begin) {
		return timestamp.toLocaleFormat ('%B %e');
	}

	return timestamp.toLocaleFormat ('%B %e, %Y')
}

/**************** Code to handle styles and ui issues ******************/

function categories_being_shown ()
{
	var category_checkboxes = document.getElementById ('topbar-left').getElementsByTagName ('input');
	var numcategs = 0;
	for (var i = 0; i < category_checkboxes.length; ++i) {
		if (category_checkboxes [i].checked) {
			numcategs++;
		}
	}
	return numcategs;
}

// FIXME: This should just use index.xsl or something
function reset_document_content ()
{
	var results = document.getElementById ('results');
	var categories = document.getElementById ('topbar-left').getElementsByTagName ('input');
	var div, div_category_name;
	// Reset the divs
	document.getElementById ('info').innerHTML = '';
	document.getElementById ('help').innerHTML = '';
	results.innerHTML = '';
	for (var i = 0; i < categories.length; ++i) {
		div = document.createElement ('div');
		div.setAttribute ('class', 'Hits');
		div.setAttribute ('id', categories [i].name);
		div_category_name = document.createElement ('div');
		// Not making it class="Hit" because it results in too much padding
		div_category_name.innerHTML = '<h3>'+categories [i].name+'</h3>';
		div.appendChild (div_category_name);
		results.appendChild (div);
	}

	div = document.createElement ('div');
	div.setAttribute ('class', 'Hits');
	div.setAttribute ('id', 'NoResults');
	div.setAttribute ('style', 'display: none;');
	div.appendChild (document.createTextNode ('No Results'));
	results.appendChild (div);
	document.getElementById ('numhits').textContent = '0';

	clear_query_state ();
}

function reset_document_style ()
{
	document.getElementById ('topbar').style.display = 'none';
	var results_categories = document.getElementById ('results').childNodes;
	// Reset the hit results' display
	for (var i = 0; i < results_categories.length; ++i) {
		results_categories [i].style.display = 'none';
	}
}

function set_results_style ()
{
	// XXX Gotcha: this code assumes the arrays below match w.r.t. their indexes
	// This will always be satisfied however, see mappings.xml: Note 2
	var category_checkboxes = document.getElementById ('topbar-left').getElementsByTagName ('input');
	var results_categories = document.getElementById ('results').childNodes;
	if (categories_being_shown () > 0) {
		for (var i = 0; i < category_checkboxes.length; ++i) {
			if (category_checkboxes [i].checked) {
				results_categories [i].style.display = 'block';
			}
		}
	} else {
		for (var i = 0; i < results_categories.length; ++i) {
			if (results_categories [i].childNodes.length > 1)
				results_categories [i].style.display = 'block';
		}
	}
}

function toggle_hit (hit_toggle)
{
	if (hit_toggle.textContent == '[-]') {
		hit_toggle.textContent = '[+]';
		//this.<span class="Uri">.<div class="Title">.<div class="Data">
		hit_toggle.parentNode.parentNode.nextSibling.style.display = 'none';

	} else {
		hit_toggle.textContent = '[-]';
		hit_toggle.parentNode.parentNode.nextSibling.style.display = 'block';
	}
}

function show_all_categories ()
{
	// Get all the category checkboxes
	var category_checkboxes = document.getElementById ('topbar-left').getElementsByTagName ('input');
	// Get all the result categories
	var results_categories = document.getElementById ('results').childNodes;
	// Uncheck all the categories
	for (var i = 0; i < category_checkboxes.length; ++i) {
		category_checkboxes [i].checked = false;
	}
	// Show results
	// This should've been used here instead of that loop from the start..
	set_results_style ();
}

function toggle_category (category)
{
	// Get all the results' categories
	var results_categories = document.getElementById ('results').childNodes;
	// If the user just checked the box
	if (category.checked) {
		// If this is the first category being shown..
		if (categories_being_shown () == 1) {
			// Hide all results except the one selected
			for (var i = 0; i < results_categories.length; ++i) {
				if (results_categories [i].id == category.name)
					continue;
				results_categories [i].style.display = 'none';
			}
		} else {
			// Show result corresponding to category
			document.getElementById (category.name).style.display = 'block';
		}
	} else {
		// If none of the categories are being shown now
		if (categories_being_shown () == 0) {
			// The user doesn't like being shown nothing; show all the categories
			show_all_categories ();
		} else {
			// Hide result corresponding to category
			document.getElementById (category.name).style.display = 'none';
		}
	}
}

// Special function to handle opening of hits.
// Override this function with your own.
function open_hit (uri, mimetype)
{
	// Do nothing for now. Use the default click handler.
	return true;
}

/******* Add to bookmark *******************************************/

function bookmark_query ()
{
	// Firefox specific !
	if (! window.sidebar) {
		alert ("Adding bookmarks only supported in Firefox!");
		return;
	}

	var title = document.queryform.querytext.value;
	if (title == "")
		return;

	var url = "http://" + document.location.host + "/?search=" + escape (title);

	// Using window.sidebar.addPanel () will create bookmark that will open in sidebar
	var answer = confirm ("Due to limitations in Firefox the bookmark will open in the sidebar. To open it in the main window, uncheck the option in the Property of this bookmark. Continue ?");
	if (answer)
		window.sidebar.addPanel (title, url, "");
}

/******* Advance search GUI ****************************************/

function show_advanced_search ()
{
	alert ("Not implemented.");
}

/******* Initial fetching and loading of the xsl/xml files *********/

// processing of query string parameters, if called as
// http://localhost:4000/?search=foo+bar

// from http://www.netlobo.com/url_query_string_javascript.html
function gup (name)
{
	name = name.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
	var regexS = "[\\?&]"+name+"=([^&#]*)";
	var regex = new RegExp( regexS );
	var results = regex.exec( window.location.search );
	if( results == null )
		return "";
	else
		return unescape (results[1]);
}

function init ()
{
	var query_str = '';
	if (window.location.search)
		query_str = gup ('search');

	query_str = query_str.replace ('+', ' ');

	if (query_str == '') {
		document.queryform.querytext.focus ();
		return;
	}

	document.queryform.querytext.value = query_str;
	search ();
}

// This works everywhere except IE
var xmlhttp = new XMLHttpRequest (); 
var status_processor = new XSLTProcessor ();
var hit_processor = new XSLTProcessor ();
var parser = new DOMParser ();
var mappings;

// Create and store a default serializer
var hit_serializer = new XMLSerializer ();

// Load statusresult.xsl using synchronous (third param is set to false) XMLHttpRequest
xmlhttp.open ("GET", "/statusresult.xsl", false);
xmlhttp.send (null);
// Process it and store it for later reuse
status_processor.importStylesheet (xmlhttp.responseXML);

// Get Hit processing xsl
xmlhttp.open ("GET", "/hitresult.xsl", false);
xmlhttp.send (null);
hit_processor.importStylesheet (xmlhttp.responseXML);

// Get the mappings.xml
xmlhttp.open ("GET", "/mappings.xml", false);
xmlhttp.send (null);
mappings = xmlhttp.responseXML;
