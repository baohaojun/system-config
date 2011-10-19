var xmlhttp = new XMLHttpRequest (); 
var parser = new DOMParser ();

function init ()
{
	var input_box = document.getElementById ("searchinput");
	input_box.addEventListener ("keypress", InputKeypressHandler, false);
}

function cleanup ()
{
	var input_box = document.getElementById ("searchinput");
	input_box.addEventListener ("keypress", InputKeypressHandler, false);
}

function InputKeypressHandler (evt)
{
	if (evt.which == 13) {
		evt.stopPropagation ();
		if (evt.cancelable) {
			evt.preventDefault();
		}

		Search ();
	}
}

function Search ()
{
	var query_str = document.getElementById ("searchinput").value;
	//alert ("Searching for '" + query_str + "'");
	if (query_str.length == 0) {
		return;
	} else if (query_str == '42') {
		window.location = "http://en.wikipedia.org/wiki/The_Answer_to_Life,_the_Universe,_and_Everything";
		return;
	} else if (query_str == '4u7h0rz') {
		window.location = "http://svn.gnome.org/viewvc/beagle/trunk/beagle/AUTHORS?view=markup";
		return;
	}

	var req_string = '<?xml version="1.0" encoding="utf-8"?> <RequestWrapper xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"> <Message xsi:type="Query"> <IsIndexListener>false</IsIndexListener> <Parts> <Part xsi:type="QueryPart_Human"> <Logic>Required</Logic> <QueryString>type:WebHistory OR type:Bookmark</QueryString> </Part><Part xsi:type="QueryPart_Human"> <Logic>Required</Logic> <QueryString>' + query_str + '</QueryString> </Part> </Parts> <QueryDomain>Local System</QueryDomain> <MaxHits>20</MaxHits> </Message> </RequestWrapper> ';

	xmlhttp.onreadystatechange = state_change_search;
	xmlhttp.onerror = error_handler;
	// If cross-site problem occurs,
	// http://blog.dirolf.com/2007/06/enabling-cross-domain-ajax-in-firefox.html
	xmlhttp.open ("POST", "http://localhost:4000/", true);
	//XHR binary charset opt by mgran 2006 [http://mgran.blogspot.com]
	xmlhttp.overrideMimeType ('text/txt; charset=utf-8'); // if charset is changed, need to handle bom
	//xmlhttp.overrideMimeType('text/txt; charset=x-user-defined');
	xmlhttp.send (req_string);
	document.getElementById ("searchinput").disabled = true;

	return;
}

function error_handler (e)
{
	var result_list = document.getElementById ("resultlist");
	// FIXME: i18n
	result_list.innerHTML = "<html:b>Error!</html:b><html:br />Beagle service needs to be running with the web interface enabled.<html:br /><html:a href='http://beagle-project.org/Beagle_Webinterface' onclick='return openlink(\"http://beagle-project.org/Beagle_Webinterface\");'>Beagle Webinterface</html:a>";
	document.getElementById ("searchinput").disabled = false;
}

function state_change_search ()
{
	if (xmlhttp.readyState == 4)
		HandleResults ();
}

function HandleResults ()
{
	if (xmlhttp.status != 200) {
		error_handler ();
		return;
	}

	document.getElementById ("searchinput").disabled = false;

	//dump("Response:\n");
	//dump(xmlhttp.responseText);
	//dump("\n");
	res = xmlhttp.responseText;

	// if charset is x-user-defined split by \uF7FF
	// if charset is utf-8, split by FFFD
	// And dont ask me why!
	var responses = res.split ('\uFFFD'); 

	var result_str = "<html:ul>";

	var no_result = true;

	// Process hit xml nodes with xsl and append with javascript
	for (var i = 0; i < responses.length; ++i) {
		if (responses [i].length <= 0)  {
			continue;
		}

		var response_dom = parser.parseFromString (responses [i], "text/xml");
		var msg_node = response_dom.getElementsByTagName ("Message") [0];
		if (msg_node.getAttributeNS ('http://www.w3.org/2001/XMLSchema-instance', 'type') != 'HitsAddedResponse')
			continue;

		var hits = msg_node.getElementsByTagName ("Hit");
		no_result &= (hits.length == 0);

		for (var j = 0; j < hits.length; ++j) {
			var uri = hits [j].getAttribute ("Uri");
			uri = EscapeAmpersand (uri); // XHTML gotcha
			var title = null;
			var identifier = null;
			var bookmark = false;

			var properties = hits [j].getElementsByTagName ("Property");
			for (var k = 0; k < properties.length; ++k) {
				var key = properties [k].getAttribute ("Key");

				if (key == "beagle:HitType" && (properties [k].getAttribute ("Value") == "Bookmark")) {
					bookmark = true;
					continue;
				}

				if (key == "dc:title") {
					title = properties [k].getAttribute ("Value");
					//title = reduce (title, 40, "..."); // FIXME
					continue;
				}

				if (key == "dc:identifier")
					identifier = properties [k].getAttribute ("Value");
			}

			if (bookmark)
				uri = identifier;

			if (title == null)
				title = uri.substr (0, 40) + "...";

			//dump (uri + "," + title + "\n");
			result_str += "<html:li>";

			if (bookmark)
				result_str += "<html:span>B</html:span>";

			result_str += "<html:a href='";
			result_str += uri;
			result_str += "' onclick='return openlink(\"";
			result_str += uri;
			result_str += "\");'>";
			result_str += title;
			result_str += "</html:a></html:li>";
		}
	}

	if (no_result)
		result_str = "<html:b>No web pages found.</html:b>";
	else
		result_str += "</html:ul>";
	//dump (result_str);
	//dump ("\n");

	var result_list = document.getElementById ("resultlist");
	result_list.innerHTML = result_str;
}

// From http://sastools.com/b2/post/79394063
function reduce(str,l,p)
{
	var words=str.split(" ");
	var numWords=words.length;
	var output=[];
	var ol,cWord,w;
	for(w=0; w<numwords; ++w)
	{
		cWord=words[w];
		cwl=cWord.length;
		if((ol+cwl)<=l)
		{
			output.push(cWord);
			ol+=cwl+1;
		}
		else
			break;
	}
	return output.join(" ")+p;
}

function openlink (link)
{
	var mainWindow = window.QueryInterface(Components.interfaces.nsIInterfaceRequestor)
				.getInterface(Components.interfaces.nsIWebNavigation)
				.QueryInterface(Components.interfaces.nsIDocShellTreeItem)
				.rootTreeItem
				.QueryInterface(Components.interfaces.nsIInterfaceRequestor)
				.getInterface(Components.interfaces.nsIDOMWindow);

	var browser = mainWindow.getBrowser ();
	browser.selectedTab = browser.addTab (link);

	return false;
}

function EscapeAmpersand (url)
{
	return url.replace (/&/g, "&amp;");
}
