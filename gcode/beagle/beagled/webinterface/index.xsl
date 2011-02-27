<?xml version="1.0"?>
<!--
//
// Copyright (2007) Debajyoti Bera
// Copyright (2007) Nirbheek Chauhan
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
-->

<!DOCTYPE xsl:stylesheet [<!ENTITY nbsp "&#160;">]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<xsl:output method="html"/>

<!-- 
	Start Template HTML doc 
-->

<xsl:template match="/">
	<html>
		<xsl:call-template name="head"/>
		<xsl:call-template name="body"/>
	</html>
</xsl:template>

<xsl:template name="head">
	<head>
		<title><xsl:value-of select="/document/title"/></title>
		<link rel="stylesheet" href="default.css" type="text/css"/>
		<script src="propname-table.js" type="text/javascript"/>
		<script src="default.js" type="text/javascript"/>
		<link rel="icon" href="images/favicon.png" type="image/png"/>
		<link rel="search" type="application/opensearchdescription+xml" title="Beagle" href="opensearch.xml"/>
	</head>
</xsl:template>

<xsl:template name="body">
	<body onload="init ()">
		<div id="header">
			<xsl:call-template name="header"/>
		</div>
		<div id="status">
			<img src="images/busy-animation.gif"/>
		</div>
		<div id="topbar">
			<xsl:call-template name="topbar"/>
		</div>
		<!-- Placeholder div -->
		<div id="login">
		</div>
		<div id="results">
			<xsl:call-template name="results"/>
		</div>
		<div id="info">
		</div>
		<div id="help">
			<h3>Quick Tips</h3>
			<ul>
				<li>Help on <a target="_blank" href="/help.html#Open_Result_Links">opening the search results</a>.</li>
				<li>You can use upper and lower case; search is case-insensitive.</li>
				<li>To search for optional terms, use OR.  ex: <b>George OR Ringo</b></li>
				<li>To exclude search terms, use the minus symbol in front, such as <b>-cats</b></li>
				<li>When searching for a phrase, add quotes. ex: <b>"There be dragons"</b></li>
			</ul>
		</div>
		<div id="footer">
			<xsl:call-template name="footer"/>
		</div>
	</body>
</xsl:template>

<xsl:template name="header">
	<div id="menubar">
		<a href="#" onclick='get_information (); return false;'>Current Status</a>&nbsp;|&nbsp;
		<a href="#" onclick='alert ("Not implemented"); return false;'>Settings</a>&nbsp;|&nbsp;
		<a href="help.html" target="_blank" >Help</a>
	</div>
	<a href="."><img src="images/beagle-logo.png"/></a>
	<form name="queryform" onsubmit='search (); return false;' action="POST">
		<input name="querytext" id="querytext" type="text" size="50" />
		<input name="querysubmit" type="submit" value="Search"/>
	</form>
	<span id="searchoptions">
		<a href="#" onclick='show_advanced_search (); return false;'>Advance search</a>&nbsp;|&nbsp;
		<a href="#" onclick='bookmark_query (); return false;'>Bookmark current search</a>
	</span>
</xsl:template>

<xsl:template name="topbar">
	<span id="topbar-left">
		<form name="categories" autocomplete="off">
			<a href="#" onclick='show_all_categories (this); return false;' name="All">Show All</a>&nbsp;|&nbsp;
			<xsl:for-each select="document ('mappings.xml')/Mappings/Categories/Category/@Name">
				<input type="checkbox" name="{.}" onClick='toggle_category (this);'/><xsl:value-of select="."/>
			</xsl:for-each>
		</form>
	</span>
	<span id="topbar-right">
		<span id="numhits">0</span> results in <span id="timetaken">0 secs</span>
	</span>
</xsl:template>

<xsl:template name="results">
	<xsl:for-each select="document ('mappings.xml')/Mappings/Categories/Category/@Name">
		<div class="Hits" id='{.}'>
			<div> <!-- Not making it class="Hit" because it results in too much padding -->
				<h3><xsl:value-of select="."/></h3>
			</div>
		</div>
	</xsl:for-each>
	<div class="Hits" id="NoResults" style="display: none;">
		No Results
	</div>
</xsl:template>

<xsl:template name="footer">
	<p><a target="_blank" href="http://beagle-project.org/Beagle_Webinterface">Web interface</a> for <a target="_blank" href="http://beagle-project.org">Beagle</a> desktop search service &nbsp;|&nbsp; <a target="_blank" href="http://svn.gnome.org/viewvc/beagle/trunk/beagle/COPYING?view=markup">Copying restrictions</a></p>
	<p class="version-info">Version: <xsl:value-of select="/document/version"/> last updated on <xsl:value-of select="/document/last_time"/></p>
</xsl:template>

<!-- 
	End Template HTML doc 
-->

</xsl:stylesheet>
