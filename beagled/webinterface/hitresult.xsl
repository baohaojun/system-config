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

<xsl:template match="NumMatches">
	<p class="NumMatches">(Showing <xsl:value-of select="text()"/> matches)</p>
</xsl:template>

<xsl:template match="@Timestamp">
	Last modified: <b><xsl:value-of select="."/></b>
</xsl:template>

<xsl:template match="Hit">
	<div class="Hit" id="{@Uri}" name="Hit">
		<div class="Title" name="Title">
			<span class="Uri" name="Uri">
				<a href="#" class="Toggle" onclick='toggle_hit(this); return false;'>
					<xsl:choose>
						<xsl:when test="@style='display: none'">[+]</xsl:when>
						<xsl:otherwise>[-]</xsl:otherwise>
					</xsl:choose>
				</a>&nbsp;
				<xsl:choose>
					<xsl:when test="Properties/Property[@Key='fixme:inside_archive']">
					    <i>Archived&nbsp;</i>
					</xsl:when>
					<xsl:when test="Properties/Property[@Key='parent:fixme:hasAttachments']">
					    <i>Attachment&nbsp;</i>
					</xsl:when>
				</xsl:choose>
				<xsl:variable name="mimetype"><xsl:value-of select="Properties/Property[@Key='beagle:MimeType']/@Value"/></xsl:variable>
				<a target="_blank" href="{@Uri}" onClick="return open_hit('{@Uri}', '{$mimetype}');">
					<xsl:call-template name="Uri"/>
				</a>
			</span>
			<span class="Timestamp" name="Timestamp">
				<!-- Pushing the filesize information to the right, just before the timestamp -->
				<xsl:if test="Properties/Property[@Key='fixme:filesize']">
					<span class="Filesize" name="Filesize">
						<xsl:variable name="filesize"><xsl:value-of select="Properties/Property[@Key='fixme:filesize']/@Value"/></xsl:variable>
						<i><xsl:value-of select="ceiling ($filesize div 1024)"/> KB</i>&nbsp;
					</span>
				</xsl:if>
				<xsl:value-of select="@Timestamp"/>
			</span>
		</div>
		<div class="Data" name="Data" style="{@style}">
			<xsl:apply-templates select="Properties"/>
			<xsl:call-template name="Snippet"/>
		</div>
	</div>
</xsl:template>

<!-- FIXME: This logic should go into mappings.xml and then be referenced from there. -->
<xsl:template name="Uri">
	<xsl:choose>
		<xsl:when test="Properties/Property[@Key='beagle:ExactFilename']">
			<xsl:value-of select="Properties/Property[@Key='beagle:ExactFilename']/@Value"/>
		</xsl:when>
		<xsl:when test="Properties/Property[@Key='dc:title']">
			<xsl:value-of select="Properties/Property[@Key='dc:title']/@Value"/>
		</xsl:when>
		<xsl:when test="Properties/Property[@Key='parent:dc:title']">
			<xsl:value-of select="Properties/Property[@Key='parent:dc:title']/@Value"/>
		</xsl:when>
		<xsl:when test="Properties/Property[@Key='beagle:HitType' and @Value='IMLog']">
			Conversation with&nbsp;
			<xsl:choose>
				<xsl:when test="Properties/Property[@Key='fixme:alias']">
					<xsl:value-of select="Properties/Property[@Key='fixme:speakingto_alias']/@Value"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="Properties/Property[@Key='fixme:speakingto']/@Value"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="@Uri"/>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="Snippet">
	<xsl:choose>
		<xsl:when test="Properties/Property[@Key='beagle:MimeType' and @Value='message/rfc822']">
			<div class="Snippet">
			    <a href="#" onclick="get_snippet (this, '{@Uri}', true); return false;">Show email</a>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="Snippet">
			    <a href="#" onclick="get_snippet (this, '{@Uri}', false); return false;">Show Snippet</a>
			</div>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<!-- FIXME: This (currently non-existant) mapping should go into mappings.xml and then be referenced from there. -->
<xsl:template match="Properties">
	<table class="Properties">
		<xsl:for-each select="Property">
			<xsl:if test="@Name != ''">
				<tr class="Property">
				<td class="SearchProperty"><a href="#" onclick="search_property(this); return false;"><img src="images/system-search.png" alt="Search for this property and value" title="Search for this property and value" /></a></td>
				<td class="PropertyKey" key="{@Key}"><xsl:value-of select="@Name"/></td>
				<td class="PropertyValue" type="{@Type}"><xsl:value-of select="@Value"/></td>
				</tr>
			</xsl:if>
		</xsl:for-each>
	</table>
</xsl:template>

<xsl:template match="Snippets">
	<xsl:for-each select="SnippetLine">
		<xsl:text>...</xsl:text>
		<xsl:for-each select="Fragment">
			<xsl:choose>
				<xsl:when test="@QueryTermIndex != '-1'">
					<!-- query term; bold this -->
					<b><xsl:value-of select="."/></b>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="."/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
		<xsl:text>...</xsl:text>
		<br/>
	</xsl:for-each>
</xsl:template>

</xsl:stylesheet>
