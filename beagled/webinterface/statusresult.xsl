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

<xsl:template match="/">
	<xsl:apply-templates select="ResponseWrapper"/>
</xsl:template>

<xsl:template match="ResponseWrapper">
	<xsl:apply-templates select="Message[@xsi:type = 'DaemonInformationResponse']"/>
</xsl:template>

<xsl:template match="Message[@xsi:type = 'DaemonInformationResponse']">
	<div id="version">
		<b>Version</b>: <i><xsl:value-of select="Version"/></i>
	</div>
	<div id="is-indexing">
		<b>Indexing in progress</b>: <i><xsl:value-of select="IsIndexing"/></i>
	</div>
	<div id="shutdown-beagle">
		<a href="#" onclick="shutdown_beagle (); return false;" title="Shutdown Beagle">Shutdown Beagle</a><br/>
	</div>
	<div id="scheduler-information">
		<xsl:apply-templates select="SchedulerInformation"/>
	</div>
	<div id="backend-information">
		<xsl:apply-templates select="IndexStatus"/>
	</div>
</xsl:template>

<xsl:template match="SchedulerInformation">
	<b>Tasks:</b><br/>
	<span class="total-task-count">(<xsl:value-of select="@TotalTaskCount"/> tasks submitted)</span>
	<ul>
		<xsl:for-each select="PendingTasks/PendingTask">
			<li><i>(Pending)</i>&nbsp;<xsl:value-of select="."/></li>
		</xsl:for-each>

		<xsl:for-each select="FutureTasks/FutureTask">
			<li><i>(Future)</i>&nbsp;<xsl:value-of select="."/></li>
		</xsl:for-each>

		<xsl:for-each select="BlockedTasks/BlockedTask">
			<li><i>(Blocked)</i>&nbsp;<xsl:value-of select="."/></li>
		</xsl:for-each>
	</ul>
</xsl:template>

<xsl:template match="IndexStatus">
	<b>Details of backends:</b><br/>
	<ul class="indexstatus">
		<xsl:for-each select="QueryableStatus">
			<li><div class="queryablestatus">
				<xsl:choose>
					<xsl:when test="@ItemCount = '-1'">
						<xsl:call-template name="StatusBackendNotUsed"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="StatusBackendInUse"/>
					</xsl:otherwise>
				</xsl:choose>
			</div></li>
		</xsl:for-each>
	</ul>
</xsl:template>

<xsl:template name="StatusBackendNotUsed">
	<span class="queryablestatus-name">
		<xsl:value-of select="@Name"/>&nbsp;:
	</span>
	<span class="queryablestatus-isindexing">
		backend currently not in use
	</span>
</xsl:template>

<xsl:template name="StatusBackendInUse">
	<span class="queryablestatus-name">
		<xsl:value-of select="@Name"/>&nbsp;:
	</span>
	<span class="queryablestatus-itemcount">
		<xsl:value-of select="@ItemCount"/> items,&nbsp;
	</span>
	<span class="queryablestatus-isindexing">
		<xsl:choose>
			<xsl:when test="@IsIndexing='true'">Currently indexing</xsl:when>
			<xsl:otherwise>Currently not indexing</xsl:otherwise>
		</xsl:choose>
	</span>
	<span class="queryablestatus-progresspercent">
		<xsl:if test="@ProgressPercent != '-1'">
		(Initial crawling: <xsl:value-of select="@ProgressPercent"/>)
		</xsl:if>
	</span>
</xsl:template>

</xsl:stylesheet>
