<?xml version="1.0"?>
<recipe>
    <instantiate from="AndroidManifest.xml.ftl"
                   to="${escapeXmlAttribute(manifestOut)}/AndroidManifest.xml" />

<#if copyIcons>
    <copy from="res/drawable-hdpi"
            to="${escapeXmlAttribute(resOut)}/drawable-hdpi" />
    <copy from="res/drawable-mdpi"
            to="${escapeXmlAttribute(resOut)}/drawable-mdpi" />
    <copy from="res/drawable-xhdpi"
            to="${escapeXmlAttribute(resOut)}/drawable-xhdpi" />
</#if>
    <instantiate from="res/values/styles.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/values/styles.xml" />
<#if buildApi gte 11 && baseTheme != "none">
    <instantiate from="res/values-v11/styles_hc.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/values-v11/styles.xml" />
</#if>
<#if buildApi gte 14 && baseTheme?contains("darkactionbar")>
    <copy from="res/values-v14/styles_ics.xml"
            to="${escapeXmlAttribute(resOut)}/values-v14/styles.xml" />
</#if>

    <instantiate from="res/values/strings.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/values/strings.xml" />
</recipe>
