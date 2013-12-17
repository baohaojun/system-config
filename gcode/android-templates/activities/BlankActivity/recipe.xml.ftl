<?xml version="1.0"?>
<recipe>
    <dependency mavenUrl="com.android.support:appcompat-v7:18.0.0"/>

    <merge from="AndroidManifest.xml.ftl"
             to="${escapeXmlAttribute(manifestOut)}/AndroidManifest.xml" />

    <instantiate from="res/menu/main.xml.ftl"
            to="${escapeXmlAttribute(resOut)}/menu/${menuName}.xml" />

    <merge from="res/values/strings.xml.ftl"
             to="${escapeXmlAttribute(resOut)}/values/strings.xml" />

    <merge from="res/values/dimens.xml"
             to="${escapeXmlAttribute(resOut)}/values/dimens.xml" />
    <merge from="res/values-sw600dp/dimens.xml"
             to="${escapeXmlAttribute(resOut)}/values-sw600dp/dimens.xml" />
    <merge from="res/values-sw720dp-land/dimens.xml"
             to="${escapeXmlAttribute(resOut)}/values-sw720dp-land/dimens.xml" />

    <!-- Decide what kind of layout to add (viewpager or not) -->
    <#if navType?contains("pager")>
        <instantiate from="res/layout/activity_pager.xml.ftl"
                       to="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />
        <instantiate from="res/layout/fragment_dummy.xml.ftl"
                       to="${escapeXmlAttribute(resOut)}/layout/fragment_${classToResource(activityClass)}_dummy.xml" />

    <#elseif navType == "tabs" || navType == "dropdown">
        <instantiate from="res/layout/activity_fragment_container.xml"
                       to="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />
        <instantiate from="res/layout/fragment_dummy.xml.ftl"
                       to="${escapeXmlAttribute(resOut)}/layout/fragment_${classToResource(activityClass)}_dummy.xml" />

    <#else>
        <instantiate from="res/layout/activity_simple.xml.ftl"
                       to="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />
    </#if>

    <!-- Decide which activity code to add -->
    <#if navType == "none">
        <instantiate from="src/app_package/SimpleActivity.java.ftl"
                       to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    <#elseif navType == "tabs_pager" || navType == "pager_strip">
        <instantiate from="src/app_package/TabsAndPagerActivity.java.ftl"
                       to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    <#elseif navType == "tabs">
        <instantiate from="src/app_package/TabsActivity.java.ftl"
                       to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    <#elseif navType == "dropdown">
        <instantiate from="src/app_package/DropdownActivity.java.ftl"
                       to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    </#if>

    <open file="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />
    <open file="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />
</recipe>
