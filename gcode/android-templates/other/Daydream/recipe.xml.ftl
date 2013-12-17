<?xml version="1.0"?>
<recipe>

    <merge from="AndroidManifest.xml.ftl" />
    <merge from="res/values/strings.xml.ftl" />

    <copy from="res/layout-v17/dream.xml"
          to="res/layout-v17/${class_name}.xml" />

    <instantiate from="src/app_package/DreamService.java.ftl"
                 to="${srcOut}/${className}.java" />

<#if configurable>
    <copy from="res/xml/dream_prefs.xml"
          to="res/xml/${prefs_name}.xml" />

    <instantiate from="src/app_package/SettingsActivity.java.ftl"
                 to="${srcOut}/${settingsClassName}.java" />

    <instantiate from="res/xml/xml_dream.xml.ftl"
                 to="res/xml/${info_name}.xml" />
</#if>

    <open file="${srcOut}/${className}.java" />

</recipe>
