<?xml version="1.0"?>
<recipe>
    <dependency mavenUrl="com.android.support:support-v4:18.0.0" />

    <merge from="AndroidManifest.xml.ftl"
             to="${escapeXmlAttribute(manifestOut)}/AndroidManifest.xml" />

    <copy from="res/xml/pref_data_sync.xml"
            to="${escapeXmlAttribute(resOut)}/xml/pref_data_sync.xml" />
    <copy from="res/xml/pref_general.xml"
            to="${escapeXmlAttribute(resOut)}/xml/pref_general.xml" />
    <merge from="res/xml/pref_headers.xml.ftl"
             to="${escapeXmlAttribute(resOut)}/xml/pref_headers.xml" />
    <copy from="res/xml/pref_notification.xml"
            to="${escapeXmlAttribute(resOut)}/xml/pref_notification.xml" />

    <instantiate from="res/values/strings.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/values/strings_${simpleName}.xml" />

    <instantiate from="src/app_package/SettingsActivity.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    <open file="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />
</recipe>
