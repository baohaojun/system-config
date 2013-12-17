<?xml version="1.0"?>
<recipe>
    <dependency mavenUrl="com.android.support:support-v4:18.0.0" />

    <merge from="AndroidManifest.xml.ftl"
             to="${escapeXmlAttribute(manifestOut)}/AndroidManifest.xml" />

    <merge from="res/values/styles.xml"
             to="${escapeXmlAttribute(resOut)}/values/styles.xml" />
    <merge from="res/values-large/styles.xml"
             to="${escapeXmlAttribute(resOut)}/values-large/styles.xml" />
    <copy from="res/menu/activity_login.xml"
            to="${escapeXmlAttribute(resOut)}/menu/${menuName}.xml" />
    <instantiate from="res/layout/activity_login.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />

    <instantiate from="res/values/strings.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/values/strings_${simpleName}.xml" />

    <instantiate from="src/app_package/LoginActivity.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${activityClass}.java" />

    <open file="${escapeXmlAttribute(resOut)}/layout/${layoutName}.xml" />
</recipe>
