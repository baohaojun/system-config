<?xml version="1.0"?>
<recipe>
    <dependency mavenUrl="com.android.support:support-v4:18.0.0" />
    
    <merge from="AndroidManifest.xml.ftl"
             to="${escapeXmlAttribute(manifestOut)}/AndroidManifest.xml" />

    <merge from="res/values-large/refs.xml.ftl"
             to="${escapeXmlAttribute(resOut)}/values-large/refs.xml" />
    <merge from="res/values-sw600dp/refs.xml.ftl"
             to="${escapeXmlAttribute(resOut)}/values-sw600dp/refs.xml" />
    <merge from="res/values/strings.xml.ftl"
             to="${escapeXmlAttribute(resOut)}/values/strings.xml" />

    <instantiate from="res/layout/activity_content_detail.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/layout/activity_${detail_name}.xml" />
    <instantiate from="res/layout/activity_content_list.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/layout/activity_${collection_name}.xml" />
    <instantiate from="res/layout/activity_content_twopane.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/layout/activity_${extractLetters(objectKind?lower_case)}_twopane.xml" />
    <instantiate from="res/layout/fragment_content_detail.xml.ftl"
                   to="${escapeXmlAttribute(resOut)}/layout/fragment_${detail_name}.xml" />

    <instantiate from="src/app_package/ContentDetailActivity.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${DetailName}Activity.java" />
    <instantiate from="src/app_package/ContentDetailFragment.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${DetailName}Fragment.java" />
    <instantiate from="src/app_package/ContentListActivity.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${CollectionName}Activity.java" />
    <instantiate from="src/app_package/ContentListFragment.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/${CollectionName}Fragment.java" />
    <instantiate from="src/app_package/dummy/DummyContent.java.ftl"
                   to="${escapeXmlAttribute(srcOut)}/dummy/DummyContent.java" />

    <open file="${escapeXmlAttribute(resOut)}/layout/fragment_${detail_name}.xml" />
    <open file="${escapeXmlAttribute(srcOut)}/${DetailName}Fragment.java" />
</recipe>
