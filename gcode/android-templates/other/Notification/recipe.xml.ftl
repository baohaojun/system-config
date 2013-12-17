<?xml version="1.0"?>
<recipe>

    <merge from="AndroidManifest.xml.ftl" />

    <#if expandedStyle == "picture">
    <copy from="res/drawable-nodpi/example_picture_large.png"
            to="res/drawable-nodpi/example_picture.png" />
    <#else>
    <copy from="res/drawable-nodpi/example_picture_small.png"
            to="res/drawable-nodpi/example_picture.png" />
    </#if>

    <#if moreActions>
    <copy from="res/drawable-hdpi" />
    <copy from="res/drawable-mdpi" />
    <copy from="res/drawable-xhdpi" />
    </#if>

    <merge from="res/values/strings.xml.ftl" />

    <instantiate from="src/app_package/NotificationHelper.java.ftl"
                   to="${srcOut}/${className}.java" />
    <open file="${srcOut}/${className}.java" />
</recipe>
