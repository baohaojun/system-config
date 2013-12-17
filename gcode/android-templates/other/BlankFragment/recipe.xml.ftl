<?xml version="1.0"?>
<recipe>

    <merge from="res/values/strings.xml" />

    <#if includeLayout>
        <instantiate from="res/layout/fragment_blank.xml.ftl"
                       to="res/layout/fragment_${classToResource(className)}.xml" />

        <open file="res/layout/fragment_${classToResource(className)}.xml" />
    </#if>

    <open file="${srcOut}/${className}.java" />

    <instantiate from="src/app_package/BlankFragment.java.ftl"
                   to="${srcOut}/${className}.java" />

</recipe>
