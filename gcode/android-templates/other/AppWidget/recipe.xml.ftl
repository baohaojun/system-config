<?xml version="1.0"?>
<recipe>

    <merge from="AndroidManifest.xml.ftl" />

    <copy from="res/drawable-nodpi/example_appwidget_preview.png" />
    <instantiate from="res/layout/appwidget.xml"
                   to="res/layout/${class_name}.xml" />


    <#if configurable>
    <instantiate from="res/layout/appwidget_configure.xml"
                   to="res/layout/${class_name}_configure.xml" />
    </#if>

    <instantiate from="res/xml/appwidget_info.xml.ftl"
                   to="res/xml/${class_name}_info.xml" />
    <merge from="res/values/strings.xml.ftl" />
    <merge from="res/values-v14/dimens.xml" />
    <merge from="res/values/dimens.xml" />

    <instantiate from="src/app_package/AppWidget.java.ftl"
                   to="${srcOut}/${className}.java" />

    <#if configurable>
    <instantiate from="src/app_package/AppWidgetConfigureActivity.java.ftl"
                   to="${srcOut}/${className}ConfigureActivity.java" />
    </#if>

    <open file="${srcOut}/${className}.java" />
</recipe>
