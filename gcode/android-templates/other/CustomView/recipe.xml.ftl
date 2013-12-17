<?xml version="1.0"?>
<recipe>
    <merge from="res/values/attrs.xml.ftl"
                   to="res/values/attrs_${view_class}.xml" />
    <instantiate from="res/layout/sample.xml.ftl"
                   to="res/layout/sample_${view_class}.xml" />

    <instantiate from="src/app_package/CustomView.java.ftl"
                   to="${srcOut}/${viewClass}.java" />

    <open file="${srcOut}/${viewClass}.java" />
    <open file="res/layout/sample_${view_class}.xml" />
</recipe>
