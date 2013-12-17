<?xml version="1.0"?>
<recipe>

<#if switchGrid == true>
    <merge from="res/values/refs.xml.ftl" />
    <merge from="res/values/refs_lrg.xml.ftl"
           to="res/values-large/refs.xml" />
    <merge from="res/values/refs_lrg.xml.ftl"
           to="res/values-sw600dp/refs.xml" />

    <instantiate from="res/layout/fragment_grid.xml"
                 to="res/layout/${fragment_layout}_grid.xml" />

    <instantiate from="res/layout/fragment_list.xml"
                 to="res/layout/${fragment_layout}_list.xml" />
</#if>

    <instantiate from="src/app_package/ListFragment.java.ftl"
                 to="${srcOut}/${className}.java" />

    <instantiate from="src/app_package/dummy/DummyContent.java.ftl"
                 to="${srcOut}/dummy/DummyContent.java" />

    <open file="${srcOut}/${className}.java" />

</recipe>
