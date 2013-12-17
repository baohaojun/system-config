<?xml version="1.0" encoding="utf-8"?>
<appwidget-provider xmlns:android="http://schemas.android.com/apk/res/android"
    android:minWidth="${-30 + 70 * minWidth?number}dp"
    android:minHeight="${-30 + 70 * minHeight?number}dp"
    android:updatePeriodMillis="86400000"
    android:previewImage="@drawable/example_appwidget_preview"
    android:initialLayout="@layout/${class_name}"
<#if configurable>
    android:configure="${packageName}.${className}ConfigureActivity"
</#if>
<#if resizable='both'>
    android:resizeMode="horizontal|vertical"
<#elseif resizable='horizontal'>
    android:resizeMode="horizontal"
<#elseif resizable='vertical'>
    android:resizeMode="vertical"
<#elseif resizable='none'>
</#if>
<#if placement='both'>
    android:widgetCategory="home_screen|keyguard"
<#elseif placement='homescreen'>
    android:widgetCategory="home_screen"
<#elseif placement='keyguard'>
    android:widgetCategory="keyguard"
</#if>
    android:initialKeyguardLayout="@layout/${class_name}">
</appwidget-provider>