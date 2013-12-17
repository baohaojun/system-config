<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="${packageName}"
    android:versionCode="1"
    android:versionName="1.0">

    <uses-sdk android:minSdkVersion="${minApi}" <#if buildApi gte 4>android:targetSdkVersion="${targetApi}" </#if>/>

    <application <#if minApiLevel gte 4 && buildApi gte 4>android:allowBackup="true"</#if>
        android:label="@string/app_name"
        android:icon="@drawable/ic_launcher"<#if baseTheme != "none">
        android:theme="@style/AppTheme"</#if>>

    </application>

</manifest>
