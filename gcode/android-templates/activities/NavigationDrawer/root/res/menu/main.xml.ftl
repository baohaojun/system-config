<menu xmlns:android="http://schemas.android.com/apk/res/android">
    <item android:id="@+id/action_settings"
        android:title="@string/action_settings"
        android:orderInCategory="100"<#if buildApi gte 11>
        android:showAsAction="never"</#if> />
    <item android:id="@+id/action_websearch"
        android:icon="@drawable/action_search"
        android:title="@string/action_websearch"
        android:showAsAction="ifRoom|withText" />
</menu>
