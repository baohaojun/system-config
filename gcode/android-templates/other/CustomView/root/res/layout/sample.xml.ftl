<FrameLayout
    xmlns:android="http://schemas.android.com/apk/res/android"
<#if !isLibraryProject>
    xmlns:app="http://schemas.android.com/apk/res/${packageName}"
<#else>
    xmlns:app="http://schemas.android.com/apk/res-auto"
</#if>
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <${packageName}.${viewClass}
        android:background="#ccc"
        android:layout_width="300dp"
        android:layout_height="300dp"
        android:paddingLeft="20dp"
        android:paddingBottom="40dp"
        app:exampleDimension="24sp"
        app:exampleColor="#33b5e5"
        app:exampleString="Hello, ${viewClass}"
        app:exampleDrawable="@android:drawable/ic_menu_add" />

</FrameLayout>
