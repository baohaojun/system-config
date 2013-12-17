<manifest xmlns:android="http://schemas.android.com/apk/res/android" >

    <application>
        <provider android:name=".${className}"
            android:authorities="${authorities}"
            android:exported="${isExported?string}"
            android:enabled="${isEnabled?string}" >
        </provider>
    </application>

</manifest>
