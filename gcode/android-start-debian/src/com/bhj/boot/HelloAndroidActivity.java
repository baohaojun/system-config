package com.bhj.boot;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

public class HelloAndroidActivity extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

	Log.e("bhj", " hello world");
	try {
	    Runtime.getRuntime().
		exec("/system/bin/sh /sdcard/screencap.sh").
		waitFor();
	} catch (Exception e) {
	    Log.e("bhj", "caught exception", e);
	}
	Log.e("bhj", " start screencap completed");
	finish();
    }
}
