package com.bhj.boot;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.os.SystemProperties;
import android.provider.Settings;

public class AutoBootReceiver extends BroadcastReceiver {
    public void onReceive(Context context, Intent intent) {
	Log.e("bhj", " hello world");
	try {
	    Runtime.getRuntime().
		exec("/system/bin/sh /system/start-debian.sh").
		waitFor();
	} catch (Exception e) {
	    Log.e("bhj", "caught exception", e);
	}
	Log.e("bhj", " start debian completed");
    }
}
