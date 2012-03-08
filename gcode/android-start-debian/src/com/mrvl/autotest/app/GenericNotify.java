package com.mrvl.autotest.app;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;
import android.widget.Toast;

import android.content.Intent;


public class GenericNotify extends Service {

    

    public class LocalBinder extends Binder {
        GenericNotify getService() {
            return GenericNotify.this;
        }
    }
    
    @Override
    public void onCreate() {
    }

    @Override
        public int onStartCommand(Intent intent, int flags, int startId) {
        onStart(intent, startId);
        
        String extra = intent.getStringExtra("HelloWorld");

        if (extra != null) {
            Toast.makeText(this, extra, Toast.LENGTH_LONG).show();
        } else {
            Toast.makeText(this, "hello world", Toast.LENGTH_LONG).show();

        }
        stopSelf();
        return START_STICKY;
    }


    @Override
    public void onDestroy() {
        
        Log.e("GenericNotify", "onDestroy()");
    }

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }
    
    private final IBinder mBinder = new LocalBinder();
}
