package com.baohaojun.crossdict;

import android.app.Notification;
import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;
import android.widget.Toast;

public class ClipMonService extends Service {
    ClipboardManager mClipboard;
    ClipboardManager.OnPrimaryClipChangedListener mClipListner;
    boolean mClipWatched = false;

    static boolean mPowerConnected = false;
    static void setPowerConnected(boolean v) {
        mPowerConnected = v;
    }
    @Override
    public void onCreate() {
        startForeground(1, new Notification());
    }
    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        Log.e("bhj", String.format("%s:%d: start watching clipboard", "ClipMonService.java", 16));
        if (mClipboard == null) {
            mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
        }

        if (mClipListner == null) {
            mClipListner = new ClipboardManager.OnPrimaryClipChangedListener() {
                    public void onPrimaryClipChanged() {
                        ClipData clip = mClipboard.getPrimaryClip();
                        if (clip != null) {
                            ClipData.Item item = clip.getItemAt(0);
                            if (item.getText() != null && ! mPowerConnected) {
                                startActivity(
                                    new Intent(ClipMonService.this, CrossDictActivity.class)
                                    .putExtra("android.intent.extra.TEXT", item.getText().toString())
                                    .setFlags(Intent.FLAG_ACTIVITY_NEW_TASK));
                            }
                        }
                    }
                };
        }
        if (!mClipWatched) {
            mClipboard.addPrimaryClipChangedListener(mClipListner);
            mClipWatched = true;
        }
        return START_STICKY;
    }
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mClipboard.removePrimaryClipChangedListener(mClipListner);
    }

}
