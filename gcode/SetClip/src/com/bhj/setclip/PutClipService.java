package com.bhj.setclip;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningTaskInfo;
import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import android.os.IBinder;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.widget.Toast;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class PutClipService extends Service {
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private String getTask() {
        ActivityManager am = (ActivityManager) getSystemService(ACTIVITY_SERVICE);
        RunningTaskInfo foregroundTaskInfo = am.getRunningTasks(1).get(0);
        return foregroundTaskInfo .topActivity.getPackageName();
    }

    private void writeFile(String str) throws IOException {
        FileWriter f = new FileWriter(new File(Environment.getExternalStorageDirectory(), "putclip.txt.1"));
        f.write(str);
        f.close();
        File txt = new File(Environment.getExternalStorageDirectory(), "putclip.txt.1");
        txt.renameTo(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
    }

    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        try {
            String picName = intent.getStringExtra("picture");
            if (picName != null) {
                picName = picName.replaceFirst("^/sdcard/", "");
                sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, Uri.fromFile(new File(Environment.getExternalStorageDirectory(), picName))));
            } else if (intent.getIntExtra("gettask", 0) == 1) {
                String foregroundTaskPackageName = getTask();
                writeFile(foregroundTaskPackageName);
            } else if (intent.getIntExtra("getclip", 0) == 1) {
                ClipboardManager mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
                String str = mClipboard.getPrimaryClip().getItemAt(0).getText().toString();
                writeFile(str);
            } else if (intent.getIntExtra("getphone", 0) == 1) {
                TelephonyManager tMgr =(TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
                String mPhoneNumber = tMgr.getLine1Number();
                writeFile(mPhoneNumber);
            } else {
                FileReader f = new FileReader(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
                if (getTask().equals("com.tencent.mm")) {
                    f.close();
                    f = new FileReader(new File(Environment.getExternalStorageDirectory(), "putclip-wx.txt"));
                }
                char[] buffer = new char[1024 * 1024];
                int n = f.read(buffer);
                String str = new String(buffer, 0, n);
                Log.e("bhj", String.format("%s:%d: str is %s", "PutClipService.java", 36, str));
                ClipboardManager mClipboard;
                mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
                mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", str));
                File putclip = new File(Environment.getExternalStorageDirectory(), "putclip.txt");
                putclip.delete();
            }
        } catch (Exception e) {
            Toast.makeText(this, "Something went wrong in putclip: " + e.getMessage(), Toast.LENGTH_SHORT).show();
            Log.e("bhj", String.format("%s:%d: ", "PutClipService.java", 77), e);
        }
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
    }
}
