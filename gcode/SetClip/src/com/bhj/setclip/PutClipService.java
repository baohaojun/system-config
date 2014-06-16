package com.bhj.setclip;

import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.Intent;
import android.os.IBinder;
import android.widget.Toast;
import java.io.File;
import java.io.FileReader;

public class PutClipService extends Service {
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        try {
            FileReader f = new FileReader(new File("/sdcard/putclip.txt"));
            char[] buffer = new char[1024];
            int n = f.read(buffer);
            String str = new String(buffer, 0, n);
            ClipboardManager mClipboard;
            mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
            mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", str));
            File putclip = new File("/sdcard/putclip.txt");
            putclip.delete();
        } catch (Exception e) {
            Toast.makeText(this, "Something went wrong in putclip: " + e.getMessage(), Toast.LENGTH_SHORT).show();
        }
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
    }
}
