package com.bhj.setclip;

import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.Intent;
import android.os.Environment;
import android.os.IBinder;
import android.util.Log;
import android.widget.Toast;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class PutClipService extends Service {
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        try {
            if (intent.getIntExtra("getclip", 0) == 1) {
                FileWriter f = new FileWriter(new File(Environment.getExternalStorageDirectory(), "putclip.txt.1"));
                ClipboardManager mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
                String str = mClipboard.getPrimaryClip().getItemAt(0).getText().toString();
                f.write(str);
                f.close();
                File txt = new File(Environment.getExternalStorageDirectory(), "putclip.txt.1");
                txt.renameTo(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
            } else {
                FileReader f = new FileReader(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
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
        }
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
    }
}
