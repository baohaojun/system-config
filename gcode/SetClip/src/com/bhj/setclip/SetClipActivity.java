package com.bhj.setclip;

import android.app.Activity;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.widget.Toast;
import java.io.File;
import java.io.FileReader;

public class SetClipActivity extends Activity
{
    ClipboardManager mClipboard;
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);


        Handler clipHandler = new Handler() {
                @Override
                public void handleMessage(Message msg) {
                    super.handleMessage(msg);

                    try {
                        FileReader f = new FileReader(new File("/sdcard/putclip.txt"));
                        char[] buffer = new char[1024];
                        int n = f.read(buffer);
                        String str = new String(buffer, 0, n);
                        mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", str));
                        Toast.makeText(SetClipActivity.this, str, Toast.LENGTH_SHORT).show();
                    } catch (Exception e) {
                        Toast.makeText(SetClipActivity.this, "Something went wrong in putclip: " + e.getMessage(), Toast.LENGTH_SHORT).show();
                    }
                    finish();
                }
            };

        clipHandler.sendEmptyMessage(0);
    }
}
