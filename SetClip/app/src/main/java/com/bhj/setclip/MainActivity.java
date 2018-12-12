package com.bhj.setclip;

import android.app.Activity;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.widget.Toast;

public class MainActivity extends Activity {
    ClipboardManager mClipboard;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.layout_main);
        
        Uri data = getIntent().getData();
        if (data != null) {
            Toast.makeText(MainActivity.this, String.format("You want to open: %s", data.getPath()), Toast.LENGTH_SHORT).show();
            mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
            mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", data.getPath()));
        }
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);

        return true;
    }
    
}
