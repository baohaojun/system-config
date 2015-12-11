package com.mycompany.myapp;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import java.io.File;

public class MainActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.layout_main);

        File file = new File("/sdcard/Download/mail_attachments/a.txt");
        try {
            for (int i = 0; i < 1000; i++) {
                boolean createSuccess = file.createNewFile();
                Log.d("FileObserver", file.getAbsolutePath() + " create success? " + createSuccess);
            }
        } catch (Exception e) {
            Log.e("bhj", String.format("%s:%d: ", "MainActivity.java", 21), e);
        }

    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

}
