package com.bhj.setclip;

import android.app.Activity;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ContentResolver;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.MediaStore;
import android.util.Log;
import android.view.Menu;
import android.widget.Toast;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;


public class MainActivity extends Activity {
    ClipboardManager mClipboard;
    private static File sdcard = Environment.getExternalStorageDirectory();

    public static String saveToFile(ContentResolver resolver, Intent intent) {
        Uri fileUri = (Uri) intent.getParcelableExtra(Intent.EXTRA_STREAM);
        if (fileUri == null) {
            fileUri = intent.getData();
            if (fileUri == null) {
                Log.e("bhj", String.format("%s:%d: got null intent for file", "MainActivity.java", 30));
                return null;
            }
        }

        String[] projection = {MediaStore.MediaColumns.DATA};
        Log.e("bhj", String.format("%s:%d: fileUri is %s", "MainActivity.java", 38, fileUri.toString()));

        String sourceFilename = fileUri.getPath();

        if (sourceFilename == null) {
            Log.e("bhj", String.format("%s:%d: Can't find source file name from fileUri", "MainActivity.java", 43));
            Cursor metaCursor = resolver.query(fileUri, projection, null, null, null);
            if (metaCursor != null) {
                try {
                    if (metaCursor.moveToFirst()) {
                        sourceFilename = metaCursor.getString(0);
                        Log.e("bhj", String.format("%s:%d: shared file is %s", "MainActivity.java", 48, sourceFilename));
                    }
                } catch (Exception e) {
                    Log.e("bhj", String.format("%s:%d: ", "MainActivity.java", 51), e);
                } finally {
                    metaCursor.close();
                }
            }
        }

        if (sourceFilename == null) {
            return null;
        }


        File WrenchDir = new File(sdcard, "Wrench");
        File WrenchFileDir = new File(WrenchDir, "files");

        File sourceFile = new File(sourceFilename);

        if (sourceFilename.startsWith(sdcard.getAbsolutePath() + "/Wrench/files/")) {
            Log.e("bhj", String.format("%s:%d: got a file that is already in Wrench", "MainActivity.java", 69));
        } else {
            File copiedFileFile = new File(WrenchFileDir, sourceFile.getName());

            WrenchFileDir.mkdirs();

            BufferedInputStream bis = null;
            BufferedOutputStream bos = null;

            try {
                bis = new BufferedInputStream(resolver.openInputStream(fileUri));
                bos = new BufferedOutputStream(new FileOutputStream(copiedFileFile, false));
                byte[] buf = new byte[1024];
                bis.read(buf);
                do {
                    bos.write(buf);
                } while(bis.read(buf) != -1);
                return copiedFileFile.getAbsolutePath();
            } catch (IOException e) {
                Log.e("bhj", String.format("%s:%d: ", "MainActivity.java", 87), e);
            } finally {
                try {
                    if (bis != null) bis.close();
                    if (bos != null) bos.close();
                } catch (IOException e) {
                    Log.e("bhj", String.format("%s:%d: ", "MainActivity.java", 93), e);
                }
            }
        }
        return null;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.layout_main);

        String savedFile = saveToFile(getContentResolver(), getIntent());
        if (savedFile != null) {
            Toast.makeText(MainActivity.this, String.format("You want to open: %s", savedFile), Toast.LENGTH_SHORT).show();
            mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
            mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", savedFile));
        }
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);

        return true;
    }
    
}
