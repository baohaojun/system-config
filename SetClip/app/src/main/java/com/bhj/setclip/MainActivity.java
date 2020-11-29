package com.bhj.setclip;

import android.app.Notification;
import android.os.Handler;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
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
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;
import android.app.NotificationChannel;
import android.app.NotificationManager;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;


public class MainActivity extends Activity {
    private static final String CHANNEL_ID = "channel_id";

    ClipboardManager mClipboard;
    private static File sdcard = Environment.getExternalStorageDirectory();
    private NotificationManager mNotificationManager;
    Button submitButton;
    private EditText editText;
    private TextWatcher tw;


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
                int n;
                while ((n = bis.read(buf)) != -1) {
                    bos.write(buf, 0, n);
                }
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

    private void sendText(String text) {
        // In this sample, we'll use this text for the title of the notification
        CharSequence title = "Wrench Input";
        if (! text.isEmpty()) {
            try {
                File WrenchDir = new File(sdcard, "Wrench");
                File capsuleDir = new File(WrenchDir, "capsule");
                File capsuleFile = new File(capsuleDir, "capsule.txt");

                capsuleDir.mkdirs();
                FileWriter fw = new FileWriter(capsuleFile, true);
                    BufferedWriter bw = new BufferedWriter(fw);
                    bw.append(text + "\n\n");
                    bw.flush();
                    bw.close();
            } catch (IOException e) {
                Log.e("bhj", String.format("%s:%d: ", "MainActivity.java", 133), e);
            }
            Notification.Builder notifBuidler = new Notification.Builder(this) // the context to use
                    .setSmallIcon(R.drawable.ic_launcher)  // the status icon
                    .setWhen(System.currentTimeMillis())  // the timestamp for the notification
                    .setContentTitle(title)  // the title for the notification
                    .setContentText(text + "，")  // the details to display in the notification
                    .setChannelId(CHANNEL_ID)
                    .setAutoCancel(true);


            mNotificationManager.notify(0, notifBuidler.build());
        }

        editText.setText("");
        editText.clearComposingText();
    }

    private void sendText() {
        // In this sample, we'll use the same text for the ticker and the expanded notification
        String text = editText.getText().toString();
        sendText(text);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.layout_main);

        submitButton = (Button) findViewById(R.id.submit_to_wrench);
        editText = (EditText) findViewById(R.id.editText);
        mNotificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);

        CharSequence name = getString(R.string.channel_name);
        String description = getString(R.string.channel_description);
        int importance = NotificationManager.IMPORTANCE_DEFAULT;
        NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name, importance);
        channel.setDescription(description);
        // Register the channel with the system; you can't change the importance
        // or other notification behaviors after this
        mNotificationManager.createNotificationChannel(channel);
        final Handler handler = new Handler();

        submitButton.setOnClickListener(new Button.OnClickListener() {
            public void onClick(View v) {
                String text = editText.getText().toString();
                if (text.isEmpty()) {
                    if (submitButton.getText().toString().equals("SubmitToWrench")) {
                        submitButton.setText("AutoSubmit");
                        tw = new TextWatcher() {
                            @Override
                            public void onTextChanged(CharSequence s, int start, int before, int count) {
                            }

                            @Override
                            public void beforeTextChanged(CharSequence s, int start, int count,
                                                          int after) {
                            }

                            @Override
                            public void afterTextChanged(Editable s) { // should send text after 1 sec
                                handler.removeCallbacksAndMessages(null);
                                if (s.toString().isEmpty()) {
                                    return;
                                }
                                final String text = s.toString();
                                handler.postDelayed(new Runnable() {
                                    @Override
                                    public void run() {
                                        sendText(text);
                                    }
                                }, 5000);

                            }
                        };
                        editText.addTextChangedListener(tw);
                    } else {
                        submitButton.setText("SubmitToWrench");
                        editText.removeTextChangedListener(tw);
                    }
                } else {
                    sendText();
                }

            }
        });

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
