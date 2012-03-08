package com.mrvl.autotest.project;

import java.io.File;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

import com.mrvl.autotest.R;


public class MainProject extends Activity {

    private static final String TAG = "MainProject";
    private Button gpsButton;
    private Button videoButton;
    private Context mContext;    
    private File file;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.project_main);
        mContext = this;

        gpsButton = (Button) findViewById(R.id.project_gps);
        videoButton = (Button) findViewById(R.id.project_video);
        
        gpsButton.setOnClickListener(new OnClickListener() {

            public void onClick(View v) {
                Intent gpsIntent;
                gpsIntent = new Intent(mContext, PGpsTest.class);
                gpsIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                mContext.startActivity(gpsIntent);

            }
        });

        videoButton.setOnClickListener(new OnClickListener() {

            public void onClick(View v) {
            	if((file = new File("/mnt/D/test.avi")).exists() ||
            			(file = new File("/mnt/sdcard/test.avi")).exists()) {               
                    Intent intent = new Intent(Intent.ACTION_VIEW);
                    Uri data = getUri(file);
                    intent.setDataAndType(data,"video/avi");
                    startActivityForResult(intent, 2);
                } else {
                    Toast.makeText(mContext,getResources().getString(R.string.video_test_err),
                            Toast.LENGTH_SHORT).show();

                    return;
                }
            }
        });
        
    }
    

    private static Uri getUri(File file) {
        if (file != null) {
            return Uri.fromFile(file);
        }
        return null;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == 2) {
        	if((file = new File("/mnt/D/test.avi")).exists() ||
        			(file = new File("/mnt/sdcard/test.avi")).exists()) {        
	            Intent intent = new Intent(Intent.ACTION_VIEW);
	            Uri data1 = getUri(file);
	            intent.setDataAndType(data1,"video/avi");
	            startActivityForResult(intent, 2);
            } else {
                Toast.makeText(mContext,getResources().getString(R.string.video_test_err),
                        Toast.LENGTH_SHORT).show();
                return;
            }
        }
    }
    
}

