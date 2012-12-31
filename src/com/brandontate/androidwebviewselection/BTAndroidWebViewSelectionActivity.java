package com.brandontate.androidwebviewselection;

import android.app.Activity;
import android.os.Bundle;
import android.view.WindowManager;
import android.view.Window;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Button;
import android.widget.EditText;
import android.view.View.OnClickListener;
import android.view.View;
import java.util.ArrayList;
import android.widget.AdapterView;
import android.util.Log;
import android.widget.TextView;
import android.view.KeyEvent;
import android.content.DialogInterface;
import android.app.AlertDialog;
import com.googlecode.toolkits.stardict.StarDict;

public class BTAndroidWebViewSelectionActivity extends Activity {
    /** Called when the activity is first created. */
    private EditText mEdit; 
    private SlowListView mListView;

    private Button mLookUpButton;
    private Button mDefinedButton;
    private Button mMatchingButton;
    private BTWebView mWebView;

    private StarDict mDict = new StarDict("/sdcard/ahd/ahd");

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, 
			     WindowManager.LayoutParams.FLAG_FULLSCREEN);

        setContentView(R.layout.main);

        mListView = (SlowListView) findViewById(R.id.nearby_dict_entries);
        mListView.createAndSetAdapter(this, mDict);
	mEdit = (EditText) findViewById(R.id.enter_dict_entry);

	mLookUpButton = (Button) findViewById(R.id.look_up_button);
	mLookUpButton.setOnClickListener(mLookUpListner);
	mWebView = (BTWebView) findViewById(R.id.webView);
	mWebView.setActivity(this);
	mWebView.setDict(mDict);
	mWebView.lookUpWord("eclair");

	mListView.setOnItemClickListener(mItemClickListener);
    }

    AdapterView.OnItemClickListener mItemClickListener = new AdapterView.OnItemClickListener() {
	    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
		TextView tv = (TextView)view;
		mWebView.lookUpWord(tv.getText().toString());
	    }
	};

    private String[] mWordHistory = new String[100];
    private int mHistoryHead = 0;
    private int mHistoryTail = 0;
    String mCurrentWord;
    public void onNewWordLoaded(String word) {
	mCurrentWord = word;
	mListView.scrollToWord(word);
	if (mPoping) {
	    return;
	}
	mWordHistory[mHistoryHead] = word;
	mHistoryHead = (mHistoryHead + 1) % mWordHistory.length;
	if (mHistoryHead == mHistoryTail) {
	    mHistoryTail = (mHistoryTail + 1) % mWordHistory.length;
	}
    }

    boolean mPoping;
    boolean popHistory() {
	if (mHistoryTail == mHistoryHead) {
	    Log.e("bhj", String.format("mHistoryTail == mHistoryHead = %d\n", mHistoryHead));
	    return false;
	}

	mHistoryHead--;
	if (mHistoryHead < 0) {
	    mHistoryHead = mWordHistory.length - 1;
	}

	if (mWordHistory[mHistoryHead] != null) {
	    if (mCurrentWord != null && ! mCurrentWord.equals(mWordHistory[mHistoryHead])) {
		Log.e("bhj", String.format("poping, mCurrentWord is %s, head is %s\n", mCurrentWord, mWordHistory[mHistoryHead]));
		mPoping = true;
		mWebView.lookUpWord(mWordHistory[mHistoryHead]);
		mPoping = false;
		if (mHistoryHead == mHistoryTail) { // push the current word back
		    mHistoryHead = (mHistoryHead + 1) % mWordHistory.length;
		}		    
		return true;
	    } else {
		return popHistory();
	    }
	}
	Log.e("bhj", String.format("head is null: head = %d, tail = %d\n", mHistoryHead, mHistoryTail));
	return false;
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
	if (keyCode == KeyEvent.KEYCODE_BACK && popHistory()) {
	    return true;
	} else {
	    final int finalKeyCode = keyCode;
	    final KeyEvent finalEvent = event;
	    new AlertDialog.Builder(BTAndroidWebViewSelectionActivity.this)
                .setTitle("Exit?")
                .setMessage("Please confirm that you want to exit.")
                .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int whichButton) {
			BTAndroidWebViewSelectionActivity.super.onKeyUp(finalKeyCode, finalEvent);
                    }
                })
                .setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int whichButton) {
			onNewWordLoaded(mCurrentWord); // when the dialog pop up, it means the history is empty
                    }
                })
                .create().show();
	    return true;
	}
    }	
	
    View.OnClickListener mLookUpListner = new OnClickListener() {
	    public void onClick(View v) {
		mWebView.lookUpWord(mEdit.getText().toString());
	    }
	};
}
