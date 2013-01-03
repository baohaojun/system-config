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
import com.googlecode.toolkits.stardict.StringArrayDict;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import android.content.Context;
import java.io.InputStreamReader;
import android.os.Handler;
import android.view.MenuItem;
import android.widget.Toast;
import android.widget.PopupMenu;
import com.googlecode.toolkits.stardict.FreqDict;

public class BTAndroidWebViewSelectionActivity extends Activity {
    /** Called when the activity is first created. */
    private EditText mEdit; 
    private SlowListView mListView;

    private Button mLookUpButton;
    private Button mDefinedButton;
    private Button mMatchingButton;
    private Button mListButton;
    private BTWebView mWebView;

    private StarDict mDict = new StarDict("/sdcard/ahd/ahd");
    private StarDict mUsageDict = new StarDict("/sdcard/ahd/usage");
    private FreqDict mFreqDict = new FreqDict("/sdcard/ahd/frequency");

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

	if (savedInstanceState != null) {
	    mCurrentWord = savedInstanceState.getString("mCurrentWord", "");
	} else {
	    try {
		InputStreamReader reader = new InputStreamReader(openFileInput("saved_word.txt"), "UTF8");
		char[] buffer = new char[128];
		int n = reader.read(buffer);
		if (n >= 0) {
		    mCurrentWord = new String(buffer, 0, n);
		}
	    } catch (Exception e) {
		Log.e("bhj", "read mCurrentWord failed", e);
	    }
	}

	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, 
			     WindowManager.LayoutParams.FLAG_FULLSCREEN);

        setContentView(R.layout.main);

        mListView = (SlowListView) findViewById(R.id.nearby_dict_entries);
	mEdit = (EditText) findViewById(R.id.enter_dict_entry);


	mWebView = (BTWebView) findViewById(R.id.webView);
	mWebView.setActivity(this);
	mWebView.setDict(mDict);


	mLookUpButton = (Button) findViewById(R.id.look_up_button);
	mLookUpButton.setOnClickListener(mLookUpListener);

	mDefinedButton = (Button) findViewById(R.id.defined_with_button);
	mDefinedButton.setOnClickListener(mDefinedListener);

	mListButton = (Button) findViewById(R.id.lists_button);
	mListButton.setOnClickListener(mListListener);

	mListView.createAndSetAdapter(BTAndroidWebViewSelectionActivity.this, mDict);
	mWebView.lookUpWord(mCurrentWord);
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
    String mCurrentWord = "";
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

    /**
     * @return if history is poped, then true; else false.
     *
     */
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
    protected void onSaveInstanceState(Bundle outState) {
	super.onSaveInstanceState(outState);
	outState.putString("mCurrentWord", mCurrentWord);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
	if (keyCode == KeyEvent.KEYCODE_VOLUME_DOWN) {
	    if (popHistory()) {
		return true;
	    } else {
		onNewWordLoaded(mCurrentWord);
		return true;
	    }
	}
	return false;
    }
    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
	if (keyCode == KeyEvent.KEYCODE_VOLUME_DOWN || keyCode == KeyEvent.KEYCODE_VOLUME_UP) {
	    return true;
	}
	if (keyCode == KeyEvent.KEYCODE_BACK) {
	    new AlertDialog.Builder(BTAndroidWebViewSelectionActivity.this)
		.setTitle("Exit?")
		.setMessage("Please confirm that you want to exit.")
		.setPositiveButton("OK", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
			    try {
				OutputStreamWriter file = new OutputStreamWriter(openFileOutput("saved_word.txt", Context.MODE_PRIVATE), "UTF8");
				file.write(mCurrentWord);
				file.close();
			    } catch (Exception e) {
				Log.e("bhj", "save mCurrentWord failed", e);
			    }
			    BTAndroidWebViewSelectionActivity.this.finish();
			}
		    })
		.setNegativeButton("Cancel", null)
		.create().show();
	    return true;
	}
	return false;
    }	
	
    View.OnClickListener mLookUpListener = new OnClickListener() {
	    public void onClick(View v) {
		boolean changed = mListView.setActiveDict(mDict);
		mWebView.lookUpWord(mEdit.getText().toString());
		if (changed) {
		    onNewWordLoaded(mCurrentWord);
		}
	    }
	};

    String mCurrentDefiner = "";
    String mCurrentMatcher = "";
    String mCurrentFreqWord = "";

    View.OnClickListener mDefinedListener = new OnClickListener() {
	    public void onClick(View v) {
		String word = mEdit.getText().toString();
		ArrayList<String> defs = mUsageDict.getExplanation(word);
		if (defs == null || defs.isEmpty()) {
		    return;
		}
		String words = defs.get(0);
		String[] splits = words.split(":");
		if (splits.length > 0) {
		    mListView.setActiveDict(new StringArrayDict(splits));
		    mWebView.lookUpWord(splits[0]);
		}
	    }
	};

    View.OnClickListener mListListener = new OnClickListener() {
	    public void onClick(View button) {
		PopupMenu popup = new PopupMenu(BTAndroidWebViewSelectionActivity.this, button);
		popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());

		popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
			public boolean onMenuItemClick(MenuItem item) {
			    if (item.getItemId() == R.id.freq_menu) {
				mListView.setActiveDict(mFreqDict);
			    }
			    return true;
			}
		    });

		popup.show();
	    }
	};
}
