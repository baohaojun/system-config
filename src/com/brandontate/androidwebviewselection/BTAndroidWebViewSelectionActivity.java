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

public class BTAndroidWebViewSelectionActivity extends Activity {
    /** Called when the activity is first created. */
    private EditText mEdit; 
    private ListView mListView;

    private Button mLookUpButton;
    private Button mDefinedButton;
    private Button mMatchingButton;
    private BTWebView mWebView;
    private ArrayAdapter mArrayAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, 
			     WindowManager.LayoutParams.FLAG_FULLSCREEN);

        setContentView(R.layout.main);

        mListView = (ListView) findViewById(R.id.nearby_dict_entries);
	mArrayAdapter = new ArrayAdapter<String>(this, R.layout.simple_list_item_1);
        mListView.setAdapter(mArrayAdapter);
	mEdit = (EditText) findViewById(R.id.enter_dict_entry);

	mLookUpButton = (Button) findViewById(R.id.look_up_button);
	mLookUpButton.setOnClickListener(mLookUpListner);
	mWebView = (BTWebView) findViewById(R.id.webView);
	mWebView.setActivity(this);
	mWebView.lookUpWord("eclair");

	mListView.setOnItemClickListener(mItemClickListener);
    }

    public void setNearByWords(ArrayList<String> nearByWords) {
	mArrayAdapter.clear();
	mArrayAdapter.addAll(nearByWords);
    }

    AdapterView.OnItemClickListener mItemClickListener = new AdapterView.OnItemClickListener() {
	    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
		TextView tv = (TextView)view;
		mWebView.lookUpWord(tv.getText().toString());
	    }
	};

    View.OnClickListener mLookUpListner = new OnClickListener() {
	    public void onClick(View v) {
		mWebView.lookUpWord(mEdit.getText().toString());
	    }
	};

}
