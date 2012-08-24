package com.baohaojun.crossdict;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebView;
import android.widget.Toast;

public class CrossDictionaryActivity extends Activity {
    /** Called when the activity is first created. */
    CrossDictWebView mWebView;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

	mWebView = (CrossDictWebView) findViewById(R.id.webview);
	mWebView.getSettings().setJavaScriptEnabled(true);
	
        mWebView.loadDataWithBaseURL("file:///sdcard/crossdict/", "<a href='x'>Hello World! - 1</a> <img src='hello.jpg'/>", "text/html", null, null);
	mWebView.setOnLongClickListener(new View.OnLongClickListener() {
		@Override
		public boolean onLongClick(View v) {
		    CrossDictWebView cdv = (CrossDictWebView)v;
		    cdv.doShit();
		    return true;
		}
	    });
    }
}
