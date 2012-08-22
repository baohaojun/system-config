package com.baohaojun.crossdict;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;

public class CrossDictionaryActivity extends Activity {
    /** Called when the activity is first created. */
    WebView mWebView;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

	mWebView = (WebView) findViewById(R.id.webview);
	mWebView.getSettings().setJavaScriptEnabled(true);
        mWebView.loadData("<a href='x'>Hello World! - 1</a>", "text/html", null);
    }
}
