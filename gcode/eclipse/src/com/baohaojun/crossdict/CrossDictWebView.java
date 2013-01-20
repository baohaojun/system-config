package com.baohaojun.crossdict;

import android.content.Context;
import android.util.AttributeSet;
import android.webkit.WebView;
import android.widget.Toast;

public class CrossDictWebView extends WebView {
    public CrossDictWebView(Context context, AttributeSet attrs, int defStyle, boolean privateBrowsing) {
	super(context, attrs, defStyle, privateBrowsing);
    }

    public CrossDictWebView(Context context, AttributeSet attrs, int defStyle) {
	super(context, attrs, defStyle);
    }

    public CrossDictWebView(Context context, AttributeSet attrs) {
	super(context, attrs);
    }

    public CrossDictWebView(Context context) {
	super(context);
    }

    void doShit() {
	Toast.makeText(getContext(), "Hello World", 5).show();
    }

}
