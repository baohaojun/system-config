package com.baohaojun.crossdict;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.AssetManager;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.util.Log;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.TextView;
import android.widget.Toast;
import com.googlecode.toolkits.stardict.FreqDict;
import com.googlecode.toolkits.stardict.MatcherDict;
import com.googlecode.toolkits.stardict.StarDict;
import com.googlecode.toolkits.stardict.StarDictInterface;
import com.googlecode.toolkits.stardict.StringArrayDict;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;

public class CrossDictActivity extends Activity {
    /** Called when the activity is first created. */
    private EditText mEdit;
    private SlowListView mListView;

    private Button mLookUpButton;
    private Button mDefinedButton;
    private Button mMatchingButton;
    private Button mListButton;
    private BTWebView mWebView;

    private StarDict mDict;
    private StarDict mUsageDict;
    private FreqDict mFreqDict;

    private StarDictInterface mActiveDict = mDict;
    private StarDictInterface mDefinedWithDict;
    private StarDictInterface mMatchingDict;

    private File mWorkingDir;

    private static final String[] dictFileBaseNames = {"ahd", "frequency", "usage", "words"};
    private static final String[] dictFileExtNames = {".dz", ".idx", ".ii"};
    private static ArrayList<String> mDictFiles = new ArrayList<String>();

    static {
        for (String base : dictFileBaseNames) {
            for (String ext : dictFileExtNames) {
                mDictFiles.add(base + ext);
            }
        }

        String[] moreFiles = {"android.selection.js", "jquery.js", "rangy-core.js", "rangy-serializer.js", "dict.css"};
        for (String file : moreFiles) {
            mDictFiles.add(file);
        }
    }

    private String checkDictFiles() {
        if (!mWorkingDir.exists()) {
            return mWorkingDir.toString();
        }

        for (String file : mDictFiles) {
            if (! new File(mWorkingDir, file).exists()) {
                return file;
            }
        }
        return null;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (savedInstanceState != null) {
            mCurrentWord = savedInstanceState.getString("mCurrentWord", "");
            mCurrentDefiner = savedInstanceState.getString("mCurrentDefiner", "");
            mCurrentDefinee = savedInstanceState.getString("mCurrentDefinee", "");
            mCurrentMatcher = savedInstanceState.getString("mCurrentMatcher", "");
            mCurrentMatchee = savedInstanceState.getString("mCurrentMatchee", "");
            mCurrentFreqWord = savedInstanceState.getString("mCurrentFreqWord", "");
        } else {
            try {
                InputStreamReader reader = new InputStreamReader(openFileInput("saved_word.txt"), "UTF8");
                char[] buffer = new char[1024];
                int n = reader.read(buffer);
                if (n >= 0) {
                    String str = new String(buffer, 0, n);
                    String[] strArr = str.split("\n");

                    switch (strArr.length) { // fall through all cases
                    case 6:
                        mCurrentFreqWord = strArr[5];
                    case 5:
                        mCurrentMatchee = strArr[4];
                    case 4:
                        mCurrentMatcher = strArr[3];
                    case 3:
                        mCurrentDefinee = strArr[2];
                    case 2:
                        mCurrentDefiner = strArr[1];
                    case 1:
                        mCurrentWord = strArr[0];
                    default:
                        ;
                    }
                }
            } catch (Exception e) {
                Log.e("bhj", "read mCurrentWord failed", e);
            }
        }

        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                             WindowManager.LayoutParams.FLAG_FULLSCREEN);

        mWorkingDir = Environment.getExternalStoragePublicDirectory("crossdict/ahd");

        setContentView(R.layout.main);
        mListView = (SlowListView) findViewById(R.id.nearby_dict_entries);
        mEdit = (EditText) findViewById(R.id.enter_dict_entry);
        // mEdit.setOnFocusChangeListener(new View.OnFocusChangeListener() {
        //         public void onFocusChange(View v, boolean hasFocus) {
        //             EditText mEdit = (EditText)v;
        //             if (!hasFocus) {
        //                 mEdit.selectAll();
        //             }
        //         }
        //     });

        mEdit.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    Log.e("bhj", String.format("%s:%d: onClick", "CrossDictActivity.java", 162));
                    mEdit.selectAll();
                }
            });


        mEdit.setOnEditorActionListener(new TextView.OnEditorActionListener() {
                @Override
                public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
                    if (actionId == EditorInfo.IME_ACTION_SEARCH) {
                        InputMethodManager imm = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
                        imm.hideSoftInputFromWindow(mEdit.getWindowToken(), 0);
                        String word = getEditText();
                        lookUpWord(word);
                        return true;
                    }
                    return false;
                }
            });
        mWebView = (BTWebView) findViewById(R.id.webView);

        mWebView.setActivity(this);


        mLookUpButton = (Button) findViewById(R.id.look_up_button);
        mLookUpButton.setOnClickListener(mLookUpListener);

        mDefinedButton = (Button) findViewById(R.id.defined_with_button);
        mDefinedButton.setOnClickListener(mDefinedListener);

        mMatchingButton = (Button) findViewById(R.id.matching_button);
        mMatchingButton.setOnClickListener(mMatchingListerner);

        mListButton = (Button) findViewById(R.id.lists_button);
        mListButton.setOnClickListener(mListListener);

        mListView.setOnItemClickListener(mItemClickListener);


        if (checkDictFiles() != null) {
            new AlertDialog.Builder(CrossDictActivity.this)
                .setTitle("Dictionary data not found...")
                .setMessage("Please download and install CrossDictGcide " +
                            "from Google Play by clicking the 'OK' button.\n\n" +
                            "Or you can click the 'Help' button for more infomation.")
                .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int whichButton) {

                            String url = "https://play.google.com/store/apps/details?id=com.baohaojun.crossdictgcide";
                            Intent i = new Intent(Intent.ACTION_VIEW);
                            i.setData(Uri.parse(url));
                            startActivity(i);
                            CrossDictActivity.this.finish();
                        }
                    })
                .setNegativeButton("Help", new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int whichButton) {
                            String url = "http://baohaojun.github.com/crossdict-gcide-help.html";
                            Intent i = new Intent(Intent.ACTION_VIEW);
                            i.setData(Uri.parse(url));
                            startActivity(i);
                            CrossDictActivity.this.finish();
                        }
                    })
                .create().show();
        } else {
            continueLoading();
        }
    }

    @Override
    protected void onNewIntent(Intent intent) {

        onLookUpRequest(intent);
    }

    private boolean onLookUpRequest(Intent intent) {
        String subject = intent
            .getStringExtra("android.intent.extra.SUBJECT");
        String text = intent.getStringExtra("android.intent.extra.TEXT");

        if (text != null) {
            mWebView.lookUpWord(text);

            try {
                OutputStreamWriter out =
                    new OutputStreamWriter(
                        new FileOutputStream(
                            new File(mWorkingDir, "new-words.txt"),
                            true));

                out.write(text + "\n");
                out.close();
            } catch (Exception e) {
                Log.e("bhj", " save new word failed", e);
            }

            return true;
        }
        return false;
    }

    void continueLoading() {
        if (new File(mWorkingDir, "derive.dz").exists()) {
            mDict = new StarDict(new File(mWorkingDir, "ahd").toString(), new File(mWorkingDir, "derive").toString());
        } else {
            mDict = new StarDict(new File(mWorkingDir, "ahd").toString());
        }
        mUsageDict = new StarDict(new File(mWorkingDir, "usage").toString());
        mFreqDict = new FreqDict(new File(mWorkingDir, "frequency").toString());

        mWebView.setBaseUrlWithDir(mWorkingDir.toString());
        mWebView.setDict(mDict);

        mListView.createAndSetAdapter(CrossDictActivity.this, mDict);

        Intent intent = getIntent();

        if (!onLookUpRequest(intent)) {
            mWebView.lookUpWord(mCurrentWord);
        }
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
    private void setEditText(String word) {
        while (word.startsWith(" ") || word.startsWith("\t")) {
            word = word.substring(1);
        }
        mEdit.setText(word);
    }

    private String getEditText() {
        String word = mEdit.getText().toString();
        while (word.startsWith(" ") || word.startsWith("\t")) {
            word = word.substring(1);
        }
        return word;
    }

    public void onNewWordLoaded(String word) {
        mCurrentWord = word;

        if (mActiveDict == mFreqDict) {
            mCurrentFreqWord = word;
        } else if (mActiveDict == mDefinedWithDict) {
            mCurrentDefinee = word;
        } else if (mActiveDict == mMatchingDict) {
            mCurrentMatchee = word;
        }

        setEditText(word);
        mListView.scrollToWord(word);
        if (mPoping) {
            return;
        }

        if (lastWord().compareTo(word) == 0) {
            return;
        }
        mWordHistory[mHistoryHead] = word;
        mHistoryHead = (mHistoryHead + 1) % mWordHistory.length;
        if (mHistoryHead == mHistoryTail) {
            mHistoryTail = (mHistoryTail + 1) % mWordHistory.length;
        }
    }

    private String lastWord() {

        int idx = mHistoryHead - 1;
        if (idx < 0) {
            idx += mWordHistory.length;
        }
        if (mWordHistory[idx] != null) {
            return mWordHistory[idx];
        }
        return "";
    }

    boolean mPoping;

    /**
     * @return if history is poped, then true; else false.
     *
     */
    boolean popHistory() {
        if (mHistoryTail == mHistoryHead) {
            return false;
        }

        mHistoryHead--;
        if (mHistoryHead < 0) {
            mHistoryHead = mWordHistory.length - 1;
        }

        if (mWordHistory[mHistoryHead] != null) {
            if (mCurrentWord != null && ! mCurrentWord.equals(mWordHistory[mHistoryHead])) {
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
        return false;
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("mCurrentWord", mCurrentWord);
        outState.putString("mCurrentDefiner", mCurrentDefiner);
        outState.putString("mCurrentDefinee", mCurrentDefinee);
        outState.putString("mCurrentMatcher", mCurrentMatcher);
        outState.putString("mCurrentMatchee", mCurrentMatchee);
        outState.putString("mCurrentFreqWord", mCurrentFreqWord);
    }

    @Override
    protected void onPause() {
        super.onPause();
        try {
            OutputStreamWriter file = new OutputStreamWriter(openFileOutput("saved_word.txt", Context.MODE_PRIVATE), "UTF8");
            file.write(mCurrentWord);
            file.write("\n");
            file.write(mCurrentDefiner);
            file.write("\n");
            file.write(mCurrentDefinee);
            file.write("\n");
            file.write(mCurrentMatcher);
            file.write("\n");
            file.write(mCurrentMatchee);
            file.write("\n");
            file.write(mCurrentFreqWord);
            file.close();
        } catch (Exception e) {
            Log.e("bhj", "save mCurrentWord failed", e);
        }
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
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_VOLUME_DOWN || keyCode == KeyEvent.KEYCODE_VOLUME_UP) {
            return true;
        }
        return super.onKeyUp(keyCode, event);
    }

    public void lookUpWord(String word) {
        mActiveDict = mDict;
        boolean changed = mListView.setActiveDict(mDict);
        mWebView.lookUpWord(word);
        if (changed) {
            onNewWordLoaded(mCurrentWord);
        }

    }
    View.OnClickListener mLookUpListener = new OnClickListener() {
            public void onClick(View v) {
                String word = getEditText();
                lookUpWord(word);
            }
        };

    String mCurrentDefiner = "";
    String mCurrentDefinee = "";
    String mCurrentMatcher = "";
    String mCurrentMatchee = "";
    String mCurrentFreqWord = "a";

    public void lookUpDefiner(String word) {
        mCurrentDefiner = word;
        ArrayList<String> defs = mUsageDict.getExplanation(word);
        if (defs == null || defs.isEmpty()) {
            return;
        }
        int idx = mUsageDict.getWordIdx(word);
        if (word.compareToIgnoreCase(mUsageDict.getWord(idx)) != 0) {
            new AlertDialog.Builder(CrossDictActivity.this)
                .setTitle("Not a definer!")
                .setMessage(String.format("%s is not a defining word, this maybe because it is too common", word))
                .setPositiveButton("OK", null)
                .create().show();
            return;
        }
        String words = defs.get(0);
        String[] splits = words.split(":");
        if (splits.length > 0) {
            mActiveDict = mDefinedWithDict = new StringArrayDict(splits);
            mListView.setActiveDict(mDefinedWithDict);
            mWebView.lookUpWord(splits[0]);
        }
    }

    public void lookUpMatching(String word) {
        mCurrentMatcher = word;
        MatcherDict.useWordsFile(new File(mWorkingDir, "words").toString());
        mActiveDict = mMatchingDict = new MatcherDict(word);
        mListView.setActiveDict(mActiveDict);
        mWebView.lookUpWord(mActiveDict.getWord(0));
    }

    View.OnClickListener mDefinedListener = new OnClickListener() {
            public void onClick(View v) {
                String word = getEditText();
                lookUpDefiner(word);
            }
        };

    View.OnClickListener mMatchingListerner = new OnClickListener() {
            public void onClick(View v) {
                String word = getEditText();
                lookUpMatching(word);
            }
        };

    PopupMenu.OnMenuItemClickListener mMenuItemClickListener = new PopupMenu.OnMenuItemClickListener() {
            public boolean onMenuItemClick(MenuItem item) {
                if (item.getItemId() == R.id.freq_menu) {
                    mActiveDict = mFreqDict;
                    mListView.setActiveDict(mActiveDict);
                    mWebView.lookUpWord(mCurrentFreqWord);
                } else if (item.getItemId() == R.id.defining_menu) {
                    if (mDefinedWithDict == null) {
                        lookUpDefiner(mCurrentDefiner);
                    } else {
                        mActiveDict = mDefinedWithDict;
                        mListView.setActiveDict(mActiveDict);
                        mWebView.lookUpWord(mCurrentDefinee);
                    }
                } else if (item.getItemId() == R.id.matching_menu) {
                    if (mMatchingDict == null) {
                        lookUpMatching(mCurrentMatcher);
                    } else {
                        mActiveDict = mMatchingDict;
                        mListView.setActiveDict(mActiveDict);
                        mWebView.lookUpWord(mCurrentMatchee);
                    }
                } else if (item.getItemId() == R.id.history_menu) {
                    mActiveDict = mDict;
                    mListView.setActiveDict(new StringArrayDict(stringReduce(mWordHistory)));
                    mWebView.lookUpWord(mWordHistory[0]);
                } else if (item.getItemId() == R.id.start_service) {
                    startService(new Intent(CrossDictActivity.this, ClipMonService.class));
                } else if (item.getItemId() == R.id.stop_service) {
                    stopService(new Intent(CrossDictActivity.this, ClipMonService.class));
                } else if (item.getItemId() == R.id.donate_menu) {
                    String url = "http://baohaojun.github.com/donate";
                    Intent i = new Intent(Intent.ACTION_VIEW);
                    i.setData(Uri.parse(url));
                    startActivity(i);
                }
                return true;
            }
        };
    View.OnClickListener mListListener = new OnClickListener() {
            public void onClick(View button) {
                PopupMenu popup = new PopupMenu(CrossDictActivity.this, button);
                popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());

                popup.setOnMenuItemClickListener(mMenuItemClickListener);

                Menu menu = popup.getMenu();

                if (mCurrentMatcher != null) {
                    menu.findItem(R.id.matching_menu).setTitle(String.format("Matching %s", mCurrentMatcher));
                }

                if (mCurrentDefiner != null) {
                    menu.findItem(R.id.defining_menu).setTitle(String.format("Defined by %s", mCurrentDefiner));
                }

                if (mCurrentFreqWord != null) {
                    menu.findItem(R.id.freq_menu).setTitle(String.format("Frequency of %s", mCurrentFreqWord));
                }
                popup.show();
            }
        };

    static String[] stringReduce(String[] strArr) {
        HashMap<String, Integer> set = new HashMap<String, Integer>();
        ArrayList<String> list = new ArrayList<String>();

        for (String s : strArr) {
            if (s != null && set.containsKey(s)) {
                continue;
            }

            set.put(s, 1);
            list.add(s);
        }
        return list.toArray(new String[0]);
    }
}
