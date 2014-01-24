package com.googlecode.toolkits.stardict;
import android.util.Log;
import android.widget.BaseAdapter;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MatcherDict implements StarDictInterface {
    static String wordsFileName;
    static String wordsStr;
    static int totalEntries;

    private BaseAdapter mAdapter = null;
    private static boolean mDebug;
    private static void debug(String format, Object... args) {
        if (mDebug) {
            new Formatter(System.err).format(format, args);
        }
    }

    public void setAdapter(BaseAdapter adapter) {
        mAdapter = adapter;
    }

    static public void useWordsFile(String fileName) {
        if (wordsFileName != null && wordsFileName.compareTo(fileName) == 0) {
            return;
        }
        wordsFileName = fileName;
        try {
            InputStreamReader reader = new InputStreamReader(new FileInputStream(wordsFileName + ".dz"), "UTF8");
            totalEntries = (int) (new File(wordsFileName + ".ii").length()/4);
            int length = (int) new File(wordsFileName + ".dz").length();
            char[] buffer = new char[length];

            int n = 0;
            do {
                int ret = reader.read(buffer, n, buffer.length - n);
                if (ret <= 0) {
                    break;
                }
                n += ret;
            } while (true);

            wordsStr = new String(buffer, 0, n);
        } catch (Exception e) {
            debug("error building words list: %s", e.toString());
            wordsStr = null;
            wordsFileName = null;
        }
    }

    String mMatcherStr;
    ArrayList<String> mMatchees;
    Pattern mMatcherPattern;
    Matcher mMatcher;
    int mTotalEntries;

    int mStart = 0;
    boolean mExhausted = false;

    public MatcherDict(String matcher) {
        mTotalEntries = totalEntries;
        mMatcherStr = matcher;
        mMatchees = new ArrayList<String>(100);
        mMatcherPattern = Pattern.compile(mMatcherStr, Pattern.CASE_INSENSITIVE|Pattern.MULTILINE);
        mMatcher = mMatcherPattern.matcher(wordsStr);

        for (int i = 0; i < 100; i++) {
            findNextMatch();
        }
    }

    private void findNextMatch() {
        if (mExhausted) {
            return;
        }

        if (mMatcher.find(mStart)) {
            int start = mMatcher.start(0);
            int end = mMatcher.end(0);
            for (; start >= 0; start--) {
                if (wordsStr.charAt(start) == '\n') {
                    break;
                }
            }
            start++;

            for (; end < wordsStr.length(); end++) {
                if (wordsStr.charAt(end) == '\n') {
                    break;
                }
            }
            mStart = end + 1;

            // this is not needed, see substring
            // end--;
            mMatchees.add(wordsStr.substring(start, end));
        } else {
            mExhausted = true;
            mTotalEntries = mMatchees.size();
            if (mAdapter != null) {
                mAdapter.notifyDataSetChanged();
            }
        }
    }

    @Override
    public int getWordIdx(String word) {
        for (int i = 0; i < mMatchees.size(); i++) {
            if (mMatchees.get(i).compareTo(word) == 0) {
                return i;
            }
        }
        return 0;
    }

    @Override
    public String getWord(int idx) {
        if (idx < mMatchees.size()) {
            return mMatchees.get(idx);
        }

        while (! mExhausted && mMatchees.size() <= idx) {
            findNextMatch();
        }
        if (idx >= mMatchees.size()) {
            return "";
        }
        return mMatchees.get(idx);
    }

    @Override
    public int getTotalNumOfEntries() {
        return mTotalEntries;
    }

    public static void main(String[] args) {
        mDebug = true;
        useWordsFile("words");
        MatcherDict dict = new MatcherDict("hello");
        for (String word : dict.mMatchees) {
            System.out.println("hello word: " + word);
        }
    }

}
