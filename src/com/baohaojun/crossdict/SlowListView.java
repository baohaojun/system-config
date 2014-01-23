package com.baohaojun.crossdict;
import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AbsListView.OnScrollListener;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.TextView;
import com.googlecode.toolkits.stardict.MatcherDict;
import com.googlecode.toolkits.stardict.StarDict;
import com.googlecode.toolkits.stardict.StarDictInterface;

public class SlowListView extends ListView implements ListView.OnScrollListener {

    private boolean mBusy = false;

    /**
     * Will not bind views while the list is scrolling
     *
     */

    public SlowListView(Context context) {
        super(context);
    }

    public SlowListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public SlowListView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    private class SlowAdapter extends BaseAdapter {
        private LayoutInflater mInflater;

        public SlowAdapter(Context context) {
            mInflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        }

        /**
         * The number of items in the list is determined by the number of speeches
         * in our array.
         *
         * @see android.widget.ListAdapter#getCount()
         */
        public int getCount() {
            return mActiveDict.getTotalNumOfEntries();
        }

        /**
         * Since the data comes from an array, just returning the index is
         * sufficent to get at the data. If we were using a more complex data
         * structure, we would return whatever object represents one row in the
         * list.
         *
         * @see android.widget.ListAdapter#getItem(int)
         */
        public Object getItem(int position) {
            return position;
        }

        /**
         * Use the array index as a unique id.
         *
         * @see android.widget.ListAdapter#getItemId(int)
         */
        public long getItemId(int position) {
            return position;
        }

        /**
         * Make a view to hold each row.
         *
         * @see android.widget.ListAdapter#getView(int, android.view.View,
         *      android.view.ViewGroup)
         */
        public View getView(int position, View convertView, ViewGroup parent) {
            TextView text;

            if (convertView == null) {
                text = (TextView)mInflater.inflate(R.layout.simple_list_item_activated_1, parent, false);
            } else {
                text = (TextView)convertView;
            }

            if (!mBusy) {
                text.setText(mActiveDict.getWord(position));
                // Null tag means the view has the correct data
                text.setTag(null);
            } else {
                text.setText("");
                // Non-null tag means the view still needs to load it's data
                text.setTag(this);
            }

            return text;
        }
    }

    public void scrollToWord(String word) {

        int idx = mActiveDict.getWordIdx(word);
        int first = getFirstVisiblePosition();
        int count = getChildCount();

        Log.e("bhj", String.format("idx is %d\n", idx));

        if (idx < first || idx >= first + count) {
            Log.e("bhj", String.format("scroll to %d\n", idx));
            setItemChecked(idx, true);
            setSelection(idx);
        } else {
            setItemChecked(idx, true);
        }
    }

    SlowAdapter mAdapter = null;
    public void createAndSetAdapter(Context context, StarDictInterface dict) {
        mAdapter = new SlowAdapter(context);

        mActiveDict = mTheDict = dict;
        MatcherDict d;
        if (dict instanceof MatcherDict) {
            d = (MatcherDict) dict;
            d.setAdapter(mAdapter);
        }

        setAdapter(mAdapter);
        setOnScrollListener(this);
        setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    }


    public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount,
            int totalItemCount) {
    }

    private StarDictInterface mTheDict;
    private StarDictInterface mActiveDict;

    /**
     * Change the active dictionary the list is using.
     *
     * @return true if the dictionary is changed, false if not
     * changed.
     */
    public boolean setActiveDict(StarDictInterface dict) {
        boolean ret = false;

        if (mActiveDict != dict) {
            ret = true;
            mActiveDict = dict;
            if (mActiveDict instanceof MatcherDict) {
                MatcherDict d = (MatcherDict) mActiveDict;
                d.setAdapter(mAdapter);
            }
            requestLayout();
            setSelection(0);
        }
        return ret;
    }

    public void onScrollStateChanged(AbsListView view, int scrollState) {
        switch (scrollState) {
        case OnScrollListener.SCROLL_STATE_IDLE:
            mBusy = false;

            int first = view.getFirstVisiblePosition();
            int count = view.getChildCount();
            for (int i=0; i<count; i++) {
                TextView t = (TextView)view.getChildAt(i);
                if (t.getTag() != null) {
                    t.setText(mActiveDict.getWord(first + i));
                    t.setTag(null);
                }
            }

            break;
        case OnScrollListener.SCROLL_STATE_TOUCH_SCROLL:
            mBusy = true;
            break;
        case OnScrollListener.SCROLL_STATE_FLING:
            mBusy = true;
            break;
        }
    }
}
