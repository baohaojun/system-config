package com.googlecode.toolkits.stardict;

public class StringArrayDict implements StarDictInterface {
    String[] mEntries;
    public StringArrayDict(String[] entries) {
	mEntries = entries;
    }
    @Override
    public int getTotalNumOfEntries() {
	return mEntries.length;
    }

    @Override
    public int getWordIdx(String word) {
	for (int i = 0; i < mEntries.length; i++) {
	    if (word.compareTo(mEntries[i]) == 0) {
		return i;
	    }
	}
	return 0;
    }

    @Override
    public String getWord(int idx) {
	if (idx >= 0 && idx < mEntries.length) {
	    return mEntries[idx];
	} else {
	    return "";
	}
    }
}
