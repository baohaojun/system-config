package com.googlecode.toolkits.stardict;
import java.util.ArrayList;

public class FreqDict implements StarDictInterface {
    StarDict mDict;
    public FreqDict(String dictname) {
	mDict = new StarDict(dictname);
    }
    @Override
    public int getTotalNumOfEntries() {
	return mDict.getTotalNumOfEntries();
    }
    
    @Override
    public String getWord(int idx) {
	ArrayList<String> defs = mDict.getExplanation(mDict.getWord(idx));
	if (defs == null || defs.size() != 1) {
	    return "no such word";
	}
	String def = defs.get(0);

	String[] strArr = def.split(":");
	if (strArr.length != 3) {
	    return "no such word";
	}

	return strArr[2];
    }

    @Override
    public int getWordIdx(String word) {
	ArrayList<String> defs = mDict.getExplanation(word);
	if (defs == null || defs.size() != 1) {
	    return 0;
	}
	String def = defs.get(0);
	String[] strArr = def.split(":");
	if (strArr.length != 3) {
	    return 0;
	}
	try {
	    return new Integer(strArr[0]);
	} catch (Exception e) {
	    return 0;
	}
    }
}
    
