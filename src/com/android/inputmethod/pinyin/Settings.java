/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.inputmethod.pinyin;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;

/**
 * Class used to maintain settings.
 */
public class Settings {
    private static final String ANDPY_CONFS_KEYSOUND_KEY = "Sound";
    private static final String ANDPY_CONFS_VIBRATE_KEY = "Vibrate";
    private static final String ANDPY_CONFS_PREDICTION_KEY = "Prediction";
    
    private static boolean mKeySound;
    private static boolean mVibrate;
    private static boolean mPrediction;
    
    private static Settings mInstance = null;

    private static int mRefCount = 0;

    private static SharedPreferences mSharedPref = null;

    protected Settings(SharedPreferences pref) {
        mSharedPref = pref;
        initConfs();
    }

    public static Settings getInstance(SharedPreferences pref) {
        if (mInstance == null) {
            mInstance = new Settings(pref);
        }
        assert (pref == mSharedPref);
        mRefCount++;
        return mInstance;
    }

    public static void writeBack() {
        Editor editor = mSharedPref.edit();
        editor.putBoolean(ANDPY_CONFS_VIBRATE_KEY, mVibrate);
        editor.putBoolean(ANDPY_CONFS_KEYSOUND_KEY, mKeySound);
        editor.putBoolean(ANDPY_CONFS_PREDICTION_KEY, mPrediction);
        editor.commit();
    }

    public static void releaseInstance() {
        mRefCount--;
        if (mRefCount == 0) {
            mInstance = null;
        }
    }

    private void initConfs() {
        mKeySound = mSharedPref.getBoolean(ANDPY_CONFS_KEYSOUND_KEY, true);
        mVibrate = mSharedPref.getBoolean(ANDPY_CONFS_VIBRATE_KEY, false);
        mPrediction = mSharedPref.getBoolean(ANDPY_CONFS_PREDICTION_KEY, true);
    }

    public static boolean getKeySound() {
        return mKeySound;
    }

    public static void setKeySound(boolean v) {
        if (mKeySound == v) return;
        mKeySound = v;
    }

    public static boolean getVibrate() {
        return mVibrate;
    }

    public static void setVibrate(boolean v) {
        if (mVibrate == v) return;
        mVibrate = v;
    }

    public static boolean getPrediction() {
        return mPrediction;
    }

    public static void setPrediction(boolean v) {
        if (mPrediction == v) return;
        mPrediction = v;
    }
}
