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

import com.android.inputmethod.pinyin.IPinyinDecoderService;

import java.io.FileDescriptor;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Vector;

import android.app.Service;
import android.content.Intent;
import android.content.res.AssetFileDescriptor;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

/**
 * This class is used to separate the input method kernel in an individual
 * service so that both IME and IME-syncer can use it.
 */
public class PinyinDecoderService extends Service {
    native static boolean nativeImOpenDecoder(byte fn_sys_dict[],
            byte fn_usr_dict[]);

    native static boolean nativeImOpenDecoderFd(FileDescriptor fd,
            long startOffset, long length, byte fn_usr_dict[]);

    native static void nativeImSetMaxLens(int maxSpsLen, int maxHzsLen);

    native static boolean nativeImCloseDecoder();

    native static int nativeImSearch(byte pyBuf[], int pyLen);

    native static int nativeImDelSearch(int pos, boolean is_pos_in_splid,
            boolean clear_fixed_this_step);

    native static void nativeImResetSearch();

    native static int nativeImAddLetter(byte ch);

    native static String nativeImGetPyStr(boolean decoded);

    native static int nativeImGetPyStrLen(boolean decoded);

    native static int[] nativeImGetSplStart();

    native static String nativeImGetChoice(int choiceId);

    native static int nativeImChoose(int choiceId);

    native static int nativeImCancelLastChoice();

    native static int nativeImGetFixedLen();

    native static boolean nativeImCancelInput();

    native static boolean nativeImFlushCache();

    native static int nativeImGetPredictsNum(String fixedStr);

    native static String nativeImGetPredictItem(int predictNo);

    // Sync related
    native static String nativeSyncUserDict(byte[] user_dict, String tomerge);

    native static boolean nativeSyncBegin(byte[] user_dict);

    native static boolean nativeSyncFinish();

    native static String nativeSyncGetLemmas();

    native static int nativeSyncPutLemmas(String tomerge);

    native static int nativeSyncGetLastCount();

    native static int nativeSyncGetTotalCount();

    native static boolean nativeSyncClearLastGot();

    native static int nativeSyncGetCapacity();

    private final static int MAX_PATH_FILE_LENGTH = 100;
    private static boolean inited = false;

    private String mUsr_dict_file;

    static {
        try {
            System.loadLibrary("jni_pinyinime");
        } catch (UnsatisfiedLinkError ule) {
            Log.e("PinyinDecoderService",
                    "WARNING: Could not load jni_pinyinime natives");
        }
    }

    // Get file name of the specified dictionary
    private boolean getUsrDictFileName(byte usr_dict[]) {
        if (null == usr_dict) {
            return false;
        }

        for (int i = 0; i < mUsr_dict_file.length(); i++)
            usr_dict[i] = (byte) mUsr_dict_file.charAt(i);
        usr_dict[mUsr_dict_file.length()] = 0;

        return true;
    }

    private void initPinyinEngine() {
        byte usr_dict[];
        usr_dict = new byte[MAX_PATH_FILE_LENGTH];

        // Here is how we open a built-in dictionary for access through
        // a file descriptor...
        AssetFileDescriptor afd = getResources().openRawResourceFd(
                R.raw.dict_pinyin);
        if (Environment.getInstance().needDebug()) {
            Log
                    .i("foo", "Dict: start=" + afd.getStartOffset()
                            + ", length=" + afd.getLength() + ", fd="
                            + afd.getParcelFileDescriptor());
        }
        if (getUsrDictFileName(usr_dict)) {
            inited = nativeImOpenDecoderFd(afd.getFileDescriptor(), afd
                    .getStartOffset(), afd.getLength(), usr_dict);
        }
        try {
            afd.close();
        } catch (IOException e) {
        }
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mUsr_dict_file = getFileStreamPath("usr_dict.dat").getPath();
        // This is a hack to make sure our "files" directory has been
        // created.
        try {
            openFileOutput("dummy", 0).close();
        } catch (FileNotFoundException e) {
        } catch (IOException e) {
        }

        initPinyinEngine();
    }

    @Override
    public void onDestroy() {
        nativeImCloseDecoder();
        inited = false;
        super.onDestroy();
    }

    private final IPinyinDecoderService.Stub mBinder = new IPinyinDecoderService.Stub() {
        public int getInt() {
            return 12345;
        }

        public void setMaxLens(int maxSpsLen, int maxHzsLen) {
            nativeImSetMaxLens(maxSpsLen, maxHzsLen);
        }

        public int imSearch(byte[] pyBuf, int pyLen) {
            return nativeImSearch(pyBuf, pyLen);
        }

        public int imDelSearch(int pos, boolean is_pos_in_splid,
                boolean clear_fixed_this_step) {
            return nativeImDelSearch(pos, is_pos_in_splid,
                    clear_fixed_this_step);
        }

        public void imResetSearch() {
            nativeImResetSearch();
        }

        public int imAddLetter(byte ch) {
            return nativeImAddLetter(ch);
        }

        public String imGetPyStr(boolean decoded) {
            return nativeImGetPyStr(decoded);
        }

        public int imGetPyStrLen(boolean decoded) {
            return nativeImGetPyStrLen(decoded);
        }

        public int[] imGetSplStart() {
            return nativeImGetSplStart();
        }

        public String imGetChoice(int choiceId) {
            return nativeImGetChoice(choiceId);
        }

        public String imGetChoices(int choicesNum) {
            String retStr = null;
            for (int i = 0; i < choicesNum; i++) {
                if (null == retStr)
                    retStr = nativeImGetChoice(i);
                else
                    retStr += " " + nativeImGetChoice(i);
            }
            return retStr;
        }

        public List<String> imGetChoiceList(int choicesStart, int choicesNum,
                int sentFixedLen) {
            Vector<String> choiceList = new Vector<String>();
            for (int i = choicesStart; i < choicesStart + choicesNum; i++) {
                String retStr = nativeImGetChoice(i);
                if (0 == i) retStr = retStr.substring(sentFixedLen);
                choiceList.add(retStr);
            }
            return choiceList;
        }

        public int imChoose(int choiceId) {
            return nativeImChoose(choiceId);
        }

        public int imCancelLastChoice() {
            return nativeImCancelLastChoice();
        }

        public int imGetFixedLen() {
            return nativeImGetFixedLen();
        }

        public boolean imCancelInput() {
            return nativeImCancelInput();
        }

        public void imFlushCache() {
            nativeImFlushCache();
        }

        public int imGetPredictsNum(String fixedStr) {
            return nativeImGetPredictsNum(fixedStr);
        }

        public String imGetPredictItem(int predictNo) {
            return nativeImGetPredictItem(predictNo);
        }

        public List<String> imGetPredictList(int predictsStart, int predictsNum) {
            Vector<String> predictList = new Vector<String>();
            for (int i = predictsStart; i < predictsStart + predictsNum; i++) {
                predictList.add(nativeImGetPredictItem(i));
            }
            return predictList;
        }

        public String syncUserDict(String tomerge) {
            byte usr_dict[];
            usr_dict = new byte[MAX_PATH_FILE_LENGTH];

            if (getUsrDictFileName(usr_dict)) {
                return nativeSyncUserDict(usr_dict, tomerge);
            }
            return null;
        }

        public boolean syncBegin() {
            byte usr_dict[];
            usr_dict = new byte[MAX_PATH_FILE_LENGTH];

            if (getUsrDictFileName(usr_dict)) {
                return nativeSyncBegin(usr_dict);
            }
            return false;
        }

        public void syncFinish() {
            nativeSyncFinish();
        }

        public int syncPutLemmas(String tomerge) {
            return nativeSyncPutLemmas(tomerge);
        }

        public String syncGetLemmas() {
            return nativeSyncGetLemmas();
        }

        public int syncGetLastCount() {
            return nativeSyncGetLastCount();
        }

        public int syncGetTotalCount() {
            return nativeSyncGetTotalCount();
        }

        public void syncClearLastGot() {
            nativeSyncClearLastGot();
        }

        public int imSyncGetCapacity() {
            return nativeSyncGetCapacity();
        }
    };

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }
}
