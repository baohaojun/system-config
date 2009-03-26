/* //com/andriod/inputmethod/pinyin/IPinyinDecoderService.aidl
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

interface IPinyinDecoderService {
    int getInt();
    void setMaxLens(int maxSpsLen, int maxHzsLen);
    int imSearch(in byte[] pyBuf, int pyLen);
    int imDelSearch(int pos, boolean is_pos_in_splid, boolean clear_fixed_this_step);
    void imResetSearch();
    int imAddLetter(byte ch);
    String imGetPyStr(boolean decoded);
    int imGetPyStrLen(boolean decoded);
    int[] imGetSplStart();
    String imGetChoice(int choiceId);
    String imGetChoices(int choicesNum);
    List<String> imGetChoiceList(int choicesStart, int choicesNum, int sentFixedLen);
    int imChoose(int choiceId);
    int imCancelLastChoice();
    int imGetFixedLen();
    boolean imCancelInput();
    void imFlushCache();
    int imGetPredictsNum(in String fixedStr);
    List<String> imGetPredictList(int predictsStart, int predictsNum);
    String imGetPredictItem(int predictNo);

    String syncUserDict(in String tomerge);
    boolean syncBegin();
    void syncFinish();
    int syncPutLemmas(in String tomerge);
    String syncGetLemmas();
    int syncGetLastCount();
    int syncGetTotalCount();
    void syncClearLastGot();
    int imSyncGetCapacity();
}
