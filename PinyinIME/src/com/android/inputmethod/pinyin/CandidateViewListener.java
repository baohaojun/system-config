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

/**
 * Interface to notify the input method when the user clicks a candidate or
 * makes a direction-gesture on candidate view.
 */
public interface CandidateViewListener {
    public void onClickChoice(int choiceId);

    public void onToLeftGesture();

    public void onToRightGesture();

    public void onToTopGesture();

    public void onToBottomGesture();
}
