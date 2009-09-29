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

import android.content.Context;
import android.media.AudioManager;

/**
 * Class used to manage related sound resources.
 */
public class SoundManager {
    private static SoundManager mInstance = null;
    private Context mContext;
    private AudioManager mAudioManager;
    // Align sound effect volume on music volume
    private final float FX_VOLUME = -1.0f;
    private boolean mSilentMode;

    private SoundManager(Context context) {
        mContext = context;
        updateRingerMode();
    }

    public void updateRingerMode() {
        if (mAudioManager == null) {
            mAudioManager = (AudioManager) mContext
                    .getSystemService(Context.AUDIO_SERVICE);
        }
        mSilentMode = (mAudioManager.getRingerMode() != AudioManager.RINGER_MODE_NORMAL);
    }

    public static SoundManager getInstance(Context context) {
        if (null == mInstance) {
            if (null != context) {
                mInstance = new SoundManager(context);
            }
        }
        return mInstance;
    }

    public void playKeyDown() {
        if (mAudioManager == null) {
            updateRingerMode();
        }
        if (!mSilentMode) {
            int sound = AudioManager.FX_KEYPRESS_STANDARD;
            mAudioManager.playSoundEffect(sound, FX_VOLUME);
        }
    }
}
