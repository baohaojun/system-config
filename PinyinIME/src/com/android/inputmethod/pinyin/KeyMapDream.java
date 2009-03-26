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

import android.view.KeyEvent;

/**
 * Class used to map the symbols on Dream's hardware keyboard to corresponding
 * Chinese full-width symbols.
 */
public class KeyMapDream {
    // Number of shift bits to store full-width symbols
    private static final int SHIFT_FWCH = 8;
    private static final int[] mKeyMap = {
            KeyEvent.KEYCODE_UNKNOWN,
            KeyEvent.KEYCODE_SOFT_LEFT,
            KeyEvent.KEYCODE_SOFT_RIGHT,
            KeyEvent.KEYCODE_HOME,
            KeyEvent.KEYCODE_BACK,
            KeyEvent.KEYCODE_CALL,
            KeyEvent.KEYCODE_ENDCALL,
            KeyEvent.KEYCODE_0 | ('\uff09' << SHIFT_FWCH), // )
            KeyEvent.KEYCODE_1 | ('\uff01' << SHIFT_FWCH), // !
            KeyEvent.KEYCODE_2 | ('\uff20' << SHIFT_FWCH), // @
            KeyEvent.KEYCODE_3 | ('\uff03' << SHIFT_FWCH), // #
            KeyEvent.KEYCODE_4 | ('\uffe5' << SHIFT_FWCH), // $ - fullwidth Yuan
            KeyEvent.KEYCODE_5 | ('\uff05' << SHIFT_FWCH), // %
            KeyEvent.KEYCODE_6 | ('\u2026' << SHIFT_FWCH), // ^ - Apostrophe
            KeyEvent.KEYCODE_7 | ('\uff06' << SHIFT_FWCH), // &
            KeyEvent.KEYCODE_8 | ('\uff0a' << SHIFT_FWCH), // *
            KeyEvent.KEYCODE_9 | ('\uff08' << SHIFT_FWCH), // (
            KeyEvent.KEYCODE_STAR,
            KeyEvent.KEYCODE_POUND,
            KeyEvent.KEYCODE_DPAD_UP,
            KeyEvent.KEYCODE_DPAD_DOWN,
            KeyEvent.KEYCODE_DPAD_LEFT,
            KeyEvent.KEYCODE_DPAD_RIGHT,
            KeyEvent.KEYCODE_DPAD_CENTER,
            KeyEvent.KEYCODE_VOLUME_UP,
            KeyEvent.KEYCODE_VOLUME_DOWN,
            KeyEvent.KEYCODE_POWER,
            KeyEvent.KEYCODE_CAMERA,
            KeyEvent.KEYCODE_CLEAR,
            KeyEvent.KEYCODE_A,
            KeyEvent.KEYCODE_B | ('\uff3d' << SHIFT_FWCH), // ]
            KeyEvent.KEYCODE_C | ('\u00a9' << SHIFT_FWCH), // copyright
            KeyEvent.KEYCODE_D | ('\u3001' << SHIFT_FWCH), // \\
            KeyEvent.KEYCODE_E | ('_' << SHIFT_FWCH), // _
            KeyEvent.KEYCODE_F | ('\uff5b' << SHIFT_FWCH), // {
            KeyEvent.KEYCODE_G | ('\uff5d' << SHIFT_FWCH), // }
            KeyEvent.KEYCODE_H | ('\uff1a' << SHIFT_FWCH), // :
            KeyEvent.KEYCODE_I | ('\uff0d' << SHIFT_FWCH), // -
            KeyEvent.KEYCODE_J | ('\uff1b' << SHIFT_FWCH), // ;
            KeyEvent.KEYCODE_K | ('\u201c' << SHIFT_FWCH), // "
            KeyEvent.KEYCODE_L | ('\u2019' << SHIFT_FWCH), // '
            KeyEvent.KEYCODE_M | ('\u300b' << SHIFT_FWCH), // > - French quotes
            KeyEvent.KEYCODE_N | ('\u300a' << SHIFT_FWCH), // < - French quotes
            KeyEvent.KEYCODE_O | ('\uff0b' << SHIFT_FWCH), // +
            KeyEvent.KEYCODE_P | ('\uff1d' << SHIFT_FWCH), // =
            KeyEvent.KEYCODE_Q | ('\t' << SHIFT_FWCH), // \t
            KeyEvent.KEYCODE_R | ('\u00ae' << SHIFT_FWCH), // trademark
            KeyEvent.KEYCODE_S | ('\uff5c' << SHIFT_FWCH), // |
            KeyEvent.KEYCODE_T | ('\u20ac' << SHIFT_FWCH), //
            KeyEvent.KEYCODE_U | ('\u00d7' << SHIFT_FWCH), // multiplier
            KeyEvent.KEYCODE_V | ('\uff3b' << SHIFT_FWCH), // [
            KeyEvent.KEYCODE_W | ('\uff40' << SHIFT_FWCH), // `
            KeyEvent.KEYCODE_X, KeyEvent.KEYCODE_Y | ('\u00f7' << SHIFT_FWCH),
            KeyEvent.KEYCODE_Z,
            KeyEvent.KEYCODE_COMMA | ('\uff1f' << SHIFT_FWCH),
            KeyEvent.KEYCODE_PERIOD | ('\uff0f' << SHIFT_FWCH),
            KeyEvent.KEYCODE_ALT_LEFT, KeyEvent.KEYCODE_ALT_RIGHT,
            KeyEvent.KEYCODE_SHIFT_LEFT, KeyEvent.KEYCODE_SHIFT_RIGHT,
            KeyEvent.KEYCODE_TAB, KeyEvent.KEYCODE_SPACE, KeyEvent.KEYCODE_SYM,
            KeyEvent.KEYCODE_EXPLORER, KeyEvent.KEYCODE_ENVELOPE,
            KeyEvent.KEYCODE_ENTER, KeyEvent.KEYCODE_DEL,
            KeyEvent.KEYCODE_GRAVE, KeyEvent.KEYCODE_MINUS,
            KeyEvent.KEYCODE_EQUALS, KeyEvent.KEYCODE_LEFT_BRACKET,
            KeyEvent.KEYCODE_RIGHT_BRACKET, KeyEvent.KEYCODE_BACKSLASH,
            KeyEvent.KEYCODE_SEMICOLON, KeyEvent.KEYCODE_APOSTROPHE,
            KeyEvent.KEYCODE_SLASH,
            KeyEvent.KEYCODE_AT | ('\uff5e' << SHIFT_FWCH),
            KeyEvent.KEYCODE_NUM, KeyEvent.KEYCODE_HEADSETHOOK,
            KeyEvent.KEYCODE_FOCUS, KeyEvent.KEYCODE_PLUS,
            KeyEvent.KEYCODE_MENU, KeyEvent.KEYCODE_NOTIFICATION,
            KeyEvent.KEYCODE_SEARCH,};

    static public char getChineseLabel(int keyCode) {
        if (keyCode <= 0 || keyCode >= KeyEvent.getMaxKeyCode()) return 0;
        assert ((mKeyMap[keyCode] & 0x000000ff) == keyCode);
        return (char) (mKeyMap[keyCode] >> SHIFT_FWCH);
    }
}
