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

import android.graphics.drawable.Drawable;

/**
 * Class for soft keys which defined in the keyboard xml file. A soft key can be
 * a basic key or a toggling key.
 * 
 * @see com.android.inputmethod.pinyin.SoftKeyToggle
 */
public class SoftKey {
    protected static final int KEYMASK_REPEAT = 0x10000000;
    protected static final int KEYMASK_BALLOON = 0x20000000;

    /**
     * For a finger touch device, after user presses a key, there will be some
     * consequent moving events because of the changing in touching pressure. If
     * the moving distance in x is within this threshold, the moving events will
     * be ignored.
     */
    public static final int MAX_MOVE_TOLERANCE_X = 0;

    /**
     * For a finger touch device, after user presses a key, there will be some
     * consequent moving events because of the changing in touching pressure. If
     * the moving distance in y is within this threshold, the moving events will
     * be ignored.
     */
    public static final int MAX_MOVE_TOLERANCE_Y = 0;

    /**
     * Used to indicate the type and attributes of this key. the lowest 8 bits
     * should be reserved for SoftkeyToggle.
     */
    protected int mKeyMask;

    protected SoftKeyType mKeyType;

    protected Drawable mKeyIcon;

    protected Drawable mKeyIconPopup;

    protected String mKeyLabel;

    protected int mKeyCode;

    /**
     * If this value is not 0, this key can be used to popup a sub soft keyboard
     * when user presses it for some time.
     */
    public int mPopupSkbId;

    public float mLeftF;
    public float mRightF;
    public float mTopF;
    public float mBottomF;
    public int mLeft;
    public int mRight;
    public int mTop;
    public int mBottom;

    public void setKeyType(SoftKeyType keyType, Drawable keyIcon,
            Drawable keyIconPopup) {
        mKeyType = keyType;
        mKeyIcon = keyIcon;
        mKeyIconPopup = keyIconPopup;
    }

    // The caller guarantees that all parameters are in [0, 1]
    public void setKeyDimensions(float left, float top, float right,
            float bottom) {
        mLeftF = left;
        mTopF = top;
        mRightF = right;
        mBottomF = bottom;
    }

    public void setKeyAttribute(int keyCode, String label, boolean repeat,
            boolean balloon) {
        mKeyCode = keyCode;
        mKeyLabel = label;

        if (repeat) {
            mKeyMask |= KEYMASK_REPEAT;
        } else {
            mKeyMask &= (~KEYMASK_REPEAT);
        }

        if (balloon) {
            mKeyMask |= KEYMASK_BALLOON;
        } else {
            mKeyMask &= (~KEYMASK_BALLOON);
        }
    }

    public void setPopupSkbId(int popupSkbId) {
        mPopupSkbId = popupSkbId;
    }

    // Call after setKeyDimensions(). The caller guarantees that the
    // keyboard with and height are valid.
    public void setSkbCoreSize(int skbWidth, int skbHeight) {
        mLeft = (int) (mLeftF * skbWidth);
        mRight = (int) (mRightF * skbWidth);
        mTop = (int) (mTopF * skbHeight);
        mBottom = (int) (mBottomF * skbHeight);
    }

    public Drawable getKeyIcon() {
        return mKeyIcon;
    }

    public Drawable getKeyIconPopup() {
        if (null != mKeyIconPopup) {
            return mKeyIconPopup;
        }
        return mKeyIcon;
    }

    public int getKeyCode() {
        return mKeyCode;
    }

    public String getKeyLabel() {
        return mKeyLabel;
    }

    public void changeCase(boolean upperCase) {
        if (null != mKeyLabel) {
            if (upperCase)
                mKeyLabel = mKeyLabel.toUpperCase();
            else
                mKeyLabel = mKeyLabel.toLowerCase();
        }
    }

    public Drawable getKeyBg() {
        return mKeyType.mKeyBg;
    }

    public Drawable getKeyHlBg() {
        return mKeyType.mKeyHlBg;
    }

    public int getColor() {
        return mKeyType.mColor;
    }

    public int getColorHl() {
        return mKeyType.mColorHl;
    }

    public int getColorBalloon() {
        return mKeyType.mColorBalloon;
    }

    public boolean isKeyCodeKey() {
        if (mKeyCode > 0) return true;
        return false;
    }

    public boolean isUserDefKey() {
        if (mKeyCode < 0) return true;
        return false;
    }

    public boolean isUniStrKey() {
        if (null != mKeyLabel && mKeyCode == 0) return true;
        return false;
    }

    public boolean needBalloon() {
        return (mKeyMask & KEYMASK_BALLOON) != 0;
    }

    public boolean repeatable() {
        return (mKeyMask & KEYMASK_REPEAT) != 0;
    }

    public int getPopupResId() {
        return mPopupSkbId;
    }

    public int width() {
        return mRight - mLeft;
    }

    public int height() {
        return mBottom - mTop;
    }

    public boolean moveWithinKey(int x, int y) {
        if (mLeft - MAX_MOVE_TOLERANCE_X <= x
                && mTop - MAX_MOVE_TOLERANCE_Y <= y
                && mRight + MAX_MOVE_TOLERANCE_X > x
                && mBottom + MAX_MOVE_TOLERANCE_Y > y) {
            return true;
        }
        return false;
    }

    @Override
    public String toString() {
        String str = "\n";
        str += "  keyCode: " + String.valueOf(mKeyCode) + "\n";
        str += "  keyMask: " + String.valueOf(mKeyMask) + "\n";
        str += "  keyLabel: " + (mKeyLabel == null ? "null" : mKeyLabel) + "\n";
        str += "  popupResId: " + String.valueOf(mPopupSkbId) + "\n";
        str += "  Position: " + String.valueOf(mLeftF) + ", "
                + String.valueOf(mTopF) + ", " + String.valueOf(mRightF) + ", "
                + String.valueOf(mBottomF) + "\n";
        return str;
    }
}
