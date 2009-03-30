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
import android.content.res.Resources;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Paint.FontMetricsInt;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup.LayoutParams;

/**
 * View used to show composing string (The Pinyin string for the unselected
 * syllables and the Chinese string for the selected syllables.)
 */
public class ComposingView extends View {
    /**
     * <p>
     * There are three statuses for the composing view.
     * </p>
     * 
     * <p>
     * {@link #SHOW_PINYIN} is used to show the current Pinyin string without
     * highlighted effect. When user inputs Pinyin characters one by one, the
     * Pinyin string will be shown in this mode.
     * </p>
     * <p>
     * {@link #SHOW_STRING_LOWERCASE} is used to show the Pinyin string in
     * lowercase with highlighted effect. When user presses UP key and there is
     * no fixed Chinese characters, composing view will switch from
     * {@link #SHOW_PINYIN} to this mode, and in this mode, user can press
     * confirm key to input the lower-case string, so that user can input
     * English letter in Chinese mode.
     * </p>
     * <p>
     * {@link #EDIT_PINYIN} is used to edit the Pinyin string (shown with
     * highlighted effect). When current status is {@link #SHOW_PINYIN} and user
     * presses UP key, if there are fixed Characters, the input method will
     * switch to {@link #EDIT_PINYIN} thus user can modify some characters in
     * the middle of the Pinyin string. If the current status is
     * {@link #SHOW_STRING_LOWERCASE} and user presses LEFT and RIGHT key, it
     * will also switch to {@link #EDIT_PINYIN}.
     * </p>
     * <p>
     * Whenever user presses down key, the status switches to
     * {@link #SHOW_PINYIN}.
     * </p>
     * <p>
     * When composing view's status is {@link #SHOW_PINYIN}, the IME's status is
     * {@link PinyinIME.ImeState#STATE_INPUT}, otherwise, the IME's status
     * should be {@link PinyinIME.ImeState#STATE_COMPOSING}.
     * </p>
     */
    public enum ComposingStatus {
        SHOW_PINYIN, SHOW_STRING_LOWERCASE, EDIT_PINYIN,
    }

    private static final int LEFT_RIGHT_MARGIN = 5;

    /**
     * Used to draw composing string. When drawing the active and idle part of
     * the spelling(Pinyin) string, the color may be changed.
     */
    private Paint mPaint;

    /**
     * Drawable used to draw highlight effect.
     */
    private Drawable mHlDrawable;

    /**
     * Drawable used to draw cursor for editing mode.
     */
    private Drawable mCursor;

    /**
     * Used to estimate dimensions to show the string .
     */
    private FontMetricsInt mFmi;

    private int mStrColor;
    private int mStrColorHl;
    private int mStrColorIdle;

    private int mFontSize;

    private ComposingStatus mComposingStatus;

    PinyinIME.DecodingInfo mDecInfo;

    public ComposingView(Context context, AttributeSet attrs) {
        super(context, attrs);

        Resources r = context.getResources();
        mHlDrawable = r.getDrawable(R.drawable.composing_hl_bg);
        mCursor = r.getDrawable(R.drawable.composing_area_cursor);

        mStrColor = r.getColor(R.color.composing_color);
        mStrColorHl = r.getColor(R.color.composing_color_hl);
        mStrColorIdle = r.getColor(R.color.composing_color_idle);

        mFontSize = r.getDimensionPixelSize(R.dimen.composing_height);

        mPaint = new Paint();
        mPaint.setColor(mStrColor);
        mPaint.setAntiAlias(true);
        mPaint.setTextSize(mFontSize);

        mFmi = mPaint.getFontMetricsInt();
    }

    public void reset() {
        mComposingStatus = ComposingStatus.SHOW_PINYIN;
    }

    /**
     * Set the composing string to show. If the IME status is
     * {@link PinyinIME.ImeState#STATE_INPUT}, the composing view's status will
     * be set to {@link ComposingStatus#SHOW_PINYIN}, otherwise the composing
     * view will set its status to {@link ComposingStatus#SHOW_STRING_LOWERCASE}
     * or {@link ComposingStatus#EDIT_PINYIN} automatically.
     */
    public void setDecodingInfo(PinyinIME.DecodingInfo decInfo,
            PinyinIME.ImeState imeStatus) {
        mDecInfo = decInfo;

        if (PinyinIME.ImeState.STATE_INPUT == imeStatus) {
            mComposingStatus = ComposingStatus.SHOW_PINYIN;
            mDecInfo.moveCursorToEdge(false);
        } else {
            if (decInfo.getFixedLen() != 0
                    || ComposingStatus.EDIT_PINYIN == mComposingStatus) {
                mComposingStatus = ComposingStatus.EDIT_PINYIN;
            } else {
                mComposingStatus = ComposingStatus.SHOW_STRING_LOWERCASE;
            }
            mDecInfo.moveCursor(0);
        }

        measure(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        requestLayout();
        invalidate();
    }

    public boolean moveCursor(int keyCode) {
        if (keyCode != KeyEvent.KEYCODE_DPAD_LEFT
                && keyCode != KeyEvent.KEYCODE_DPAD_RIGHT) return false;

        if (ComposingStatus.EDIT_PINYIN == mComposingStatus) {
            int offset = 0;
            if (keyCode == KeyEvent.KEYCODE_DPAD_LEFT)
                offset = -1;
            else if (keyCode == KeyEvent.KEYCODE_DPAD_RIGHT) offset = 1;
            mDecInfo.moveCursor(offset);
        } else if (ComposingStatus.SHOW_STRING_LOWERCASE == mComposingStatus) {
            if (keyCode == KeyEvent.KEYCODE_DPAD_LEFT
                    || keyCode == KeyEvent.KEYCODE_DPAD_RIGHT) {
                mComposingStatus = ComposingStatus.EDIT_PINYIN;

                measure(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
                requestLayout();
            }

        }
        invalidate();
        return true;
    }

    public ComposingStatus getComposingStatus() {
        return mComposingStatus;
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        float width;
        int height;
        height = mFmi.bottom - mFmi.top + mPaddingTop + mPaddingBottom;

        if (null == mDecInfo) {
            width = 0;
        } else {
            width = mPaddingLeft + mPaddingRight + LEFT_RIGHT_MARGIN * 2;

            String str;
            if (ComposingStatus.SHOW_STRING_LOWERCASE == mComposingStatus) {
                str = mDecInfo.getOrigianlSplStr().toString();
            } else {
                str = mDecInfo.getComposingStrForDisplay();
            }
            width += mPaint.measureText(str, 0, str.length());
        }
        setMeasuredDimension((int) (width + 0.5f), height);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        if (ComposingStatus.EDIT_PINYIN == mComposingStatus
                || ComposingStatus.SHOW_PINYIN == mComposingStatus) {
            drawForPinyin(canvas);
            return;
        }

        float x, y;
        x = mPaddingLeft + LEFT_RIGHT_MARGIN;
        y = -mFmi.top + mPaddingTop;

        mPaint.setColor(mStrColorHl);
        mHlDrawable.setBounds(mPaddingLeft, mPaddingTop, getWidth()
                - mPaddingRight, getHeight() - mPaddingBottom);
        mHlDrawable.draw(canvas);

        String splStr = mDecInfo.getOrigianlSplStr().toString();
        canvas.drawText(splStr, 0, splStr.length(), x, y, mPaint);
    }

    private void drawCursor(Canvas canvas, float x) {
        mCursor.setBounds((int) x, mPaddingTop, (int) x
                + mCursor.getIntrinsicWidth(), getHeight() - mPaddingBottom);
        mCursor.draw(canvas);
    }

    private void drawForPinyin(Canvas canvas) {
        float x, y;
        x = mPaddingLeft + LEFT_RIGHT_MARGIN;
        y = -mFmi.top + mPaddingTop;

        mPaint.setColor(mStrColor);

        int cursorPos = mDecInfo.getCursorPosInCmpsDisplay();
        int cmpsPos = cursorPos;
        String cmpsStr = mDecInfo.getComposingStrForDisplay();
        int activeCmpsLen = mDecInfo.getActiveCmpsDisplayLen();
        if (cursorPos > activeCmpsLen) cmpsPos = activeCmpsLen;
        canvas.drawText(cmpsStr, 0, cmpsPos, x, y, mPaint);
        x += mPaint.measureText(cmpsStr, 0, cmpsPos);
        if (cursorPos <= activeCmpsLen) {
            if (ComposingStatus.EDIT_PINYIN == mComposingStatus) {
                drawCursor(canvas, x);
            }
            canvas.drawText(cmpsStr, cmpsPos, activeCmpsLen, x, y, mPaint);
        }

        x += mPaint.measureText(cmpsStr, cmpsPos, activeCmpsLen);

        if (cmpsStr.length() > activeCmpsLen) {
            mPaint.setColor(mStrColorIdle);
            int oriPos = activeCmpsLen;
            if (cursorPos > activeCmpsLen) {
                if (cursorPos > cmpsStr.length()) cursorPos = cmpsStr.length();
                canvas.drawText(cmpsStr, oriPos, cursorPos, x, y, mPaint);
                x += mPaint.measureText(cmpsStr, oriPos, cursorPos);

                if (ComposingStatus.EDIT_PINYIN == mComposingStatus) {
                    drawCursor(canvas, x);
                }

                oriPos = cursorPos;
            }
            canvas.drawText(cmpsStr, oriPos, cmpsStr.length(), x, y, mPaint);
        }
    }
}
