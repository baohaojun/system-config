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
import android.inputmethodservice.InputMethodService;
import android.os.Handler;
import android.os.SystemClock;
import android.os.SystemProperties;
import android.util.AttributeSet;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.PopupWindow;
import android.widget.RelativeLayout;
import android.widget.ViewFlipper;

/**
 * The top container to host soft keyboard view(s).
 */
public class SkbContainer extends RelativeLayout implements OnTouchListener {
    /**
     * For finger touch, user tends to press the bottom part of the target key,
     * or he/she even presses the area out of it, so it is necessary to make a
     * simple bias correction. If the input method runs on emulator, no bias
     * correction will be used.
     */
    private static final int Y_BIAS_CORRECTION = -10;

    /**
     * Used to skip these move events whose position is too close to the
     * previous touch events.
     */
    private static final int MOVE_TOLERANCE = 6;

    /**
     * If this member is true, PopupWindow is used to show on-key highlight
     * effect.
     */
    private static boolean POPUPWINDOW_FOR_PRESSED_UI = false;

    /**
     * The current soft keyboard layout.
     * 
     * @see com.android.inputmethod.pinyin.InputModeSwitcher for detailed layout
     *      definitions.
     */
    private int mSkbLayout = 0;

    /**
     * The input method service.
     */
    private InputMethodService mService;

    /**
     * Input mode switcher used to switch between different modes like Chinese,
     * English, etc.
     */
    private InputModeSwitcher mInputModeSwitcher;

    /**
     * The gesture detector.
     */
    private GestureDetector mGestureDetector;

    private Environment mEnvironment;

    private ViewFlipper mSkbFlipper;

    /**
     * The popup balloon hint for key press/release.
     */
    private BalloonHint mBalloonPopup;

    /**
     * The on-key balloon hint for key press/release.
     */
    private BalloonHint mBalloonOnKey = null;

    /** The major sub soft keyboard. */
    private SoftKeyboardView mMajorView;

    /**
     * The last parameter when function {@link #toggleCandidateMode(boolean)}
     * was called.
     */
    private boolean mLastCandidatesShowing;

    /** Used to indicate whether a popup soft keyboard is shown. */
    private boolean mPopupSkbShow = false;

    /**
     * Used to indicate whether a popup soft keyboard is just shown, and waits
     * for the touch event to release. After the release, the popup window can
     * response to touch events.
     **/
    private boolean mPopupSkbNoResponse = false;

    /** Popup sub keyboard. */
    private PopupWindow mPopupSkb;

    /** The view of the popup sub soft keyboard. */
    private SoftKeyboardView mPopupSkbView;

    private int mPopupX;

    private int mPopupY;

    /**
     * When user presses a key, a timer is started, when it times out, it is
     * necessary to detect whether user still holds the key.
     */
    private volatile boolean mWaitForTouchUp = false;

    /**
     * When user drags on the soft keyboard and the distance is enough, this
     * drag will be recognized as a gesture and a gesture-based action will be
     * taken, in this situation, ignore the consequent events.
     */
    private volatile boolean mDiscardEvent = false;

    /**
     * For finger touch, user tends to press the bottom part of the target key,
     * or he/she even presses the area out of it, so it is necessary to make a
     * simple bias correction in Y.
     */
    private int mYBiasCorrection = 0;

    /**
     * The x coordination of the last touch event.
     */
    private int mXLast;

    /**
     * The y coordination of the last touch event.
     */
    private int mYLast;

    /**
     * The soft keyboard view.
     */
    private SoftKeyboardView mSkv;

    /**
     * The position of the soft keyboard view in the container.
     */
    private int mSkvPosInContainer[] = new int[2];

    /**
     * The key pressed by user.
     */
    private SoftKey mSoftKeyDown = null;

    /**
     * Used to timeout a press if user holds the key for a long time.
     */
    private LongPressTimer mLongPressTimer;

    /**
     * For temporary use.
     */
    private int mXyPosTmp[] = new int[2];

    public SkbContainer(Context context, AttributeSet attrs) {
        super(context, attrs);

        mEnvironment = Environment.getInstance();

        mLongPressTimer = new LongPressTimer(this);

        // If it runs on an emulator, no bias correction
        if ("1".equals(SystemProperties.get("ro.kernel.qemu"))) {
            mYBiasCorrection = 0;
        } else {
            mYBiasCorrection = Y_BIAS_CORRECTION;
        }
        mBalloonPopup = new BalloonHint(context, this, MeasureSpec.AT_MOST);
        if (POPUPWINDOW_FOR_PRESSED_UI) {
            mBalloonOnKey = new BalloonHint(context, this, MeasureSpec.AT_MOST);
        }

        mPopupSkb = new PopupWindow(mContext);
        mPopupSkb.setBackgroundDrawable(null);
        mPopupSkb.setClippingEnabled(false);
    }

    public void setService(InputMethodService service) {
        mService = service;
    }

    public void setInputModeSwitcher(InputModeSwitcher inputModeSwitcher) {
        mInputModeSwitcher = inputModeSwitcher;
    }

    public void setGestureDetector(GestureDetector gestureDetector) {
        mGestureDetector = gestureDetector;
    }

    public boolean isCurrentSkbSticky() {
        if (null == mMajorView) return true;
        SoftKeyboard skb = mMajorView.getSoftKeyboard();
        if (null != skb) {
            return skb.getStickyFlag();
        }
        return true;
    }

    public void toggleCandidateMode(boolean candidatesShowing) {
        if (null == mMajorView || !mInputModeSwitcher.isChineseText()
                || mLastCandidatesShowing == candidatesShowing) return;
        mLastCandidatesShowing = candidatesShowing;

        SoftKeyboard skb = mMajorView.getSoftKeyboard();
        if (null == skb) return;

        int state = mInputModeSwitcher.getTooggleStateForCnCand();
        if (!candidatesShowing) {
            skb.disableToggleState(state, false);
            skb.enableToggleStates(mInputModeSwitcher.getToggleStates());
        } else {
            skb.enableToggleState(state, false);
        }

        mMajorView.invalidate();
    }

    public void updateInputMode() {
        int skbLayout = mInputModeSwitcher.getSkbLayout();
        if (mSkbLayout != skbLayout) {
            mSkbLayout = skbLayout;
            updateSkbLayout();
        }

        mLastCandidatesShowing = false;

        if (null == mMajorView) return;

        SoftKeyboard skb = mMajorView.getSoftKeyboard();
        if (null == skb) return;
        skb.enableToggleStates(mInputModeSwitcher.getToggleStates());
        invalidate();
        return;
    }

    private void updateSkbLayout() {
        int screenWidth = mEnvironment.getScreenWidth();
        int keyHeight = mEnvironment.getKeyHeight();
        int skbHeight = mEnvironment.getSkbHeight();

        Resources r = mContext.getResources();
        if (null == mSkbFlipper) {
            mSkbFlipper = (ViewFlipper) findViewById(R.id.alpha_floatable);
        }
        mMajorView = (SoftKeyboardView) mSkbFlipper.getChildAt(0);

        SoftKeyboard majorSkb = null;
        SkbPool skbPool = SkbPool.getInstance();

        switch (mSkbLayout) {
        case R.xml.skb_qwerty:
            majorSkb = skbPool.getSoftKeyboard(R.xml.skb_qwerty,
                    R.xml.skb_qwerty, screenWidth, skbHeight, mContext);
            break;

        case R.xml.skb_sym1:
            majorSkb = skbPool.getSoftKeyboard(R.xml.skb_sym1, R.xml.skb_sym1,
                    screenWidth, skbHeight, mContext);
            break;

        case R.xml.skb_sym2:
            majorSkb = skbPool.getSoftKeyboard(R.xml.skb_sym2, R.xml.skb_sym2,
                    screenWidth, skbHeight, mContext);
            break;

        case R.xml.skb_smiley:
            majorSkb = skbPool.getSoftKeyboard(R.xml.skb_smiley,
                    R.xml.skb_smiley, screenWidth, skbHeight, mContext);
            break;

        case R.xml.skb_phone:
            majorSkb = skbPool.getSoftKeyboard(R.xml.skb_phone,
                    R.xml.skb_phone, screenWidth, skbHeight, mContext);
            break;
        default:
        }

        if (null == majorSkb || !mMajorView.setSoftKeyboard(majorSkb)) {
            return;
        }
        mMajorView.setBalloonHint(mBalloonOnKey, mBalloonPopup, false);
        mMajorView.invalidate();
    }

    private void responseKeyEvent(SoftKey sKey) {
        if (null == sKey) return;
        ((PinyinIME) mService).responseSoftKeyEvent(sKey);
        return;
    }

    private SoftKeyboardView inKeyboardView(int x, int y,
            int positionInParent[]) {
        if (mPopupSkbShow) {
            if (mPopupX <= x && mPopupX + mPopupSkb.getWidth() > x
                    && mPopupY <= y && mPopupY + mPopupSkb.getHeight() > y) {
                positionInParent[0] = mPopupX;
                positionInParent[1] = mPopupY;
                mPopupSkbView.setOffsetToSkbContainer(positionInParent);
                return mPopupSkbView;
            }
            return null;
        }

        return mMajorView;
    }

    private void popupSymbols() {
        int popupResId = mSoftKeyDown.getPopupResId();
        if (popupResId > 0) {
            int skbContainerWidth = getWidth();
            int skbContainerHeight = getHeight();
            // The paddings of the background are not included.
            int miniSkbWidth = (int) (skbContainerWidth * 0.8);
            int miniSkbHeight = (int) (skbContainerHeight * 0.23);

            SkbPool skbPool = SkbPool.getInstance();
            SoftKeyboard skb = skbPool.getSoftKeyboard(popupResId, popupResId,
                    miniSkbWidth, miniSkbHeight, mContext);
            if (null == skb) return;

            mPopupX = (skbContainerWidth - skb.getSkbTotalWidth()) / 2;
            mPopupY = (skbContainerHeight - skb.getSkbTotalHeight()) / 2;

            if (null == mPopupSkbView) {
                mPopupSkbView = new SoftKeyboardView(mContext, null);
                mPopupSkbView.onMeasure(LayoutParams.WRAP_CONTENT,
                        LayoutParams.WRAP_CONTENT);
            }
            mPopupSkbView.setOnTouchListener(this);
            mPopupSkbView.setSoftKeyboard(skb);
            mPopupSkbView.setBalloonHint(mBalloonOnKey, mBalloonPopup, true);

            mPopupSkb.setContentView(mPopupSkbView);
            mPopupSkb.setWidth(skb.getSkbCoreWidth()
                    + mPopupSkbView.getPaddingLeft()
                    + mPopupSkbView.getPaddingRight());
            mPopupSkb.setHeight(skb.getSkbCoreHeight()
                    + mPopupSkbView.getPaddingTop()
                    + mPopupSkbView.getPaddingBottom());

            getLocationInWindow(mXyPosTmp);
            mPopupSkb.showAtLocation(this, Gravity.NO_GRAVITY, mPopupX, mPopupY
                    + mXyPosTmp[1]);
            mPopupSkbShow = true;
            mPopupSkbNoResponse = true;
            // Invalidate itself to dim the current soft keyboards.
            dimSoftKeyboard(true);
            resetKeyPress(0);
        }
    }

    private void dimSoftKeyboard(boolean dimSkb) {
        mMajorView.dimSoftKeyboard(dimSkb);
    }

    private void dismissPopupSkb() {
        mPopupSkb.dismiss();
        mPopupSkbShow = false;
        dimSoftKeyboard(false);
        resetKeyPress(0);
    }

    private void resetKeyPress(long delay) {
        mLongPressTimer.removeTimer();

        if (null != mSkv) {
            mSkv.resetKeyPress(delay);
        }
    }

    public boolean handleBack(boolean realAction) {
        if (mPopupSkbShow) {
            if (!realAction) return true;

            dismissPopupSkb();
            mDiscardEvent = true;
            return true;
        }
        return false;
    }

    public void dismissPopups() {
        handleBack(true);
        resetKeyPress(0);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        Environment env = Environment.getInstance();
        int measuredWidth = env.getScreenWidth();
        int measuredHeight = getPaddingTop();
        measuredHeight += env.getSkbHeight();
        widthMeasureSpec = MeasureSpec.makeMeasureSpec(measuredWidth,
                MeasureSpec.EXACTLY);
        heightMeasureSpec = MeasureSpec.makeMeasureSpec(measuredHeight,
                MeasureSpec.EXACTLY);
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        super.onTouchEvent(event);

        if (mSkbFlipper.isFlipping()) {
            resetKeyPress(0);
            return true;
        }

        int x = (int) event.getX();
        int y = (int) event.getY();
        // Bias correction
        y = y + mYBiasCorrection;

        // Ignore short-distance movement event to get better performance.
        if (event.getAction() == MotionEvent.ACTION_MOVE) {
            if (Math.abs(x - mXLast) <= MOVE_TOLERANCE
                    && Math.abs(y - mYLast) <= MOVE_TOLERANCE) {
                return true;
            }
        }

        mXLast = x;
        mYLast = y;

        if (!mPopupSkbShow) {
            if (mGestureDetector.onTouchEvent(event)) {
                resetKeyPress(0);
                mDiscardEvent = true;
                return true;
            }
        }

        switch (event.getAction()) {
        case MotionEvent.ACTION_DOWN:
            resetKeyPress(0);

            mWaitForTouchUp = true;
            mDiscardEvent = false;

            mSkv = null;
            mSoftKeyDown = null;
            mSkv = inKeyboardView(x, y, mSkvPosInContainer);
            if (null != mSkv) {
                mSoftKeyDown = mSkv.onKeyPress(x - mSkvPosInContainer[0], y
                        - mSkvPosInContainer[1], mLongPressTimer, false);
            }
            break;

        case MotionEvent.ACTION_MOVE:
            if (x < 0 || x >= getWidth() || y < 0 || y >= getHeight()) {
                break;
            }
            if (mDiscardEvent) {
                resetKeyPress(0);
                break;
            }

            if (mPopupSkbShow && mPopupSkbNoResponse) {
                break;
            }

            SoftKeyboardView skv = inKeyboardView(x, y, mSkvPosInContainer);
            if (null != skv) {
                if (skv != mSkv) {
                    mSkv = skv;
                    mSoftKeyDown = mSkv.onKeyPress(x - mSkvPosInContainer[0], y
                            - mSkvPosInContainer[1], mLongPressTimer, true);
                } else if (null != skv) {
                    if (null != mSkv) {
                        mSoftKeyDown = mSkv.onKeyMove(
                                x - mSkvPosInContainer[0], y
                                        - mSkvPosInContainer[1]);
                        if (null == mSoftKeyDown) {
                            mDiscardEvent = true;
                        }
                    }
                }
            }
            break;

        case MotionEvent.ACTION_UP:
            if (mDiscardEvent) {
                resetKeyPress(0);
                break;
            }

            mWaitForTouchUp = false;

            // The view which got the {@link MotionEvent#ACTION_DOWN} event is
            // always used to handle this event.
            if (null != mSkv) {
                mSkv.onKeyRelease(x - mSkvPosInContainer[0], y
                        - mSkvPosInContainer[1]);
            }

            if (!mPopupSkbShow || !mPopupSkbNoResponse) {
                responseKeyEvent(mSoftKeyDown);
            }

            if (mSkv == mPopupSkbView && !mPopupSkbNoResponse) {
                dismissPopupSkb();
            }
            mPopupSkbNoResponse = false;
            break;

        case MotionEvent.ACTION_CANCEL:
            break;
        }

        if (null == mSkv) {
            return false;
        }

        return true;
    }

    // Function for interface OnTouchListener, it is used to handle touch events
    // which will be delivered to the popup soft keyboard view.
    public boolean onTouch(View v, MotionEvent event) {
        // Translate the event to fit to the container.
        MotionEvent newEv = MotionEvent.obtain(event.getDownTime(), event
                .getEventTime(), event.getAction(), event.getX() + mPopupX,
                event.getY() + mPopupY, event.getPressure(), event.getSize(),
                event.getMetaState(), event.getXPrecision(), event
                        .getYPrecision(), event.getDeviceId(), event
                        .getEdgeFlags());
        boolean ret = onTouchEvent(newEv);
        return ret;
    }

    class LongPressTimer extends Handler implements Runnable {
        /**
         * When user presses a key for a long time, the timeout interval to
         * generate first {@link #LONG_PRESS_KEYNUM1} key events.
         */
        public static final int LONG_PRESS_TIMEOUT1 = 500;

        /**
         * When user presses a key for a long time, after the first
         * {@link #LONG_PRESS_KEYNUM1} key events, this timeout interval will be
         * used.
         */
        private static final int LONG_PRESS_TIMEOUT2 = 100;

        /**
         * When user presses a key for a long time, after the first
         * {@link #LONG_PRESS_KEYNUM2} key events, this timeout interval will be
         * used.
         */
        private static final int LONG_PRESS_TIMEOUT3 = 100;

        /**
         * When user presses a key for a long time, after the first
         * {@link #LONG_PRESS_KEYNUM1} key events, timeout interval
         * {@link #LONG_PRESS_TIMEOUT2} will be used instead.
         */
        public static final int LONG_PRESS_KEYNUM1 = 1;

        /**
         * When user presses a key for a long time, after the first
         * {@link #LONG_PRESS_KEYNUM2} key events, timeout interval
         * {@link #LONG_PRESS_TIMEOUT3} will be used instead.
         */
        public static final int LONG_PRESS_KEYNUM2 = 3;

        SkbContainer mSkbContainer;

        private int mResponseTimes = 0;

        public LongPressTimer(SkbContainer skbContainer) {
            mSkbContainer = skbContainer;
        }

        public void startTimer() {
            postAtTime(this, SystemClock.uptimeMillis() + LONG_PRESS_TIMEOUT1);
            mResponseTimes = 0;
        }

        public boolean removeTimer() {
            removeCallbacks(this);
            return true;
        }

        public void run() {
            if (mWaitForTouchUp) {
                mResponseTimes++;
                if (mSoftKeyDown.repeatable()) {
                    if (mSoftKeyDown.isUserDefKey()) {
                        if (1 == mResponseTimes) {
                            if (mInputModeSwitcher
                                    .tryHandleLongPressSwitch(mSoftKeyDown.mKeyCode)) {
                                mDiscardEvent = true;
                                resetKeyPress(0);
                            }
                        }
                    } else {
                        responseKeyEvent(mSoftKeyDown);
                        long timeout;
                        if (mResponseTimes < LONG_PRESS_KEYNUM1) {
                            timeout = LONG_PRESS_TIMEOUT1;
                        } else if (mResponseTimes < LONG_PRESS_KEYNUM2) {
                            timeout = LONG_PRESS_TIMEOUT2;
                        } else {
                            timeout = LONG_PRESS_TIMEOUT3;
                        }
                        postAtTime(this, SystemClock.uptimeMillis() + timeout);
                    }
                } else {
                    if (1 == mResponseTimes) {
                        popupSymbols();
                    }
                }
            }
        }
    }
}
