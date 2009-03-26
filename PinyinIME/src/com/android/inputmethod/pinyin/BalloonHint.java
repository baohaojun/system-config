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
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Paint.FontMetricsInt;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.view.Gravity;
import android.view.View;
import android.view.View.MeasureSpec;
import android.widget.PopupWindow;

/**
 * Subclass of PopupWindow used as the feedback when user presses on a soft key
 * or a candidate.
 */
public class BalloonHint extends PopupWindow {
    /**
     * Delayed time to show the balloon hint.
     */
    public static final int TIME_DELAY_SHOW = 0;

    /**
     * Delayed time to dismiss the balloon hint.
     */
    public static final int TIME_DELAY_DISMISS = 200;

    /**
     * The padding information of the balloon. Because PopupWindow's background
     * can not be changed unless it is dismissed and shown again, we set the
     * real background drawable to the content view, and make the PopupWindow's
     * background transparent. So actually this padding information is for the
     * content view.
     */
    private Rect mPaddingRect = new Rect();

    /**
     * The context used to create this balloon hint object.
     */
    private Context mContext;

    /**
     * Parent used to show the balloon window.
     */
    private View mParent;

    /**
     * The content view of the balloon.
     */
    BalloonView mBalloonView;

    /**
     * The measuring specification used to determine its size. Key-press
     * balloons and candidates balloons have different measuring specifications.
     */
    private int mMeasureSpecMode;

    /**
     * Used to indicate whether the balloon needs to be dismissed forcibly.
     */
    private boolean mForceDismiss;

    /**
     * Timer used to show/dismiss the balloon window with some time delay.
     */
    private BalloonTimer mBalloonTimer;

    private int mParentLocationInWindow[] = new int[2];

    public BalloonHint(Context context, View parent, int measureSpecMode) {
        super(context);
        mParent = parent;
        mMeasureSpecMode = measureSpecMode;

        setInputMethodMode(PopupWindow.INPUT_METHOD_NOT_NEEDED);
        setTouchable(false);
        setBackgroundDrawable(new ColorDrawable(0));

        mBalloonView = new BalloonView(context);
        mBalloonView.setClickable(false);
        setContentView(mBalloonView);

        mBalloonTimer = new BalloonTimer();
    }

    public Context getContext() {
        return mContext;
    }

    public Rect getPadding() {
        return mPaddingRect;
    }

    public void setBalloonBackground(Drawable drawable) {
        // We usually pick up a background from a soft keyboard template,
        // and the object may has been set to this balloon before.
        if (mBalloonView.getBackground() == drawable) return;
        mBalloonView.setBackgroundDrawable(drawable);

        if (null != drawable) {
            drawable.getPadding(mPaddingRect);
        } else {
            mPaddingRect.set(0, 0, 0, 0);
        }
    }

    /**
     * Set configurations to show text label in this balloon.
     *
     * @param label The text label to show in the balloon.
     * @param textSize The text size used to show label.
     * @param textBold Used to indicate whether the label should be bold.
     * @param textColor The text color used to show label.
     * @param width The desired width of the balloon. The real width is
     *        determined by the desired width and balloon's measuring
     *        specification.
     * @param height The desired width of the balloon. The real width is
     *        determined by the desired width and balloon's measuring
     *        specification.
     */
    public void setBalloonConfig(String label, float textSize,
            boolean textBold, int textColor, int width, int height) {
        mBalloonView.setTextConfig(label, textSize, textBold, textColor);
        setBalloonSize(width, height);
    }

    /**
     * Set configurations to show text label in this balloon.
     *
     * @param icon The icon used to shown in this balloon.
     * @param width The desired width of the balloon. The real width is
     *        determined by the desired width and balloon's measuring
     *        specification.
     * @param height The desired width of the balloon. The real width is
     *        determined by the desired width and balloon's measuring
     *        specification.
     */
    public void setBalloonConfig(Drawable icon, int width, int height) {
        mBalloonView.setIcon(icon);
        setBalloonSize(width, height);
    }


    public boolean needForceDismiss() {
        return mForceDismiss;
    }

    public int getPaddingLeft() {
        return mPaddingRect.left;
    }

    public int getPaddingTop() {
        return mPaddingRect.top;
    }

    public int getPaddingRight() {
        return mPaddingRect.right;
    }

    public int getPaddingBottom() {
        return mPaddingRect.bottom;
    }

    public void delayedShow(long delay, int locationInParent[]) {
        if (mBalloonTimer.isPending()) {
            mBalloonTimer.removeTimer();
        }
        if (delay <= 0) {
            mParent.getLocationInWindow(mParentLocationInWindow);
            showAtLocation(mParent, Gravity.LEFT | Gravity.TOP,
                    locationInParent[0], locationInParent[1]
                            + mParentLocationInWindow[1]);
        } else {
            mBalloonTimer.startTimer(delay, BalloonTimer.ACTION_SHOW,
                    locationInParent, -1, -1);
        }
    }

    public void delayedUpdate(long delay, int locationInParent[],
            int width, int height) {
        mBalloonView.invalidate();
        if (mBalloonTimer.isPending()) {
            mBalloonTimer.removeTimer();
        }
        if (delay <= 0) {
            mParent.getLocationInWindow(mParentLocationInWindow);
            update(locationInParent[0], locationInParent[1]
                    + mParentLocationInWindow[1], width, height);
        } else {
            mBalloonTimer.startTimer(delay, BalloonTimer.ACTION_UPDATE,
                    locationInParent, width, height);
        }
    }

    public void delayedDismiss(long delay) {
        if (mBalloonTimer.isPending()) {
            mBalloonTimer.removeTimer();
            int pendingAction = mBalloonTimer.getAction();
            if (0 != delay && BalloonTimer.ACTION_HIDE != pendingAction) {
                mBalloonTimer.run();
            }
        }
        if (delay <= 0) {
            dismiss();
        } else {
            mBalloonTimer.startTimer(delay, BalloonTimer.ACTION_HIDE, null, -1,
                    -1);
        }
    }

    public void removeTimer() {
        if (mBalloonTimer.isPending()) {
            mBalloonTimer.removeTimer();
        }
    }

    private void setBalloonSize(int width, int height) {
        int widthMeasureSpec = MeasureSpec.makeMeasureSpec(width,
                mMeasureSpecMode);
        int heightMeasureSpec = MeasureSpec.makeMeasureSpec(height,
                mMeasureSpecMode);
        mBalloonView.measure(widthMeasureSpec, heightMeasureSpec);

        int oldWidth = getWidth();
        int oldHeight = getHeight();
        int newWidth = mBalloonView.getMeasuredWidth() + getPaddingLeft()
                + getPaddingRight();
        int newHeight = mBalloonView.getMeasuredHeight() + getPaddingTop()
                + getPaddingBottom();
        setWidth(newWidth);
        setHeight(newHeight);

        // If update() is called to update both size and position, the system
        // will first MOVE the PopupWindow to the new position, and then
        // perform a size-updating operation, so there will be a flash in
        // PopupWindow if user presses a key and moves finger to next one whose
        // size is different.
        // PopupWindow will handle the updating issue in one go in the future,
        // but before that, if we find the size is changed, a mandatory dismiss
        // operation is required. In our UI design, normal QWERTY keys' width
        // can be different in 1-pixel, and we do not dismiss the balloon when
        // user move between QWERTY keys.
        mForceDismiss = false;
        if (isShowing()) {
            mForceDismiss = oldWidth - newWidth > 1 || newWidth - oldWidth > 1;
        }
    }


    private class BalloonTimer extends Handler implements Runnable {
        public static final int ACTION_SHOW = 1;
        public static final int ACTION_HIDE = 2;
        public static final int ACTION_UPDATE = 3;

        /**
         * The pending action.
         */
        private int mAction;

        private int mPositionInParent[] = new int[2];
        private int mWidth;
        private int mHeight;

        private boolean mTimerPending = false;

        public void startTimer(long time, int action, int positionInParent[],
                int width, int height) {
            mAction = action;
            if (ACTION_HIDE != action) {
                mPositionInParent[0] = positionInParent[0];
                mPositionInParent[1] = positionInParent[1];
            }
            mWidth = width;
            mHeight = height;
            postDelayed(this, time);
            mTimerPending = true;
        }

        public boolean isPending() {
            return mTimerPending;
        }

        public boolean removeTimer() {
            if (mTimerPending) {
                mTimerPending = false;
                removeCallbacks(this);
                return true;
            }

            return false;
        }

        public int getAction() {
            return mAction;
        }

        public void run() {
            switch (mAction) {
            case ACTION_SHOW:
                mParent.getLocationInWindow(mParentLocationInWindow);
                showAtLocation(mParent, Gravity.LEFT | Gravity.TOP,
                        mPositionInParent[0], mPositionInParent[1]
                                + mParentLocationInWindow[1]);
                break;
            case ACTION_HIDE:
                dismiss();
                break;
            case ACTION_UPDATE:
                mParent.getLocationInWindow(mParentLocationInWindow);
                update(mPositionInParent[0], mPositionInParent[1]
                        + mParentLocationInWindow[1], mWidth, mHeight);
            }
            mTimerPending = false;
        }
    }

    private class BalloonView extends View {
        /**
         * Suspension points used to display long items.
         */
        private static final String SUSPENSION_POINTS = "...";

        /**
         * The icon to be shown. If it is not null, {@link #mLabel} will be
         * ignored.
         */
        private Drawable mIcon;

        /**
         * The label to be shown. It is enabled only if {@link #mIcon} is null.
         */
        private String mLabel;

        private int mLabeColor = 0xff000000;
        private Paint mPaintLabel;
        private FontMetricsInt mFmi;

        /**
         * The width to show suspension points.
         */
        private float mSuspensionPointsWidth;


        public BalloonView(Context context) {
            super(context);
            mPaintLabel = new Paint();
            mPaintLabel.setColor(mLabeColor);
            mPaintLabel.setAntiAlias(true);
            mPaintLabel.setFakeBoldText(true);
            mFmi = mPaintLabel.getFontMetricsInt();
        }

        public void setIcon(Drawable icon) {
            mIcon = icon;
        }

        public void setTextConfig(String label, float fontSize,
                boolean textBold, int textColor) {
            // Icon should be cleared so that the label will be enabled.
            mIcon = null;
            mLabel = label;
            mPaintLabel.setTextSize(fontSize);
            mPaintLabel.setFakeBoldText(textBold);
            mPaintLabel.setColor(textColor);
            mFmi = mPaintLabel.getFontMetricsInt();
            mSuspensionPointsWidth = mPaintLabel.measureText(SUSPENSION_POINTS);
        }

        @Override
        protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
            final int widthMode = MeasureSpec.getMode(widthMeasureSpec);
            final int heightMode = MeasureSpec.getMode(heightMeasureSpec);
            final int widthSize = MeasureSpec.getSize(widthMeasureSpec);
            final int heightSize = MeasureSpec.getSize(heightMeasureSpec);

            if (widthMode == MeasureSpec.EXACTLY) {
                setMeasuredDimension(widthSize, heightSize);
                return;
            }

            int measuredWidth = mPaddingLeft + mPaddingRight;
            int measuredHeight = mPaddingTop + mPaddingBottom;
            if (null != mIcon) {
                measuredWidth += mIcon.getIntrinsicWidth();
                measuredHeight += mIcon.getIntrinsicHeight();
            } else if (null != mLabel) {
                measuredWidth += (int) (mPaintLabel.measureText(mLabel));
                measuredHeight += mFmi.bottom - mFmi.top;
            }
            if (widthSize > measuredWidth || widthMode == MeasureSpec.AT_MOST) {
                measuredWidth = widthSize;
            }

            if (heightSize > measuredHeight
                    || heightMode == MeasureSpec.AT_MOST) {
                measuredHeight = heightSize;
            }

            int maxWidth = Environment.getInstance().getScreenWidth() -
                    mPaddingLeft - mPaddingRight;
            if (measuredWidth > maxWidth) {
                measuredWidth = maxWidth;
            }
            setMeasuredDimension(measuredWidth, measuredHeight);
        }

        @Override
        protected void onDraw(Canvas canvas) {
            int width = getWidth();
            int height = getHeight();
            if (null != mIcon) {
                int marginLeft = (width - mIcon.getIntrinsicWidth()) / 2;
                int marginRight = width - mIcon.getIntrinsicWidth()
                        - marginLeft;
                int marginTop = (height - mIcon.getIntrinsicHeight()) / 2;
                int marginBottom = height - mIcon.getIntrinsicHeight()
                        - marginTop;
                mIcon.setBounds(marginLeft, marginTop, width - marginRight,
                        height - marginBottom);
                mIcon.draw(canvas);
            } else if (null != mLabel) {
                float labelMeasuredWidth = mPaintLabel.measureText(mLabel);
                float x = mPaddingLeft;
                x += (width - labelMeasuredWidth - mPaddingLeft - mPaddingRight) / 2.0f;
                String labelToDraw = mLabel;
                if (x < mPaddingLeft) {
                    x = mPaddingLeft;
                    labelToDraw = getLimitedLabelForDrawing(mLabel,
                            width - mPaddingLeft - mPaddingRight);
                }

                int fontHeight = mFmi.bottom - mFmi.top;
                float marginY = (height - fontHeight) / 2.0f;
                float y = marginY - mFmi.top;
                canvas.drawText(labelToDraw, x, y, mPaintLabel);
            }
        }

        private String getLimitedLabelForDrawing(String rawLabel,
                float widthToDraw) {
            int subLen = rawLabel.length();
            if (subLen <= 1) return rawLabel;
            do {
                subLen--;
                float width = mPaintLabel.measureText(rawLabel, 0, subLen);
                if (width + mSuspensionPointsWidth <= widthToDraw || 1 >= subLen) {
                    return rawLabel.substring(0, subLen) +
                            SUSPENSION_POINTS;
                }
            } while (true);
        }
    }
}
