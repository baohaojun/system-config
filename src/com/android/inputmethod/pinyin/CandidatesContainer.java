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

import com.android.inputmethod.pinyin.PinyinIME.DecodingInfo;

import android.content.Context;
import android.util.AttributeSet;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.TranslateAnimation;
import android.view.animation.Animation.AnimationListener;
import android.widget.ImageButton;
import android.widget.RelativeLayout;
import android.widget.ViewFlipper;

interface ArrowUpdater {
    void updateArrowStatus();
}


/**
 * Container used to host the two candidate views. When user drags on candidate
 * view, animation is used to dismiss the current candidate view and show a new
 * one. These two candidate views and their parent are hosted by this container.
 * <p>
 * Besides the candidate views, there are two arrow views to show the page
 * forward/backward arrows.
 * </p>
 */
public class CandidatesContainer extends RelativeLayout implements
        OnTouchListener, AnimationListener, ArrowUpdater {
    /**
     * Alpha value to show an enabled arrow.
     */
    private static int ARROW_ALPHA_ENABLED = 0xff;

    /**
     * Alpha value to show an disabled arrow.
     */
    private static int ARROW_ALPHA_DISABLED = 0x40;

    /**
     * Animation time to show a new candidate view and dismiss the old one.
     */
    private static int ANIMATION_TIME = 200;

    /**
     * Listener used to notify IME that user clicks a candidate, or navigate
     * between them.
     */
    private CandidateViewListener mCvListener;

    /**
     * The left arrow button used to show previous page.
     */
    private ImageButton mLeftArrowBtn;

    /**
     * The right arrow button used to show next page.
     */
    private ImageButton mRightArrowBtn;

    /**
     * Decoding result to show.
     */
    private DecodingInfo mDecInfo;

    /**
     * The animation view used to show candidates. It contains two views.
     * Normally, the candidates are shown one of them. When user navigates to
     * another page, animation effect will be performed.
     */
    private ViewFlipper mFlipper;

    /**
     * The x offset of the flipper in this container.
     */
    private int xOffsetForFlipper;

    /**
     * Animation used by the incoming view when the user navigates to a left
     * page.
     */
    private Animation mInAnimPushLeft;

    /**
     * Animation used by the incoming view when the user navigates to a right
     * page.
     */
    private Animation mInAnimPushRight;

    /**
     * Animation used by the incoming view when the user navigates to a page
     * above. If the page navigation is triggered by DOWN key, this animation is
     * used.
     */
    private Animation mInAnimPushUp;

    /**
     * Animation used by the incoming view when the user navigates to a page
     * below. If the page navigation is triggered by UP key, this animation is
     * used.
     */
    private Animation mInAnimPushDown;

    /**
     * Animation used by the outgoing view when the user navigates to a left
     * page.
     */
    private Animation mOutAnimPushLeft;

    /**
     * Animation used by the outgoing view when the user navigates to a right
     * page.
     */
    private Animation mOutAnimPushRight;

    /**
     * Animation used by the outgoing view when the user navigates to a page
     * above. If the page navigation is triggered by DOWN key, this animation is
     * used.
     */
    private Animation mOutAnimPushUp;

    /**
     * Animation used by the incoming view when the user navigates to a page
     * below. If the page navigation is triggered by UP key, this animation is
     * used.
     */
    private Animation mOutAnimPushDown;

    /**
     * Animation object which is used for the incoming view currently.
     */
    private Animation mInAnimInUse;

    /**
     * Animation object which is used for the outgoing view currently.
     */
    private Animation mOutAnimInUse;

    /**
     * Current page number in display.
     */
    private int mCurrentPage = -1;

    public CandidatesContainer(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public void initialize(CandidateViewListener cvListener,
            BalloonHint balloonHint, GestureDetector gestureDetector) {
        mCvListener = cvListener;

        mLeftArrowBtn = (ImageButton) findViewById(R.id.arrow_left_btn);
        mRightArrowBtn = (ImageButton) findViewById(R.id.arrow_right_btn);
        mLeftArrowBtn.setOnTouchListener(this);
        mRightArrowBtn.setOnTouchListener(this);

        mFlipper = (ViewFlipper) findViewById(R.id.candidate_flipper);
        mFlipper.setMeasureAllChildren(true);

        invalidate();
        requestLayout();

        for (int i = 0; i < mFlipper.getChildCount(); i++) {
            CandidateView cv = (CandidateView) mFlipper.getChildAt(i);
            cv.initialize(this, balloonHint, gestureDetector, mCvListener);
        }
    }

    public void showCandidates(PinyinIME.DecodingInfo decInfo,
            boolean enableActiveHighlight) {
        if (null == decInfo) return;
        mDecInfo = decInfo;
        mCurrentPage = 0;

        if (decInfo.isCandidatesListEmpty()) {
            showArrow(mLeftArrowBtn, false);
            showArrow(mRightArrowBtn, false);
        } else {
            showArrow(mLeftArrowBtn, true);
            showArrow(mRightArrowBtn, true);
        }

        for (int i = 0; i < mFlipper.getChildCount(); i++) {
            CandidateView cv = (CandidateView) mFlipper.getChildAt(i);
            cv.setDecodingInfo(mDecInfo);
        }
        stopAnimation();

        CandidateView cv = (CandidateView) mFlipper.getCurrentView();
        cv.showPage(mCurrentPage, 0, enableActiveHighlight);

        updateArrowStatus();
        invalidate();
    }

    public int getCurrentPage() {
        return mCurrentPage;
    }

    public void enableActiveHighlight(boolean enableActiveHighlight) {
        CandidateView cv = (CandidateView) mFlipper.getCurrentView();
        cv.enableActiveHighlight(enableActiveHighlight);
        invalidate();
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        Environment env = Environment.getInstance();
        int measuredWidth = env.getScreenWidth();
        int measuredHeight = getPaddingTop();
        measuredHeight += env.getHeightForCandidates();
        widthMeasureSpec = MeasureSpec.makeMeasureSpec(measuredWidth,
                MeasureSpec.EXACTLY);
        heightMeasureSpec = MeasureSpec.makeMeasureSpec(measuredHeight,
                MeasureSpec.EXACTLY);
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        if (null != mLeftArrowBtn) {
            xOffsetForFlipper = mLeftArrowBtn.getMeasuredWidth();
        }
    }

    public boolean activeCurseBackward() {
        if (mFlipper.isFlipping() || null == mDecInfo) {
            return false;
        }

        CandidateView cv = (CandidateView) mFlipper.getCurrentView();

        if (cv.activeCurseBackward()) {
            cv.invalidate();
            return true;
        } else {
            return pageBackward(true, true);
        }
    }

    public boolean activeCurseForward() {
        if (mFlipper.isFlipping() || null == mDecInfo) {
            return false;
        }

        CandidateView cv = (CandidateView) mFlipper.getCurrentView();

        if (cv.activeCursorForward()) {
            cv.invalidate();
            return true;
        } else {
            return pageForward(true, true);
        }
    }

    public boolean pageBackward(boolean animLeftRight,
            boolean enableActiveHighlight) {
        if (null == mDecInfo) return false;

        if (mFlipper.isFlipping() || 0 == mCurrentPage) return false;

        int child = mFlipper.getDisplayedChild();
        int childNext = (child + 1) % 2;
        CandidateView cv = (CandidateView) mFlipper.getChildAt(child);
        CandidateView cvNext = (CandidateView) mFlipper.getChildAt(childNext);

        mCurrentPage--;
        int activeCandInPage = cv.getActiveCandiatePosInPage();
        if (animLeftRight)
            activeCandInPage = mDecInfo.mPageStart.elementAt(mCurrentPage + 1)
                    - mDecInfo.mPageStart.elementAt(mCurrentPage) - 1;

        cvNext.showPage(mCurrentPage, activeCandInPage, enableActiveHighlight);
        loadAnimation(animLeftRight, false);
        startAnimation();

        updateArrowStatus();
        return true;
    }

    public boolean pageForward(boolean animLeftRight,
            boolean enableActiveHighlight) {
        if (null == mDecInfo) return false;

        if (mFlipper.isFlipping() || !mDecInfo.preparePage(mCurrentPage + 1)) {
            return false;
        }

        int child = mFlipper.getDisplayedChild();
        int childNext = (child + 1) % 2;
        CandidateView cv = (CandidateView) mFlipper.getChildAt(child);
        int activeCandInPage = cv.getActiveCandiatePosInPage();
        cv.enableActiveHighlight(enableActiveHighlight);

        CandidateView cvNext = (CandidateView) mFlipper.getChildAt(childNext);
        mCurrentPage++;
        if (animLeftRight) activeCandInPage = 0;

        cvNext.showPage(mCurrentPage, activeCandInPage, enableActiveHighlight);
        loadAnimation(animLeftRight, true);
        startAnimation();

        updateArrowStatus();
        return true;
    }

    public int getActiveCandiatePos() {
        if (null == mDecInfo) return -1;
        CandidateView cv = (CandidateView) mFlipper.getCurrentView();
        return cv.getActiveCandiatePosGlobal();
    }

    public void updateArrowStatus() {
        if (mCurrentPage < 0) return;
        boolean forwardEnabled = mDecInfo.pageForwardable(mCurrentPage);
        boolean backwardEnabled = mDecInfo.pageBackwardable(mCurrentPage);

        if (backwardEnabled) {
            enableArrow(mLeftArrowBtn, true);
        } else {
            enableArrow(mLeftArrowBtn, false);
        }
        if (forwardEnabled) {
            enableArrow(mRightArrowBtn, true);
        } else {
            enableArrow(mRightArrowBtn, false);
        }
    }

    private void enableArrow(ImageButton arrowBtn, boolean enabled) {
        arrowBtn.setEnabled(enabled);
        if (enabled)
            arrowBtn.setAlpha(ARROW_ALPHA_ENABLED);
        else
            arrowBtn.setAlpha(ARROW_ALPHA_DISABLED);
    }

    private void showArrow(ImageButton arrowBtn, boolean show) {
        if (show)
            arrowBtn.setVisibility(View.VISIBLE);
        else
            arrowBtn.setVisibility(View.INVISIBLE);
    }

    public boolean onTouch(View v, MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            if (v == mLeftArrowBtn) {
                mCvListener.onToRightGesture();
            } else if (v == mRightArrowBtn) {
                mCvListener.onToLeftGesture();
            }
        } else if (event.getAction() == MotionEvent.ACTION_UP) {
            CandidateView cv = (CandidateView) mFlipper.getCurrentView();
            cv.enableActiveHighlight(true);
        }

        return false;
    }

    // The reason why we handle candiate view's touch events here is because
    // that the view under the focused view may get touch events instead of the
    // focused one.
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        event.offsetLocation(-xOffsetForFlipper, 0);
        CandidateView cv = (CandidateView) mFlipper.getCurrentView();
        cv.onTouchEventReal(event);
        return true;
    }

    public void loadAnimation(boolean animLeftRight, boolean forward) {
        if (animLeftRight) {
            if (forward) {
                if (null == mInAnimPushLeft) {
                    mInAnimPushLeft = createAnimation(1.0f, 0, 0, 0, 0, 1.0f,
                            ANIMATION_TIME);
                    mOutAnimPushLeft = createAnimation(0, -1.0f, 0, 0, 1.0f, 0,
                            ANIMATION_TIME);
                }
                mInAnimInUse = mInAnimPushLeft;
                mOutAnimInUse = mOutAnimPushLeft;
            } else {
                if (null == mInAnimPushRight) {
                    mInAnimPushRight = createAnimation(-1.0f, 0, 0, 0, 0, 1.0f,
                            ANIMATION_TIME);
                    mOutAnimPushRight = createAnimation(0, 1.0f, 0, 0, 1.0f, 0,
                            ANIMATION_TIME);
                }
                mInAnimInUse = mInAnimPushRight;
                mOutAnimInUse = mOutAnimPushRight;
            }
        } else {
            if (forward) {
                if (null == mInAnimPushUp) {
                    mInAnimPushUp = createAnimation(0, 0, 1.0f, 0, 0, 1.0f,
                            ANIMATION_TIME);
                    mOutAnimPushUp = createAnimation(0, 0, 0, -1.0f, 1.0f, 0,
                            ANIMATION_TIME);
                }
                mInAnimInUse = mInAnimPushUp;
                mOutAnimInUse = mOutAnimPushUp;
            } else {
                if (null == mInAnimPushDown) {
                    mInAnimPushDown = createAnimation(0, 0, -1.0f, 0, 0, 1.0f,
                            ANIMATION_TIME);
                    mOutAnimPushDown = createAnimation(0, 0, 0, 1.0f, 1.0f, 0,
                            ANIMATION_TIME);
                }
                mInAnimInUse = mInAnimPushDown;
                mOutAnimInUse = mOutAnimPushDown;
            }
        }

        mInAnimInUse.setAnimationListener(this);

        mFlipper.setInAnimation(mInAnimInUse);
        mFlipper.setOutAnimation(mOutAnimInUse);
    }

    private Animation createAnimation(float xFrom, float xTo, float yFrom,
            float yTo, float alphaFrom, float alphaTo, long duration) {
        AnimationSet animSet = new AnimationSet(getContext(), null);
        Animation trans = new TranslateAnimation(Animation.RELATIVE_TO_SELF,
                xFrom, Animation.RELATIVE_TO_SELF, xTo,
                Animation.RELATIVE_TO_SELF, yFrom, Animation.RELATIVE_TO_SELF,
                yTo);
        Animation alpha = new AlphaAnimation(alphaFrom, alphaTo);
        animSet.addAnimation(trans);
        animSet.addAnimation(alpha);
        animSet.setDuration(duration);
        return animSet;
    }

    private void startAnimation() {
        mFlipper.showNext();
    }

    private void stopAnimation() {
        mFlipper.stopFlipping();
    }

    public void onAnimationEnd(Animation animation) {
        if (!mLeftArrowBtn.isPressed() && !mRightArrowBtn.isPressed()) {
            CandidateView cv = (CandidateView) mFlipper.getCurrentView();
            cv.enableActiveHighlight(true);
        }
    }

    public void onAnimationRepeat(Animation animation) {
    }

    public void onAnimationStart(Animation animation) {
    }
}
