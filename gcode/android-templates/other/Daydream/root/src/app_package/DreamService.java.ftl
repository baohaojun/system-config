package ${packageName};

import java.util.Random;

import android.animation.Animator;
import android.animation.Animator.AnimatorListener;
import android.animation.AnimatorListenerAdapter;
import android.animation.TimeInterpolator;
import android.annotation.TargetApi;
import android.content.SharedPreferences;
import android.graphics.Point;
import android.os.Build;
import android.preference.PreferenceManager;
import android.service.dreams.DreamService;
import android.view.ViewPropertyAnimator;
import android.view.animation.LinearInterpolator;
import android.widget.TextView;

/**
 * This class is a sample implementation of a DreamService. When activated, a
 * TextView will repeatedly, move from the left to the right of screen, at a
 * random y-value.
<#if configurable>
 * The generated {@link BlahDreamServiceSettingsActivity} allows
 * the user to change the text which is displayed.
</#if>
 * <p />
 * Daydreams are only available on devices running API v17+.
 */
@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
public class ${className} extends DreamService {

    private static final TimeInterpolator sInterpolator = new LinearInterpolator();

    private final AnimatorListener mAnimListener = new AnimatorListenerAdapter() {

        @Override
        public void onAnimationEnd(Animator animation) {
            // Start animation again
            startTextViewScrollAnimation();
        }

    };

    private final Random mRandom = new Random();
    private final Point mPointSize = new Point();

    private TextView mDreamTextView;
    private ViewPropertyAnimator mAnimator;

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();

        // Exit dream upon user touch?
<#if isInteractive>
        setInteractive(true);
<#else>
        setInteractive(false);
</#if>

        // Hide system UI?
<#if isFullscreen>
        setFullscreen(true);
<#else>
        setFullscreen(false);
</#if>

        // Keep screen at full brightness?
<#if isScreenBright>
        setScreenBright(true);
<#else>
        setScreenBright(false);
</#if>

        // Set the content view, just like you would with an Activity.
        setContentView(R.layout.${class_name});

        mDreamTextView = (TextView) findViewById(R.id.dream_text);
        mDreamTextView.setText(getTextFromPreferences());
    }

    @Override
    public void onDreamingStarted() {
        super.onDreamingStarted();

        // TODO: Begin animations or other behaviors here.

        startTextViewScrollAnimation();
    }

    @Override
    public void onDreamingStopped() {
        super.onDreamingStopped();

        // TODO: Stop anything that was started in onDreamingStarted()

        mAnimator.cancel();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();

        // TODO: Dismantle resources
        // (for example, detach from handlers and listeners).
    }

    private String getTextFromPreferences() {
        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
        return prefs.getString(getString(R.string.pref_dream_text_key),
                getString(R.string.pref_dream_text_default));
    }

    private void startTextViewScrollAnimation() {
        // Refresh Size of Window
        getWindowManager().getDefaultDisplay().getSize(mPointSize);

        final int windowWidth = mPointSize.x;
        final int windowHeight = mPointSize.y;

        // Move TextView so it's moved all the way to the left
        mDreamTextView.setTranslationX(-mDreamTextView.getWidth());

        // Move TextView to random y value
        final int yRange = windowHeight - mDreamTextView.getHeight();
        mDreamTextView.setTranslationY(mRandom.nextInt(yRange));

        // Create an Animator and keep a reference to it
        mAnimator = mDreamTextView.animate().translationX(windowWidth)
            .setDuration(3000)
            .setStartDelay(500)
            .setListener(mAnimListener)
            .setInterpolator(sInterpolator);

        // Start the animation
        mAnimator.start();
    }

}
