
package com.mrvl.autotest.app;

import com.mrvl.autotest.R;

import android.app.Activity;
import android.os.Bundle;
import android.graphics.drawable.ColorDrawable;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.util.AttributeSet;
import android.content.Context;
import android.os.CountDownTimer;
import java.io.FileWriter;
import android.util.Log;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;


class LcdTestView extends TextView implements GestureDetector.OnGestureListener {

    // Begin GestureDetector.OnGestureListener methods

    public LcdTestView(Context context) {
        super(context);
    }

    public LcdTestView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);

    }

    public LcdTestView(Context context, AttributeSet attrs,
		       int defStyle) {
        super(context, attrs, defStyle);
    }

    public boolean onSingleTapUp(MotionEvent e) {
        switchBgColor();
        invalidate();
        return true;
    }

    public void onLongPress(MotionEvent e) {
    }

    public boolean onScroll(MotionEvent e1, MotionEvent e2,
                            float distanceX, float distanceY) {
        return true;
    }

    public void onSingleTapConfirmed(MotionEvent e) {
    }

    public boolean onJumpTapDown(MotionEvent e1, MotionEvent e2) {
        return true;
    }

    public boolean onJumpTapUp(MotionEvent e1, MotionEvent e2) {
        return true;
    }

    public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
                           float velocityY) {
        return true;
    }

    public void onShowPress(MotionEvent e) {
    }

    public boolean onDown(MotionEvent e) {
        return true;
    }

    // End GestureDetector.OnGestureListener methods

    private int mCurretnBgColor = 0;
    public Activity mActivity;
    public CountDownTimer mTimer;
    private boolean mFinished = false;

    public void switchBgColor() {
        mTimer.cancel();

        if (mFinished) {
            return;
        }

        int [] color = {0xffff0000, 0xff00ff00, 0xff0000ff, 0};
        int i;

        for (i = 0; i < color.length; i++) {
            if (mCurretnBgColor == color[i]) {
                break;
            }
        }

        i = (i + 1) % color.length;

        if (color[i] == 0 && mActivity != null) {
            mActivity.showDialog(0); // we have only 1 dialog
            mFinished = true;
        }
        
        mCurretnBgColor = color[i];

        setBackgroundDrawable(new ColorDrawable(color[i]));
        invalidate();
        mTimer.start();

    }
        
    public GestureDetector mGestureDetector = null;

    @Override public boolean onTouchEvent(MotionEvent ev) {
        if (mGestureDetector != null) {
            return mGestureDetector.onTouchEvent(ev);
        } else {
            return false;
        }
    }
}

public class LcdTest extends Activity
{
    
    private LcdTestView mView = null;

    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        LayoutInflater inflater = LayoutInflater.from(this);
        mView = (LcdTestView) inflater.inflate(R.layout.lcd_test, null);
        mView.mGestureDetector = new GestureDetector(mView);
        mView.mActivity = this;
        mView.mTimer = new CountDownTimer(10000, 10000) {
                public void onTick(long millisUntilFinished) {
                }
                public void onFinish() {
                    mView.switchBgColor();
                }
            };

        setContentView(mView);
        mView.mTimer.start();
        
    }

    @Override
    protected Dialog onCreateDialog(int id) {
	return new AlertDialog.Builder(this)
	    .setTitle(R.string.rgb_summary)
	    .setPositiveButton(R.string.rgb_yes, new DialogInterface.OnClickListener() {
		    public void onClick(DialogInterface dialog, int whichButton) {
			mView.mTimer.cancel();
			finish();
		    }
		})
	    .setNegativeButton(R.string.rgb_no, new DialogInterface.OnClickListener() {
		    public void onClick(DialogInterface dialog, int whichButton) {
			mView.mTimer.cancel();
			finish();
		    }
		}).create();
    }

}
