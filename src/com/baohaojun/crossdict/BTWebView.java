package com.baohaojun.crossdict;

import org.json.JSONException;
import org.json.JSONObject;

import com.blahti.drag.DragController;
import com.blahti.drag.DragLayer;
import com.blahti.drag.DragListener;
import com.blahti.drag.DragSource;
import com.blahti.drag.MyAbsoluteLayout;

import android.content.Context;
import android.graphics.Rect;
import android.graphics.Region;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Display;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;
import android.view.WindowManager;
import android.webkit.WebSettings.PluginState;
import android.webkit.WebView;
import android.widget.ImageView;
import android.widget.ListView;
import com.googlecode.toolkits.stardict.StarDict;
import java.util.ArrayList;
import net.londatiga.android.ActionItem;
import net.londatiga.android.QuickAction;
import net.londatiga.android.QuickAction.OnDismissListener;

public class BTWebView extends WebView implements TextSelectionJavascriptInterfaceListener,
                                                  OnTouchListener, OnLongClickListener, OnDismissListener, DragListener{

    /** The logging tag. */
    private static final String TAG = "BTWebView";

    /** Context. */
    protected   Context ctx;

    /** The context menu. */
    private final String html_head = "<html> <head> <link rel='stylesheet' href='dict.css' type='text/css'> <script src='jquery.js'></script> <script src='rangy-core.js'></script> <script src='rangy-serializer.js'></script> <script src='android.selection.js'></script> </head> <body>";
    private final String html_tail = "</body></html>";
    private QuickAction mContextMenu;

    /** The drag layer for selection. */
    private DragLayer mSelectionDragLayer;

    /** The drag controller for selection. */
    private DragController mDragController;

    /** The start selection handle. */
    private ImageView mStartSelectionHandle;

    /** the end selection handle. */
    private ImageView mEndSelectionHandle;

    /** The selection bounds. */
    private Rect mSelectionBounds = null;

    /** The previously selected region. */
    protected Region lastSelectedRegion = null;

    /** The selected range. */
    protected String selectedRange = "";

    /** The selected text. */
    protected String selectedText = "";

    /** Javascript interface for catching text selection. */
    protected TextSelectionJavascriptInterface textSelectionJSInterface = null;

    /** Selection mode flag. */
    protected boolean inSelectionMode = false;

    /** Flag to stop from showing context menu twice. */
    protected boolean contextMenuVisible = false;

    /** The current content width. */
    protected int contentWidth = 0;


    /** Identifier for the selection start handle. */
    private final int SELECTION_START_HANDLE = 0;

    /** Identifier for the selection end handle. */
    private final int SELECTION_END_HANDLE = 1;


    /** Last touched selection handle. */
    private int mLastTouchedSelectionHandle = -1;

    private CrossDictActivity mActivity;

    private String mBaseUrl = "file:///";

    public void setBaseUrlWithDir(String dir) {
        mBaseUrl = "file://" + dir + "/";
        Log.e("bhj", String.format("mBaseUrl is set to %s\n", mBaseUrl));
    }

    void setActivity(CrossDictActivity activity) {
        mActivity = activity;
    }

    private StarDict mDict;
    void setDict(StarDict dict) {
        mDict = dict;
    }

    public BTWebView(Context context) {
        super(context);

        this.ctx = context;
        this.setup(context);
    }

    public BTWebView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);

        this.ctx = context;
        this.setup(context);

    }

    public BTWebView(Context context, AttributeSet attrs) {
        super(context, attrs);

        this.ctx = context;
        this.setup(context);

    }


    //*****************************************************
    //*
    //*         Touch Listeners
    //*
    //*****************************************************

    private boolean mScrolling = false;
    private float mScrollDiffY = 0;
    private float mLastTouchY = 0;
    private float mScrollDiffX = 0;
    private float mLastTouchX = 0;

    @Override
    public boolean onTouch(View v, MotionEvent event) {

        float xPoint = getDensityIndependentValue(event.getX(), ctx) / getDensityIndependentValue(this.getScale(), ctx);
        float yPoint = getDensityIndependentValue(event.getY(), ctx) / getDensityIndependentValue(this.getScale(), ctx);

        // TODO: Need to update this to use this.getScale() as a factor.

        if(event.getAction() == MotionEvent.ACTION_DOWN){

            String startTouchUrl = String.format("javascript:android.selection.startTouch(%f, %f);",
                                                 xPoint, yPoint);

            mLastTouchX = xPoint;
            mLastTouchY = yPoint;

            this.loadUrl(startTouchUrl);
        }
        else if(event.getAction() == MotionEvent.ACTION_UP){
            if(!mScrolling){
                this.endSelectionMode();
            }

            mScrollDiffX = 0;
            mScrollDiffY = 0;
            mScrolling = false;

        }
        else if(event.getAction() == MotionEvent.ACTION_MOVE){

            mScrollDiffX += (xPoint - mLastTouchX);
            mScrollDiffY += (yPoint - mLastTouchY);

            mLastTouchX = xPoint;
            mLastTouchY = yPoint;


            // Only account for legitimate movement.
            if(Math.abs(mScrollDiffX) > 10 || Math.abs(mScrollDiffY) > 10){
                mScrolling = true;

            }


        }

        // If this is in selection mode, then nothing else should handle this touch
        return false;
    }

    @Override
    public boolean onLongClick(View v){
        this.loadUrl("javascript:android.selection.longTouch();");
        mScrolling = true;
        return true;
    }

    protected void setup(Context context){


        // On Touch Listener
        this.setOnLongClickListener(this);
        this.setOnTouchListener(this);


        // Webview setup
        this.getSettings().setJavaScriptEnabled(true);
        this.getSettings().setJavaScriptCanOpenWindowsAutomatically(true);
        this.getSettings().setPluginState(PluginState.ON);

        // Zoom out fully
        //this.getSettings().setLoadWithOverviewMode(true);
        //this.getSettings().setUseWideViewPort(true);

        // Javascript interfaces
        this.textSelectionJSInterface = new TextSelectionJavascriptInterface(context, this);
        this.addJavascriptInterface(this.textSelectionJSInterface, this.textSelectionJSInterface.getInterfaceName());


        // Create the selection handles
        createSelectionLayer(context);


        // Set to the empty region
        Region region = new Region();
        region.setEmpty();
        this.lastSelectedRegion = region;
    }

    String mCurrentWord;
    void lookUpWord(String word) {
        Log.e("bhj", String.format("lookUpWord '%s'\n", word));
        if (mCurrentWord != null && mCurrentWord.equals(word)) {
            Log.e("bhj", String.format("no need to load %s, mCurrentWord is %s\n", word, mCurrentWord));
            mActivity.onNewWordLoaded(word);
            return;
        }
        mCurrentWord = word;
        ArrayList<String> defs = mDict.getExplanation(word);
        Log.e("bhj", String.format("got explanation\n"));
        if (defs == null) {
            return;
        }

        String html = html_head;
        for (String def : defs) {
            html = html + def;
        }
        html = html + html_tail;

        this.loadDataWithBaseURL(mBaseUrl, html, null, "UTF8", null);

        mActivity.onNewWordLoaded(word);
    }

    //*****************************************************
    //*
    //*         Selection Layer Handling
    //*
    //*****************************************************

    /**
     * Creates the selection layer.
     *
     * @param context
     */
    protected void createSelectionLayer(Context context){

        LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        this.mSelectionDragLayer = (DragLayer) inflater.inflate(R.layout.selection_drag_layer, null);


        // Make sure it's filling parent
        this.mDragController = new DragController(context);
        this.mDragController.setDragListener(this);
        this.mDragController.addDropTarget(mSelectionDragLayer);
        this.mSelectionDragLayer.setDragController(mDragController);


        this.mStartSelectionHandle = (ImageView) this.mSelectionDragLayer.findViewById(R.id.startHandle);
        this.mStartSelectionHandle.setTag(new Integer(SELECTION_START_HANDLE));
        this.mEndSelectionHandle = (ImageView) this.mSelectionDragLayer.findViewById(R.id.endHandle);
        this.mEndSelectionHandle.setTag(new Integer(SELECTION_END_HANDLE));

        OnTouchListener handleTouchListener = new OnTouchListener(){

                @Override
                public boolean onTouch(View v, MotionEvent event) {

                    boolean handledHere = false;

                    final int action = event.getAction();

                    // Down event starts drag for handle.
                    if (action == MotionEvent.ACTION_DOWN) {
                        handledHere = startDrag (v);
                        mLastTouchedSelectionHandle = (Integer) v.getTag();
                    }

                    return handledHere;


                }


            };

        this.mStartSelectionHandle.setOnTouchListener(handleTouchListener);
        this.mEndSelectionHandle.setOnTouchListener(handleTouchListener);


    }

    /**
     * Starts selection mode on the UI thread
     */
    private Handler startSelectionModeHandler = new Handler(){

            public void handleMessage(Message m){

                if(mSelectionBounds == null)
                    return;

                addView(mSelectionDragLayer);

                drawSelectionHandles();


                int contentHeight = (int) Math.ceil(getDensityDependentValue(getContentHeight(), ctx));

                // Update Layout Params
                ViewGroup.LayoutParams layerParams = mSelectionDragLayer.getLayoutParams();
                layerParams.height = contentHeight;
                layerParams.width = contentWidth;
                mSelectionDragLayer.setLayoutParams(layerParams);

            }

        };

    /**
     * Starts selection mode.
     *
     * @param   selectionBounds
     */
    public void startSelectionMode(){

        this.startSelectionModeHandler.sendEmptyMessage(0);

    }

    // Ends selection mode on the UI thread
    private Handler endSelectionModeHandler = new Handler(){
            public void handleMessage(Message m){

                removeView(mSelectionDragLayer);
                if(getParent() != null && mContextMenu != null && contextMenuVisible){
                    // This will throw an error if the webview is being redrawn.
                    // No error handling needed, just need to stop the crash.
                    try{
                        mContextMenu.dismiss();
                    }
                    catch(Exception e){

                    }
                }
                mSelectionBounds = null;
                mLastTouchedSelectionHandle = -1;
                loadUrl("javascript: android.selection.clearSelection();");

            }
        };

    /**
     * Ends selection mode.
     */
    public void endSelectionMode(){

        this.endSelectionModeHandler.sendEmptyMessage(0);

    }

    /**
     * Calls the handler for drawing the selection handles.
     */
    private void drawSelectionHandles(){
        this.drawSelectionHandlesHandler.sendEmptyMessage(0);
    }

    /**
     * Handler for drawing the selection handles on the UI thread.
     */
    private Handler drawSelectionHandlesHandler = new Handler(){
            public void handleMessage(Message m){

                MyAbsoluteLayout.LayoutParams startParams = (com.blahti.drag.MyAbsoluteLayout.LayoutParams) mStartSelectionHandle.getLayoutParams();
                startParams.x = (int) (mSelectionBounds.left - mStartSelectionHandle.getDrawable().getIntrinsicWidth());
                startParams.y = (int) (mSelectionBounds.top - mStartSelectionHandle.getDrawable().getIntrinsicHeight());

                // Stay on screen.
                startParams.x = (startParams.x < 0) ? 0 : startParams.x;
                startParams.y = (startParams.y < 0) ? 0 : startParams.y;

                mStartSelectionHandle.setLayoutParams(startParams);

                MyAbsoluteLayout.LayoutParams endParams = (com.blahti.drag.MyAbsoluteLayout.LayoutParams) mEndSelectionHandle.getLayoutParams();
                endParams.x = (int) mSelectionBounds.right;
                endParams.y = (int) mSelectionBounds.bottom;

                // Stay on screen
                endParams.x = (endParams.x < 0) ? 0 : endParams.x;
                endParams.y = (endParams.y < 0) ? 0 : endParams.y;

                mEndSelectionHandle.setLayoutParams(endParams);

            }
        };

    /**
     * Checks to see if this view is in selection mode.
     * @return
     */
    public boolean isInSelectionMode(){

        return this.mSelectionDragLayer.getParent() != null;


    }

    //*****************************************************
    //*
    //*         DragListener Methods
    //*
    //*****************************************************

    /**
     * Start dragging a view.
     *
     */
    private boolean startDrag (View v)
    {
        // Let the DragController initiate a drag-drop sequence.
        // I use the dragInfo to pass along the object being dragged.
        // I'm not sure how the Launcher designers do this.
        Object dragInfo = v;
        mDragController.startDrag (v, mSelectionDragLayer, dragInfo, DragController.DRAG_ACTION_MOVE);
        return true;
    }


    @Override
    public void onDragStart(DragSource source, Object info, int dragAction) {
        // TODO Auto-generated method stub

    }

    @Override
    public void onDragEnd() {
        // TODO Auto-generated method stub

        MyAbsoluteLayout.LayoutParams startHandleParams = (MyAbsoluteLayout.LayoutParams) this.mStartSelectionHandle.getLayoutParams();
        MyAbsoluteLayout.LayoutParams endHandleParams = (MyAbsoluteLayout.LayoutParams) this.mEndSelectionHandle.getLayoutParams();

        float scale = getDensityIndependentValue(this.getScale(), ctx);

        float startX = startHandleParams.x - this.getScrollX();
        float startY = startHandleParams.y - this.getScrollY();
        float endX = endHandleParams.x - this.getScrollX();
        float endY = endHandleParams.y - this.getScrollY();

        startX = getDensityIndependentValue(startX, ctx) / scale;
        startY = getDensityIndependentValue(startY, ctx) / scale;
        endX = getDensityIndependentValue(endX, ctx) / scale;
        endY = getDensityIndependentValue(endY, ctx) / scale;


        if(mLastTouchedSelectionHandle == SELECTION_START_HANDLE && startX > 0 && startY > 0){
            String saveStartString = String.format("javascript: android.selection.setStartPos(%f, %f);", startX, startY);
            this.loadUrl(saveStartString);
        }

        if(mLastTouchedSelectionHandle == SELECTION_END_HANDLE && endX > 0 && endY > 0){
            String saveEndString = String.format("javascript: android.selection.setEndPos(%f, %f);", endX, endY);
            this.loadUrl(saveEndString);
        }

    }


    /**
     * Shows the context menu using the given region as an anchor point.
     * @param region
     */
    private void showContextMenu(Rect displayRect){

        // Don't show this twice
        if(this.contextMenuVisible){
            return;
        }

        // Don't use empty rect
        if(displayRect.right <= displayRect.left){
            return;
        }



        //Copy action item
        ActionItem buttonOne = new ActionItem();

        buttonOne.setTitle("Look up");
        buttonOne.setActionId(1);
        buttonOne.setIcon(getResources().getDrawable(R.drawable.menu_search));


        //Highlight action item
        ActionItem buttonTwo = new ActionItem();

        buttonTwo.setTitle("Defined with...");
        buttonTwo.setActionId(2);
        buttonTwo.setIcon(getResources().getDrawable(R.drawable.menu_info));

        ActionItem buttonThree = new ActionItem();

        buttonThree.setTitle("Matching...");
        buttonThree.setActionId(3);
        buttonThree.setIcon(getResources().getDrawable(R.drawable.menu_eraser));



        // The action menu
        mContextMenu  = new QuickAction(this.getContext());
        mContextMenu.setOnDismissListener(this);

        // Add buttons
        mContextMenu.addActionItem(buttonOne);
        mContextMenu.addActionItem(buttonTwo);
        mContextMenu.addActionItem(buttonThree);



        //setup the action item click listener
        mContextMenu.setOnActionItemClickListener(new QuickAction.OnActionItemClickListener() {

                @Override
                public void onItemClick(QuickAction source, int pos,
                                        int actionId) {
                    if (actionId == 1) {
                        mActivity.runOnUiThread(new Runnable() {
                                public void run() {
                                    mActivity.lookUpWord(selectedText);
                                }
                            });
                    }
                    else if (actionId == 2) {
                        mActivity.runOnUiThread(new Runnable() {
                                public void run() {
                                    mActivity.lookUpDefiner(selectedText);
                                }
                            });
                    }
                    else if (actionId == 3) {
                        mActivity.runOnUiThread(new Runnable() {
                                public void run() {
                                    mActivity.lookUpMatching(selectedText);
                                }
                            });
                    }
                    contextMenuVisible = false;
                    endSelectionMode();
                }

            });

        this.contextMenuVisible = true;
        mContextMenu.show(this, displayRect);
    }


    //*****************************************************
    //*
    //*         OnDismiss Listener
    //*
    //*****************************************************

    /**
     * Clears the selection when the context menu is dismissed.
     */
    public void onDismiss(){
        //clearSelection();
        this.contextMenuVisible = false;
    }

    //*****************************************************
    //*
    //*         Text Selection Javascript Interface Listener
    //*
    //*****************************************************


    /**
     * Shows/updates the context menu based on the range
     */
    public void tsjiJSError(String error){
        Log.e(TAG, "JSError: " + error);
    }


    /**
     * The user has started dragging the selection handles.
     */
    public void tsjiStartSelectionMode(){

        this.startSelectionMode();


    }

    /**
     * The user has stopped dragging the selection handles.
     */
    public void tsjiEndSelectionMode(){

        this.endSelectionMode();
    }

    /**
     * The selection has changed
     * @param range
     * @param text
     * @param handleBounds
     * @param menuBounds
     * @param showHighlight
     * @param showUnHighlight
     */
    public void tsjiSelectionChanged(String range, String text, String handleBounds, String menuBounds){
        try {
            JSONObject selectionBoundsObject = new JSONObject(handleBounds);

            float scale = getDensityIndependentValue(this.getScale(), ctx);

            Rect handleRect = new Rect();
            handleRect.left = (int) (getDensityDependentValue(selectionBoundsObject.getInt("left"), getContext()) * scale);
            handleRect.top = (int) (getDensityDependentValue(selectionBoundsObject.getInt("top"), getContext()) * scale);
            handleRect.right = (int) (getDensityDependentValue(selectionBoundsObject.getInt("right"), getContext()) * scale);
            handleRect.bottom = (int) (getDensityDependentValue(selectionBoundsObject.getInt("bottom"), getContext()) * scale);

            this.mSelectionBounds = handleRect;
            this.selectedRange = range;
            this.selectedText = text;

            JSONObject menuBoundsObject = new JSONObject(menuBounds);

            Rect displayRect = new Rect();
            displayRect.left = (int) (getDensityDependentValue(menuBoundsObject.getInt("left"), getContext()) * scale);
            displayRect.top = (int) (getDensityDependentValue(menuBoundsObject.getInt("top") - 25, getContext()) * scale);
            displayRect.right = (int) (getDensityDependentValue(menuBoundsObject.getInt("right"), getContext()) * scale);
            displayRect.bottom = (int) (getDensityDependentValue(menuBoundsObject.getInt("bottom") + 25, getContext()) * scale);

            if(!this.isInSelectionMode()){
                this.startSelectionMode();
            }

            // This will send the menu rect
            this.showContextMenu(displayRect);

            drawSelectionHandles();


        } catch (JSONException e) {
            // TODO Auto-generated catch block
            Log.e("bhj", " JSONException", e);
        } catch (Exception e) {
            Log.e("bhj", " Exception", e);
        }


    }


    /**
     * Receives the content width for the page.
     */
    public void tsjiSetContentWidth(float contentWidth){
        this.contentWidth = (int) this.getDensityDependentValue(contentWidth, ctx);
    }


    //*****************************************************
    //*
    //*         Density Conversion
    //*
    //*****************************************************

    /**
     * Returns the density dependent value of the given float
     * @param val
     * @param ctx
     * @return
     */
    public float getDensityDependentValue(float val, Context ctx){

        // Get display from context
        Display display = ((WindowManager) ctx.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();

        // Calculate min bound based on metrics
        DisplayMetrics metrics = new DisplayMetrics();
        display.getMetrics(metrics);

        return val * (metrics.densityDpi / 160f);

        //return TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, val, metrics);

    }

    /**
     * Returns the density independent value of the given float
     * @param val
     * @param ctx
     * @return
     */
    public float getDensityIndependentValue(float val, Context ctx){

        // Get display from context
        Display display = ((WindowManager) ctx.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();

        // Calculate min bound based on metrics
        DisplayMetrics metrics = new DisplayMetrics();
        display.getMetrics(metrics);


        return val / (metrics.densityDpi / 160f);

        //return TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_PX, val, metrics);

    }
}
