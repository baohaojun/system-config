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

import com.android.inputmethod.pinyin.SoftKeyboard.KeyRow;

import android.content.Context;
import android.content.res.Resources;
import android.content.res.XmlResourceParser;
import android.graphics.drawable.Drawable;

import java.io.IOException;
import java.util.regex.Pattern;

import org.xmlpull.v1.XmlPullParserException;

/**
 * Class used to load a soft keyboard or a soft keyboard template from xml
 * files.
 */
public class XmlKeyboardLoader {
    /**
     * The tag used to define an xml-based soft keyboard template.
     */
    private static final String XMLTAG_SKB_TEMPLATE = "skb_template";

    /**
     * The tag used to indicate the soft key type which is defined inside the
     * {@link #XMLTAG_SKB_TEMPLATE} element in the xml file. file.
     */
    private static final String XMLTAG_KEYTYPE = "key_type";

    /**
     * The tag used to define a default key icon for enter/delete/space keys. It
     * is defined inside the {@link #XMLTAG_SKB_TEMPLATE} element in the xml
     * file.
     */
    private static final String XMLTAG_KEYICON = "key_icon";

    /**
     * Attribute tag of the left and right margin for a key. A key's width
     * should be larger than double of this value. Defined inside
     * {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYBOARD}.
     */
    private static final String XMLATTR_KEY_XMARGIN = "key_xmargin";

    /**
     * Attribute tag of the top and bottom margin for a key. A key's height
     * should be larger than double of this value. Defined inside
     * {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYBOARD}.
     */
    private static final String XMLATTR_KEY_YMARGIN = "key_ymargin";

    /**
     * Attribute tag of the keyboard background image. Defined inside
     * {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYBOARD}.
     */
    private static final String XMLATTR_SKB_BG = "skb_bg";

    /**
     * Attribute tag of the balloon background image for key press. Defined
     * inside {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYBOARD}.
     */
    private static final String XMLATTR_BALLOON_BG = "balloon_bg";

    /**
     * Attribute tag of the popup balloon background image for key press or
     * popup mini keyboard. Defined inside {@link #XMLTAG_SKB_TEMPLATE} and
     * {@link #XMLTAG_KEYBOARD}.
     */
    private static final String XMLATTR_POPUP_BG = "popup_bg";

    /**
     * Attribute tag of the color to draw key label. Defined inside
     * {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYTYPE}.
     */
    private static final String XMLATTR_COLOR = "color";

    /**
     * Attribute tag of the color to draw key's highlighted label. Defined
     * inside {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYTYPE}.
     */
    private static final String XMLATTR_COLOR_HIGHLIGHT = "color_highlight";

    /**
     * Attribute tag of the color to draw key's label in the popup balloon.
     * Defined inside {@link #XMLTAG_SKB_TEMPLATE} and {@link #XMLTAG_KEYTYPE}.
     */
    private static final String XMLATTR_COLOR_BALLOON = "color_balloon";

    /**
     * Attribute tag of the id of {@link #XMLTAG_KEYTYPE} and
     * {@link #XMLTAG_KEY}. Key types and keys defined in a soft keyboard
     * template should have id, because a soft keyboard needs the id to refer to
     * these default definitions. If a key defined in {@link #XMLTAG_KEYBOARD}
     * does not id, that means the key is newly defined; if it has id (and only
     * has id), the id is used to find the default definition from the soft
     * keyboard template.
     */
    private static final String XMLATTR_ID = "id";

    /**
     * Attribute tag of the key background for a specified key type. Defined
     * inside {@link #XMLTAG_KEYTYPE}.
     */
    private static final String XMLATTR_KEYTYPE_BG = "bg";

    /**
     * Attribute tag of the key high-light background for a specified key type.
     * Defined inside {@link #XMLTAG_KEYTYPE}.
     */
    private static final String XMLATTR_KEYTYPE_HLBG = "hlbg";

    /**
     * Attribute tag of the starting x-position of an element. It can be defined
     * in {@link #XMLTAG_ROW} and {@link #XMLTAG_KEY} in {XMLTAG_SKB_TEMPLATE}.
     * If not defined, 0 will be used. For a key defined in
     * {@link #XMLTAG_KEYBOARD}, it always use its previous keys information to
     * calculate its own position.
     */
    private static final String XMLATTR_START_POS_X = "start_pos_x";

    /**
     * Attribute tag of the starting y-position of an element. It can be defined
     * in {@link #XMLTAG_ROW} and {@link #XMLTAG_KEY} in {XMLTAG_SKB_TEMPLATE}.
     * If not defined, 0 will be used. For a key defined in
     * {@link #XMLTAG_KEYBOARD}, it always use its previous keys information to
     * calculate its own position.
     */
    private static final String XMLATTR_START_POS_Y = "start_pos_y";

    /**
     * Attribute tag of a row's id. Defined {@link #XMLTAG_ROW}. If not defined,
     * -1 will be used. Rows with id -1 will be enabled always, rows with same
     * row id will be enabled when the id is the same to the activated id of the
     * soft keyboard.
     */
    private static final String XMLATTR_ROW_ID = "row_id";

    /** The tag used to indicate the keyboard element in the xml file. */
    private static final String XMLTAG_KEYBOARD = "keyboard";

    /** The tag used to indicate the row element in the xml file. */
    private static final String XMLTAG_ROW = "row";

    /** The tag used to indicate key-array element in the xml file. */
    private static final String XMLTAG_KEYS = "keys";

    /**
     * The tag used to indicate a key element in the xml file. If the element is
     * defined in a soft keyboard template, it should have an id. If it is
     * defined in a soft keyboard, id is not required.
     */
    private static final String XMLTAG_KEY = "key";

    /** The tag used to indicate a key's toggle element in the xml file. */
    private static final String XMLTAG_TOGGLE_STATE = "toggle_state";

    /**
     * Attribute tag of the toggle state id for toggle key. Defined inside
     * {@link #XMLTAG_TOGGLE_STATE}
     */
    private static final String XMLATTR_TOGGLE_STATE_ID = "state_id";

    /** Attribute tag of key template for the soft keyboard. */
    private static final String XMLATTR_SKB_TEMPLATE = "skb_template";

    /**
     * Attribute tag used to indicate whether this soft keyboard needs to be
     * cached in memory for future use. {@link #DEFAULT_SKB_CACHE_FLAG}
     * specifies the default value.
     */
    private static final String XMLATTR_SKB_CACHE_FLAG = "skb_cache_flag";

    /**
     * Attribute tag used to indicate whether this soft keyboard is sticky. A
     * sticky soft keyboard will keep the current layout unless user makes a
     * switch explicitly. A none sticky soft keyboard will automatically goes
     * back to the previous keyboard after click a none-function key.
     * {@link #DEFAULT_SKB_STICKY_FLAG} specifies the default value.
     */
    private static final String XMLATTR_SKB_STICKY_FLAG = "skb_sticky_flag";

    /** Attribute tag to indicate whether it is a QWERTY soft keyboard. */
    private static final String XMLATTR_QWERTY = "qwerty";

    /**
     * When the soft keyboard is a QWERTY one, this attribute tag to get the
     * information that whether it is defined in upper case.
     */
    private static final String XMLATTR_QWERTY_UPPERCASE = "qwerty_uppercase";

    /** Attribute tag of key type. */
    private static final String XMLATTR_KEY_TYPE = "key_type";

    /** Attribute tag of key width. */
    private static final String XMLATTR_KEY_WIDTH = "width";

    /** Attribute tag of key height. */
    private static final String XMLATTR_KEY_HEIGHT = "height";

    /** Attribute tag of the key's repeating ability. */
    private static final String XMLATTR_KEY_REPEAT = "repeat";

    /** Attribute tag of the key's behavior for balloon. */
    private static final String XMLATTR_KEY_BALLOON = "balloon";

    /** Attribute tag of the key splitter in a key array. */
    private static final String XMLATTR_KEY_SPLITTER = "splitter";

    /** Attribute tag of the key labels in a key array. */
    private static final String XMLATTR_KEY_LABELS = "labels";

    /** Attribute tag of the key codes in a key array. */
    private static final String XMLATTR_KEY_CODES = "codes";

    /** Attribute tag of the key label in a key. */
    private static final String XMLATTR_KEY_LABEL = "label";

    /** Attribute tag of the key code in a key. */
    private static final String XMLATTR_KEY_CODE = "code";

    /** Attribute tag of the key icon in a key. */
    private static final String XMLATTR_KEY_ICON = "icon";

    /** Attribute tag of the key's popup icon in a key. */
    private static final String XMLATTR_KEY_ICON_POPUP = "icon_popup";

    /** The id for a mini popup soft keyboard. */
    private static final String XMLATTR_KEY_POPUP_SKBID = "popup_skb";

    private static boolean DEFAULT_SKB_CACHE_FLAG = true;

    private static boolean DEFAULT_SKB_STICKY_FLAG = true;

    /**
     * The key type id for invalid key type. It is also used to generate next
     * valid key type id by adding 1.
     */
    private static final int KEYTYPE_ID_LAST = -1;

    private Context mContext;

    private Resources mResources;

    /** The event type in parsing the xml file. */
    private int mXmlEventType;

    /**
     * The current soft keyboard template used by the current soft keyboard
     * under loading.
     **/
    private SkbTemplate mSkbTemplate;

    /** The x position for the next key. */
    float mKeyXPos;

    /** The y position for the next key. */
    float mKeyYPos;

    /** The width of the keyboard to load. */
    int mSkbWidth;

    /** The height of the keyboard to load. */
    int mSkbHeight;

    /** Key margin in x-way. */
    float mKeyXMargin = 0;

    /** Key margin in y-way. */
    float mKeyYMargin = 0;

    /**
     * Used to indicate whether next event has been fetched during processing
     * the the current event.
     */
    boolean mNextEventFetched = false;

    String mAttrTmp;

    class KeyCommonAttributes {
        XmlResourceParser mXrp;
        int keyType;
        float keyWidth;
        float keyHeight;
        boolean repeat;
        boolean balloon;

        KeyCommonAttributes(XmlResourceParser xrp) {
            mXrp = xrp;
            balloon = true;
        }

        // Make sure the default object is not null.
        boolean getAttributes(KeyCommonAttributes defAttr) {
            keyType = getInteger(mXrp, XMLATTR_KEY_TYPE, defAttr.keyType);
            keyWidth = getFloat(mXrp, XMLATTR_KEY_WIDTH, defAttr.keyWidth);
            keyHeight = getFloat(mXrp, XMLATTR_KEY_HEIGHT, defAttr.keyHeight);
            repeat = getBoolean(mXrp, XMLATTR_KEY_REPEAT, defAttr.repeat);
            balloon = getBoolean(mXrp, XMLATTR_KEY_BALLOON, defAttr.balloon);
            if (keyType < 0 || keyWidth <= 0 || keyHeight <= 0) {
                return false;
            }
            return true;
        }
    }

    public XmlKeyboardLoader(Context context) {
        mContext = context;
        mResources = mContext.getResources();
    }

    public SkbTemplate loadSkbTemplate(int resourceId) {
        if (null == mContext || 0 == resourceId) {
            return null;
        }
        Resources r = mResources;
        XmlResourceParser xrp = r.getXml(resourceId);

        KeyCommonAttributes attrDef = new KeyCommonAttributes(xrp);
        KeyCommonAttributes attrKey = new KeyCommonAttributes(xrp);

        mSkbTemplate = new SkbTemplate(resourceId);
        int lastKeyTypeId = KEYTYPE_ID_LAST;
        int globalColor = 0;
        int globalColorHl = 0;
        int globalColorBalloon = 0;
        try {
            mXmlEventType = xrp.next();
            while (mXmlEventType != XmlResourceParser.END_DOCUMENT) {
                mNextEventFetched = false;
                if (mXmlEventType == XmlResourceParser.START_TAG) {
                    String attribute = xrp.getName();
                    if (XMLTAG_SKB_TEMPLATE.compareTo(attribute) == 0) {
                        Drawable skbBg = getDrawable(xrp, XMLATTR_SKB_BG, null);
                        Drawable balloonBg = getDrawable(xrp,
                                XMLATTR_BALLOON_BG, null);
                        Drawable popupBg = getDrawable(xrp, XMLATTR_POPUP_BG,
                                null);
                        if (null == skbBg || null == balloonBg
                                || null == popupBg) {
                            return null;
                        }
                        mSkbTemplate.setBackgrounds(skbBg, balloonBg, popupBg);

                        float xMargin = getFloat(xrp, XMLATTR_KEY_XMARGIN, 0);
                        float yMargin = getFloat(xrp, XMLATTR_KEY_YMARGIN, 0);
                        mSkbTemplate.setMargins(xMargin, yMargin);

                        // Get default global colors.
                        globalColor = getColor(xrp, XMLATTR_COLOR, 0);
                        globalColorHl = getColor(xrp, XMLATTR_COLOR_HIGHLIGHT,
                                0xffffffff);
                        globalColorBalloon = getColor(xrp,
                                XMLATTR_COLOR_BALLOON, 0xffffffff);
                    } else if (XMLTAG_KEYTYPE.compareTo(attribute) == 0) {
                        int id = getInteger(xrp, XMLATTR_ID, KEYTYPE_ID_LAST);
                        Drawable bg = getDrawable(xrp, XMLATTR_KEYTYPE_BG, null);
                        Drawable hlBg = getDrawable(xrp, XMLATTR_KEYTYPE_HLBG,
                                null);
                        int color = getColor(xrp, XMLATTR_COLOR, globalColor);
                        int colorHl = getColor(xrp, XMLATTR_COLOR_HIGHLIGHT,
                                globalColorHl);
                        int colorBalloon = getColor(xrp, XMLATTR_COLOR_BALLOON,
                                globalColorBalloon);
                        if (id != lastKeyTypeId + 1) {
                            return null;
                        }
                        SoftKeyType keyType = mSkbTemplate.createKeyType(id,
                                bg, hlBg);
                        keyType.setColors(color, colorHl, colorBalloon);
                        if (!mSkbTemplate.addKeyType(keyType)) {
                            return null;
                        }
                        lastKeyTypeId = id;
                    } else if (XMLTAG_KEYICON.compareTo(attribute) == 0) {
                        int keyCode = getInteger(xrp, XMLATTR_KEY_CODE, 0);
                        Drawable icon = getDrawable(xrp, XMLATTR_KEY_ICON, null);
                        Drawable iconPopup = getDrawable(xrp,
                                XMLATTR_KEY_ICON_POPUP, null);
                        if (null != icon && null != iconPopup) {
                            mSkbTemplate.addDefaultKeyIcons(keyCode, icon,
                                    iconPopup);
                        }
                    } else if (XMLTAG_KEY.compareTo(attribute) == 0) {
                        int keyId = this.getInteger(xrp, XMLATTR_ID, -1);
                        if (-1 == keyId) return null;

                        if (!attrKey.getAttributes(attrDef)) {
                            return null;
                        }

                        // Update the key position for the key.
                        mKeyXPos = getFloat(xrp, XMLATTR_START_POS_X, 0);
                        mKeyYPos = getFloat(xrp, XMLATTR_START_POS_Y, 0);

                        SoftKey softKey = getSoftKey(xrp, attrKey);
                        if (null == softKey) return null;
                        mSkbTemplate.addDefaultKey(keyId, softKey);
                    }
                }
                // Get the next tag.
                if (!mNextEventFetched) mXmlEventType = xrp.next();
            }
            xrp.close();
            return mSkbTemplate;
        } catch (XmlPullParserException e) {
            // Log.e(TAG, "Ill-formatted keyboard template resource file");
        } catch (IOException e) {
            // Log.e(TAG, "Unable to keyboard template resource file");
        }
        return null;
    }

    public SoftKeyboard loadKeyboard(int resourceId, int skbWidth, int skbHeight) {
        if (null == mContext) return null;
        Resources r = mResources;
        SkbPool skbPool = SkbPool.getInstance();
        XmlResourceParser xrp = mContext.getResources().getXml(resourceId);
        mSkbTemplate = null;
        SoftKeyboard softKeyboard = null;
        Drawable skbBg;
        Drawable popupBg;
        Drawable balloonBg;
        SoftKey softKey = null;

        KeyCommonAttributes attrDef = new KeyCommonAttributes(xrp);
        KeyCommonAttributes attrSkb = new KeyCommonAttributes(xrp);
        KeyCommonAttributes attrRow = new KeyCommonAttributes(xrp);
        KeyCommonAttributes attrKeys = new KeyCommonAttributes(xrp);
        KeyCommonAttributes attrKey = new KeyCommonAttributes(xrp);

        mKeyXPos = 0;
        mKeyYPos = 0;
        mSkbWidth = skbWidth;
        mSkbHeight = skbHeight;

        try {
            mKeyXMargin = 0;
            mKeyYMargin = 0;
            mXmlEventType = xrp.next();
            while (mXmlEventType != XmlResourceParser.END_DOCUMENT) {
                mNextEventFetched = false;
                if (mXmlEventType == XmlResourceParser.START_TAG) {
                    String attr = xrp.getName();
                    // 1. Is it the root element, "keyboard"?
                    if (XMLTAG_KEYBOARD.compareTo(attr) == 0) {
                        // 1.1 Get the keyboard template id.
                        int skbTemplateId = xrp.getAttributeResourceValue(null,
                                XMLATTR_SKB_TEMPLATE, 0);

                        // 1.2 Try to get the template from pool. If it is not
                        // in, the pool will try to load it.
                        mSkbTemplate = skbPool.getSkbTemplate(skbTemplateId,
                                mContext);

                        if (null == mSkbTemplate
                                || !attrSkb.getAttributes(attrDef)) {
                            return null;
                        }

                        boolean cacheFlag = getBoolean(xrp,
                                XMLATTR_SKB_CACHE_FLAG, DEFAULT_SKB_CACHE_FLAG);
                        boolean stickyFlag = getBoolean(xrp,
                                XMLATTR_SKB_STICKY_FLAG,
                                DEFAULT_SKB_STICKY_FLAG);
                        boolean isQwerty = getBoolean(xrp, XMLATTR_QWERTY,
                                false);
                        boolean isQwertyUpperCase = getBoolean(xrp,
                                XMLATTR_QWERTY_UPPERCASE, false);

                        softKeyboard = new SoftKeyboard(resourceId,
                                mSkbTemplate, mSkbWidth, mSkbHeight);
                        softKeyboard.setFlags(cacheFlag, stickyFlag, isQwerty,
                                isQwertyUpperCase);

                        mKeyXMargin = getFloat(xrp, XMLATTR_KEY_XMARGIN,
                                mSkbTemplate.getXMargin());
                        mKeyYMargin = getFloat(xrp, XMLATTR_KEY_YMARGIN,
                                mSkbTemplate.getYMargin());
                        skbBg = getDrawable(xrp, XMLATTR_SKB_BG, null);
                        popupBg = getDrawable(xrp, XMLATTR_POPUP_BG, null);
                        balloonBg = getDrawable(xrp, XMLATTR_BALLOON_BG, null);
                        if (null != skbBg) {
                            softKeyboard.setSkbBackground(skbBg);
                        }
                        if (null != popupBg) {
                            softKeyboard.setPopupBackground(popupBg);
                        }
                        if (null != balloonBg) {
                            softKeyboard.setKeyBalloonBackground(balloonBg);
                        }
                        softKeyboard.setKeyMargins(mKeyXMargin, mKeyYMargin);
                    } else if (XMLTAG_ROW.compareTo(attr) == 0) {
                        if (!attrRow.getAttributes(attrSkb)) {
                            return null;
                        }
                        // Get the starting positions for the row.
                        mKeyXPos = getFloat(xrp, XMLATTR_START_POS_X, 0);
                        mKeyYPos = getFloat(xrp, XMLATTR_START_POS_Y, mKeyYPos);
                        int rowId = getInteger(xrp, XMLATTR_ROW_ID,
                                KeyRow.ALWAYS_SHOW_ROW_ID);
                        softKeyboard.beginNewRow(rowId, mKeyYPos);
                    } else if (XMLTAG_KEYS.compareTo(attr) == 0) {
                        if (null == softKeyboard) return null;
                        if (!attrKeys.getAttributes(attrRow)) {
                            return null;
                        }

                        String splitter = xrp.getAttributeValue(null,
                                XMLATTR_KEY_SPLITTER);
                        splitter = Pattern.quote(splitter);
                        String labels = xrp.getAttributeValue(null,
                                XMLATTR_KEY_LABELS);
                        String codes = xrp.getAttributeValue(null,
                                XMLATTR_KEY_CODES);
                        if (null == splitter || null == labels) {
                            return null;
                        }
                        String labelArr[] = labels.split(splitter);
                        String codeArr[] = null;
                        if (null != codes) {
                            codeArr = codes.split(splitter);
                            if (labelArr.length != codeArr.length) {
                                return null;
                            }
                        }

                        for (int i = 0; i < labelArr.length; i++) {
                            softKey = new SoftKey();
                            int keyCode = 0;
                            if (null != codeArr) {
                                keyCode = Integer.valueOf(codeArr[i]);
                            }
                            softKey.setKeyAttribute(keyCode, labelArr[i],
                                    attrKeys.repeat, attrKeys.balloon);

                            softKey.setKeyType(mSkbTemplate
                                    .getKeyType(attrKeys.keyType), null, null);

                            float left, right, top, bottom;
                            left = mKeyXPos;

                            right = left + attrKeys.keyWidth;
                            top = mKeyYPos;
                            bottom = top + attrKeys.keyHeight;

                            if (right - left < 2 * mKeyXMargin) return null;
                            if (bottom - top < 2 * mKeyYMargin) return null;

                            softKey.setKeyDimensions(left, top, right, bottom);
                            softKeyboard.addSoftKey(softKey);
                            mKeyXPos = right;
                            if ((int) mKeyXPos * mSkbWidth > mSkbWidth) {
                                return null;
                            }
                        }
                    } else if (XMLTAG_KEY.compareTo(attr) == 0) {
                        if (null == softKeyboard) {
                            return null;
                        }
                        if (!attrKey.getAttributes(attrRow)) {
                            return null;
                        }

                        int keyId = this.getInteger(xrp, XMLATTR_ID, -1);
                        if (keyId >= 0) {
                            softKey = mSkbTemplate.getDefaultKey(keyId);
                        } else {
                            softKey = getSoftKey(xrp, attrKey);
                        }
                        if (null == softKey) return null;

                        // Update the position for next key.
                        mKeyXPos = softKey.mRightF;
                        if ((int) mKeyXPos * mSkbWidth > mSkbWidth) {
                            return null;
                        }
                        // If the current xml event type becomes a starting tag,
                        // it indicates that we have parsed too much to get
                        // toggling states, and we started a new row. In this
                        // case, the row starting position information should
                        // be updated.
                        if (mXmlEventType == XmlResourceParser.START_TAG) {
                            attr = xrp.getName();
                            if (XMLTAG_ROW.compareTo(attr) == 0) {
                                mKeyYPos += attrRow.keyHeight;
                                if ((int) mKeyYPos * mSkbHeight > mSkbHeight) {
                                    return null;
                                }
                            }
                        }
                        softKeyboard.addSoftKey(softKey);
                    }
                } else if (mXmlEventType == XmlResourceParser.END_TAG) {
                    String attr = xrp.getName();
                    if (XMLTAG_ROW.compareTo(attr) == 0) {
                        mKeyYPos += attrRow.keyHeight;
                        if ((int) mKeyYPos * mSkbHeight > mSkbHeight) {
                            return null;
                        }
                    }
                }

                // Get the next tag.
                if (!mNextEventFetched) mXmlEventType = xrp.next();
            }
            xrp.close();
            softKeyboard.setSkbCoreSize(mSkbWidth, mSkbHeight);
            return softKeyboard;
        } catch (XmlPullParserException e) {
            // Log.e(TAG, "Ill-formatted keybaord resource file");
        } catch (IOException e) {
            // Log.e(TAG, "Unable to read keyboard resource file");
        }
        return null;
    }

    // Caller makes sure xrp and r are valid.
    private SoftKey getSoftKey(XmlResourceParser xrp,
            KeyCommonAttributes attrKey) throws XmlPullParserException,
            IOException {
        int keyCode = getInteger(xrp, XMLATTR_KEY_CODE, 0);
        String keyLabel = getString(xrp, XMLATTR_KEY_LABEL, null);
        Drawable keyIcon = getDrawable(xrp, XMLATTR_KEY_ICON, null);
        Drawable keyIconPopup = getDrawable(xrp, XMLATTR_KEY_ICON_POPUP, null);
        int popupSkbId = xrp.getAttributeResourceValue(null,
                XMLATTR_KEY_POPUP_SKBID, 0);

        if (null == keyLabel && null == keyIcon) {
            keyIcon = mSkbTemplate.getDefaultKeyIcon(keyCode);
            keyIconPopup = mSkbTemplate.getDefaultKeyIconPopup(keyCode);
            if (null == keyIcon || null == keyIconPopup) return null;
        }

        // Dimension information must been initialized before
        // getting toggle state, because mKeyYPos may be changed
        // to next row when trying to get toggle state.
        float left, right, top, bottom;
        left = mKeyXPos;
        right = left + attrKey.keyWidth;
        top = mKeyYPos;
        bottom = top + attrKey.keyHeight;

        if (right - left < 2 * mKeyXMargin) return null;
        if (bottom - top < 2 * mKeyYMargin) return null;

        // Try to find if the next tag is
        // {@link #XMLTAG_TOGGLE_STATE_OF_KEY}, if yes, try to
        // create a toggle key.
        boolean toggleKey = false;
        mXmlEventType = xrp.next();
        mNextEventFetched = true;

        SoftKey softKey;
        if (mXmlEventType == XmlResourceParser.START_TAG) {
            mAttrTmp = xrp.getName();
            if (mAttrTmp.compareTo(XMLTAG_TOGGLE_STATE) == 0) {
                toggleKey = true;
            }
        }
        if (toggleKey) {
            softKey = new SoftKeyToggle();
            if (!((SoftKeyToggle) softKey).setToggleStates(getToggleStates(
                    attrKey, (SoftKeyToggle) softKey, keyCode))) {
                return null;
            }
        } else {
            softKey = new SoftKey();
        }

        // Set the normal state
        softKey.setKeyAttribute(keyCode, keyLabel, attrKey.repeat,
                attrKey.balloon);
        softKey.setPopupSkbId(popupSkbId);
        softKey.setKeyType(mSkbTemplate.getKeyType(attrKey.keyType), keyIcon,
                keyIconPopup);

        softKey.setKeyDimensions(left, top, right, bottom);
        return softKey;
    }

    private SoftKeyToggle.ToggleState getToggleStates(
            KeyCommonAttributes attrKey, SoftKeyToggle softKey, int defKeyCode)
            throws XmlPullParserException, IOException {
        XmlResourceParser xrp = attrKey.mXrp;
        int stateId = getInteger(xrp, XMLATTR_TOGGLE_STATE_ID, 0);
        if (0 == stateId) return null;

        String keyLabel = getString(xrp, XMLATTR_KEY_LABEL, null);
        int keyTypeId = getInteger(xrp, XMLATTR_KEY_TYPE, KEYTYPE_ID_LAST);
        int keyCode;
        if (null == keyLabel) {
            keyCode = getInteger(xrp, XMLATTR_KEY_CODE, defKeyCode);
        } else {
            keyCode = getInteger(xrp, XMLATTR_KEY_CODE, 0);
        }
        Drawable icon = getDrawable(xrp, XMLATTR_KEY_ICON, null);
        Drawable iconPopup = getDrawable(xrp, XMLATTR_KEY_ICON_POPUP, null);
        if (null == icon && null == keyLabel) {
            return null;
        }
        SoftKeyToggle.ToggleState rootState = softKey.createToggleState();
        rootState.setStateId(stateId);
        rootState.mKeyType = null;
        if (KEYTYPE_ID_LAST != keyTypeId) {
            rootState.mKeyType = mSkbTemplate.getKeyType(keyTypeId);
        }
        rootState.mKeyCode = keyCode;
        rootState.mKeyIcon = icon;
        rootState.mKeyIconPopup = iconPopup;
        rootState.mKeyLabel = keyLabel;

        boolean repeat = getBoolean(xrp, XMLATTR_KEY_REPEAT, attrKey.repeat);
        boolean balloon = getBoolean(xrp, XMLATTR_KEY_BALLOON, attrKey.balloon);
        rootState.setStateFlags(repeat, balloon);

        rootState.mNextState = null;

        // If there is another toggle state.
        mXmlEventType = xrp.next();
        while (mXmlEventType != XmlResourceParser.START_TAG
                && mXmlEventType != XmlResourceParser.END_DOCUMENT) {
            mXmlEventType = xrp.next();
        }
        if (mXmlEventType == XmlResourceParser.START_TAG) {
            String attr = xrp.getName();
            if (attr.compareTo(XMLTAG_TOGGLE_STATE) == 0) {
                SoftKeyToggle.ToggleState nextState = getToggleStates(attrKey,
                        softKey, defKeyCode);
                if (null == nextState) return null;
                rootState.mNextState = nextState;
            }
        }

        return rootState;
    }

    private int getInteger(XmlResourceParser xrp, String name, int defValue) {
        int resId = xrp.getAttributeResourceValue(null, name, 0);
        String s;
        if (resId == 0) {
            s = xrp.getAttributeValue(null, name);
            if (null == s) return defValue;
            try {
                int ret = Integer.valueOf(s);
                return ret;
            } catch (NumberFormatException e) {
                return defValue;
            }
        } else {
            return Integer.parseInt(mContext.getResources().getString(resId));
        }
    }

    private int getColor(XmlResourceParser xrp, String name, int defValue) {
        int resId = xrp.getAttributeResourceValue(null, name, 0);
        String s;
        if (resId == 0) {
            s = xrp.getAttributeValue(null, name);
            if (null == s) return defValue;
            try {
                int ret = Integer.valueOf(s);
                return ret;
            } catch (NumberFormatException e) {
                return defValue;
            }
        } else {
            return mContext.getResources().getColor(resId);
        }
    }

    private String getString(XmlResourceParser xrp, String name, String defValue) {
        int resId = xrp.getAttributeResourceValue(null, name, 0);
        if (resId == 0) {
            return xrp.getAttributeValue(null, name);
        } else {
            return mContext.getResources().getString(resId);
        }
    }

    private float getFloat(XmlResourceParser xrp, String name, float defValue) {
        int resId = xrp.getAttributeResourceValue(null, name, 0);
        if (resId == 0) {
            String s = xrp.getAttributeValue(null, name);
            if (null == s) return defValue;
            try {
                float ret;
                if (s.endsWith("%p")) {
                    ret = Float.parseFloat(s.substring(0, s.length() - 2)) / 100;
                } else {
                    ret = Float.parseFloat(s);
                }
                return ret;
            } catch (NumberFormatException e) {
                return defValue;
            }
        } else {
            return mContext.getResources().getDimension(resId);
        }
    }

    private boolean getBoolean(XmlResourceParser xrp, String name,
            boolean defValue) {
        String s = xrp.getAttributeValue(null, name);
        if (null == s) return defValue;
        try {
            boolean ret = Boolean.parseBoolean(s);
            return ret;
        } catch (NumberFormatException e) {
            return defValue;
        }
    }

    private Drawable getDrawable(XmlResourceParser xrp, String name,
            Drawable defValue) {
        int resId = xrp.getAttributeResourceValue(null, name, 0);
        if (0 == resId) return defValue;
        return mResources.getDrawable(resId);
    }
}
