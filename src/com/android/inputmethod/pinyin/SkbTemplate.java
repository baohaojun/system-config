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

import java.util.Vector;

/**
 * Key icon definition. It is defined in soft keyboard template. A soft keyboard
 * can refer to such an icon in its xml file directly to improve performance.
 */
class KeyIconRecord {
    int keyCode;
    Drawable icon;
    Drawable iconPopup;
}


/**
 * Default definition for a certain key. It is defined in soft keyboard
 * template. A soft keyboard can refer to a default key in its xml file. Nothing
 * of the key can be overwritten, including the size.
 */
class KeyRecord {
    int keyId;
    SoftKey softKey;
}


/**
 * Soft keyboard template used by soft keyboards to share common resources. In
 * this way, memory cost is reduced.
 */
public class SkbTemplate {
    private int mSkbTemplateId;
    private Drawable mSkbBg;
    private Drawable mBalloonBg;
    private Drawable mPopupBg;
    private float mXMargin = 0;
    private float mYMargin = 0;
    /** Key type list. */
    private Vector<SoftKeyType> mKeyTypeList = new Vector<SoftKeyType>();

    /**
     * Default key icon list. It is only for keys which do not have popup icons.
     */
    private Vector<KeyIconRecord> mKeyIconRecords = new Vector<KeyIconRecord>();

    /**
     * Default key list.
     */
    private Vector<KeyRecord> mKeyRecords = new Vector<KeyRecord>();

    public SkbTemplate(int skbTemplateId) {
        mSkbTemplateId = skbTemplateId;
    }

    public int getSkbTemplateId() {
        return mSkbTemplateId;
    }

    public void setBackgrounds(Drawable skbBg, Drawable balloonBg,
            Drawable popupBg) {
        mSkbBg = skbBg;
        mBalloonBg = balloonBg;
        mPopupBg = popupBg;
    }

    public Drawable getSkbBackground() {
        return mSkbBg;
    }

    public Drawable getBalloonBackground() {
        return mBalloonBg;
    }

    public Drawable getPopupBackground() {
        return mPopupBg;
    }

    public void setMargins(float xMargin, float yMargin) {
        mXMargin = xMargin;
        mYMargin = yMargin;
    }

    public float getXMargin() {
        return mXMargin;
    }

    public float getYMargin() {
        return mYMargin;
    }

    public SoftKeyType createKeyType(int id, Drawable bg, Drawable hlBg) {
        return new SoftKeyType(id, bg, hlBg);
    }

    public boolean addKeyType(SoftKeyType keyType) {
        // The newly added item should have the right id.
        if (mKeyTypeList.size() != keyType.mKeyTypeId) return false;
        mKeyTypeList.add(keyType);
        return true;
    }

    public SoftKeyType getKeyType(int typeId) {
        if (typeId < 0 || typeId > mKeyTypeList.size()) return null;
        return mKeyTypeList.elementAt(typeId);
    }

    public void addDefaultKeyIcons(int keyCode, Drawable icon,
            Drawable iconPopup) {
        if (null == icon || null == iconPopup) return;

        KeyIconRecord iconRecord = new KeyIconRecord();
        iconRecord.icon = icon;
        iconRecord.iconPopup = iconPopup;
        iconRecord.keyCode = keyCode;

        int size = mKeyIconRecords.size();
        int pos = 0;
        while (pos < size) {
            if (mKeyIconRecords.get(pos).keyCode >= keyCode) break;
            pos++;
        }
        mKeyIconRecords.add(pos, iconRecord);
    }

    public Drawable getDefaultKeyIcon(int keyCode) {
        int size = mKeyIconRecords.size();
        int pos = 0;
        while (pos < size) {
            KeyIconRecord iconRecord = mKeyIconRecords.get(pos);
            if (iconRecord.keyCode < keyCode) {
                pos++;
                continue;
            }
            if (iconRecord.keyCode == keyCode) {
                return iconRecord.icon;
            }
            return null;
        }
        return null;
    }

    public Drawable getDefaultKeyIconPopup(int keyCode) {
        int size = mKeyIconRecords.size();
        int pos = 0;
        while (pos < size) {
            KeyIconRecord iconRecord = mKeyIconRecords.get(pos);
            if (iconRecord.keyCode < keyCode) {
                pos++;
                continue;
            }
            if (iconRecord.keyCode == keyCode) {
                return iconRecord.iconPopup;
            }
            return null;
        }
        return null;
    }

    public void addDefaultKey(int keyId, SoftKey softKey) {
        if (null == softKey) return;

        KeyRecord keyRecord = new KeyRecord();
        keyRecord.keyId = keyId;
        keyRecord.softKey = softKey;

        int size = mKeyRecords.size();
        int pos = 0;
        while (pos < size) {
            if (mKeyRecords.get(pos).keyId >= keyId) break;
            pos++;
        }
        mKeyRecords.add(pos, keyRecord);
    }

    public SoftKey getDefaultKey(int keyId) {
        int size = mKeyRecords.size();
        int pos = 0;
        while (pos < size) {
            KeyRecord keyRecord = mKeyRecords.get(pos);
            if (keyRecord.keyId < keyId) {
                pos++;
                continue;
            }
            if (keyRecord.keyId == keyId) {
                return keyRecord.softKey;
            }
            return null;
        }
        return null;
    }
}


class SoftKeyType {
    public static final int KEYTYPE_ID_NORMAL_KEY = 0;

    public int mKeyTypeId;
    public Drawable mKeyBg;
    public Drawable mKeyHlBg;
    public int mColor;
    public int mColorHl;
    public int mColorBalloon;

    SoftKeyType(int id, Drawable bg, Drawable hlBg) {
        mKeyTypeId = id;
        mKeyBg = bg;
        mKeyHlBg = hlBg;
    }

    public void setColors(int color, int colorHl, int colorBalloon) {
        mColor = color;
        mColorHl = colorHl;
        mColorBalloon = colorBalloon;
    }
}
