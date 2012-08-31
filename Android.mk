LOCAL_PATH:= $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE_TAGS := optional

LOCAL_SRC_FILES := \
         $(call all-subdir-java-files)

LOCAL_PACKAGE_NAME := PinyinIME

LOCAL_JNI_SHARED_LIBRARIES := libjni_pinyinime

LOCAL_REQUIRED_MODULES := libjni_pinyinime

LOCAL_STATIC_JAVA_LIBRARIES := com.android.inputmethod.pinyin.lib

LOCAL_CERTIFICATE := shared

LOCAL_PROGUARD_FLAG_FILES := proguard.cfg

# Make sure our dictionary file is not compressed, so we can read it with
# a raw file descriptor.
LOCAL_AAPT_FLAGS := -0 .dat
LOCAL_AAPT_FLAGS += -0 .dict

LOCAL_PROGUARD_FLAG_FILES := proguard.flags

include $(BUILD_PACKAGE)

MY_PATH := $(LOCAL_PATH)

include $(MY_PATH)/jni/Android.mk
include $(MY_PATH)/lib/Android.mk
