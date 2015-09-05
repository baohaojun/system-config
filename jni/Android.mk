LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_LDLIBS := -llog

LOCAL_MODULE    := t1wrench-jni
LOCAL_SRC_FILES := t1wrench-jni.cpp

include $(BUILD_SHARED_LIBRARY)
