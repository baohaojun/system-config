LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_LDLIBS := -llog

LOCAL_MODULE    := wrench-jni
LOCAL_SRC_FILES := wrench-jni.cpp

include $(BUILD_SHARED_LIBRARY)
