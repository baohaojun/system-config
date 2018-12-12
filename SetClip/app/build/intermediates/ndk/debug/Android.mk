LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE := wrench-jni
LOCAL_LDFLAGS := -Wl,--build-id
LOCAL_SRC_FILES := \
	/home/bhj/StudioProjects/SetClip/app/src/main/jni/Android.mk \
	/home/bhj/StudioProjects/SetClip/app/src/main/jni/wrench-jni.cpp \
	/home/bhj/StudioProjects/SetClip/app/src/main/jni/Application.mk \

LOCAL_C_INCLUDES += /home/bhj/StudioProjects/SetClip/app/src/debug/jni
LOCAL_C_INCLUDES += /home/bhj/StudioProjects/SetClip/app/src/main/jni

include $(BUILD_SHARED_LIBRARY)
