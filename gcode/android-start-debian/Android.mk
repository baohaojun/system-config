#
# Copyright (C) 2008 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This makefile shows how to build a shared library and an activity that
# bundles the shared library and calls it using JNI.

TOP_LOCAL_PATH:= $(call my-dir)

# Build activity

LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE_TAGS := optional

LOCAL_SRC_FILES := $(call all-subdir-java-files)

LOCAL_PACKAGE_NAME := MrvlTerm
LOCAL_CERTIFICATE := platform
LOCAL_JNI_SHARED_LIBRARIES := libmrvl-auto-test

include $(BUILD_PACKAGE)

include $(CLEAR_VARS)

ifneq ($(wildcard $(LOCAL_PATH)/products/$(TARGET_PRODUCT)/*),)

PRODUCT_COPY_FILES += \
 $(foreach script,$(wildcard $(LOCAL_PATH)/products/$(TARGET_PRODUCT)/*),$(script):system/lib/mrvl-auto-test/$(notdir $(script)))

else ifneq ($(wildcard $(LOCAL_PATH)/products/$(TARGET_DEVICE)/*),)

$(info you are using device test cases from $(TARGET_DEVICE), not product cases from $(TARGET_PRODUCT))
PRODUCT_COPY_FILES += \
 $(foreach script,$(wildcard $(LOCAL_PATH)/products/$(TARGET_DEVICE)/*),$(script):system/lib/mrvl-auto-test/$(notdir $(script)))

else

$(warning you have not set up your factory test cases!)
PRODUCT_COPY_FILES += \
 $(foreach script,$(wildcard $(LOCAL_PATH)/scripts/*),$(script):system/lib/mrvl-auto-test/$(notdir $(script)))

endif

PRODUCT_COPY_FILES += $(LOCAL_PATH)/mrvl-auto-test.sh:system/etc/mrvl-auto-test.sh

PRODUCT_COPY_FILES += \
 $(foreach config,$(wildcard $(LOCAL_PATH)/configs/*),$(config):system/lib/mrvl-auto-test-configs/$(notdir $(config)))

PRODUCT_COPY_FILES += $(LOCAL_PATH)/adbcompgen.sh:system/bin/adbcompgen.sh

ifeq ($(TARGET_BUILD_VARIANT),user)
PRODUCT_COPY_FILES += $(LOCAL_PATH)/default_factory_mode:data/default_factory_mode
endif

# ============================================================

# Also build all of the sub-targets under this one: the shared library.
include $(call all-makefiles-under,$(LOCAL_PATH))
