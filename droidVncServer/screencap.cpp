/*
 * Copyright (C) 2010 The Android Open Source Project
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

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include <linux/fb.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

#include <binder/ProcessState.h>

#include <gui/SurfaceComposerClient.h>
#include <gui/ISurfaceComposer.h>

#include <ui/DisplayInfo.h>
#include <ui/PixelFormat.h>

// TODO: Fix Skia.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#include <SkImageEncoder.h>
#include <SkData.h>
#pragma GCC diagnostic pop

using namespace android;

static uint32_t DEFAULT_DISPLAY_ID = ISurfaceComposer::eDisplayIdMain;

static void usage(const char* pname)
{
    fprintf(stderr,
            "usage: %s [-hp] [-d display-id] [FILENAME]\n"
            "   -h: this message\n"
            "   -p: save the file as a png.\n"
            "   -j: save the file as a jpg.\n"
            "   -d: specify the display id to capture, default %d.\n"
            "   -i: specify minimum layer\n"
            "   -a: specify maximum layer\n"
            "   -x: specify the left\n"
            "   -y: specify the top\n"
            "   -X: specify the right\n"
            "   -Y: specify the bottom\n"
            "If FILENAME ends with .png it will be saved as a png.\n"
            "If FILENAME ends with .jpg it will be saved as a jpg.\n"
            "If FILENAME is not given, the results will be printed to stdout.\n",
            pname, DEFAULT_DISPLAY_ID
    );
}

static SkColorType flinger2skia(PixelFormat f)
{
    switch (f) {
        case PIXEL_FORMAT_RGB_565:
            return kRGB_565_SkColorType;
        default:
            return kN32_SkColorType;
    }
}

static status_t notifyMediaScanner(const char* fileName) {
    String8 cmd("am broadcast -a android.intent.action.MEDIA_SCANNER_SCAN_FILE -d file://");
    String8 fileUrl("\"");
    fileUrl.append(fileName);
    fileUrl.append("\"");
    cmd.append(fileName);
    cmd.append(" > /dev/null");
    int result = system(cmd.string());
    if (result < 0) {
        fprintf(stderr, "Unable to broadcast intent for media scanner.\n");
        return UNKNOWN_ERROR;
    }
    return NO_ERROR;
}

template <typename T> void bhj_swap(T& a, T& b)
{
    T t;
    t = a;
    a = b;
    b = t;
}

int main(int argc, char** argv)
{
    ProcessState::self()->startThreadPool();

    const char* pname = argv[0];
    bool png = false;
    bool jpg = false;
    int x = 0;
    int y = 0;
    int X = 0;
    int Y = 0;
    int32_t displayId = DEFAULT_DISPLAY_ID;
    int c;
    uint32_t minLayer = 0;
    uint32_t maxLayer = -1U;
    uint32_t sticky_status_bar_height = 0;
    while ((c = getopt(argc, argv, "jphd:i:a:x:y:X:Y:s:")) != -1) {
        switch (c) {
        case 'p':
            png = true;
            break;
        case 'j':
            jpg = true;
            break;

        case 'd':
            displayId = atoi(optarg);
            break;
        case 'i':
            fprintf(stderr, "i = %s\n", optarg);
            minLayer = atoi(optarg);
            break;
        case 'a':
            fprintf(stderr, "a = %s\n", optarg);
            maxLayer = atoi(optarg);
            break;
        case 'x':
            fprintf(stderr, "x = %s\n", optarg);
            x = atoi(optarg);
            break;
        case 'y':
            y = atoi(optarg);
            break;
        case 'X':
            X = atoi(optarg);
            break;
        case 'Y':
            Y = atoi(optarg);
            break;
        case 's':
            sticky_status_bar_height = atoi(optarg);
            break;
        case '?':
        case 'h':
            usage(pname);
            return 1;
        }
    }
    argc -= optind;
    argv += optind;
    fprintf(stderr, "minLayer = %u, maxLayer = %u\n", minLayer, maxLayer);
    int fd = -1;
    const char* fn = NULL;
    if (argc == 0) {
        fd = dup(STDOUT_FILENO);
    } else if (argc == 1) {
        fn = argv[0];
        fd = open(fn, O_WRONLY | O_CREAT | O_TRUNC, 0664);
        if (fd == -1) {
            fprintf(stderr, "Error opening file: %s (%s)\n", fn, strerror(errno));
            return 1;
        }
        const int len = strlen(fn);
        if (len >= 4) {
            if (0 == strcmp(fn+len-4, ".png")) {
                png = true;
            } else if (0 == strcmp(fn+len-4, ".jpg")) {
                jpg = true;
            }
        }
    }

    if (fd == -1) {
        usage(pname);
        return 1;
    }

    void const* mapbase = MAP_FAILED;
    ssize_t mapsize = -1;

    void const* base = NULL;
    uint32_t w, s, h, f;
    size_t size = 0;

    // Maps orientations from DisplayInfo to ISurfaceComposer
    static const uint32_t ORIENTATION_MAP[] = {
        ISurfaceComposer::eRotateNone, // 0 == DISPLAY_ORIENTATION_0
        ISurfaceComposer::eRotate270, // 1 == DISPLAY_ORIENTATION_90
        ISurfaceComposer::eRotate180, // 2 == DISPLAY_ORIENTATION_180
        ISurfaceComposer::eRotate90, // 3 == DISPLAY_ORIENTATION_270
    };

    ScreenshotClient screenshot;
    sp<IBinder> display = SurfaceComposerClient::getBuiltInDisplay(displayId);
    if (display == NULL) {
        fprintf(stderr, "Unable to get handle for display %d\n", displayId);
        return 1;
    }

    Vector<DisplayInfo> configs;
    SurfaceComposerClient::getDisplayConfigs(display, &configs);
    int activeConfig = SurfaceComposerClient::getActiveConfig(display);
    if (static_cast<size_t>(activeConfig) >= configs.size()) {
        fprintf(stderr, "Active config %d not inside configs (size %zu)\n",
                activeConfig, configs.size());
        return 1;
    }
    uint8_t displayOrientation = configs[activeConfig].orientation;
    uint32_t captureOrientation = ORIENTATION_MAP[displayOrientation];

    uint32_t phone_w = configs[activeConfig].w;
    uint32_t phone_h = configs[activeConfig].h;

    uint32_t updateW = X - x;
    uint32_t updateH = Y - y;

    if (displayOrientation % 2 == 1) {
        if (displayOrientation == 1) {
            y = phone_w - y;
            Y = phone_w -Y;
            bhj_swap(y, Y);
        } else {
            x = phone_h - x;
            X = phone_h - X;
            bhj_swap(x, X);
        }
        bhj_swap(x, y);
        bhj_swap(X, Y);

        if (displayOrientation == 1) {
            y += sticky_status_bar_height;
            Y += sticky_status_bar_height;
        }
    }

    status_t result = screenshot.update(display, Rect(x, y, X, Y), updateW, updateH, minLayer, maxLayer,
            false, captureOrientation);
    if (result == NO_ERROR) {
        base = screenshot.getPixels();
        w = screenshot.getWidth();
        h = screenshot.getHeight();
        s = screenshot.getStride();
        f = screenshot.getFormat();
        size = screenshot.getSize();
        fprintf(stderr, "capture ok : x = %u, y = %u, X = %u, Y = %u, w = %u, h = %u, s = %u, f = %u, size = %lu, orientation: %u\n", x, y, X, Y, w, h, s, f, size, displayOrientation);
    }

    if (base != NULL) {
        if (png || jpg) {
            const SkImageInfo info = SkImageInfo::Make(w, h, flinger2skia(f),
                                                       kPremul_SkAlphaType);
            SkAutoTUnref<SkData> data(SkImageEncoder::EncodeData(info, base, s*bytesPerPixel(f),
                     png ? SkImageEncoder::kPNG_Type : SkImageEncoder::kJPEG_Type, SkImageEncoder::kDefaultQuality));
            if (data.get()) {
                write(fd, data->data(), data->size());
            }
            if (fn != NULL && !jpg) {
                notifyMediaScanner(fn);
            }
        } else {
            write(fd, &w, 4);
            write(fd, &h, 4);
            write(fd, &f, 4);
            size_t Bpp = bytesPerPixel(f);
            for (size_t y=0 ; y<h ; y++) {
                write(fd, base, w*Bpp);
                base = (void *)((char *)base + s*Bpp);
            }
        }
    }
    close(fd);
    if (mapbase != MAP_FAILED) {
        munmap((void *)mapbase, mapsize);
    }
    return 0;
}
