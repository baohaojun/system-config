/*
  droid vnc server - Android VNC server
  Copyright (C) 2009 Jose Pereira <onaips@gmail.com>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "flinger.h"
#include "screenFormat.h"

#include <binder/IPCThreadState.h>
#include <binder/ProcessState.h>
#include <binder/IServiceManager.h>

#include <binder/IMemory.h>
#include <gui/ISurfaceComposer.h>
#include <gui/SurfaceComposerClient.h>

/*
 * Copyright (C) 2007 The Android Open Source Project
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

#include <ui/PixelFormat.h>
#include <hardware/hardware.h>

// ----------------------------------------------------------------------------
namespace android {



    struct PixelFormatInfo {
        enum {
            INDEX_ALPHA   = 0,
            INDEX_RED     = 1,
            INDEX_GREEN   = 2,
            INDEX_BLUE    = 3
        };

        enum { // components
            ALPHA   = 1,
            RGB     = 2,
            RGBA    = 3,
            L       = 4,
            LA      = 5,
            OTHER   = 0xFF
        };

        struct szinfo {
            uint8_t h;
            uint8_t l;
        };

        inline PixelFormatInfo() : version(sizeof(PixelFormatInfo)) { }
        size_t getScanlineSize(unsigned int width) const;
        size_t getSize(size_t ci) const {
            return (ci <= 3) ? (cinfo[ci].h - cinfo[ci].l) : 0;
        }
        size_t      version;
        PixelFormat format;
        size_t      bytesPerPixel;
        size_t      bitsPerPixel;
        union {
            szinfo      cinfo[4];
            struct {
                uint8_t     h_alpha;
                uint8_t     l_alpha;
                uint8_t     h_red;
                uint8_t     l_red;
                uint8_t     h_green;
                uint8_t     l_green;
                uint8_t     h_blue;
                uint8_t     l_blue;
            };
        };
        uint8_t     components;
        uint8_t     reserved0[3];
        uint32_t    reserved1;
    };

    status_t    getPixelFormatInfo(PixelFormat format, PixelFormatInfo* info);

// ----------------------------------------------------------------------------

    static const int COMPONENT_YUV = 0xFF;

    struct Info {
        size_t      size;
        size_t      bitsPerPixel;
        struct {
            uint8_t     ah;
            uint8_t     al;
            uint8_t     rh;
            uint8_t     rl;
            uint8_t     gh;
            uint8_t     gl;
            uint8_t     bh;
            uint8_t     bl;
        };
        uint8_t     components;
    };

    static Info const sPixelFormatInfos[] = {
        { 0,  0, { 0, 0,   0, 0,   0, 0,   0, 0 }, 0 },
        { 4, 32, {32,24,   8, 0,  16, 8,  24,16 }, PixelFormatInfo::RGBA },
        { 4, 24, { 0, 0,   8, 0,  16, 8,  24,16 }, PixelFormatInfo::RGB  },
        { 3, 24, { 0, 0,   8, 0,  16, 8,  24,16 }, PixelFormatInfo::RGB  },
        { 2, 16, { 0, 0,  16,11,  11, 5,   5, 0 }, PixelFormatInfo::RGB  },
        { 4, 32, {32,24,  24,16,  16, 8,   8, 0 }, PixelFormatInfo::RGBA },
        { 2, 16, { 1, 0,  16,11,  11, 6,   6, 1 }, PixelFormatInfo::RGBA },
        { 2, 16, { 4, 0,  16,12,  12, 8,   8, 4 }, PixelFormatInfo::RGBA },
        { 1,  8, { 8, 0,   0, 0,   0, 0,   0, 0 }, PixelFormatInfo::ALPHA},
        { 1,  8, { 0, 0,   8, 0,   8, 0,   8, 0 }, PixelFormatInfo::L    },
        { 2, 16, {16, 8,   8, 0,   8, 0,   8, 0 }, PixelFormatInfo::LA   },
        { 1,  8, { 0, 0,   8, 5,   5, 2,   2, 0 }, PixelFormatInfo::RGB  },
    };

    static const Info* gGetPixelFormatTable(size_t* numEntries) {
        if (numEntries) {
            *numEntries = sizeof(sPixelFormatInfos)/sizeof(Info);
        }
        return sPixelFormatInfos;
    }

// ----------------------------------------------------------------------------

    size_t PixelFormatInfo::getScanlineSize(unsigned int width) const
    {
        size_t size;
        if (components == COMPONENT_YUV) {
            // YCbCr formats are different.
            size = (width * bitsPerPixel)>>3;
        } else {
            size = width * bytesPerPixel;
        }
        return size;
    }

    status_t getPixelFormatInfo(PixelFormat format, PixelFormatInfo* info)
    {
        if (format <= 0)
            return BAD_VALUE;

        if (info->version != sizeof(PixelFormatInfo))
            return INVALID_OPERATION;

        // YUV format from the HAL are handled here
        switch (format) {
        case HAL_PIXEL_FORMAT_YCbCr_422_SP:
        case HAL_PIXEL_FORMAT_YCbCr_422_I:
            info->bitsPerPixel = 16;
            goto done;
        case HAL_PIXEL_FORMAT_YCrCb_420_SP:
        case HAL_PIXEL_FORMAT_YV12:
            info->bitsPerPixel = 12;
        done:
            info->format = format;
            info->components = COMPONENT_YUV;
            info->bytesPerPixel = 1;
            info->h_alpha = 0;
            info->l_alpha = 0;
            info->h_red = info->h_green = info->h_blue = 8;
            info->l_red = info->l_green = info->l_blue = 0;
            return NO_ERROR;
        }

        size_t numEntries;
        const Info *i = gGetPixelFormatTable(&numEntries) + format;
        bool valid = uint32_t(format) < numEntries;
        if (!valid) {
            return BAD_INDEX;
        }

        info->format = format;
        info->bytesPerPixel = i->size;
        info->bitsPerPixel  = i->bitsPerPixel;
        info->h_alpha       = i->ah;
        info->l_alpha       = i->al;
        info->h_red         = i->rh;
        info->l_red         = i->rl;
        info->h_green       = i->gh;
        info->l_green       = i->gl;
        info->h_blue        = i->bh;
        info->l_blue        = i->bl;
        info->components    = i->components;

        return NO_ERROR;
    }

// ----------------------------------------------------------------------------
}; // namespace android
// ----------------------------------------------------------------------------



using namespace android;

ScreenshotClient *screenshotClient=NULL;

extern "C" screenFormat getscreenformat_flinger()
{
    //get format on PixelFormat struct
    PixelFormat f=screenshotClient->getFormat();

    PixelFormatInfo pf;
    getPixelFormatInfo(f,&pf);

    screenFormat format;

    format.bitsPerPixel = pf.bitsPerPixel;
    format.width = screenshotClient->getWidth();
    format.pad = screenshotClient->getStride() - format.width;
    format.height =     screenshotClient->getHeight();
    format.size = pf.bitsPerPixel*format.width*format.height/CHAR_BIT;
    format.redShift = pf.l_red;
    format.redMax = pf.h_red;
    format.greenShift = pf.l_green;
    format.greenMax = pf.h_green-pf.h_red;
    format.blueShift = pf.l_blue;
    format.blueMax = pf.h_blue-pf.h_green;
    format.alphaShift = pf.l_alpha;
    format.alphaMax = pf.h_alpha-pf.h_blue;
    L("f is %d, pad is %d\n", f, format.pad);

    return format;
}

static uint32_t DEFAULT_DISPLAY_ID = ISurfaceComposer::eDisplayIdMain;
extern "C" int init_flinger()
{
    int errno;

    L("--Initializing gingerbread access method--\n");

    screenshotClient = new ScreenshotClient();
    sp<IBinder> display = SurfaceComposerClient::getBuiltInDisplay(DEFAULT_DISPLAY_ID);
    if (display == NULL)
        return -1;
    errno = screenshotClient->update(display, Rect(), true);
    if (errno != NO_ERROR) {
        return -1;
    }

    if (!screenshotClient->getPixels())
        return -1;

    return 0;
}

extern "C" unsigned int *readfb_flinger()
{
    sp<IBinder> display = SurfaceComposerClient::getBuiltInDisplay(DEFAULT_DISPLAY_ID);
    if (display == NULL)
        return NULL;
    if (screenshotClient->update(display, Rect(), true) != NO_ERROR) {
        return NULL;
    }
    return (unsigned int*)screenshotClient->getPixels();
}

extern "C" void close_flinger()
{
    free(screenshotClient);
}
