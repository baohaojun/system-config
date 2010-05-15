/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 * screensaver-glue.c
 *
 * Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
 * Copyright (C) 2004-2007 Novell, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <config.h>
#include <stdlib.h>

#ifdef HAVE_LIBXSS
#include <glib.h>

#include <X11/Xlib.h>
#include <X11/extensions/scrnsaver.h>

// Once an X-connection is eshtablished, if it breaks, the program terminates.
// So, we can safely store the DISPLAY once it is set and re-use it.
static Display *dsp = NULL;
#endif

#ifdef HAVE_LIBXSS
static gboolean
x_hangup_callback (GIOChannel *channel, GIOCondition cond, gpointer user_data)
{
    // This will cause the X connection to break and shut down the daemon.
    XEvent ignored;
    XNextEvent (dsp, &ignored);

    // Should never get here...
    return FALSE;
}
#endif

int
screensaver_glue_init ()
{
#ifdef HAVE_LIBXSS
    int x_fd;
    GIOChannel *channel;

    // screensaver_info is called only from the Scheduler thread; thus we dont need to enable XInitThreads()
    dsp = XOpenDisplay(getenv("DISPLAY"));

    if (dsp == NULL)
        return 0;

    x_fd = ConnectionNumber (dsp);

    if (x_fd < 0)
        return 0;

    channel = g_io_channel_unix_new (x_fd);
    g_io_add_watch (channel,
                    G_IO_HUP,
                    x_hangup_callback,
                    NULL);
    g_io_channel_unref (channel);

    return 1;
#else
    return 0;
#endif
}

int 
screensaver_info (int *state, int *kind, unsigned long *til_or_since, unsigned long *idle)
{
#ifdef HAVE_LIBXSS
    XScreenSaverInfo ss_info;
    int retval;
    static int inited = 0;
    int event_base, error_base;

    if(dsp == NULL) {
    	return 0;
    }
    
    if (XScreenSaverQueryExtension (dsp, &event_base, &error_base))
        retval = XScreenSaverQueryInfo (dsp, RootWindow(dsp, XDefaultScreen(dsp)), &ss_info);
    else
        retval = 0;
    
    if (retval != 0) {
        *state = ss_info.state;
        *kind = ss_info.kind;
        *til_or_since = ss_info.til_or_since;
        *idle = ss_info.idle;
        return 1;
    } else {
        return 0;
    }
#else
    return 0;
#endif
}


