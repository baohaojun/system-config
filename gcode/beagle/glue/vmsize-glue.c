/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 * vmsize-glue.c
 *
 * Copyright (C) 2004 Novell, Inc.
 *
 */

/*
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

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

/*
  FIXME: It is not safe to call this function from multiple threads.
*/

int
get_vmsize (void)
{
    static char proc_filename[64] = {'\0'};
    static char buffer [1024];
    int fd;
    int vmsize = -1;

    if (proc_filename[0] == '\0')
        snprintf (proc_filename, 64, "/proc/%d/status", getpid ());

    fd = open (proc_filename, O_RDONLY);
    if (fd >= 0) {
        if (read (fd, buffer, sizeof (buffer)) > 0) {
            char *pos = strstr (buffer, "VmSize:");
            char *endpos = NULL;
            if (pos != NULL && strlen (pos) > 7) {
                pos += 7;
                while (*pos && isspace (*pos))
                    ++pos;
                if (*pos != '\0') {
                    vmsize = (int) strtol (pos, &endpos, 10);
                    if (pos == endpos || *endpos != ' ')
                        vmsize = -1;
                }
            }
        }
        close (fd);
    }

    return vmsize;
}

/* a stupid cut&paste */
int
get_vmrss (void)
{
    static char proc_filename[64] = {'\0'};
    static char buffer [1024];
    int fd;
    int vmsize = -1;

    if (proc_filename[0] == '\0')
        snprintf (proc_filename, 64, "/proc/%d/status", getpid ());

    fd = open (proc_filename, O_RDONLY);
    if (fd >= 0) {
        if (read (fd, buffer, sizeof (buffer)) > 0) {
            char *pos = strstr (buffer, "VmRSS:");
            char *endpos = NULL;
            if (pos != NULL && strlen (pos) > 7) {
                pos += 7;
                while (*pos && isspace (*pos))
                    ++pos;
                if (*pos != '\0') {
                    vmsize = (int) strtol (pos, &endpos, 10);
                    if (pos == endpos || *endpos != ' ')
                        vmsize = -1;
                }
            }
        }
        close (fd);
    }

    return vmsize;
}
