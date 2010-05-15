/*
 * ioprio-glue.c - I/O priority wrappers for the big dog
 *
 * Robert Love	<rml@novell.com>
 *
 * Copyright (C) 2005 Novell, Inc.
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


#include <sys/syscall.h>
#include <unistd.h>

#if defined(__i386__)
#define __NR_ioprio_set		289
#define __NR_ioprio_get		290
#elif defined(__powerpc__) || defined(__powerpc64__)
#define __NR_ioprio_set		273
#define __NR_ioprio_get		274
#elif defined(__x86_64__)
#define __NR_ioprio_set		251
#define __NR_ioprio_get		252
#elif defined(__ia64__)
#define __NR_ioprio_set		1274
#define __NR_ioprio_get		1275
#elif defined(__alpha__)
#define __NR_ioprio_set		442
#define __NR_ioprio_get		443
#elif defined(__s390x__) || defined(__s390__)
#define __NR_ioprio_set		282
#define __NR_ioprio_get		283
#elif defined(__SH4__)
#define __NR_ioprio_set		288
#define __NR_ioprio_get		289
#elif defined(__SH5__)
#define __NR_ioprio_set		316
#define __NR_ioprio_get		317
#elif defined(__sparc__) || defined(__sparc64__)
#define __NR_ioprio_set		196
#define __NR_ioprio_get		218
#elif defined(__arm__)
#define __NR_ioprio_set		314
#define __NR_ioprio_get		315
#else
#error "Unsupported architecture!"
#endif

static inline int ioprio_set (int which, int who, int ioprio)
{
	return syscall (__NR_ioprio_set, which, who, ioprio);
}

static inline int ioprio_get (int which, int who)
{
	return syscall (__NR_ioprio_get, which, who);
}

enum {
	IOPRIO_CLASS_NONE,
	IOPRIO_CLASS_RT,
	IOPRIO_CLASS_BE,
	IOPRIO_CLASS_IDLE,
};

enum {
	IOPRIO_WHO_PROCESS = 1,
	IOPRIO_WHO_PGRP,
	IOPRIO_WHO_USER,
};

#define IOPRIO_CLASS_SHIFT	13
#define IOPRIO_PRIO_MASK	0xff

int set_io_priority_idle (void)
{
	int ioprio, ioclass;

	ioprio = 7; /* priority is ignored with idle class */
	ioclass = IOPRIO_CLASS_IDLE << IOPRIO_CLASS_SHIFT;

	return ioprio_set (IOPRIO_WHO_PROCESS, 0, ioprio | ioclass);
}

int set_io_priority_best_effort (int ioprio)
{
	int ioclass;

	ioclass = IOPRIO_CLASS_BE << IOPRIO_CLASS_SHIFT;

	return ioprio_set (IOPRIO_WHO_PROCESS, 0, ioprio | ioclass);
}
