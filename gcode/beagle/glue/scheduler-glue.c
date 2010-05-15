/*
 * scheduler-glue.c: Functions for setting Linux scheduler policies.
 *
 * Copyright (C) 2007 Novell, Inc.
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

#define _GNU_SOURCE
#include <sched.h>

#ifndef SCHED_BATCH
#warning SCHED_BATCH is not defined; it probably will not work.
#define SCHED_BATCH 3 // see /usr/include/bits/sched.h
#endif

int set_scheduler_policy_batch (void)
{
	struct sched_param param;

	param.sched_priority = 0;

	return sched_setscheduler (0, SCHED_BATCH, &param);
}

int set_scheduler_policy_other (void)
{
	struct sched_param param;

	param.sched_priority = 0;

	return sched_setscheduler (0, SCHED_OTHER, &param);
}
