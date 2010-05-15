/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/*
 * inotify-glue.c
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

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/inotify.h>

#define PROCFS_PREFIX           "/proc/sys/fs/inotify"

#define PROCFS_MAX_USER_DEVICES  PROCFS_PREFIX "/max_user_instances"
#define PROCFS_MAX_USER_WATCHES  PROCFS_PREFIX "/max_user_watches"
#define PROCFS_MAX_QUEUED_EVENTS PROCFS_PREFIX "/max_queued_events"

/* Inotify sysfs knobs, initialized to their pre-sysfs defaults */
static int max_user_instances = 8;
static int max_user_watches = 8192;
static int max_queued_events = 256;

static int snarf_cancellation_pipe [2];

/* Paranoid code to read an integer from a sysfs (well, any) file. */
static void
read_int (const char *filename, int *var)
{
	int fd, n;
	char buffer[32];
	char *buffer_endptr = NULL;

	fd = open (filename, O_RDONLY);
	if (fd == -1)
		return;
	if (read (fd, buffer, 31) > 0) {
		n = (int) strtol (buffer, &buffer_endptr, 10);
		if (*buffer != '\0' && *buffer_endptr == '\0')
			*var = n;
	}
	close (fd);
}


int
inotify_glue_init (void)
{
	static int fd = 0;

	if (fd)
		return fd;

	fd = inotify_init ();
	if (fd < 0)
		return -errno;

	if (pipe (snarf_cancellation_pipe) == -1)
		perror ("Can't create snarf_cancellation_pipe");
	
	read_int (PROCFS_MAX_USER_DEVICES, &max_user_instances);
	read_int (PROCFS_MAX_USER_WATCHES, &max_user_watches);
	read_int (PROCFS_MAX_QUEUED_EVENTS, &max_queued_events);

	return fd;
}


int
inotify_glue_watch (int fd, const char *filename, uint32_t mask)
{
	int wd;

	wd = inotify_add_watch (fd, filename, mask);
	if (wd < 0)
		return -errno;

	return wd;
}


int
inotify_glue_ignore (int fd, uint32_t wd)
{
	int ret;

	ret = inotify_rm_watch (fd, wd);
	if (ret < 0)
		return -errno;

	return ret;
}

void
inotify_snarf_cancel ()
{
	write (snarf_cancellation_pipe [1],
	       &snarf_cancellation_pipe, 1); // write a convenient byte
}


#define MAX_PENDING_COUNT		5
#define PENDING_PAUSE_NANOSECONDS	2000000
#define PENDING_THRESHOLD(qsize)	((unsigned int) (qsize) >> 1)
#define PENDING_MARGINAL_COST(p)	((unsigned int) (1 << (p)))

void
inotify_snarf_events (int fd, int *nr, void **buffer_out)
{
	struct pollfd pollfd [2]  = { { fd, POLLIN | POLLPRI, 0 }, { snarf_cancellation_pipe [0], POLLIN, 0} };
	unsigned int prev_pending = 0, pending_count = 0;
	static struct inotify_event *buffer = NULL;
	static size_t buffer_size;
	int ret;

	/* Allocate our buffer the first time we try to read events. */
	if (buffer == NULL) {
		/* guess the avg len */
		buffer_size = sizeof (struct inotify_event) + 16;
		buffer_size *= max_queued_events;
		buffer = malloc (buffer_size);
		if (!buffer) {
			perror ("malloc");
			*buffer_out = NULL;
			return;
		}
	}

	/* Set nr to 0, so it will be sure to contain something
	   valid if the poll times out. */
	*nr = 0;

	/* Wait for the file descriptor to be ready to read. */
	ret = poll (pollfd, 2, -1);
	if (ret == -1) {
		if (errno != EINTR)
			perror ("poll");
		return;
	} else if (ret == 0)
		return;

	/* Return immediately if something happened on the
	   snarf cancellation pipe. */
	if (pollfd [1].revents != 0)
		return;

	/* Reading events in groups significantly helps performance.
	 * If there are some events (but not too many!) ready, wait a
	 * bit more to see if more events come in. */

	while (pending_count < MAX_PENDING_COUNT) {
		struct timespec ts = {0, PENDING_PAUSE_NANOSECONDS};
		unsigned int pending;

		if (ioctl (fd, FIONREAD, &pending) == -1)
			break;

		/* Don't wait if the number of pending events is too close
		 * to the maximum queue size. */
		pending /= sizeof (struct inotify_event) + 16;		
		if (pending > PENDING_THRESHOLD (max_queued_events))
			break;

		/* With each successive iteration, the minimum rate for
		 * further sleep doubles. */
		if (pending-prev_pending < PENDING_MARGINAL_COST(pending_count))
			break;

		prev_pending = pending;
		++pending_count;

		nanosleep (&ts, NULL);
	}

	*nr = read (fd, buffer, buffer_size);

	*buffer_out = buffer;
}
