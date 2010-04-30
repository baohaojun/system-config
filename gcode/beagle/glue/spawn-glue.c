/*
 * spawn-glue.c: Functions for spawning processes with limits
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

#include <sys/time.h>
#include <sys/resource.h>

#include <glib.h>

/*
 * A specialized version of g_spawn_async_with_pipes that sets up rlimits.
 */

typedef struct {
	int cpu_limit;
	int mem_limit;
} LimitInfo;

static void limit_setup_func (gpointer user_data)
{
	LimitInfo *info = user_data;
	struct rlimit rlim;

	if (info->cpu_limit > 0) {
		rlim.rlim_cur = rlim.rlim_max = info->cpu_limit;
		setrlimit (RLIMIT_CPU, &rlim);
	}

	if (info->mem_limit > 0) {
		rlim.rlim_cur = rlim.rlim_max = info->mem_limit;
		setrlimit (RLIMIT_AS, &rlim);
	}
}	

void
spawn_async_with_pipes_and_limits (char   **argv,
				   char   **envp,
				   int      cpu_limit,
				   int      mem_limit,
				   GPid    *child_pid,
				   int     *stdin,
				   int     *stdout,
				   int     *stderr,
				   GError **error)
{
	LimitInfo info;
	GSpawnFlags flag = G_SPAWN_SEARCH_PATH;

	info.cpu_limit = cpu_limit;
	info.mem_limit = mem_limit;

	/* If stderr is null, that means SafeProcess has been asked
	 * to not redirect standard error.
	 * So ignore the error.
	 */
	if (stderr == NULL)
		flag |= G_SPAWN_STDERR_TO_DEV_NULL;

	g_spawn_async_with_pipes (NULL,
				  argv,
				  envp,
				  flag,
				  limit_setup_func,
				  &info,
				  child_pid,
				  stdin,
				  stdout,
				  stderr,
				  error);
}
