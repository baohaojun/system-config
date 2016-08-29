/*
 * Copyright (c) 2009
 *	Tama Communications Corporation
 *
 * This file is part of GNU GLOBAL.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#elif HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "checkalloc.h"
#include "die.h"
#include "queue.h"
#include "statistics.h"
#include "strbuf.h"

#if !defined(timeradd)
#define timeradd(a, b, r) do {					\
	(r)->tv_sec = (a)->tv_sec + (b)->tv_sec;		\
	(r)->tv_usec = (a)->tv_usec + (b)->tv_usec;		\
	if ((r)->tv_usec >= 1000000) {				\
		(r)->tv_sec++;					\
		(r)->tv_usec -= 1000000;			\
	}							\
} while (0)
#endif

#if !defined(timersub)
#define timersub(a, b, r) do {					\
	(r)->tv_sec = (a)->tv_sec - (b)->tv_sec;		\
	(r)->tv_usec = (a)->tv_usec - (b)->tv_usec;		\
	if ((r)->tv_usec < 0) {					\
		(r)->tv_sec--;					\
		(r)->tv_usec += 1000000;			\
	}							\
} while (0)
#endif

#if HAVE_GETTIMEOFDAY
typedef struct timeval ELAPSED_TIME_TYPE;
#define GET_ELAPSED_TIME(p)	gettimeofday(p, NULL)
#define SUB_ELAPSED_TIME(a, b, r) do {				\
	struct timeval diff;					\
	timersub(a, b, &diff);					\
	*(r) = diff.tv_sec + diff.tv_usec * 1e-6;		\
} while (0)
#if HAVE_GETRUSAGE
typedef struct timeval CPU_TIME_TYPE;
#define GET_CPU_TIME(pu, ps) do {				\
	struct rusage self, children;				\
	getrusage(RUSAGE_SELF, &self);				\
	getrusage(RUSAGE_CHILDREN, &children);			\
	timeradd(&self.ru_utime, &children.ru_utime, pu);	\
	timeradd(&self.ru_stime, &children.ru_stime, ps);	\
} while (0)
#define SUB_CPU_TIME		SUB_ELAPSED_TIME
#define CPU_TIME_AVAILABLE	1
#else
#define CPU_TIME_AVAILABLE	0
#endif
#else
typedef time_t ELAPSED_TIME_TYPE;
#define GET_ELAPSED_TIME(p)	time(p)
#define SUB_ELAPSED_TIME(a, b, r)	(*(r) = *(a) - *(b))
#define CPU_TIME_AVAILABLE	0
#endif

struct statistics_time {
	STAILQ_ENTRY(statistics_time) next;

	ELAPSED_TIME_TYPE elapsed_start;
	double elapsed;		/* Elapsed time in seconds. */

#if CPU_TIME_AVAILABLE
	CPU_TIME_TYPE user_start;
	CPU_TIME_TYPE system_start;
	double user;		/* User time in seconds. */
	double system;		/* System time in seconds. */
	double percent;		/* (user + system) * 100 / elapsed */
				/* percent may be NaN or infinity. */
#endif

	int name_len;
	char name[1];
};

static STRBUF *sb;
static STATISTICS_TIME *T_all;
static STAILQ_HEAD(statistics_time_list, statistics_time)
	statistics_time_list = STAILQ_HEAD_INITIALIZER(statistics_time_list);

void
init_statistics(void)
{
	assert(sb == NULL);
	sb = strbuf_open(0);
	T_all = statistics_time_start("The entire time");
}

STATISTICS_TIME *
statistics_time_start(const char *fmt, ...)
{
	STATISTICS_TIME *t;
	va_list ap;

	strbuf_reset(sb);

	va_start(ap, fmt);
	strbuf_vsprintf(sb, fmt, ap);
	va_end(ap);

	t = check_malloc(offsetof(STATISTICS_TIME, name) + strbuf_getlen(sb) + 1);

	t->name_len = strbuf_getlen(sb);
	strcpy(t->name, strbuf_value(sb));

	GET_ELAPSED_TIME(&t->elapsed_start);

#if CPU_TIME_AVAILABLE
	GET_CPU_TIME(&t->user_start, &t->system_start);
#endif

	return t;
}

void
statistics_time_end(STATISTICS_TIME *t)
{
	ELAPSED_TIME_TYPE elapsed_end;
#if CPU_TIME_AVAILABLE
	CPU_TIME_TYPE user_end;
	CPU_TIME_TYPE system_end;
#endif

	GET_ELAPSED_TIME(&elapsed_end);
	SUB_ELAPSED_TIME(&elapsed_end, &t->elapsed_start, &t->elapsed);

#if CPU_TIME_AVAILABLE
	GET_CPU_TIME(&user_end, &system_end);
	SUB_CPU_TIME(&user_end, &t->user_start, &t->user);
	SUB_CPU_TIME(&system_end, &t->system_start, &t->system);

	t->percent = (t->elapsed == 0) ? (
#if defined(NAN)
		(t->user + t->system == 0) ? NAN :
#endif
#if defined(INFINITY)
		INFINITY
#else
		HUGE_VAL
#endif
		) : ((t->user + t->system) / t->elapsed * 100);
#endif

	STAILQ_INSERT_TAIL(&statistics_time_list, t, next);
}

struct printing_width {
	int name;
	int elapsed;
#if CPU_TIME_AVAILABLE
	int user;
	int system;
	int percent;
#endif
};

static int
decimal_width(unsigned long num)
{
	int width = 1;

	while (num >= 10) {
		num /= 10;
		width++;
	}

	return width;
}

#define ELAPSED_PRECISION	3
#define USER_PRECISION		3
#define SYSTEM_PRECISION	3
#define PERCENT_PRECISION	1

#define STR(x)			#x
#define XSTR(x)			STR(x)
#define PRECISION_STRING(x)	XSTR(x##_PRECISION)

static void
get_max_width(struct printing_width *max_width)
{
	const STATISTICS_TIME *t;
	int w;
#if CPU_TIME_AVAILABLE
	char buf[64];
#endif

	STAILQ_FOREACH(t, &statistics_time_list, next) {
		if (t->name_len > max_width->name)
			max_width->name = t->name_len;

		w = decimal_width(t->elapsed) + ELAPSED_PRECISION + 1;
		if (w > max_width->elapsed)
			max_width->elapsed = w;

#if CPU_TIME_AVAILABLE
		w = decimal_width(t->user) + USER_PRECISION + 1;
		if (w > max_width->user )
			max_width->user = w;

		w = decimal_width(t->system) + SYSTEM_PRECISION + 1;
		if (w > max_width->system)
			max_width->system = w;

		/*
		 * Printing style of NaN and infinity is implementation-defined.
		 * Therefore, it is impossible to know printing width without calling snprintf.
		 */
		w = snprintf(buf, sizeof(buf), "%." PRECISION_STRING(PERCENT) "f", t->percent);
		if (w > max_width->percent)
			max_width->percent = w;
#endif
	}
}

#define MIN_DOTS_LEN		3

static void
print_header_list(void **ppriv)
{
	struct printing_width max_width;
	char *dots;

	memset(&max_width, 0, sizeof(max_width));
	get_max_width(&max_width);
	*ppriv = dots = check_malloc(sizeof(max_width) + max_width.name + MIN_DOTS_LEN + 1);
	memcpy(dots, &max_width, sizeof(max_width));
	dots += sizeof(max_width);
	memset(dots, '.', max_width.name + MIN_DOTS_LEN);
	dots[max_width.name + MIN_DOTS_LEN] = '\0';

	setverbose();
}

static void
print_time_list(const STATISTICS_TIME *t, void *priv)
{
	const struct printing_width *max_width = priv;
	const char *dots = (const char *)&max_width[1];

#if CPU_TIME_AVAILABLE
	message("- %s %s"
		" user %*." PRECISION_STRING(USER) "fs"
		" system %*." PRECISION_STRING(SYSTEM) "fs"
		" elapsed %*." PRECISION_STRING(ELAPSED) "fs"
		" %*." PRECISION_STRING(PERCENT) "f%%",
		t->name, dots + t->name_len,
		max_width->user, t->user,
		max_width->system, t->system,
		max_width->elapsed, t->elapsed,
		max_width->percent, t->percent);
#else
	message("- %s %s"
		" elapsed %*." PRECISION_STRING(ELAPSED) "fs",
		t->name, dots + t->name_len,
		max_width->elapsed, t->elapsed);
#endif
}

static const char name_heading_string[] = "period";
static const char elapsed_heading_string[] = "elapsed[sec]";
#if CPU_TIME_AVAILABLE
static const char user_heading_string[] = "user[sec]";
static const char system_heading_string[] = "system[sec]";
static const char percent_heading_string[] = "%CPU";
#endif

static void
print_header_table(void **ppriv)
{
	struct printing_width max_width;
	char *bar;
	int bar_len;

	max_width.name = sizeof(name_heading_string) - 1;
	max_width.elapsed = sizeof(elapsed_heading_string) - 1;
#if CPU_TIME_AVAILABLE
	max_width.user = sizeof(user_heading_string) - 1;
	max_width.system = sizeof(system_heading_string) - 1;
	max_width.percent = sizeof(percent_heading_string) - 1;
#endif
	get_max_width(&max_width);

	bar_len = (max_width.name > max_width.elapsed)
		? max_width.name : max_width.elapsed;
#if CPU_TIME_AVAILABLE
	if (max_width.user > bar_len)
		bar_len = max_width.user;
	if (max_width.system > bar_len)
		bar_len = max_width.system;
	if (max_width.percent > bar_len)
		bar_len = max_width.percent;
#endif

	*ppriv = bar = check_malloc(sizeof(max_width) + bar_len + 1);
	memcpy(bar, &max_width, sizeof(max_width));
	bar += sizeof(max_width);
	memset(bar, '-', bar_len);
	bar[bar_len] = '\0';

	setverbose();

#if CPU_TIME_AVAILABLE
	message("%-*s %*s %*s %*s %*s",
		max_width.name, name_heading_string,
		max_width.user, user_heading_string,
		max_width.system, system_heading_string,
		max_width.elapsed, elapsed_heading_string,
		max_width.percent, percent_heading_string);
	message("%.*s %.*s %.*s %.*s %.*s",
		max_width.name, bar,
		max_width.user, bar,
		max_width.system, bar,
		max_width.elapsed, bar,
		max_width.percent, bar);
#else
	message("%-*s %*s",
		max_width.name, name_heading_string,
		max_width.elapsed, elapsed_heading_string);
	message("%.*s %.*s",
		max_width.name, bar,
		max_width.elapsed, bar);
#endif
}

static void
print_time_table(const STATISTICS_TIME *t, void *priv)
{
	const struct printing_width *max_width = priv;
	const char *bar = (const char *)&max_width[1];

	if (t == T_all) {
#if CPU_TIME_AVAILABLE
		message("%.*s %.*s %.*s %.*s %.*s",
			max_width->name, bar,
			max_width->user, bar,
			max_width->system, bar,
			max_width->elapsed, bar,
			max_width->percent, bar);
#else
		message("%.*s %.*s",
			max_width->name, bar,
			max_width->elapsed, bar);
#endif
	}

#if CPU_TIME_AVAILABLE
	message("%-*s"
		" %*." PRECISION_STRING(USER) "f"
		" %*." PRECISION_STRING(SYSTEM) "f"
		" %*." PRECISION_STRING(ELAPSED) "f"
		" %*." PRECISION_STRING(PERCENT) "f",
		max_width->name, t->name,
		max_width->user, t->user,
		max_width->system, t->system,
		max_width->elapsed, t->elapsed,
		max_width->percent, t->percent);
#else
	message("%-*s"
		" %*." PRECISION_STRING(ELAPSED) "f",
		max_width->name, t->name,
		max_width->elapsed, t->elapsed);
#endif
}

static void
print_footer_common(void *priv)
{
	free(priv);
}

struct printng_style {
	void (*print_header)(void **);
	void (*print_time)(const STATISTICS_TIME *, void *);
	void (*print_footer)(void *);
};

static const struct printng_style printing_styles[] = {
	/* STATISTICS_STYLE_NONE */
	{ NULL, NULL, NULL },
	/* STATISTICS_STYLE_LIST */
	{ print_header_list, print_time_list, print_footer_common },
	/* STATISTICS_STYLE_TABLE */
	{ print_header_table, print_time_table, print_footer_common },
};

#if !defined(ARRAY_SIZE)
#define ARRAY_SIZE(x)		(sizeof(x) / sizeof((x)[0]))
#endif

void
print_statistics(int style_no)
{
	const struct printng_style *style;
	STATISTICS_TIME *t;
	void *priv;

	assert(T_all != NULL);
	statistics_time_end(T_all);

	assert(style_no >= 0 && style_no < ARRAY_SIZE(printing_styles));
	style = &printing_styles[style_no];

	if (style->print_header != NULL)
		style->print_header(&priv);

	while (!STAILQ_EMPTY(&statistics_time_list)) {
		t = STAILQ_FIRST(&statistics_time_list);

		if (style->print_time != NULL)
			style->print_time(t, priv);

		STAILQ_REMOVE_HEAD(&statistics_time_list, next);
		free(t);
	}

	if (style->print_footer != NULL)
		style->print_footer(priv);

	strbuf_close(sb);
	T_all = NULL;
	sb = NULL;
}

