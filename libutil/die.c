/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2002, 2003
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "die.h"

static int quiet;
static int verbose;
static int debug;
static void (*exit_proc)(void);

void
setquiet(void)
{
	quiet = 1;
}
void
setverbose(void)
{
	verbose = 1;
}
void
setdebug(void)
{
	debug = 1;
}
void
sethandler(void (*proc)(void))
{
	exit_proc = proc;
}
void
die(const char *s, ...)
{
	va_list ap;

	if (!quiet) {
		fprintf(stderr, "%s: ", progname);
		va_start(ap, s);
		(void)vfprintf(stderr, s, ap);
		va_end(ap);
		fputs("\n", stderr);
	}
	if (exit_proc)
		(*exit_proc)();
	if (debug)
		abort();
	exit(1);
}

void
die_with_code(int n, const char *s, ...)
{
	va_list ap;

	if (!quiet) {
		fprintf(stderr, "%s: ", progname);
		va_start(ap, s);
		(void)vfprintf(stderr, s, ap);
		va_end(ap);
		fputs("\n", stderr);
	}
	if (exit_proc)
		(*exit_proc)();
	if (debug)
		abort();
	exit(n);
}
void
message(const char *s, ...)
{
	va_list ap;

	if (!quiet && verbose) {
		va_start(ap, s);
		(void)vfprintf(stderr, s, ap);
		va_end(ap);
		fputs("\n", stderr);
	}
}
void
warning(const char *s, ...)
{
	va_list ap;

	if (!quiet) {
		fputs("Warning: ", stderr);
		va_start(ap, s);
		(void)vfprintf(stderr, s, ap);
		va_end(ap);
		fputs("\n", stderr);
	}
}
