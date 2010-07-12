/*
 * Copyright (c) 2001 Tama Communications Corporation
 *
 * Contributed by Jason Hood <jadoxa@yahoo.com.au>, 2001.
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
#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
#include <stdlib.h>
#ifdef __DJGPP__
#include <sys/system.h>
#endif
#endif

#include "is_unixy.h"

/*
 * is_unixy: whether running in a unix-like shell or not
 *
 *	r)		1: unixy shell, 0: DOS shell (COMMAND.COM)
 */
int
is_unixy(void)
{
#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
	static int unix_shell = -1;

	if (unix_shell == -1) {
		char *s = getenv("SHELL");
#ifdef __DJGPP__
		/* Assume if SHELL isn't defined, COMSPEC is DOS. */
		unix_shell = (s == NULL) ? 0 : _is_unixy_shell(s);
#else
		unix_shell = (s != 0);
#endif
	}
	return unix_shell;
#else
	return 1;
#endif
}
