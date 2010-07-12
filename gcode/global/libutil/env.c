/*
 * Copyright (c) 2003, 2005 Tama Communications Corporation
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
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef HAVE_HOME_ETC_H
#include <home_etc.h>
#endif

#include "die.h"
#include "env.h"
#include "strbuf.h"

extern char **environ;

/*
 * set_env: put environment variable.
 *
 *	i)	var	environment variable
 *	i)	val	value
 */
void
set_env(const char *var, const char *val)
{
/*
 * sparc-sun-solaris2.6 doesn't have setenv(3).
 */
#ifdef HAVE_PUTENV
	STRBUF *sb = strbuf_open(0);

	strbuf_sprintf(sb, "%s=%s", var, val);
	putenv(strbuf_value(sb));
	/* Don't free memory. putenv(3) require it. */
#else
	setenv(var, val, 1);
#endif
}
/*
 * get_home_directory: get environment dependent home directory.
 *
 *	r)	home directory
 */
char *
get_home_directory(void)
{
#ifdef HAVE_HOME_ETC_H
	return _HEdir;
#else
	return getenv("HOME");
#endif
}

/*
 * env_size: calculate the size of area used by environment.
 */
int
env_size(void)
{
	char **e;
	int size = 0;

	for (e = environ; *e != NULL; e++)
		size += strlen(*e) + 1;

	return size;
}
