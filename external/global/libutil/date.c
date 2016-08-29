/*
 * Copyright (c) 2004 Tama Communications Corporation
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
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#include <time.h>
#include "die.h"
#include "strlimcpy.h"
#include "date.h"
/*
 * now: current date and time
 *
 *	r)		date and time
 */
const char *
now(void)
{
	static char buf[128];

#ifdef HAVE_STRFTIME
	time_t tval;

	if (time(&tval) == -1)
		die("cannot get current time.");
	(void)strftime(buf, sizeof(buf), "%a %b %d %H:%M:%S %Z %Y", localtime(&tval));
#else
	FILE *ip;

	strlimcpy(buf, "unknown time", sizeof(buf));
	if ((ip = popen("date", "r")) != NULL) {
		if (fgets(buf, sizeof(buf), ip))
			buf[strlen(buf) - 1] = 0;
		pclose(ip);
	}
#endif
	return buf;
}
