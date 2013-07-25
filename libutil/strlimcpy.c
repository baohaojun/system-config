/*
 * Copyright (c) 2002 Tama Communications Corporation
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
#include "die.h"
#include "strlimcpy.h"

/*
 * strlimcpy: copy string with limit.
 *
 *	o)	dest	destination string
 *	i)	source	source string
 *	i)	limit	size of dest
 *
 * NOTE: This function is similar to strlcpy of OpenBSD but is different
 * because strlimcpy abort when it beyond the limit.
 */
void
strlimcpy(char *dest, const char *const source, const int limit)
{
	int n = (int)limit;
	const char *s = source;

	while (n--)
		if (!(*dest++ = *s++))
			return;
	die("buffer overflow. strlimcpy(dest, '%s', %d).", source, limit);
}
