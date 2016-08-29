/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2008
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

#ifndef _PATH_H_
#define _PATH_H_

#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
#include <unistd.h>
#endif

/*
 * PATHSEP - Define OS-specific directory and path seperators
 */
#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
#define PATHSEP ";"
#else
#define PATHSEP ":"
#endif

#define isdrivechar(x) (((x) >= 'A' && (x) <= 'Z') || ((x) >= 'a' && (x) <= 'z'))

int isabspath(const char *);
char *canonpath(char *);
#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
char *realpath(const char *, char *);
#endif
int makedirectories(const char *, const char *, int);

#endif /* ! _PATH_H_ */
