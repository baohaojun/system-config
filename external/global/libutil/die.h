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

#ifndef _DIE_H_
#define _DIE_H_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdarg.h>

#ifndef __attribute__
/* This feature is available in gcc versions 2.5 and later.  */
# if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 5) || __STRICT_ANSI__
#  define __attribute__(x)
# endif
/* The __-protected variants of `format' and `printf' attributes
   are accepted by gcc versions 2.6.4 (effectively 2.7) and later.  */
# if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
#  define __format__ format
#  define __printf__ printf
# endif
#endif

extern	const char *progname;

void setquiet(void);
void setverbose(void);
void setdebug(void);
void sethandler(void (*proc)(void));
void die(const char *s, ...)
	__attribute__ ((__noreturn__, __format__ (__printf__, 1, 2)));
void die_with_code(int n, const char *s, ...)
	__attribute__ ((__noreturn__, __format__ (__printf__, 2, 3)));
void message(const char *s, ...)
	__attribute__ ((__format__ (__printf__, 1, 2)));
void warning(const char *s, ...)
	__attribute__ ((__format__ (__printf__, 1, 2)));

#endif /* ! _DIE_H_ */
