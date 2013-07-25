/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2002, 2005, 2006, 2010
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

#ifndef _STRBUF_H
#define _STRBUF_H

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
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

#define INITIALSIZE 80
#define EXPANDSIZE 80

/* for strbuf_fgets() */
#define STRBUF_APPEND	1
#define STRBUF_NOCRLF	2

typedef struct _strbuf {
	char *name;
	char *sbuf;
	char *endp;
	char *curp;
	int sbufsize;
	int alloc_failed;
} STRBUF;

/*
 * STATIC_STRBUF(sb):
 *
 * This macro is used for static string buffer which is suitable for
 * work area and(or) return value of function. The area allocated once
 * is repeatedly used though the area is never released.
 * You must call strbuf_clear(sb) every time before using.
 * You must not call strbuf_close(sb) for it.
 *
 * Usage:
 *      function(...) {
 *              STATIC_STRBUF(sb);
 *
 *              strbuf_clear(sb);
 *              ...
 *		strbuf_puts(sb, "xxxxx");
 *              ...
 *              return strbuf_value(sb);
 *      }
 */
#define STATIC_STRBUF(sb) static STRBUF sb[1]

#define strbuf_empty(sb) (sb->sbufsize == 0)

#define strbuf_putc(sb, c)	do {\
	if (!sb->alloc_failed) {\
		if (sb->curp >= sb->endp)\
			__strbuf_expandbuf(sb, 0);\
		*sb->curp++ = c;\
	}\
} while (0)

#define strbuf_puts0(sb, s) do {\
	strbuf_puts(sb, s);\
	strbuf_putc(sb, '\0');\
} while (0)

#define strbuf_getlen(sb) (sb->curp - sb->sbuf)
#define strbuf_setlen(sb, len) do {\
	unsigned int _length = len;\
	if (!sb->alloc_failed) {\
		if (_length < strbuf_getlen(sb))\
			sb->curp = sb->sbuf + _length;\
		else if (_length > strbuf_getlen(sb))\
			__strbuf_expandbuf(sb, _length - strbuf_getlen(sb));\
	}\
} while (0)
#define strbuf_lastchar(sb) (*(sb->curp - 1))

#ifdef DEBUG
void strbuf_dump(char *);
#endif
void __strbuf_expandbuf(STRBUF *, int);
STRBUF *strbuf_open(int);
void strbuf_reset(STRBUF *);
void strbuf_clear(STRBUF *);
void strbuf_nputs(STRBUF *, const char *, int);
void strbuf_nputc(STRBUF *, int, int);
void strbuf_puts(STRBUF *, const char *);
void strbuf_puts_withterm(STRBUF *, const char *, int);
void strbuf_puts_nl(STRBUF *, const char *);
void strbuf_putn(STRBUF *, int);
int strbuf_unputc(STRBUF *, int);
char *strbuf_value(STRBUF *);
void strbuf_trim(STRBUF *);
void strbuf_close(STRBUF *);
char *strbuf_fgets(STRBUF *, FILE *, int);
void strbuf_sprintf(STRBUF *, const char *, ...)
	__attribute__ ((__format__ (__printf__, 2, 3)));
void strbuf_vsprintf(STRBUF *, const char *, va_list)
	__attribute__ ((__format__ (__printf__, 2, 0)));
STRBUF *strbuf_open_tempbuf(void);
void strbuf_release_tempbuf(STRBUF *);

#endif /* ! _STRBUF_H */
