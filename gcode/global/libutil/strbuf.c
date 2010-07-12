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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <ctype.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "checkalloc.h"
#include "die.h"
#include "strbuf.h"

#ifndef isblank
#define isblank(c)	((c) == ' ' || (c) == '\t')
#endif
/*

String buffer: usage and memory status

					[xxx]: string buffer
					'v': current pointer

Function call                           Memory status
----------------------------------------------------------
                                        (not exist)
                                         v
sb = strbuf_open(0);                    []
                                          v
strbuf_putc(sb, 'a');                   [a]
                                          v
char *s = strbuf_value(sb);             [a\0]           s == "a"
                                            v
strbuf_puts(sb, "bc");                  [abc]
                                            v
char *s = strbuf_value(sb);             [abc\0]         s == "abc"
                                            v
int len = strbuf_getlen(sb);            [abc\0]         len == 3
                                         v
strbuf_reset(sb);                       [abc\0]
                                         v
int len = strbuf_getlen(sb);            [abc\0]         len == 0
                                           v
strbuf_puts(sb, "XY");                  [XYc\0]
                                           v
char *s = strbuf_value(sb);             [XY\0]          s == "XY"

fp = fopen("/etc/passwd", "r");                                             v
char *s = strbuf_fgets(sb, fp, 0)       [root:*:0:0:Charlie &:/root:/bin/csh\0]
fclose(fp)				s == "root:*:0:0:Charlie &:/root:/bin/csh"

strbuf_close(sb);                       (not exist)

*/

static void print_and_abort (void);
void (*strbuf_alloc_failed_handler) (void) = print_and_abort;

static void
print_and_abort(void)
{
	die("short of memory.");
}

/*
 * __strbuf_expandbuf: expand buffer so that afford to the length data at least.
 *
 *	i)	sb	STRBUF structure
 *	i)	length	required room
 */
void
__strbuf_expandbuf(STRBUF *sb, int length)
{
	int count = sb->curp - sb->sbuf;
	int newsize = sb->sbufsize + (length > EXPANDSIZE ? length : EXPANDSIZE);
	char *newbuf;

	if (sb->alloc_failed)
		return;
	newbuf = (char *)check_realloc(sb->sbuf, newsize + 1);
	sb->sbufsize = newsize;
	sb->sbuf = newbuf;

	sb->curp = sb->sbuf + count;
	sb->endp = sb->sbuf + sb->sbufsize;
}
/*
 * strbuf_open: open string buffer.
 *
 *	i)	init	initial buffer size
 *			if 0 is specified then use default value.
 *	r)	sb	STRBUF structure
 */
STRBUF *
strbuf_open(int init)
{
	STRBUF *sb = (STRBUF *)check_calloc(sizeof(STRBUF), 1);

	sb->sbufsize = (init > 0) ? init : INITIALSIZE;
	sb->sbuf = (char *)check_malloc(sb->sbufsize + 1);
	sb->curp = sb->sbuf;
	sb->endp = sb->sbuf + sb->sbufsize;

	return sb;
}
/*
 * strbuf_reset: reset string buffer.
 *
 *	i)	sb	string buffer
 */
void
strbuf_reset(STRBUF *sb)
{
	sb->curp = sb->sbuf;
	sb->alloc_failed = 0;
}
/*
 * strbuf_clear: clear static string buffer.
 *
 *	i)	sb	statically defined string buffer
 *
 * This function is used for the initializing of static string buffer.
 * For the detail, see 'STATIC_STRBUF(sb)' macro in strbuf.h.
 */
void
strbuf_clear(STRBUF *sb)
{
	if (sb == NULL)
		die("NULL string buffer. (strbuf_clear)");
	if (strbuf_empty(sb)) {
		sb->sbufsize = INITIALSIZE;
		sb->sbuf = (char *)check_malloc(sb->sbufsize + 1);
		sb->curp = sb->sbuf;
		sb->endp = sb->sbuf + sb->sbufsize;
	} else {
		strbuf_reset(sb);
	}
}
/*
 * strbuf_nputs: Put string with length
 *
 *	i)	sb	string buffer
 *	i)	s	string
 *	i)	len	length of string
 */
void
strbuf_nputs(STRBUF *sb, const char *s, int len)
{
	if (!sb->alloc_failed && len > 0) {
		if (sb->curp + len > sb->endp)
			__strbuf_expandbuf(sb, len);
		while (len-- > 0)
			*sb->curp++ = *s++;
	}
}
/*
 * strbuf_nputc: Put characters with length
 *
 *	i)	sb	string buffer
 *	i)	c	character
 *	i)	len	length of string
 */
void
strbuf_nputc(STRBUF *sb, int c, int len)
{
	if (!sb->alloc_failed && len > 0) {
		if (sb->curp + len > sb->endp)
			__strbuf_expandbuf(sb, len);
		while (len-- > 0)
			*sb->curp++ = c;
	}
}
/*
 * strbuf_puts: Put string
 *
 *	i)	sb	string buffer
 *	i)	s	string
 */
void
strbuf_puts(STRBUF *sb, const char *s)
{
	if (!sb->alloc_failed) {
		while (*s) {
			if (sb->curp >= sb->endp)
				__strbuf_expandbuf(sb, 0);
			*sb->curp++ = *s++;
		}
	}
}
/*
 * strbuf_puts_withterm: Put string until the terminator
 *
 *	i)	sb	string buffer
 *	i)	s	string
 *	i)	c	terminator
 *	r)		pointer to the terminator
 */
void
strbuf_puts_withterm(STRBUF *sb, const char *s, int c)
{
	if (!sb->alloc_failed) {
		while (*s && *s != c) {
			if (sb->curp >= sb->endp)
				__strbuf_expandbuf(sb, 0);
			*sb->curp++ = *s++;
		}
	}
}
/*
 * strbuf_puts_nl: Put string with a new line
 *
 *	i)	sb	string buffer
 *	i)	s	string
 */
void
strbuf_puts_nl(STRBUF *sb, const char *s)
{
	if (!sb->alloc_failed) {
		while (*s) {
			if (sb->curp >= sb->endp)
				__strbuf_expandbuf(sb, 0);
			*sb->curp++ = *s++;
		}
		if (sb->curp >= sb->endp)
			__strbuf_expandbuf(sb, 0);
		*sb->curp++ = '\n';
	}
}
/*
 * strbuf_putn: put digit string at the last of buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	n	number
 */
void
strbuf_putn(STRBUF *sb, int n)
{
	if (n == 0) {
		strbuf_putc(sb, '0');
	} else {
		char num[128];
		int i = 0;

		while (n) {
			if (i >= sizeof(num))
				die("Too big integer value.");
			num[i++] = n % 10 + '0';
			n = n / 10;
		}
		while (--i >= 0)
			strbuf_putc(sb, num[i]);
	}
}
/*
 * strbuf_unputc: remove specified char from the last of buffer
 *
 *	i)	sb	STRBUF structure
 *	i)	c	character
 *	r)		0: do nothing, 1: removed
 */
int
strbuf_unputc(STRBUF *sb, int c)
{
	if (sb->curp > sb->sbuf && *(sb->curp - 1) == c) {
		sb->curp--;
		return 1;
	}
	return 0;
}
/*
 * strbuf_value: return the content of string buffer.
 *
 *	i)	sb	STRBUF structure
 *	r)		string
 */
char *
strbuf_value(STRBUF *sb)
{
	*sb->curp = 0;
	return sb->sbuf;
}
/*
 * strbuf_trim: trim following blanks.
 *
 *	i)	sb	STRBUF structure
 */
void
strbuf_trim(STRBUF *sb)
{
	char *p = sb->curp;

	while (p > sb->sbuf && isblank(*(p - 1)))
		*--p = 0;
	sb->curp = p;
}
/*
 * strbuf_fgets: read whole record into string buffer
 *
 *	o)	sb	string buffer
 *	i)	ip	input stream
 *	i)	flags	flags
 *			STRBUF_NOCRLF	remove last '\n' if exist.
 *			STRBUF_APPEND	append next record to existing data
 *	r)		record buffer (NULL at end of file)
 *
 * Returned buffer has whole record.
 * The buffer end with '\0'.If STRBUF_NOCRLF is set then buffer doesn't
 * include '\r' and '\n'.
 */
char *
strbuf_fgets(STRBUF *sb, FILE *ip, int flags)
{
	if (!(flags & STRBUF_APPEND))
		strbuf_reset(sb);

	if (sb->curp >= sb->endp)
		__strbuf_expandbuf(sb, EXPANDSIZE);	/* expand buffer */
	if (sb->alloc_failed)
		return sb->sbuf;

	for (;;) {
		if (!fgets(sb->curp, sb->endp - sb->curp, ip)) {
			if (sb->curp == sb->sbuf)
				return NULL;
			break;
		}
		sb->curp += strlen(sb->curp);
		if (sb->curp > sb->sbuf && *(sb->curp - 1) == '\n')
			break;
		else if (feof(ip)) {
			return sb->sbuf;
		}
		__strbuf_expandbuf(sb, EXPANDSIZE);	/* expand buffer */
		if (sb->alloc_failed)
			return sb->sbuf;
	}
	if (flags & STRBUF_NOCRLF) {
		if (*(sb->curp - 1) == '\n')
			*(--sb->curp) = 0;
		if (sb->curp > sb->sbuf && *(sb->curp - 1) == '\r')
			*(--sb->curp) = 0;
	}
	return sb->sbuf;
}
/*
 * strbuf_sprintf: do sprintf into string buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	s	similar to sprintf()
 *			Currently the following format is supported.
 *			%s, %d, %<number>d, %<number>s, %-<number>d, %-<number>s
 */
void
strbuf_sprintf(STRBUF *sb, const char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	strbuf_vsprintf(sb, s, ap);
	va_end(ap);
}
/*
 * strbuf_vsprintf: do sprintf into string buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	s	similar to vsprintf()
 *			Currently the following format is supported.
 *			%s, %d, %<number>d, %<number>s, %-<number>d, %-<number>s
 */
void
strbuf_vsprintf(STRBUF *sb, const char *s, va_list ap)
{
	if (sb->alloc_failed)
		return;
	for (; *s; s++) {
		/*
		 * Put the before part of '%'.
		 */
		{
			const char *p;
			for (p = s; *p && *p != '%'; p++)
				;
			if (p > s) {
				strbuf_nputs(sb, s, p - s);
				s = p;
			}
		}
		if (*s == '\0')
			break;
		if (*s == '%') {
			int c = (unsigned char)*++s;
			/*
			 * '%%' means '%'.
			 */
			if (c == '%') {
				strbuf_putc(sb, c);
			}
			/*
			 * If the optional number is specified then
			 * we forward the job to snprintf(3).
			 * o %<number>d
			 * o %<number>s
			 * o %-<number>d
			 * o %-<number>s
			 */
			else if (isdigit(c) || (c == '-' && isdigit((unsigned char)*(s + 1)))) {
				char format[32], buf[1024];
				int i = 0;

				format[i++] = '%';
				if (c == '-')
					format[i++] = *s++;
				while (isdigit((unsigned char)*s))
					format[i++] = *s++;
				format[i++] = c = *s;
				format[i] = '\0';
				if (c == 'd' || c == 'x')
					snprintf(buf, sizeof(buf), format, va_arg(ap, int));
				else if (c == 's')
					snprintf(buf, sizeof(buf), format, va_arg(ap, char *));
				else
					die("Unsupported control character '%c'.", c);
				strbuf_puts(sb, buf);
			} else if (c == 's') {
				strbuf_puts(sb, va_arg(ap, char *));
			} else if (c == 'd') {
				strbuf_putn(sb, va_arg(ap, int));
			} else {
				die("Unsupported control character '%c'.", c);
			}
		}
	}
}
/*
 * strbuf_close: close string buffer.
 *
 *	i)	sb	STRBUF structure
 */
void
strbuf_close(STRBUF *sb)
{
	if (sb->name)
		(void)free(sb->name);
	(void)free(sb->sbuf);
	(void)free(sb);
}
/*
 * Temporary string buffer for general purpose.
 *
 * Usage:
 *
 *	STRBUF *sbt = strbuf_open_tempbuf();
 *	....
 *	strbuf_puts(sbtemp, "xxx");
 *	...
 *	strbuf_release_tempbuf(sbt);
 *
 */
int used = 0;

STRBUF *
strbuf_open_tempbuf(void)
{
	STATIC_STRBUF(sb);
	if (used)
		die("Internal error: temporary string buffer is already used.");
	used = 1;
	strbuf_clear(sb);
	return sb;
}
void
strbuf_release_tempbuf(STRBUF *sb)
{
	used = 0;
}
