/*
 * Copyright (c) 2005, 2006, 2010 Tama Communications Corporation
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
#include <ctype.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "abs2rel.h"
#include "checkalloc.h"
#include "die.h"
#include "format.h"
#include "gparam.h"
#include "gpathop.h"
#include "pathconvert.h"
#include "strbuf.h"
#include "strlimcpy.h"

static unsigned char encode[256];
static int encoding;

#define required_encode(c) encode[(unsigned char)c]
/*
 * set_encode_chars: stores chars to be encoded.
 */
void
set_encode_chars(const unsigned char *chars)
{
	unsigned int i;

	/* clean the table */
	memset(encode, 0, sizeof(encode));
	/* set bits */
	encoding = 0;
	for (i = 0; chars[i]; i++) {
		encode[(unsigned char)chars[i]] = 1;
		encoding = 1;
	}
	/* '%' is always encoded when encode is enable. */
	encode['%'] = 1;
}
#define outofrange(c)	(c < '0' || c > 'f')
#define h2int(c) (c >= 'a' ? c - 'a' : c - '0')
/*
 * decode_path: decode encoded path name.
 *
 *	i)	path	encoded path name
 *	r)		decoded path name
 */
char *
decode_path(const unsigned char *path)
{
	STATIC_STRBUF(sb);
	const unsigned char *p;

	if (strchr((const char *)path, '%') == NULL)
		return (char *)path;
	strbuf_clear(sb);
	for (p = path; *p; p++) {
		if (*p == '%') {
			unsigned char c1, c2;
			c1 = *++p;
			c2 = *++p;
			if (outofrange(c1) || outofrange(c2))
				die("decode_path: unexpected character. (%%%c%c)", c1, c2);
			strbuf_putc(sb, h2int(c1) * 16 + h2int(c2));
		} else
			strbuf_putc(sb, *p);
	}
	return strbuf_value(sb);
}
/*
 * Path filter for the output of global(1).
 */
static const char *
convert_pathname(CONVERT *cv, const char *path)
{
	static char buf[MAXPATHLEN];
	const char *a, *b;

	if (cv->type != PATH_THROUGH) {
		/*
		 * make absolute path name.
		 * 'path + 1' means skipping "." at the head.
		 */
		strbuf_setlen(cv->abspath, cv->start_point);
		strbuf_puts(cv->abspath, path + 1);
		/*
		 * print path name with converting.
		 */
		switch (cv->type) {
		case PATH_ABSOLUTE:
			path = strbuf_value(cv->abspath);
			break;
		case PATH_RELATIVE:
			a = strbuf_value(cv->abspath);
			b = cv->basedir;
#if defined(_WIN32) || defined(__DJGPP__)
			while (*a != '/')
				a++;
			while (*b != '/')
				b++;
#endif
			if (!abs2rel(a, b, buf, sizeof(buf)))
				die("abs2rel failed. (path=%s, base=%s).", a, b);
			path = buf;
			break;
		default:
			die("unknown path type.");
			break;
		}
	}
	/*
	 * encoding of the path name.
	 */
	if (encoding) {
		const char *p;
		int required = 0;

		for (p = path; *p; p++) {
			if (required_encode(*p)) {
				required = 1;
				break;
			}
		}
		if (required) {
			static char buf[MAXPATHLEN];
			char c[16] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
			char *q = buf;

			for (p = path; *p; p++) {
				if (required_encode(*p)) {
					*q++ = '%';
					*q++ = c[*p / 16];
					*q++ = c[*p % 16];
				} else
					*q++ = *p;
			}
			*q = '\0';
			path = buf;
		}
	}
	return (const char *)path;
}
/*
 * convert_open: open convert filter
 *
 *	i)	type	PATH_ABSOLUTE, PATH_RELATIVE, PATH_THROUGH
 *	i)	format	tag record format
 *	i)	root	root directory of source tree
 *	i)	cwd	current directory
 *	i)	dbpath	dbpath directory
 *	i)	op	output file
 */
CONVERT *
convert_open(int type, int format, const char *root, const char *cwd, const char *dbpath, FILE *op)
{
	CONVERT *cv = (CONVERT *)check_calloc(sizeof(CONVERT), 1);
	/*
	 * set base directory.
	 */
	cv->abspath = strbuf_open(MAXPATHLEN);
	strbuf_puts(cv->abspath, root);
	strbuf_unputc(cv->abspath, '/');
	cv->start_point = strbuf_getlen(cv->abspath);
	/*
	 * copy elements.
	 */
	if (strlen(cwd) > MAXPATHLEN)
		die("current directory name too long.");
	strlimcpy(cv->basedir, cwd, sizeof(cv->basedir));
	cv->type = type;
	cv->format = format;
	cv->op = op;
	/*
	 * open GPATH.
	 */
	if (gpath_open(dbpath, 0) < 0)
		die("GPATH not found.");
	return cv;
}
/*
 * convert_put: convert path into relative or absolute and print.
 *
 *	i)	cv	CONVERT structure
 *	i)	ctags_x	tag record (ctags-x format)
 *
 * Note: This function is only called by gtags with the --path option.
 */
void
convert_put(CONVERT *cv, const char *ctags_x)
{
	char *tagnextp = NULL;
	int tagnextc = 0;
	char *tag = NULL, *lineno = NULL, *path, *rest = NULL;
	const char *fid = NULL;

	if (cv->format == FORMAT_PATH)
		die("convert_put: internal error.");	/* Use convert_put_path() */
	/*
	 * parse tag line.
	 * Don't use split() function not to destroy line image.
	 */
	{
		char *p = (char *)ctags_x;
		/*
		 * tag name
		 */
		tag = p;
		for (; *p && !isspace(*p); p++)
			;
		if (*p == '\0')
			die("illegal ctags-x format (line number not found).");
		tagnextp = p;
		tagnextc = *p;
		*p++ = '\0';
		/* skip blanks */
		for (; *p && isspace(*p); p++)
			;
		if (*p == '\0')
			die("illegal ctags-x format (line number not found).");
		/*
		 * line number
		 */
		lineno = p;
		for (; *p && !isspace(*p); p++)
			;
		if (*p == '\0')
			die("illegal ctags-x format (path name not found).");
		*p++ = '\0';
		/* skip blanks */
		for (; *p && isspace(*p); p++)
			;
		if (*p == '\0')
			die("illegal ctags-x format (path name not found).");
		/*
		 * path name
		 */
		path = p;
		for (; *p && !isspace(*p); p++)
			;
		if (*p == '\0')
			die("illegal ctags-x format (line image not found).");
		*p++ = '\0';
		rest = p;
	}
	/*
	 * The path name has already been encoded.
	 */
	path = decode_path((unsigned char *)path);
	switch (cv->format) {
	case FORMAT_CTAGS:
		fputs(tag, cv->op);
		fputc('\t', cv->op);
		fputs(convert_pathname(cv, path), cv->op);
		fputc('\t', cv->op);
		fputs(lineno, cv->op);
		break;
	case FORMAT_CTAGS_XID:
		fid = gpath_path2fid(path, NULL);
		if (fid == NULL)
			die("convert_put: unknown file. '%s'", path);
		fputs(fid, cv->op);
		fputc(' ', cv->op);
		/* PASS THROUGH */
	case FORMAT_CTAGS_X:
		/*
		 * print until path name.
		 */
		*tagnextp = tagnextc;
		fputs(ctags_x, cv->op);
		fputc(' ', cv->op);
		/*
		 * print path name and the rest.
		 */
		fputs(convert_pathname(cv, path), cv->op);
		fputc(' ', cv->op);
		fputs(rest, cv->op);
		break;
	case FORMAT_GREP:
		fputs(convert_pathname(cv, path), cv->op);
		fputc(':', cv->op);
		fputs(lineno, cv->op);
		fputc(':', cv->op);
		fputs(rest, cv->op);
		break;
	case FORMAT_CSCOPE:
		fputs(convert_pathname(cv, path), cv->op);
		fputc(' ', cv->op);
		fputs(tag, cv->op);
		fputc(' ', cv->op);
		fputs(lineno, cv->op);
		fputc(' ', cv->op);
		for (; *rest && isspace(*rest); rest++)
			;
		fputs(rest, cv->op);
		break;
	default:
		die("unknown format type.");
	}
	(void)fputc('\n', cv->op);
}
/*
 * convert_put_path: convert path into relative or absolute and print.
 *
 *	i)	cv	CONVERT structure
 *	i)	path	path name
 */
void
convert_put_path(CONVERT *cv, const char *path)
{
	if (cv->format != FORMAT_PATH)
		die("convert_put_path: internal error.");
	fputs(convert_pathname(cv, path), cv->op);
	(void)fputc('\n', cv->op);
}
/*
 * convert_put_using: convert path into relative or absolute and print.
 *
 *	i)	cv	CONVERT structure
 *      i)      tag     tag name
 *      i)      path    path name
 *      i)      lineno  line number
 *      i)      line    line image
 *	i)	fid	file id (only when fid != NULL)
 */
void
convert_put_using(CONVERT *cv, const char *tag, const char *path, int lineno, const char *rest, const char *fid)
{
	switch (cv->format) {
	case FORMAT_PATH:
		fputs(convert_pathname(cv, path), cv->op);
		break;
	case FORMAT_CTAGS:
		fputs(tag, cv->op);
		fputc('\t', cv->op);
		fputs(convert_pathname(cv, path), cv->op);
		fputc('\t', cv->op);
		fprintf(cv->op, "%d", lineno);
		break;
	case FORMAT_CTAGS_XID:
		if (fid == NULL) {
			fid = gpath_path2fid(path, NULL);
			if (fid == NULL)
				die("convert_put_using: unknown file. '%s'", path);
		}
		fputs(fid, cv->op);
		fputc(' ', cv->op);
		/* PASS THROUGH */
	case FORMAT_CTAGS_X:
		fprintf(cv->op, "%-16s %4d %-16s %s",
			tag, lineno, convert_pathname(cv, path), rest);
		break;
	case FORMAT_GREP:
		fputs(convert_pathname(cv, path), cv->op);
		fputc(':', cv->op);
		fprintf(cv->op, "%d", lineno);
		fputc(':', cv->op);
		fputs(rest, cv->op);
		break;
	case FORMAT_CSCOPE:
		fputs(convert_pathname(cv, path), cv->op);
		fputc(' ', cv->op);
		fputs(tag, cv->op);
		fputc(' ', cv->op);
		fprintf(cv->op, "%d", lineno);
		fputc(' ', cv->op);
		for (; *rest && isspace(*rest); rest++)
			;
		fputs(rest, cv->op);
		break;
	default:
		die("unknown format type.");
	}
	(void)fputc('\n', cv->op);
}
void
convert_close(CONVERT *cv)
{
	strbuf_close(cv->abspath);
	gpath_close();
	free(cv);
}
