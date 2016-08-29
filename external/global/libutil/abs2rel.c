/*
 * Copyright (c) 1997, 1999, 2008 Tama Communications Corporation
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
#include <errno.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "abs2rel.h"
#include "die.h"
#include "gparam.h"
#include "locatestring.h"
#include "strlimcpy.h"
#include "path.h"

/*

NAME
     abs2rel - make a relative path name from an absolute path

SYNOPSIS
     char *
     abs2rel(const char *path, const char *base, char *result, size_t size)

DESCRIPTION
     The abs2rel() function makes a relative path name from an absolute path
     name path based on a directory base and copies the resulting path name
     into the memory referenced by result.  The result argument must refer to
     a buffer capable of storing at least size characters.

     The resulting path name may include symbolic links.  The abs2rel() func-
     tion doesn't check whether or not any path exists.

RETURN VALUES
     The abs2rel() function returns relative path name on success.  If an er-
     ror occurs, it returns NULL.

ERRORS
     The abs2rel() function may fail and set the external variable errno to
     indicate the error.

     [EINVAL]           The base directory isn't an absolute path name or the
                        size argument is zero.

     [ERANGE]           The size argument is greater than zero but smaller
                        than the length of the pathname plus 1.

EXAMPLE
         char result[MAXPATHLEN];
         char *path = abs2rel("/usr/src/sys", "/usr/local/lib", result, MAX-
     PATHLEN);

     yields:

         path == "../../src/sys"

     Similarly,

         path1 = abs2rel("/usr/src/sys", "/usr", result, MAXPATHLEN);
         path2 = abs2rel("/usr/src/sys", "/usr/src/sys", result, MAXPATHLEN);

     yields:

         path1 == "src/sys"
         path2 == "."


BUGS
     If the base directory includes symbolic links, the abs2rel() function
     produces the wrong path.  For example, if '/sys' is a symbolic link to
     '/usr/src/sys',

         char *path = abs2rel("/usr/local/lib", "/sys", result, MAXPATHLEN);

     yields:

         path == "../usr/local/lib"         -- It's wrong!!

     You should convert the base directory into a real path in advance.

         path = abs2rel("/sys/kern", realpath("/sys", resolvedname), result,
     MAXPATHLEN);

     yields:

         path == "../../../sys/kern"        -- It's correct but ...

     That is correct, but a little redundant. If you wish get the simple an-
     swer 'kern', do the following.

         path = abs2rel(realpath("/sys/kern", r1), realpath("/sys", r2),
                                             result, MAXPATHLEN);

     The realpath() function assures correct result, but don't forget that
     realpath() requires that all but the last component of the path exist.

-------------------------------------------------------------------------------
NAME
     rel2abs - make an absolute path name from a relative path

SYNOPSIS
     char *
     rel2abs(const char *path, const char *base, char *result, size_t size)

DESCRIPTION
     The rel2abs() function makes an absolute path name from a relative path
     name path based on a directory base and copies the resulting path name
     into the memory referenced by result.  The result argument must refer to
     a buffer capable of storing at least size character

     The resulting path name may include symbolic links.  abs2rel() doesn't
     check whether or not any path exists.

RETURN VALUES
     The rel2abs() function returns absolute path name on success.  If an er-
     ror occurs, it returns NULL.

ERRORS
     The rel2abs() function may fail and set the external variable errno to
     indicate the error.

     [EINVAL]           The base directory isn't an absolute path name or the
                        size argument is zero.

     [ERANGE]           The size argument is greater than zero but smaller
                        than the length of the pathname plus 1

EXAMPLE
         char result[MAXPATHLEN];
         char *path = rel2abs("../../src/sys", "/usr/local/lib", result, MAX-
     PATHLEN);

     yields:

         path == "/usr/src/sys"

     Similarly,

         path1 = rel2abs("src/sys", "/usr", result, MAXPATHLEN);
         path2 = rel2abs(".", "/usr/src/sys", result, MAXPATHLEN);

     yields:

         path1 == "/usr/src/sys"
         path2 == "/usr/src/sys"

*/
/*
 * normalize: normalize path name
 *
 *	i)	path	path name
 *	i)	root	root of project (must be end with a '/')
 *	i)	cwd	current directory
 *	o)	result	normalized path name
 *	i)	size	size of the result
 *	r)		==NULL: error
 *			!=NULL: result
 */
char *
normalize(const char *path, const char *root, const char *cwd, char *result, const int size)
{
	char *p, abs[MAXPATHLEN];

	if (normalize_pathname(path, result, size) == NULL)
		goto toolong;
	if (isabspath(path)) {
		if (strlen(result) > MAXPATHLEN)
			goto toolong;
		strcpy(abs, result);
	} else {
		if (rel2abs(result, cwd, abs, sizeof(abs)) == NULL)
			goto toolong;
	}
	/*
	 * Remove the root part of path and insert './'.
	 *      rootdir  /a/b/
	 *      path     /a/b/c/d.c -> c/d.c -> ./c/d.c
	 */
	p = locatestring(abs, root, MATCH_AT_FIRST);
	if (p == NULL)
		return NULL;
	strlimcpy(result, "./", size);
	strlimcpy(result + 2, p, size - 2);
	return result;
toolong:
	die("path name is too long.");
}
/*
 * normalize_pathname: normalize relative path name.
 *
 *	i)	path	relative path name
 *	o)	result	result buffer
 *	i)	size	size of result buffer
 *	r)		!= NULL: normalized path name
 *			== NULL: error
 *
 * [examples]
 *
 * path			result
 * ---------------------------
 * /a			/a
 * ./a/./b/c		a/b/c
 * a////b///c		a/b/c
 * ../a/b/c		../a/b/c
 * a/d/../b/c		a/b/c
 * a/../b/../c/../d	d
 * a/../../d		../d
 * /a/../../d		/d
 */
char *
normalize_pathname(const char *path, char *result, const int size)
{
	const char *savep, *p = path;
	char *final, *q = result;
	char *endp = result + size - 1;

	/* accept the first '/' */
	if (isabspath(p)) {
		*q++ = *p++;
#if defined(_WIN32) || defined(__DJGPP__)
		if (*p == ':') {
			*q++ = *p++;
			*q++ = *p++;
		}
#endif
		final = q;
	}
	do {
		savep = p;
		while (!strncmp(p, "./", 2))	/* skip "./" at the head of the path */
			p += 2;
		while (!strncmp(p, "../", 3)) {	/* accept the first "../" */
			if (q + 3 > endp)
				goto erange;
			strcpy(q, "../");
			p += 3;
			q += 3;
		}
	} while (savep != p);

	final = q;
	while (*p) {
		if (*p == '/') {
			p++;
			do {
				savep = p;
				/* skip consecutive '/' */
				while (*p == '/')	
					p++;
				/* skip consecutive './' */
				while (!strncmp(p, "./", 2))
					p += 2;
				/* resolve '../'(parent directory) */
				while (!strncmp(p, "../", 3)) {
					p += 3;
					if (q > final) {
						while (q > final && *--q != '/')
							;
					} else if (!(*result == '/' && result + 1 == q)) {
						if (q + 3 > endp)
							goto erange;
						strcpy(q, "../");
						q += 3;
						final = q;
					}
				}
			} while (savep != p);
			if (q > endp)
				goto erange;
			if (q > final) {
				*q++ = '/';
			}
		} else {
			if (q > endp)
				goto erange;
			*q++ = *p++;
		}
	}
	*q = '\0';
	return result;
erange:
	errno = ERANGE;
	return NULL;
}
/*
 * abs2rel: convert an absolute path name into relative.
 *
 *	i)	path	absolute path
 *	i)	base	base directory (must be absolute path)
 *	o)	result	result buffer
 *	i)	size	size of result buffer
 *	r)		!= NULL: relative path
 *			== NULL: error
 */
char *
abs2rel(const char *path, const char *base, char *result, const int size)
{
	const char *pp, *bp, *branch;
	/*
	 * endp points the last position which is safe in the result buffer.
	 */
	const char *endp = result + size - 1;
	char *rp;

	if (!isabspath(path)) {
		if (strlen(path) >= size)
			goto erange;
		strcpy(result, path);
		goto finish;
	} else if (!isabspath(base) || !size) {
		errno = EINVAL;
		return (NULL);
	} else if (size == 1)
		goto erange;
	/*
	 * seek to branched point.
	 */
	branch = path;
	for (pp = path, bp = base; *pp && *bp && *pp == *bp; pp++, bp++)
		if (*pp == '/')
			branch = pp;
	if ((*pp == 0 || (*pp == '/' && *(pp + 1) == 0)) &&
	    (*bp == 0 || (*bp == '/' && *(bp + 1) == 0))) {
		rp = result;
		*rp++ = '.';
		if (*pp == '/' || *(pp - 1) == '/')
			*rp++ = '/';
		if (rp > endp)
			goto erange;
		*rp = 0;
		goto finish;
	}
	if ((*pp == 0 && *bp == '/') || (*pp == '/' && *bp == 0))
		branch = pp;
	/*
	 * up to root.
	 */
	rp = result;
	for (bp = base + (branch - path); *bp; bp++)
		if (*bp == '/' && *(bp + 1) != 0) {
			if (rp + 3 > endp)
				goto erange;
			*rp++ = '.';
			*rp++ = '.';
			*rp++ = '/';
		}
	if (rp > endp)
		goto erange;
	*rp = 0;
	/*
	 * down to leaf.
	 */
	if (*branch) {
		if (rp + strlen(branch + 1) > endp)
			goto erange;
		strcpy(rp, branch + 1);
	} else
		*--rp = 0;
finish:
	return result;
erange:
	errno = ERANGE;
	return (NULL);
}
/*
 * rel2abs: convert an relative path name into absolute.
 *
 *	i)	path	relative path
 *	i)	base	base directory (must be absolute path)
 *	o)	result	result buffer
 *	i)	size	size of result buffer
 *	r)		!= NULL: absolute path
 *			== NULL: error
 */
char *
rel2abs(const char *path, const char *base, char *result, const int size)
{
	const char *pp, *bp;
	/*
	 * endp points the last position which is safe in the result buffer.
	 */
	const char *endp = result + size - 1;
	char *rp;
	int length;

	if (isabspath(path)) {
		if (strlen(path) >= size)
			goto erange;
		strcpy(result, path);
		goto finish;
	} else if (!isabspath(base) || !size) {
		errno = EINVAL;
		return (NULL);
	} else if (size == 1)
		goto erange;

	length = strlen(base);

	if (!strcmp(path, ".") || !strcmp(path, "./")) {
		if (length >= size)
			goto erange;
		strcpy(result, base);
		/*
		 * rp points the last char.
		 */
		rp = result + length - 1;
		/*
		 * remove the last '/'.
		 */
		if (*rp == '/') {
			if (length > 1)
				*rp = 0;
		} else
			rp++;
		/* rp point NULL char */
		if (*++path == '/') {
			/*
			 * Append '/' to the tail of path name.
			 */
			*rp++ = '/';
			if (rp > endp)
				goto erange;
			*rp = 0;
		}
		goto finish;
	}
	bp = base + length;
	if (*(bp - 1) == '/')
		--bp;
	/*
	 * up to root.
	 */
	for (pp = path; *pp && *pp == '.'; ) {
		if (!strncmp(pp, "../", 3)) {
			pp += 3;
			while (bp > base && *--bp != '/')
				;
		} else if (!strncmp(pp, "./", 2)) {
			pp += 2;
		} else if (!strncmp(pp, "..\0", 3)) {
			pp += 2;
			while (bp > base && *--bp != '/')
				;
		} else
			break;
	}
	/*
	 * down to leaf.
	 */
	length = bp - base;
	if (length >= size)
		goto erange;
	strncpy(result, base, length);
	rp = result + length;
	if (*pp || *(pp - 1) == '/' || length == 0)
		*rp++ = '/';
	if (rp + strlen(pp) > endp)
		goto erange;
	strcpy(rp, pp);
finish:
	return result;
erange:
	errno = ERANGE;
	return (NULL);
}
