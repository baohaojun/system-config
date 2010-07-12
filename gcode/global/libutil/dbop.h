/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2006, 2009, 2010
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

#ifndef _DBOP_H_
#define _DBOP_H_

#include "gparam.h"
#ifdef USE_DB185_COMPAT
#include <db_185.h>
#else
#include "db.h"
#endif
#include "regex.h"
#include "strbuf.h"

#if defined(_WIN32) && !defined(__CYGWIN__)
typedef void* HANDLE;
#endif

#define DBOP_PAGESIZE	8192
#define VERSIONKEY	" __.VERSION"

typedef	struct {
	/*
	 * (1) COMMON PART
	 */
	int mode;			/* 0:read, 1:create, 2:modify */
	int openflags;			/* flags of xxxx_open() */
	int ioflags;			/* flags of xxxx_first() */
	char *lastdat;			/* the data of last located record */
	int lastsize;			/* the size of the lastdat */
	char *lastkey;			/* the key of last located record */
	int lastkeysize;		/* the size of the key */
	regex_t	*preg;			/* compiled regular expression */
	int unread;			/* leave record to read again */
	const char *put_errmsg;		/* error message for put_xxx() */
	/*
	 * (2) DB185 PART
	 */
	DB *db;				/* descripter of DB */
	char dbname[MAXPATHLEN];	/* dbname */
	char key[MAXKEYLEN];		/* key */
	int keylen;			/* key length */
	char prev[MAXKEYLEN];		/* previous key value */
	int perm;			/* file permission */
	/*
	 * (3) sorted write
	 */
	FILE *sortout;			/* write to sort command */
	FILE *sortin;			/* read from sort command */
#if defined(_WIN32) && !defined(__CYGWIN__)
	HANDLE pid;
#else
	int pid;			/* sort process id */
#endif
} DBOP;

/*
 * openflags
 */
#define	DBOP_DUP	1		/* allow duplicate records	*/
/*
 * ioflags
 */
#define DBOP_KEY		1	/* read key part		*/
#define DBOP_PREFIX		2	/* prefixed read		*/
#define DBOP_RAW		4	/* raw read			*/
#define DBOP_SORTED_WRITE	8	/* sorted write			*/

DBOP *dbop_open(const char *, int, int, int);
const char *dbop_get(DBOP *, const char *);
void dbop_put(DBOP *, const char *, const char *);
void dbop_put_withlen(DBOP *, const char *, const char *, int);
void dbop_delete(DBOP *, const char *);
void dbop_update(DBOP *, const char *, const char *);
const char *dbop_first(DBOP *, const char *, regex_t *, int);
const char *dbop_next(DBOP *);
void dbop_unread(DBOP *);
const char *dbop_lastdat(DBOP *, int *);
const char *dbop_getflag(DBOP *);
const char *dbop_getoption(DBOP *, const char *);
void dbop_putoption(DBOP *, const char *, const char *);
int dbop_getversion(DBOP *);
void dbop_putversion(DBOP *, int);
void dbop_close(DBOP *);

#endif /* _DBOP_H_ */
