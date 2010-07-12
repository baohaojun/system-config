/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2005, 2006, 2007, 2010
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

#ifndef _GTOP_H_
#define _GTOP_H_

#include <stdio.h>

#include "gparam.h"
#include "dbop.h"
#include "idset.h"
#include "strbuf.h"
#include "strhash.h"
#include "varray.h"

#define COMPACTKEY	" __.COMPACT"
#define COMPRESSKEY	" __.COMPRESS"
#define COMPLINEKEY	" __.COMPLINE"
#define COMPNAMEKEY	" __.COMPNAME"

#define GPATH		0
#define GTAGS		1
#define GRTAGS		2
#define GSYMS		3
#define GTAGLIM		4

#define	GTAGS_READ	0
#define GTAGS_CREATE	1
#define GTAGS_MODIFY	2

/* gtags_open() */
#define GTAGS_COMPACT		1	/* compact option */
#define GTAGS_COMPRESS		2	/* compression option */
#define GTAGS_COMPLINE		4	/* compression option for line number */
#define GTAGS_COMPNAME		8	/* compression option for line number */
#define GTAGS_EXTRACTMETHOD	16	/* extract method from class definition */
#define GTAGS_DEBUG		65536	/* print information for debug */
/* gtags_first() */
#define GTOP_KEY		1	/* read key part */
#define GTOP_PATH		2	/* read path part */
#define GTOP_PREFIX		4	/* prefixed read */
#define GTOP_NOREGEX		8	/* don't use regular expression */
#define GTOP_IGNORECASE		16	/* ignore case distinction */
#define GTOP_BASICREGEX		32	/* use basic regular expression */
#define GTOP_NOSORT		64	/* don't sort */

/*
 * This entry corresponds to one raw record.
 */
typedef struct {
	const char *tagline;
	const char *path;
	const char *tag;
	int lineno;
} GTP;

typedef struct {
	DBOP *dbop;			/* descripter of DBOP */
	DBOP *gtags;			/* descripter of GTAGS */
	int format_version;		/* format version */
	int format;			/* GTAGS_COMPACT, GTAGS_COMPRESS */
	int mode;			/* mode */
	int db;				/* 0:GTAGS, 1:GRTAGS, 2:GSYMS */
	int openflags;			/* flags value of gtags_open() */
	int flags;			/* flags */
	char root[MAXPATHLEN];	/* root directory of source tree */
	/*
	 * Stuff for GTOP_PATH.
	 */
	int path_count;
	int path_index;
	char **path_array;
	/*
	 * Stuff for segment_read().
	 */
	int gtp_count;
	int gtp_index;
	GTP *gtp_array;
	GTP gtp;
	POOL *segment_pool;
	VARRAY *vb;
	char cur_tagname[IDENTLEN];	/* current tag name */
	/*
	 * Stuff for compact format
	 */
	char cur_path[MAXPATHLEN];	/* current path */
	STRBUF *sb;			/* string buffer */
	/* used for compact format and path name only read */
	STRHASH *path_hash;
} GTOP;

const char *dbname(int);
GTOP *gtags_open(const char *, const char *, int, int, int);
void gtags_put_using(GTOP *, const char *, int, const char *, const char *);
void gtags_flush(GTOP *, const char *);
void gtags_delete(GTOP *, IDSET *);
GTP *gtags_first(GTOP *, const char *, int);
GTP *gtags_next(GTOP *);
void gtags_close(GTOP *);

#endif /* ! _GTOP_H_ */
