/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2005, 2006
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

#ifndef _PATHOP_H_
#define _PATHOP_H_
#include <stdio.h>

#include "gparam.h"
#include "dbop.h"

#define NEXTKEY		" __.NEXTKEY"

/*
 * File type
 */
#define GPATH_SOURCE	1
#define GPATH_OTHER	2
#define GPATH_BOTH	3

typedef struct {
	/* set by gfind_open() */
	DBOP *dbop;
	const char *prefix;
	int target;
	int version;
	/* set by gfind_open() and gfind_read() */
	int first;
	int eod;		/* end of data */
	/* set by gfind_read() */
	int type;		/* File type */
	const char *path;	/* return value of gfind_read() */
} GFIND;

int gpath_open(const char *, int);
const char *gpath_path2fid(const char *, int *);
const char *gpath_fid2path(const char *, int *);
void gpath_put(const char *, int);
void gpath_delete(const char *);
void gpath_close(void);
int gpath_nextkey(void);
GFIND *gfind_open(const char *, const char *, int);
const char *gfind_read(GFIND *);
void gfind_close(GFIND *);

#endif /* ! _PATHOP_H_ */
