/*
 * Copyright (c) 2005 Tama Communications Corporation
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

#ifndef _XARGS_H_
#define _XARGS_H_

#include <stdio.h>

#include "strbuf.h"

/*
 * Types
 */
#define	XARGS_FILE	1
#define XARGS_ARGV	2
#define XARGS_STRBUF	3
#define XARGS_FIND	4
/*
 * Options
 */
#define XA_IGNORE_ERROR		1
#define XA_SKIP_NOTSOURCE	2
#define XA_PUT_GPATH		4
#define XA_TRIM_LINE		8

typedef struct {
	/*
	 * common area
	 */
	char *command;
	FILE *pipe;
	STRBUF *result;
	int end_of_arg;
	int unread;
	int seqno;		/* sequencial number */
	int type;		/* XARGS_XXX Types */
	/*
	 * options
	 *
	 * These variables are set to directly by calling procedures.
	 * This might have to be reviewed.
	 */
	int ignore_error;
	int max_args;		/* 0: no limit, >0: limit */
	int put_gpath;
	int trim_line;
	int skip_assembly;
	void (*verbose)(char *, int, int);
	/*
	 * XARGS_FILE
	 */
	FILE *ip;
	long fptr;
	STRBUF *path;
	/*
	 * XARGS_ARGV
	 */
	int argc;
	char *const *argv;
	/*
	 * XARGS_STRBUF
	 */
	char *curp;
	char *endp;
	/*
	 * XARGS_FIND
 	 */
} XARGS;

XARGS *xargs_open_with_file(const char *, int, FILE *);
XARGS *xargs_open_with_argv(const char *, int, int, char *const *);
XARGS *xargs_open_with_strbuf(const char *, int, STRBUF *);
XARGS *xargs_open_with_find(const char *, int);
char *xargs_read(XARGS *);
void xargs_unread(XARGS *);
int xargs_close(XARGS *);

#endif /*! _XARGS_H_ */
