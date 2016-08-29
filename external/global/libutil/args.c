/*
 * Copyright (c) 2010 Tama Communications Corporation
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
#include "die.h"
#include "strbuf.h"
#include "gpathop.h"

#define ARGS_NOP	0
#define ARGS_ARGS	1
#define ARGS_FILELIST	2
#define ARGS_GFIND	3
#define ARGS_BOTH	4

int type;
const char **argslist;
FILE *ip;
GFIND *gp;

/*
 * args_open:
 *
 *	i)	args	args array
 */
void
args_open(const char **args)
{
	type = ARGS_ARGS;
	argslist = args;
}
/*
 * args_open_filelist: args_open like interface for handling output of find(1).
 *
 *	i)	filename	file including list of file names.
 *				When "-" is specified, read from standard input.
 */
void
args_open_filelist(const char *filename)
{
	type = ARGS_FILELIST;
	if (!strcmp(filename, "-")) {
		ip = stdin;
	} else {
		ip = fopen(filename, "r");
		if (ip == NULL)
			die("cannot open '%s'.", filename);
	}
}
/*
 * args_open_both: args_open like interface for argument and file list.
 *
 *	i)	args		args array
 *	i)	filename	file including list of file names.
 *				When "-" is specified, read from standard input.
 */
void
args_open_both(const char **args, const char *filename)
{
	type = ARGS_BOTH;
	argslist = args;
	if (!strcmp(filename, "-")) {
		ip = stdin;
	} else {
		ip = fopen(filename, "r");
		if (ip == NULL)
			die("cannot open '%s'.", filename);
	}
}
/*
 * args_open_gfind: args_open like interface for handling output of gfind.
 *
 *	i)	agp	GFIND descriptor
 */
void
args_open_gfind(GFIND *agp)
{
	type = ARGS_GFIND;
	gp = agp;
}
void
args_open_nop()
{
	type = ARGS_NOP;
}
/*
 * args_read: read path From args.
 *
 *	r)		path (NULL: end of argument)
 */
const char *
args_read(void)
{
	const char *p;
	STATIC_STRBUF(sb);

	strbuf_clear(sb);
	switch (type) {
	case ARGS_NOP:
		p = NULL;
		break;
	case ARGS_ARGS:
		p = *argslist++;
		break;
	case ARGS_FILELIST:
		p = strbuf_fgets(sb, ip, STRBUF_NOCRLF);
		break;
	case ARGS_GFIND:
		p = gfind_read(gp);
		break;
	case ARGS_BOTH:
		if (*argslist != NULL)
			p = *argslist++;
		else
			p = strbuf_fgets(sb, ip, STRBUF_NOCRLF);
		break;
	default:
		die("args_read: illegal type.");
	}
	return p;
}
/*
 * args_close: close args.
 */
void
args_close(void)
{
	switch (type) {
	case ARGS_NOP:
	case ARGS_ARGS:
		break;
	case ARGS_FILELIST:
	case ARGS_BOTH:
		if (ip != NULL && ip != stdin)
			fclose(ip);
		ip = NULL;
		break;
	case ARGS_GFIND:
		if (gp != NULL)
			gfind_close(gp);
		gp = NULL;
		break;
	default:
		die("something wrong.");
	}
}
