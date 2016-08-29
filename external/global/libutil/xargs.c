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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include "checkalloc.h"
#include "die.h"
#include "env.h"
#include "find.h"
#include "gpathop.h"
#include "locatestring.h"
#include "strbuf.h"
#include "test.h"
#include "xargs.h"

#if !defined(ARG_MAX) && defined(_SC_ARG_MAX)
#define ARG_MAX         sysconf(_SC_ARG_MAX)
#endif

static int exec_line_limit(int);
static char *repeat_find_read(void);
static void repeat_find_next(void);
static FILE *execute_command(XARGS *);
static XARGS *xargs_open_generic(const char *, int);

/*
 * usage: a piece of code to achieve the following command line.
 *
 *	find . -type f -print | xargs grep pattern
 *
 * char *p;
 * FILE *ip = popen("find . -type f -print", "r");
 * XARGS *xp = xargs_open_with_file("grep pattern", ip)
 * xp->ignore_error = 1;
 * while ((p = xargs_read(xp)) != NULL)
 *	puts(p);
 * xargs_close(xp);
 */
/*
 * exec_line_limit: upper limit of bytes of exec line.
 *
 *	i)	length	command line length
 *	r)	0: unknown or cannot afford long line.
 *		> 0: upper limit of exec line
 */
static int
exec_line_limit(int length)
{
	int limit = 0;

#ifdef ARG_MAX
	/*
	 * POSIX.2 limits the exec(2) line length to ARG_MAX - 2048.
	 */
	limit = ARG_MAX - 2048;
	/*
	 * The reason is unknown but the xargs(1) in GNU findutils
	 * use this limit.
	 */
	if (limit > 20 * 1024)
		limit = 20 * 1024;
	/*
	 * Add the command line length.
	 * We estimates additional 80 bytes for popen(3).
	 *
	 * for "/bin/sh -c "				11bytes
	 * reserve					69bytes
	 * ----------------------------------------------------
	 * Total					80 bytes
	 */
	limit -= length + 80;

	limit -= env_size();
#endif
#if !defined(ARG_MAX) && defined(_WIN32)
	/*
	 * The limit lenght of the command line of cmd.exe is 2047
	 * characters on Windows 2000 or 8191 characters on Windows XP
	 * and later. The 80 below is for safety.
	 */
	limit = 2047 - length - 80;
#endif
	if (limit < 0)
		limit = 0;
	return limit;
}
/*
 * repeatable find_read
 */
static char *repeat_lastpath;
static char *
repeat_find_read(void)
{
	if (repeat_lastpath)
		return repeat_lastpath;
	return repeat_lastpath = find_read();
}
static void
repeat_find_next(void)
{
	repeat_lastpath = NULL;
}

/*
 * Specified limitation by user. (e.g. gtags --max-args)
 */
#define LT_MAX ((xp->max_args == 0 || count < xp->max_args))

/*
 * Common processing for each XARGS_XXXX type.
 */
#ifdef _WIN32
#define QUOTE	'"'
#else
#define QUOTE	'\''
#endif
#define APPEND_ARGUMENT(p) {\
	char *path = (p);\
	length = strlen(path);\
	if (*path == ' ') {\
		if (xp->put_gpath && !test("b", ++path))\
			gpath_put(path, GPATH_OTHER);\
		continue;\
	}\
	if (strbuf_getlen(comline) + length + 2 > limit)\
		break;\
	xp->seqno++;\
	if (xp->put_gpath)\
		gpath_put(path, GPATH_SOURCE);\
	if (xp->skip_assembly && locatestring(path, ".s", MATCH_AT_LAST|IGNORE_CASE) != NULL) {\
		if (xp->verbose)\
			xp->verbose(path + 2, xp->seqno, 1);\
	} else {\
		if (xp->verbose)\
			xp->verbose(path + 2, xp->seqno, 0);\
		strbuf_putc(comline, ' ');\
		strbuf_putc(comline, QUOTE);\
		strbuf_puts(comline, path);\
		strbuf_putc(comline, QUOTE);\
		count++;\
	}\
}

/*
 * execute_command
 *
 *	i)	xp	xargs structure
 *	r)		!=NULL: file pointer
 *			==NULL: end of argument
 *
 * This function constructs command line from the following,
 *	command: xp->command
 *	argument: each argument provider
 * execute it on a pipe line, and return the file pointer.
 */
static FILE *
execute_command(XARGS *xp)
{
	int limit;
	STRBUF *comline = strbuf_open(0);
	int count = 0;
	int length;
	FILE *pipe = NULL;
	char *p, *meta_p;

	/*
	 * Copy the part before '%s' of the command skeleton.
	 * The '%s' in the skeleton is replaced with given arguments.
	 */
	meta_p = locatestring(xp->command, "%s", MATCH_FIRST);
	if (meta_p) {
		strbuf_nputs(comline, xp->command, meta_p - xp->command);
		limit = exec_line_limit(strlen(meta_p + 2));
	} else {
		strbuf_puts(comline, xp->command);
		limit = exec_line_limit(0);
	}
	/*
	 * Append arguments as many as possible.
	 */
	switch (xp->type) {
	case XARGS_FILE:
		for (
			/* initial */
			fseek(xp->ip, xp->fptr, SEEK_SET)
			;
			/* continuation condition */
			(LT_MAX &&
				((p = (strbuf_getlen(xp->path) > 0 ?
				strbuf_value(xp->path) :
				strbuf_fgets(xp->path, xp->ip, STRBUF_NOCRLF))) != NULL))
			;
			/* preparation */
			strbuf_reset(xp->path)
		)
			APPEND_ARGUMENT(p);
		xp->fptr = ftell(xp->ip);
		break;
	case XARGS_ARGV:
		for (; LT_MAX && xp->argc > 0; xp->argc--, xp->argv++)
			APPEND_ARGUMENT(xp->argv[0])
		break;
	case XARGS_STRBUF:
		for (; LT_MAX && xp->curp < xp->endp; xp->curp += length + 1)
			APPEND_ARGUMENT(xp->curp)
		break;
	case XARGS_FIND:
		for (; LT_MAX && (p = repeat_find_read()) != NULL; repeat_find_next())
			APPEND_ARGUMENT(p)
		break;
	}
	/*
	 * Copy the left part of the command skeleton.
	 */
	if (meta_p) {
		strbuf_putc(comline, ' ');
		strbuf_puts(comline, meta_p + 2);
	}
	if (count > 0) {
		pipe = popen(strbuf_value(comline), "r");
		if (pipe == NULL)
			die("cannot execute command '%s'.", strbuf_value(comline));
	}
	strbuf_close(comline);
	return pipe;
}
/*
 * xargs_open_generic: allocate generic part of xargs structure.
 *
 *	i)	command	command line except for the arguments.
 *	i)	max_args 0: no limit, >0: max argument
 *	r)		xargs structure	
 */
static XARGS *
xargs_open_generic(const char *command, int max_args)
{
	XARGS *xp;

	xp  = check_calloc(sizeof(XARGS), 1);
	xp->command = check_strdup(command);
	xp->type = 0;
	xp->pipe = NULL;
	xp->result = strbuf_open(0);
	xp->end_of_arg = 0;
	xp->unread = 0;
	xp->max_args = max_args;
	xp->seqno = 0;
	xp->skip_assembly = 0;
	/*
	 * Procedure to display verbose message.
	 * proc(path, seqno, skip)
	 */
	xp->verbose = NULL;
	/*
	 * By default, the error status returned by pclose() is not ignored,
	 * because global(1) doesn't return error code when the target
	 * was not found.
	 * Some commands like grep(1) return error code when the target
	 * was not found. If you would like to use such command, set the
	 * flag to 1.
	 */
	xp->ignore_error = 0;
	/*
	 * By default, we doesn't put the path to GPATH.
	 * This option is prepared for createtags() and updatetags().
	 */
	xp->put_gpath = 0;
	/*
	 * By default, we don't cut off the blanks at the tail of the line.
	 */
	xp->trim_line = 0;

	return xp;
}
/*
 * xargs_open_with_file: open xargs stream using file
 *
 *	i)	command	command skeleton.
 *	i)	max_args 0: no limit, >0: max argument
 *	i)	ip	file pointer
 *	r)		xargs structure
 *
 * The '%s' in the command skeleton is replaced with given arguments.
 * If '%s' doesn't exist, the arguments is appended to the tail of the
 * skeleton.
 */
XARGS *
xargs_open_with_file(const char *command, int max_args, FILE *ip)
{
	XARGS *xp = xargs_open_generic(command, max_args);

	xp->type = XARGS_FILE;
	xp->ip = ip;
	xp->path = strbuf_open(0);
	xp->fptr = 0;
	return xp;
}
/*
 * xargs_open_with_argv: open xargs stream using argv
 *
 *	i)	command	command skeleton.
 *	i)	max_args 0: no limit, >0: max argument
 *	i)	argc	argument number
 *	i)	argv	argument array
 *	r)		xargs structure
 *
 * The '%s' in the command skeleton is replaced with given arguments.
 * If '%s' doesn't exist, the arguments is appended to the tail of the
 * skeleton.
 */
XARGS *
xargs_open_with_argv(const char *command, int max_args, int argc, char *const *argv)
{
	XARGS *xp = xargs_open_generic(command, max_args);

	xp->type = XARGS_ARGV;
	xp->argc = argc;
	xp->argv = argv;
	return xp;
}
/*
 * xargs_open_with_strbuf: open xargs stream using string buffer
 *
 *	i)	command	command skeleton.
 *	i)	max_args 0: no limit, >0: max argument
 *	i)	sb	string buffer
 *	r)		xargs structure
 *
 * The '%s' in the command skeleton is replaced with given arguments.
 * If '%s' doesn't exist, the arguments is appended to the tail of the
 * skeleton.
 */
XARGS *
xargs_open_with_strbuf(const char *command, int max_args, STRBUF *sb)
{
	XARGS *xp = xargs_open_generic(command, max_args);

	xp->type = XARGS_STRBUF;
	xp->curp = strbuf_value(sb);
	xp->endp = xp->curp + strbuf_getlen(sb);
	return xp;
}
/*
 * xargs_open_with_find: open xargs stream using find().
 *
 *	i)	command	command skeleton.
 *	i)	max_args 0: no limit, >0: max argument
 *	r)		xargs structure
 *
 * The '%s' in the command skeleton is replaced with given arguments.
 * If '%s' doesn't exist, the arguments is appended to the tail of the
 * skeleton.
 */
XARGS *
xargs_open_with_find(const char *command, int max_args)
{
	XARGS *xp = xargs_open_generic(command, max_args);

	xp->type = XARGS_FIND;
	return xp;
}
/*
 * xargs_read: read a record from xargs stream
 *
 *	i)	xp	xargs structure
 *	r)		result line
 */
char *
xargs_read(XARGS *xp)
{
	assert(xp != NULL);
	if (xp->end_of_arg)
		return NULL;
	if (xp->unread) {
		xp->unread = 0;
		return strbuf_value(xp->result);
	}
	if (xp->pipe && strbuf_fgets(xp->result, xp->pipe, STRBUF_NOCRLF) != NULL) {
		if (xp->trim_line)
			strbuf_trim(xp->result);
		return strbuf_value(xp->result);
	}
	if (xp->pipe)
		if (pclose(xp->pipe) != 0 && !xp->ignore_error)
			die("command failed in xargs_read().");
	/*
	 * Switch to the next segment.
	 */
	do {
		xp->pipe = execute_command(xp);
		if (xp->pipe && strbuf_fgets(xp->result, xp->pipe, STRBUF_NOCRLF) != NULL) {
			if (xp->trim_line)
				strbuf_trim(xp->result);
			return strbuf_value(xp->result);
		}
		if (xp->pipe) {
			if (pclose(xp->pipe) != 0 && !xp->ignore_error)
				die("command failed in xargs_read().");
		} else {
			xp->end_of_arg = 1;
		}
	} while (!xp->end_of_arg);

	return NULL;
}
/*
 * xargs_unread: push back a record to xargs stream
 *
 *	i)	xp	xargs structure
 */
void
xargs_unread(XARGS *xp)
{
	assert(xp != NULL);
	xp->unread = 1;
}
/*
 * xargs_close(xp)
 *
 *	i)	xp	xargs structure
 */
int
xargs_close(XARGS *xp)
{
	int count;

	assert(xp != NULL);
	count = xp->seqno;
	assert(xp->pipe == NULL);
	free(xp->command);
	strbuf_close(xp->result);

	switch (xp->type) {
	case XARGS_FILE:
		strbuf_close(xp->path);
		break;
	case XARGS_ARGV:
	case XARGS_STRBUF:
		/* Nothing to do */
		break;
	}
	free(xp);
	return count;
}
