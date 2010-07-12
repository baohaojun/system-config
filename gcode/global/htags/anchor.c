/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
 *	2008	Tama Communications Corporation
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
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif
#include "global.h"
#include "anchor.h"
#include "htags.h"
#include "path2url.h"

static XARGS *anchor_input[GTAGLIM];
static struct anchor *table;
static VARRAY *vb;

static struct anchor *start;
static struct anchor *curp;
static struct anchor *end;
static struct anchor *CURRENT;

/* compare routine for qsort(3) */
static int
cmp(const void *s1, const void *s2)
{
	return ((struct anchor *)s1)->lineno - ((struct anchor *)s2)->lineno;
}
/*
 * Pointers (as lineno).
 */
static int FIRST;
static int LAST;
static struct anchor *CURRENTDEF;

/*
 * anchor_prepare: setup input stream.
 *
 *	i)	anchor_stream	file pointer of path list
 */
void
anchor_prepare(FILE *anchor_stream)
{
	/*
	 * Option table:
	 * We use blank string as null option instead of null string("")
	 * not to change the command length. This length influences
	 * the number of arguments in the xargs processing.
	 */
	static const char *const options[] = {NULL, " ", "r", "s"};
	char comline[MAXFILLEN];
	int db;

	for (db = GTAGS; db < GTAGLIM; db++) {
		anchor_input[db] = NULL;
		/*
		 * Htags(1) should not use gtags-parser(1) directly;
		 * it should use global(1) with the -f option instead.
		 * Because gtags-parser is part of the implementation of
		 * gtags(1) and global(1), and htags is only an application
		 * program which uses global(1). If htags depends on
		 * gtags-parser, it will become difficult to change the
		 * implementation of gtags and global.
		 *
		 * Though the output of global -f is not necessarily sorted
		 * by the path, it is guaranteed that the records concerning
		 * the same file are consecutive.
		 */
		if (gtags_exist[db] == 1) {
			snprintf(comline, sizeof(comline), "%s -f%s --encode-path=\" \t\" --result=ctags-xid --nofilter=path", global_path, options[db]);
			anchor_input[db] = xargs_open_with_file(comline, 0, anchor_stream);
		}
	}
}
/*
 * anchor_load: load anchor table
 *
 *	i)	path	path name
 */
void
anchor_load(const char *path)
{
	int db, current_fid;

	/* Get fid of the path */
	{
		const char *p = path2fid(path);
		if (p == NULL)
			die("anchor_load: internal error. file '%s' not found in GPATH.", path);
		current_fid = atoi(p);
	}
	FIRST = LAST = 0;
	end = CURRENT = NULL;

	if (vb == NULL)
		vb = varray_open(sizeof(struct anchor), 1000);
	else
		varray_reset(vb);

	for (db = GTAGS; db < GTAGLIM; db++) {
		XARGS *xp;
		char *ctags_xid;

		if ((xp = anchor_input[db]) == NULL)
			continue;
		/*
		 * Read from input stream until it reaches end of file
		 * or the line of another file appears.
		 */
		while ((ctags_xid = xargs_read(xp)) != NULL) {
			SPLIT ptable;
			struct anchor *a;
			int type, fid;
			const char *ctags_x = parse_xid(ctags_xid, NULL, &fid);

			/*
			 * It is the following file.
			 */
			if (current_fid != fid) {
				xargs_unread(xp);
				break;
			}
			if (split(ctags_x, 4, &ptable) < 4) {
				recover(&ptable);
				die("too small number of parts in anchor_load().\n'%s'", ctags_x);
			}
			if (db == GTAGS) {
				char *p = ptable.part[PART_LINE].start;

				for (; *p && isspace((unsigned char)*p); p++)
					;
				if (!*p) {
					recover(&ptable);
					die("The output of parser is illegal.\n%s", ctags_x);
				}
				/*
				 * Function header is applied only to the anchor whoes type is 'D'.
				 * (D: function, M: macro, T: type)
				 */
				type = 'T';
				if (*p == '#')
					type = 'M';
				else if (locatestring(p, "typedef", MATCH_AT_FIRST))
					type = 'T';
				else if ((p = locatestring(p, ptable.part[PART_TAG].start, MATCH_FIRST)) != NULL) {
					/* skip a tag and the following blanks */
					p += strlen(ptable.part[PART_TAG].start);
					for (; *p && isspace((unsigned char)*p); p++)
						;
					if (*p == '(')
						type = 'D';
				}
			}  else if (db == GRTAGS)
				type = 'R';
			else
				type = 'Y';
			/* allocate an entry */
			a = varray_append(vb);
			a->lineno = atoi(ptable.part[PART_LNO].start);
			a->type = type;
			a->done = 0;
			settag(a, ptable.part[PART_TAG].start);
			recover(&ptable);
		}
		if (ctags_xid == NULL) {
			xargs_close(anchor_input[db]);
			anchor_input[db] = NULL;
		}
	}
	if (vb->length == 0) {
		table = NULL;
	} else {
		int i, used = vb->length;
		/*
		 * Sort by lineno.
		 */
		table = varray_assign(vb, 0, 0);
		qsort(table, used, sizeof(struct anchor), cmp); 
		/*
		 * Setup some lineno.
		 */
		for (i = 0; i < used; i++)
			if (table[i].type == 'D')
				break;
		if (i < used)
			FIRST = table[i].lineno;
		for (i = used - 1; i >= 0; i--)
			if (table[i].type == 'D')
				break;
		if (i >= 0)
			LAST = table[i].lineno;
	}
	/*
	 * Setup loop range.
	 */
	start = table;
	curp = NULL;
	end = &table[vb->length];
	/* anchor_dump(stderr, 0);*/
}
/*
 * anchor_unload: unload anchor table
 */
void
anchor_unload(void)
{
	struct anchor *a;

	for (a = start; a && a < end; a++) {
		if (a->reserve) {
			free(a->reserve);
			a->reserve = NULL;
		}
	}
	/* We don't free varray */
	/* varray_close(vb); */
	FIRST = LAST = 0;
	start = curp = end = NULL;
}
/*
 * anchor_first: return the first anchor
 */
struct anchor *
anchor_first(void)
{
	if (!start || start == end)
		return NULL;
	CURRENT = start;
	if (CURRENT->type == 'D')
		CURRENTDEF = CURRENT;
	return CURRENT;
}
/*
 * anchor_next: return the next anchor
 */
struct anchor *
anchor_next(void)
{
	if (!start)
		return NULL;
	if (++CURRENT >= end)
		return NULL;
	if (CURRENT->type == 'D')
		CURRENTDEF = CURRENT;
	return CURRENT;
}
/*
 * anchor_get: return the specified anchor
 *
 *	i)	name	name of anchor
 *	i)	length	lenght of the name
 *	i)	type	==0: not specified
 *			!=0: D, M, T, R, Y
 */
struct anchor *
anchor_get(const char *name, int length, int type, int lineno)
{
	struct anchor *p = curp ? curp : start;

	if (table == NULL)
		return NULL;
	if (p->lineno > lineno)
		return NULL;
	/*
	 * set pointer to the top of the cluster.
	 */
	for (; p < end && p->lineno < lineno; p++)
		;
	if (p >= end || p->lineno != lineno)
		return NULL;
	curp = p;
	for (; p < end && p->lineno == lineno; p++)
		if (!p->done && p->length == length && !strcmp(gettag(p), name))
			if (!type || p->type == type)
				return p;
	return NULL;
}
/*
 * define_line: check whether or not this is a define line.
 *
 *	i)	lineno	line number
 *	go)	curp	pointer to the current cluster
 *	r)		1: definition, 0: not definition
 */
int
define_line(int lineno)
{
	struct anchor *p = curp ? curp : start;

	if (table == NULL)
		return 0;
	if (p->lineno > lineno)
		return 0;
	/*
	 * set pointer to the top of the cluster.
	 */
	for (; p < end && p->lineno < lineno; p++)
		;
	if (p >= end || p->lineno != lineno)
		return 0;
	curp = p;
	for (; p < end && p->lineno == lineno; p++)
		if (p->type == 'D')
			return 1;
	return 0;
}
/*
 * anchor_getlinks: return anchor link array
 *		(previous, next, first, last, top, bottom)
 */
int *
anchor_getlinks(int lineno)
{
	static int ref[A_SIZE];
	int i;

	for (i = 0; i < A_SIZE; i++)
		ref[i] = 0;
	if (lineno >= 1 && start) {
		struct anchor *c, *p;

		if (CURRENTDEF == NULL) {
			for (c = start; c < end; c++)
				if (c->lineno == lineno && c->type == 'D')
					break;
			CURRENTDEF = c;
		} else {
			for (c = CURRENTDEF; c >= start; c--)
				if (c->lineno == lineno && c->type == 'D')
					break;
		}
		for (p = c - 1; p >= start; p--)
			if (p->type == 'D') {
				ref[A_PREV] = p->lineno;
				break;
			}
		for (p = c + 1; p < end; p++)
			if (p->type == 'D') {
				ref[A_NEXT] = p->lineno;
				break;
			}
	}
	if (FIRST > 0 && lineno != FIRST)
		ref[A_FIRST] = FIRST;
	if (LAST > 0 && lineno != LAST)
		ref[A_LAST] = LAST;
	if (lineno != 0)
		ref[A_TOP] = -1;
	if (lineno != -1)
		ref[A_BOTTOM] = -2;
	if (FIRST > 0 && FIRST == LAST) {
		if (lineno == 0)
			ref[A_LAST] = 0;
		if (lineno == -1)
			ref[A_FIRST] = 0;
	}
	return ref;
}
void
anchor_dump(FILE *op, int lineno)
{
	struct anchor *a;

	if (op == NULL)
		op = stderr;
	for (a = start; a < end ; a++)
		if (lineno == 0 || a->lineno == lineno)
			fprintf(op, "%d %s(%c)\n",
				a->lineno, gettag(a), a->type);
}
