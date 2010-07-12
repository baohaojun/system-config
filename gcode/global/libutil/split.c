/*
 * Copyright (c) 2002, 2010 Tama Communications Corporation
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
#include "split.h"
#include "die.h"

/*
 * Substring manager like perl's split.
 *
 * Initial status.
 *              +------------------------------------------------------
 *         line |main         100    ./main.c        main(argc, argv)\n
 *
 * The result of split(line, 4, &list):
 *
 *              +------------------------------------------------------
 * list    line |main\0       100\0  ./main.c\0      main(argc, argv)\n
 * +---------+   ^   ^        ^  ^   ^       ^       ^
 * |npart=4  |   |   |        |  |   |       |       |
 * +---------+   |   |        |  |   |       |       |
 * | start  *----+   |        |  |   |       |       |
 * | end    *--------+        |  |   |       |       |
 * | save ' '|                |  |   |       |       |
 * +---------+                |  |   |       |       |
 * | start  *-----------------+  |   |       |       |
 * | end    *--------------------+   |       |       |
 * | save ' '|                       |       |       |
 * +---------+                       |       |       |
 * | start  *------------------------+       |       |
 * | end    *--------------------------------+       |
 * | save ' '|                                       |
 * +---------+                                       |
 * | start  *----------------------------------------+
 * | end    *--+
 * | save    | |
 * +---------+ =
 *
 * The result of split(line, 2, &list):
 *
 *              +------------------------------------------------------
 * list    line |main\0       100    ./main.c        main(argc, argv)\n
 * +---------+   ^   ^        ^
 * |npart=2  |   |   |        |
 * +---------+   |   |        |
 * | start  *----+   |        |
 * | end    *--------+        |
 * | save ' '|                |
 * +---------+                |
 * | start  *-----------------+
 * | end    *--+
 * | save    | |
 * +---------+ =
 *
 * The result of recover().
 *              +------------------------------------------------------
 *         line |main         100    ./main.c        main(argc, argv)\n
 *
 * Recover() recover initial status of line with saved char in savec.
 */

#define isblank(c)	((c) == ' ' || (c) == '\t')

/*
 * split: split a string into pieces
 *
 *	i)	line	string
 *	i)	npart	parts number
 *	io)	list	split table
 *	r)		part count
 */
int
split(const char *line, int npart, SPLIT *list)		/* virtually const */
{
	char *s = (char *)line;
	struct part *part = &list->part[0];
	int count;

	if (npart > NPART)
		npart = NPART;
	npart--;
	for (count = 0; *s && count < npart; count++) {
		while (*s && isblank(*s))
			s++;
		if (*s == '\0')
			break;
		part->start = s;
		while (*s && !isblank(*s))
			s++;
		part->end = s;
		part->savec = *s;
		part++;
	}
	if (*s) {
		while (*s && isblank(*s))
			s++;
		part->start = s;
		part->end = (char *)0;
		part->savec = 0;
		count++;
		part++;
	}
	while (part-- > &list->part[0]) {
		if (part->savec != '\0')
			*part->end = '\0';
	}
	return list->npart = count;
}
/*
 * recover: recover initial status of line.
 *
 *	io)	list	split table
 */
void
recover(SPLIT *list)
{
	int i, c;
	for (i = 0; i < list->npart; i++) {
		if ((c = list->part[i].savec) != '\0')
			*(list->part[i].end) = c;
	}
}
/*
 * split_dump: dump split structure.
 */
void
split_dump(SPLIT *list)
{
	int i;
	struct part *part;

	fprintf(stderr, "npart: %d\n", list->npart);
	
	for (i = 0; i < list->npart; i++) {
		part = &list->part[i];
		fprintf(stderr, "string[%d]: |%s|\n", i, part->start);
		fprintf(stderr, "savec[%d] : |%c|\n", i, part->savec);
	}
}
/*
 * parse_xid: extract fid from ctags_xid format record.
 *
 *	i)	ctags_xid	ctags-xid record
 *	o)	s_fid		file id(string) if not NULL
 *	o)	n_fid		file id(integer) if not NULL
 *	r)			pointer to the ctags_x part
 */
const char *
parse_xid(const char *ctags_xid, char *s_fid, int *n_fid)
{
	const char *p;
	int i, n;

	i = n = 0;
	for (p = ctags_xid; *p && *p >= '0' && *p <= '9'; p++) {
		n = n * 10 + (*p - '0');
		if (s_fid)
			s_fid[i++] = *p;
	}
	if (*p++ != ' ')
		die("illegal ctags-xid format record. '%s'", ctags_xid);
	if (s_fid)
		s_fid[i] = '\0';
	if (n_fid != NULL)
		*n_fid = n;
	return p;
}
/*
 * nextstring: seek to the next string.
 *
 *      i)      s       original string
 *      r)              next string
 *
 *  s       v
 * "aaaaaa\0bbbbb\0"
 */
const char *
nextstring(const char *s)
{
	while (*s)
		s++;
	return s + 1;
}
/*
 * nextelement: seek to the next element
 *
 *	i)	s	point the current element or the following blanks
 *	r)		next element
 *
 *	s     s   return value
 *	v     v   v
 *	xxxxx     yyyyy    zzzzzz
 *	(current) (next)
 */
const char *
nextelement(const char *s)
{
	while (*s && !isblank(*s))
		s++;
	if (!*s)
		die("nextelement: unexpected end of string(1).");
	while (*s && isblank(*s))
		s++;
	if (!*s)
		die("nextelement: unexpected end of string(2).");
	return s;
}
