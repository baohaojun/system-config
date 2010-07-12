/*
 * Copyright (c) 2003, 2004, 2006 Tama Communications Corporation
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
#include "config.h"
#endif
#include <ctype.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#include "global.h"
#include "incop.h"

#if defined(_WIN32) || defined(__DJGPP__)
static const char *
strtolower(const char *s)
{
	static char lower[MAXPATHLEN];
	char *t = lower, *end = lower + sizeof(lower);

	do {
		if (t == end)
			die("name is too long.");
	} while ((*t++ = tolower((unsigned char)*s++)) != '\0');

	return lower;
}
#define HASH_KEY(name)	strtolower(name)
#else
#define HASH_KEY(name)	(name)
#endif
/*----------------------------------------------------------------------*/
/* Include path list							*/
/*----------------------------------------------------------------------*/
static STRHASH *head_inc;

/*
 * init_inc: initialize include file list.
 */
void
init_inc(void)
{
	head_inc = strhash_open(1024);
}
/*
 * put_inc: put include file.
 *
 *	i)	file	file name (the last component of the path)
 *	i)	path	path name or command line.
 *	i)	id	path id
 */
void
put_inc(const char *file, const char *path, int id)
{
	struct sh_entry *entry;
	struct data *data;

	entry = strhash_assign(head_inc, HASH_KEY(file), 1);
	data = entry->value;
	if (data == NULL) {
		data = (struct data *)check_malloc(sizeof(struct data));
#if defined(_WIN32) || defined(__DJGPP__)
		strlimcpy(data->name, file, sizeof(data->name));
#else
		data->name = entry->name;
#endif
		data->id = id;
		data->contents = strbuf_open(0);
		data->ref_contents = NULL;
		data->count = 0;
		data->ref_count = 0;
		entry->value = data;
	}
	strbuf_puts0(data->contents, path);
	data->count++;
}
/*
 * get_inc: get include file.
 *
 *	i)	path	path name or command line.
 *	r)		descriptor
 */
struct data *
get_inc(const char *name)
{
	struct sh_entry *entry = strhash_assign(head_inc, HASH_KEY(name), 0);

	return entry ? entry->value : NULL;
}
/*
 * first_inc: get the first include file.
 *
 *	r)		descriptor
 */
struct data *
first_inc(void)
{
	struct sh_entry *entry = strhash_first(head_inc);

	return entry ? entry->value : NULL;
}
/*
 * next_inc: get the next include file.
 *
 *	r)		descriptor
 */
struct data *
next_inc(void)
{
	struct sh_entry *entry = strhash_next(head_inc);

	return entry ? entry->value : NULL;
}


/*
 * put_included: put include file reference.
 *
 *	i)	data	inc structure
 *	i)	path	path name or command line.
 */
void
put_included(struct data *data, const char *path)
{
	if (data->ref_contents == NULL)
		data->ref_contents = strbuf_open(0);
	strbuf_puts0(data->ref_contents, path);
	data->ref_count++;
}
/*
 * get_included: get included file.
 *
 *	i)	path	path name or command line.
 *	r)		descriptor
 */
struct data *
get_included(const char *name)
{
	struct data *data = get_inc(name);

	return (data && data->ref_count) ? data : NULL;
}
/*
 * Terminate function is not needed.
 */
