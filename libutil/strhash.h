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
#ifndef _STRHASH_H
#define _STRHASH_H

#include "pool.h"
#include "queue.h"

struct sh_entry {
	SLIST_ENTRY(sh_entry) ptr;
	char *name;			/* name:  hash key		*/
	void *value;			/* value: user structure	*/
};

SLIST_HEAD(sh_head, sh_entry);

typedef struct {
	int buckets;			/* number of buckets		*/
	struct sh_head *htab;		/* hash buckets			*/
	POOL *pool;			/* memory pool			*/
	unsigned long entries;		/* number of entries		*/
	/*
	 * iterator
	 */
	struct sh_entry *cur_entry;
	int cur_bucket;
} STRHASH;

STRHASH *strhash_open(int);
struct sh_entry *strhash_assign(STRHASH *, const char *, int);
char * strhash_strdup(STRHASH *, const char *, int);
struct sh_entry *strhash_first(STRHASH *);
struct sh_entry *strhash_next(STRHASH *);
void strhash_reset(STRHASH *);
void strhash_close(STRHASH *);

#endif /* ! _STRHASH_H */
