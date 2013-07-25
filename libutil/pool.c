/*
 * Copyright (c) 2006 Tama Communications Corporation
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
#include <stdlib.h>
#include <string.h>

#include "checkalloc.h"
#include "pool.h"

/*

Pool: usage and memory status

pool = pool_open();					[head]

memory = pool_alloc(pool, 10);				[head] [..........]
memory = pool_alloc(pool, 10);				[head] [..........][..........]
pool_reset(pool);					[head] [++++++++++][++++++++++]
string = pool_strdup(pool, "12345", 0);			[head] [12345]++++][++++++++++]
string = pool_strdup_withterm(pool, "12345:678", ':');	[head] [12345][12345]+++++++++]
								(...: alloc, +++: free)
pool_close(pool);					(nothing)

*/

#define obstack_chunk_alloc check_malloc
#define obstack_chunk_free free

/*
 * pool_open: open memory pool
 *
 *	r)	pool	POOL structure
 */
POOL *
pool_open(void)
{
	POOL *pool = (POOL *)check_calloc(sizeof(POOL), 1);

	obstack_init(&pool->obstack);
	pool->first_object = obstack_alloc(&pool->obstack, 1);
	return pool;
}
/*
 * pool_malloc: allocate memory from pool
 *
 *	i)	pool	POOL structure
 *	i)	size	memory size
 *	r)		allocated memory
 */
void *
pool_malloc(POOL *pool, int size)
{
	return obstack_alloc(&pool->obstack, size);
}
/*
 * pool_strdup: memory pool version of strdup()
 *
 *	i)	pool	POOL structure
 *	i)	s	string
 *	r)		allocated memory
 */
char *
pool_strdup(POOL *pool, const char *string, int size)
{
	if (size == 0)
		size = strlen(string);
	return obstack_copy0(&pool->obstack, string, size);
}
/*
 * pool_strdup_withterm: memory pool version of strdup()
 *
 *	i)	pool	POOL structure
 *	i)	s	string
 *	i)	term	terminate character
 *	r)		allocated memory
 */
char *
pool_strdup_withterm(POOL *pool, const char *string, int term)
{
	const char *p = strchr(string, term);
	int size = p ? p - string : strlen(string);
	return obstack_copy0(&pool->obstack, string, size);
}
/*
 * pool_reset: reset memory pool
 *
 *	i)	pool	POOL structure
 */
void
pool_reset(POOL *pool)
{
	/*
	 * Free all memory in pool->obstack but leave it valid for further allocation.
	 */
	obstack_free(&pool->obstack, pool->first_object);
}
/*
 * pool_close: close memory pool
 *
 *	i)	sh	POOL structure
 */
void
pool_close(POOL *pool)
{
	obstack_free(&pool->obstack, NULL);
	free(pool);
}
