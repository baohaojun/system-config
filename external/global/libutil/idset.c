/*
 * Copyright (c) 2005, 2006, 2007 Tama Communications Corporation
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
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include "checkalloc.h"
#include "die.h"
#include "idset.h"

#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#undef LONG_BIT
#define LONG_BIT	(sizeof(long) * CHAR_BIT)	/* maybe 32 or 64 */

/*
 * idset->min is initialized to END_OF_ID.
 * (You may use idset->max instead of idset->min.)
 */
#define IS_EMPTY(idset)	 ((idset)->min == END_OF_ID ? 1 : 0)

/*
Idset: usage and memory status

				idset->set
				[]

idset = idset_open(21)		000000000000000000000___________
				 v
idset_add(idset, 1)		010000000000000000000___________
				  v
idset_add(idset, 2)		011000000000000000000___________
				                    v
idset_add(idset, 20)		011000000000000000001___________

idset_contains(idset, 2) == true
idset_contains(idset, 3) == false

idset_close(idset)		[]

Idset's index always start from 0 according to the custom of C language.
I you want to treat 1-20 then you must invoke idset_open() with a argument 21.

        idset = idset_open(21);
        idset_add(idset, 0);            => OK
        idset_add(idset, 1);            => OK
                ...
        idset_add(idset, 20);           => OK
        idset_add(idset, 21);           => ERROR (idset_add: id is out of range.)

The range of value is from 0 to the maximum value expressible by unsigned integer - 1.
You should define index as an unsigned integer, and use END_OF_ID like follows:

	unsigned int id;
	for (id = idset_first(set); id != END_OF_ID; id = idset_next(set))
		-- processing about an id --
 */
/*
 * bit mask table
 * Prepare all bit mask for performance.
 */
static unsigned long *bit;

/*
 * Allocate memory for new idset.
 */
IDSET *
idset_open(unsigned int size)
{
	IDSET *idset = (IDSET *)check_malloc(sizeof(IDSET));
	int i;

	if (bit == NULL) {
		bit = (unsigned long *)check_calloc(sizeof(unsigned long), LONG_BIT);
		for (i = 0; i < LONG_BIT; i++)
			bit[i] = 1UL << i;
	}
	idset->set = (unsigned long *)check_calloc(sizeof(unsigned long), (size + LONG_BIT - 1) / LONG_BIT);
	idset->size = size;
	/*
	 * Initialize all id expressions using invalid value.
	 * END_OF_ID means 'no value' or 'out of range'.
	 */
	idset->min = idset->max = idset->lastid = END_OF_ID;
	return idset;
}
/*
 * Return true if idset is empty.
 *
 *	i)	idset	idset structure
 *	r)		1: empty, 0: not empty
 */
int
idset_empty(IDSET *idset)
{
	return IS_EMPTY(idset);
}
/*
 * Add id to the idset.
 *
 *	i)	idset	idset structure
 *	i)	id	id number
 */
void
idset_add(IDSET *idset, unsigned int id)
{
	int empty = IS_EMPTY(idset);

	if (id >= idset->size)
		die("idset_add: id is out of range.");
	idset->set[id / LONG_BIT] |= bit[id % LONG_BIT];
	if (empty)
		idset->max = idset->min = id;
	else if (id > idset->max)
		idset->max = id;
	else if (id < idset->min)
		idset->min = id;
}
/*
 * Whether or not idset includes specified id.
 *
 *	i)	idset	idset structure
 *	i)	id	id number
 *	r)		true: contains, false: doesn't contain
 */
int
idset_contains(IDSET *idset, unsigned int id)
{
	if (IS_EMPTY(idset))
		return 0;
	if (id < idset->min || id > idset->max)
		return 0;
	return (idset->set[id / LONG_BIT] & bit[id % LONG_BIT]) != 0;
}
/*
 * Get first id.
 *
 *      i)      idset   idset structure
 *      r)              id (END_OF_ID: end of id)
 *
 */
unsigned int
idset_first(IDSET *idset)
{
	/* There is no need to check whether idset is empty
	   because idset->min is initialized with END_OF_ID. */
	return idset->lastid = idset->min;
}
/*
 * Get next id.
 *
 *      i)      idset   idset structure
 *      r)              id (END_OF_ID: end of id)
 *
 */
unsigned int
idset_next(IDSET *idset)
{
	unsigned int i, limit;
	int index0, index1;

	if (IS_EMPTY(idset))
		return END_OF_ID;
	if (idset->lastid >= idset->max)
		return END_OF_ID;
	limit = idset->max / LONG_BIT + 1;
	index0 = idset->lastid / LONG_BIT;
	index1 = idset->lastid % LONG_BIT;
	for (i = ++index1; i < LONG_BIT; i++)
		if (bit[i] & idset->set[index0])
			return idset->lastid = index0 * LONG_BIT + i;
	index0++;
	for (i = index0; i < limit && idset->set[i] == 0; i++)
		;
	if (i >= limit)
		die("idset_next: internal error.");
	index0 = i;
	for (i = 0; i < LONG_BIT; i++)
		if (bit[i] & idset->set[index0])
			return idset->lastid = index0 * LONG_BIT + i;
	die("idset_next: internal error.");
}
/*
 * Return the number of bits.
 *
 *	i)	idset	idset structure
 *	r)		number of bits
 */
unsigned int
idset_count(IDSET *idset)
{
	unsigned int id, count = 0;

	for (id = idset_first(idset); id != END_OF_ID; id = idset_next(idset))
		count++;
	return count;
}
/*
 * Free memory for the idset.
 */
void
idset_close(IDSET *idset)
{
	free(idset->set);
	free(idset);
}
