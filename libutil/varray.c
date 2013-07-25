/*
 * Copyright (c) 2004 Tama Communications Corporation
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
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#include <stdio.h>
#include "checkalloc.h"
#include "die.h"
#include "varray.h"

/*

Virtual array: usage and memory status

_: allocated but not assigned.
@: assigned but the value is uncertainty.

Function call                           Memory status
----------------------------------------------------------
                                        (not exist)
vb = varray_open(sizeof(int), 5);	||

int *a = varray_assign(vb, 0, 0);	||

	a == NULL

int *a = varray_assign(vb, 0, 1);	|@|_|_|_|_|		// expand

printf("%d allocated, %d assigned.\n", vb->alloced, vb->length);

	"5 allocated, 1 assigned."
					v
*a = 3;					|3|_|_|_|_|

					      v
int *a = varray_assign(vb, 3, 1);	|3|@|@|@|_|
					      v
*a = 8;					|3|@|@|8|_|
					          v
int *a = varray_assign(vb, 5, 1);	|3|@|@|8|@|@|_|_|_|_|	// expand
					          v
*a = 5;					|3|@|@|8|@|5|_|_|_|_|

printf("%d allocated, %d assigned.\n", vb->alloced, vb->length);

	"10 allocated, 6 assigned."

// After construction, you can treat it as a C array.

int i, *a = varray_assign(vb, 0, 0);

for (i = 0; i < vb->length; i++)
	printf("%d: %d\n", i, a[i]); // a[1], a[2], a[4] is uncertainty.
*/

#define DEFAULT_EXPAND	100
static int debug = 0;
/*
 * varray_open: open virtual array.
 *
 *	i)	size	size of entry
 *	i)	expand	expand array size
 *			if 0 is specified then use DEFAULT_EXPAND.
 *	r)	vb	VARRAY structure
 */
VARRAY *
varray_open(int size, int expand)
{
	VARRAY *vb = (VARRAY *)check_calloc(sizeof(VARRAY), 1);

	if (size < 1)
		die("varray_open: size < 1.");
	if (expand < 0)
		die("varray_open: expand < 0.");
	vb->size = size;
	vb->alloced = vb->length = 0;
	vb->expand = (expand == 0) ? DEFAULT_EXPAND : expand;
	vb->vbuf = NULL;
	return vb;
}
/*
 * varray_assign: assign varray entry.
 *
 *	i)	vb	VARRAY structure
 *	i)	index	index
 *	i)	force	if entry not found, create it.
 *	r)		pointer of the entry
 *
 * If specified entry is found then it is returned, else it is allocated
 * and returned.
 * This procedure doesn't operate the contents of the array.
 */
void *
varray_assign(VARRAY *vb, int index, int force)
{
	if (index < 0)
		die("varray_assign: illegal index value.");
	if (index >= vb->length) {
		if (force)
			vb->length = index + 1;
		else if (index == 0 && vb->length == 0)
			return NULL;
		else
			die("varray_assign: index(=%d) is out of range.", index);
	}
	/*
 	 * Expand the area.
	 */
	if (index >= vb->alloced) {
		int old_alloced = vb->alloced;

		while (index >= vb->alloced)
			vb->alloced += vb->expand;
		/*
		 * Old implementations of realloc() may crash
		 * when a null pointer is passed.
		 * Therefore, we cannot use realloc(NULL, ...).
		 */
		if (vb->vbuf == NULL)
			vb->vbuf = (char *)check_malloc(vb->size * vb->alloced);
		else
			vb->vbuf = (char *)check_realloc(vb->vbuf, vb->size * vb->alloced);
		if (debug)
			fprintf(stderr, "Expanded: from %d to %d.\n", old_alloced, vb->alloced);
	}
	return (void *)(vb->vbuf + vb->size * index);
}

/*
 * varray_append: append varray entry.
 *
 *	i)	vb	VARRAY structure
 *	r)		pointer of the entry
 *
 * This procedure doesn't operate the contents of the array.
 */
void *
varray_append(VARRAY *vb)
{
	return varray_assign(vb, vb->length, 1);
}
/*
 * varray_reset: reset varray array.
 *
 *	i)	vb	VARRAY structure
 */
void
varray_reset(VARRAY *vb)
{
	vb->length = 0;
}
/*
 * varray_close: close varray array.
 *
 *	i)	vb	VARRAY structure
 */
void
varray_close(VARRAY *vb)
{
	if (vb) {
		if (vb->vbuf)
			(void)free(vb->vbuf);
		(void)free(vb);
	}
}
