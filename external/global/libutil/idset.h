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
#ifndef _IDSET_H_
#define _IDSET_H_

/*
 * Any id is not equal to END_OF_ID.
 */
#define END_OF_ID ((unsigned int)(-1))

typedef struct {
	unsigned int size;
	unsigned int min;
	unsigned int max;
	unsigned int lastid;		/* used by idset_first() and idset_next() */
	unsigned long *set;
} IDSET;

IDSET *idset_open(unsigned int);
int idset_empty(IDSET *);
void idset_add(IDSET *, unsigned int);
int idset_contains(IDSET *, unsigned int);
unsigned int idset_first(IDSET *);
unsigned int idset_next(IDSET *);
unsigned int idset_count(IDSET *);
void idset_close(IDSET *);

#endif /* ! _IDSET_H_ */
