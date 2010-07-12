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
#ifndef _VARRAY_H
#define _VARRAY_H

typedef struct _varray {
        char *vbuf;
	int size;
	int length;
	int alloced;
	int expand;
} VARRAY;

VARRAY *varray_open(int, int);
void *varray_assign(VARRAY *, int, int);
void *varray_append(VARRAY *);
void varray_reset(VARRAY *);
void varray_close(VARRAY *);

#endif /* ! _VARRAY_H */
