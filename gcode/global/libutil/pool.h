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
#ifndef _POOL_H
#define _POOL_H

#include "obstack.h"

typedef struct {
	struct obstack obstack;		/* memory pool */
	char *first_object;		/* first object	(for reset) */
} POOL;

POOL *pool_open(void);
void *pool_malloc(POOL *, int);
char *pool_strdup(POOL *, const char *, int);
char *pool_strdup_withterm(POOL *, const char *, int);
void pool_reset(POOL *);
void pool_close(POOL *);

#endif /* ! _POOL_H */
