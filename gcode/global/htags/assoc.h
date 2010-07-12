/*
 * Copyright (c) 2004, 2010 Tama Communications Corporation
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
#ifndef _ASSOC_H_
#define _ASSOC_H_

#include "dbop.h"

typedef struct {
	DBOP *dbop;
} ASSOC;

ASSOC *assoc_open(void);
void assoc_close(ASSOC *);
void assoc_put(ASSOC *, const char *, const char *);
void assoc_put_withlen(ASSOC *, const char *, const char *, int);
const char *assoc_get(ASSOC *, const char *);

#endif /* ! _ASSOC_H_ */
