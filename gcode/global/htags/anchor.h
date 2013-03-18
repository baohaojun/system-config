/*
 * Copyright (c) 2004, 2005 Tama Communications Corporation
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
#ifndef _ANCHOR_H_
#define _ANCHOR_H_

#include "checkalloc.h"
/*
 * Anchor table.
 *
 * Most names are written to tag[] directly.
 * Long name whose length > ANCHOR_NAMELEN are written to newly allocated
 * memory and are linked to reserve. It is necessary to clear the variable
 * which is not used.
 */
#define ANCHOR_NAMELEN	32
struct anchor {
        int lineno;
        char type;
	char done;
	int length;
        char tag[ANCHOR_NAMELEN];
	char *reserve;
};

#define gettag(a)	(a->tag[0] ? a->tag : a->reserve)
#define settag(a, b)	do {						\
	char *tag = b;							\
	(a)->length = strlen(tag);					\
	if ((a)->length < ANCHOR_NAMELEN) {				\
		strlimcpy((a)->tag, tag, sizeof((a)->tag));		\
		(a)->reserve = NULL;					\
	} else {							\
		(a)->reserve = check_strdup(tag);			\
		(a)->tag[0] = '\0';					\
	}								\
} while (0)

#define	A_PREV		0
#define	A_NEXT		1
#define	A_FIRST		2
#define	A_LAST		3
#define	A_TOP		4
#define	A_BOTTOM	5
#define A_SIZE		6

#define A_INDEX		6
#define A_HELP		7
#define A_LIMIT		8

void anchor_prepare(FILE *);
void anchor_load(const char *);
void anchor_unload(void);
struct anchor *anchor_first(void);
struct anchor *anchor_next(void);
struct anchor *anchor_get(const char *, int, int, int);
int define_line(int);
int *anchor_getlinks(int);
void anchor_dump(FILE *, int);

#endif /* _ANCHOR_H_ */
