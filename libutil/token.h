/*
 * Copyright (c) 1998, 1999, 2000, 2003
 *	Tama Communications Corporation
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

#ifndef _TOKEN_H_
#define _TOKEN_H_

#include "gparam.h"
#include "strbuf.h"

#define SYMBOL		0

extern const char *sp, *cp, *lp;
extern int lineno;
extern int crflag;
extern int cmode;
extern int cppmode;
extern int ymode;
extern char token[MAXTOKEN];
extern char curfile[MAXPATHLEN];
extern int continued_line;

#define nextchar() \
	(cp == NULL ? \
		((sp = cp = strbuf_fgets(ib, ip, STRBUF_NOCRLF)) == NULL ? \
			EOF : \
			(lineno++, *cp == 0 ? \
				(lp = cp, cp = NULL, continued_line = 0, '\n') : \
				(unsigned char)*cp++)) : \
		(*cp == 0 ? \
			(lp = cp, cp = NULL, continued_line = 0, '\n') : \
			(unsigned char)*cp++))
#define atfirst (sp && sp == (cp ? cp - 1 : lp))

int opentoken(const char *);
void closetoken(void);
int nexttoken(const char *, int (*)(const char *, int));
void pushbacktoken(void);
int peekc(int);
void throwaway_nextchar();
int atfirst_exceptspace(void);

#endif /* ! _TOKEN_H_ */
