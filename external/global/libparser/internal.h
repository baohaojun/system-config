/*
 * Copyright (c) 1998, 1999, 2000, 2001, 2003, 2010
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

#ifndef _INTERNAL_H_
#define _INTERNAL_H_

#include "parser.h"
#include "strbuf.h"
#include "token.h"

#define PUT(type, tag, lno, line) do {					\
	DBG_PRINT(level, line);						\
	param->put(type, tag, lno, curfile, line, param->arg);		\
} while (0)

#define DBG_PRINT(level, a) do {					\
	if (param->flags & PARSER_DEBUG)				\
		dbg_print(level, a);					\
} while (0)

/* parser procedures */
void C(const struct parser_param *);
void yacc(const struct parser_param *);
void Cpp(const struct parser_param *);
void java(const struct parser_param *);
void php(const struct parser_param *);
void assembly(const struct parser_param *);

void dbg_print(int, const char *);

extern STRBUF *asm_symtable;
void asm_initscan(void);
int asm_lex(const struct parser_param *);

#endif
