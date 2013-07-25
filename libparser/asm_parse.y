%{
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <assert.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "internal.h"
#include "die.h"
#include "linetable.h"
#include "strbuf.h"

#define YYLTYPE		int
#define YYLLOC_DEFAULT(Current, Rhs, N)	((Current) = (Rhs)[1])

#undef PUT
#define PUT(type, tag, lno) do {					\
	const char *line_image = linetable_get(lno, NULL);		\
	char *nl = strchr(line_image, '\n');				\
	if (nl != NULL)							\
		*nl = '\0';						\
	param->put(type, tag, lno, param->file, line_image, param->arg);\
	if (nl != NULL)							\
		*nl = '\n';						\
} while (0)

#define GET_SYM(offset) (assert((offset) < strbuf_getlen(asm_symtable)),\
			 &strbuf_value(asm_symtable)[offset])

STRBUF *asm_symtable;

static void yyerror(const struct parser_param *, const char *);

%}

%token ASM_CONST		/* number, string, character */

%token ASM_CALL			/* call, jsr */
%token ASM_ENTRY		/* ENTRY, ALTENTRY, ... */
%token ASM_EXT			/* EXT, SYMBOL_NAME, ... */
%token ASM_SYMBOL
%token ASM_LABEL		/* ^sym */

%token ASM_DEFINE "#define"
%token ASM_UNDEF "#undef"
%token ASM_DIRECTIVE		/* #xxx */

%token ASM_MACRO		/* .macro */
%token ASM_EQU			/* .equ */

%start input
%name-prefix="asm_"

%parse-param { const struct parser_param *param }
%lex-param { const struct parser_param *param }

%%

input:	/* empty */
	| input line
;

line:	ASM_ENTRY '(' ASM_SYMBOL ')' error '\n'
		{
			PUT(PARSER_REF_SYM, GET_SYM($1), @1);
			PUT(PARSER_DEF, GET_SYM($3), @3);
			strbuf_reset(asm_symtable);
		}
	| ASM_CALL ASM_SYMBOL error '\n'
		{
			const char *sym = GET_SYM($2);

			if (sym[0] == '_') {
				int c = (unsigned char)sym[1];

				if (isalpha(c) || c == '_' || c >= 0x80)
					PUT(PARSER_REF_SYM, &sym[1], @2);
			}
			strbuf_reset(asm_symtable);
		}
	| ASM_CALL ASM_EXT '(' ASM_SYMBOL ')' error '\n'
		{
			PUT(PARSER_REF_SYM, GET_SYM($2), @2);
			PUT(PARSER_REF_SYM, GET_SYM($4), @4);
			strbuf_reset(asm_symtable);
		}
	| "#define" ASM_SYMBOL error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($2), @2);
			strbuf_reset(asm_symtable);
		}
	| "#undef" ASM_SYMBOL error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($2), @2);
			strbuf_reset(asm_symtable);
		}
	| ASM_MACRO ASM_SYMBOL error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($2), @2);
			strbuf_reset(asm_symtable);
		}
	| ASM_LABEL ASM_MACRO error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($1), @1);
			strbuf_reset(asm_symtable);
		}
	| ASM_EQU ASM_SYMBOL ',' error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($2), @2);
			strbuf_reset(asm_symtable);
		}
	| ASM_LABEL ASM_EQU error '\n'
		{
			PUT(PARSER_DEF, GET_SYM($1), @1);
			strbuf_reset(asm_symtable);
		}
	| error '\n'
		{ strbuf_reset(asm_symtable); }
;

%%

void
assembly(const struct parser_param *param)
{
	if (linetable_open(param->file) == -1)
		die("'%s' cannot open.", param->file);

	asm_symtable = strbuf_open(0);
	asm_initscan();

	asm_parse(param);

	strbuf_close(asm_symtable);
	linetable_close();
}

static void
yyerror(const struct parser_param *param, const char *s)
{

}
