%{
/*
 * Copyright (c) 2002, 2004 Tama Communications Corporation
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

/*
 * scanner for java source code.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#include "global.h"
#include "anchor.h"
#include "common.h"
#include "htags.h"
#include "../libparser/java_res.h"

#define lex_symbol_generation_rule(x) java_ ## x
#include "lexcommon.h"

#ifdef ECHO
#undef ECHO
#endif
#define ECHO	echos(LEXTEXT)

#define YY_USER_ACTION DEFAULT_YY_USER_ACTION

%}
 /* Definitions */
H		0[Xx][0-9A-Fa-f]+
N		[0-9]+
L		{N}L?
D1		{N}\.{N}([Ee][+-]?{N})?
D2		\.{N}([Ee][+-]?{N})?
NUMBER		-?({L}|{D1}|{D2})
ALPHA		[a-zA-Z_\x80-\xff]
ALPHANUM	[a-zA-Z_\x80-\xff0-9]
WORD		{ALPHA}{ALPHANUM}*

%start	JAVA C_COMMENT CPP_COMMENT SHELL_COMMENT STRING LITERAL PREPROCESSOR_LINE
%option 8bit noyywrap noyy_top_state stack never-interactive prefix="java_"
%%
 /* Comment */
<JAVA>"/*"	{ echos(comment_begin); ECHO; yy_push_state(C_COMMENT); }
<C_COMMENT>"*/"	{ ECHO; echos(comment_end); yy_pop_state(); }
<C_COMMENT>.	{ put_char(LEXTEXT[0]); }
<JAVA>"//"	{ echos(comment_begin); ECHO; yy_push_state(CPP_COMMENT); }

 /* String */
<JAVA>\"	{ ECHO; yy_push_state(STRING); }
<STRING>\"	{ ECHO; yy_pop_state(); }
<STRING>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Literal */
<JAVA>\'	{ ECHO; yy_push_state(LITERAL); }
<LITERAL>\'	{ ECHO; yy_pop_state(); }
<LITERAL>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

<JAVA>^import/[ \t] {
		int c = 0;

		put_reserved_word(LEXTEXT);
		while ((c = input()) && c != '\n' && c != ';')
			echoc(c);
		if (c == '\n')
			unput(c);
		else if (c)
			echoc(c);
	}

<JAVA>{NUMBER}	ECHO;
<JAVA>{WORD} {
		if (java_reserved_word(LEXTEXT, LEXLENG))
			put_reserved_word(LEXTEXT);
		else {
			struct anchor *a = anchor_get(LEXTEXT, LEXLENG, 0, LINENO);
			if (a) {
				put_anchor(gettag(a), a->type, LINENO);
				a->done = 1;
			} else {
				ECHO;
			}
		}
	}
<JAVA>[{}]	{ put_brace(LEXTEXT); }
 /* New line */
\n		DEFAULT_END_OF_LINE_ACTION
.		{ put_char(LEXTEXT[0]); }

%%
void
java_parser_init(FILE *ip)
{
	DEFAULT_BEGIN_OF_FILE_ACTION
	BEGIN JAVA;
}
