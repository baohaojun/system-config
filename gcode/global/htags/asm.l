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
 * scanner for assembly source code.
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
#include "incop.h"
#include "common.h"
#include "htags.h"
#include "../libparser/asm_res.h"

#define lex_symbol_generation_rule(x) asm_ ## x
#include "lexcommon.h"

#ifdef ECHO
#undef ECHO
#endif
#define ECHO	echos(LEXTEXT)

#define YY_USER_ACTION DEFAULT_YY_USER_ACTION

static int last_directive;
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

%start	ASM C_COMMENT CPP_COMMENT SHELL_COMMENT STRING LITERAL PREPROCESSOR_LINE
%option 8bit noyywrap noyy_top_state stack never-interactive prefix="asm_"
%%
 /* Backslash-newline */
\\\n		DEFAULT_BACKSLASH_NEWLINE_ACTION

 /* Comment */
<ASM,PREPROCESSOR_LINE>"/*"	{ echos(comment_begin); ECHO; yy_push_state(C_COMMENT); }
<C_COMMENT>"*/"	{ ECHO; echos(comment_end); yy_pop_state(); }
<C_COMMENT>.	{ put_char(LEXTEXT[0]); }
<C_COMMENT><<EOF>> {
		if (wflag)
			unexpected_eof(LINENO);
		yyterminate();
	}
<ASM,PREPROCESSOR_LINE>"//"	{ echos(comment_begin); ECHO; yy_push_state(CPP_COMMENT); }

 /* String */
<ASM,PREPROCESSOR_LINE>\"	{ ECHO; yy_push_state(STRING); }
<STRING>\"	{ ECHO; yy_pop_state(); }
<STRING>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Literal */
<ASM,PREPROCESSOR_LINE>\'	{ ECHO; yy_push_state(LITERAL); }
<LITERAL>\'	{ ECHO; yy_pop_state(); }
<LITERAL>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Preprocessing directive */
<ASM>^[ \t]*\#[ \t]*(import|include|include_next) {
		int c;

		put_macro(LEXTEXT);
		/*
		 * #include|   <aaa/bbb.h>|
		 *         ~~~~~~~~~~~~~~~~
		 */
		while ((c = input()) && c != '\n' && isspace(c))
			echoc(c);
		if (c == '\n') {
			unput(c);
		} else if (c) {
			char path[MAXPATHLEN], *p = path;
			int sep = 0;

			if (c == '"')
				sep = c;
			else if (c == '<')
				sep = '>';
			put_char(c);

			/* pick up path name */
			while ((c = input()) && c != '\n' && c != sep)
				*p++ = c;
			*p = '\0';
			if (c == sep) {
				struct data *inc;
				const char *basename = locatestring(path, "/", MATCH_LAST);

				if (basename)
					basename++;
				else
					basename = path;
				inc = get_inc(basename);
				if (inc)
					put_include_anchor(inc, path);
				else
					echos(path);
				put_char(sep);
			} else {
				echos(path);
				if (c)
					unput(c);
			}
		}
	}
<ASM>^[ \t]*\#[ \t]*{WORD} {
		if ((last_directive = asm_reserved_sharp(LEXTEXT, LEXLENG)) != 0) {
			put_macro(LEXTEXT);
			yy_push_state(PREPROCESSOR_LINE);
		} else {
			/*
			 * Don't warn about unknown directive.
			 * '#' may be used as the start of a comment.
			 */
			echos(comment_begin);
			ECHO;
			yy_push_state(SHELL_COMMENT);
		}
	}
 /* Null directive */
<ASM>^[ \t]*\#	{ put_macro(LEXTEXT); }

<ASM,PREPROCESSOR_LINE>{NUMBER}	ECHO;
<ASM,PREPROCESSOR_LINE>{WORD} {
		if (YY_START == PREPROCESSOR_LINE
		    && (last_directive == SHARP_IF || last_directive == SHARP_ELIF)
		    && strcmp(LEXTEXT, "defined") == 0)
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
<ASM,PREPROCESSOR_LINE>[{}]	{ put_brace(LEXTEXT); }
 /* New line */
\n		DEFAULT_END_OF_LINE_ACTION
.		{ put_char(LEXTEXT[0]); }

%%
void
asm_parser_init(FILE *ip)
{
	newline_terminate_string = 1;
	DEFAULT_BEGIN_OF_FILE_ACTION
	BEGIN ASM;
}
