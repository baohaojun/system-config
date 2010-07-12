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
 * scanner for C and yacc source code.
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
#include "../libparser/c_res.h"

#define lex_symbol_generation_rule(x) c_ ## x
#include "lexcommon.h"

#ifdef ECHO
#undef ECHO
#endif
#define ECHO	echos(LEXTEXT)

#define YY_USER_ACTION DEFAULT_YY_USER_ACTION

static int last_directive;
static int brace_level;
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
YACC_DIRECTIVE	%{ALPHA}({ALPHANUM}|-)*
YACC_IDENTIFIER	({ALPHA}|\.)({ALPHANUM}|\.)*

%start	C C_COMMENT CPP_COMMENT SHELL_COMMENT STRING LITERAL PREPROCESSOR_LINE
%start	YACC_DECLARATIONS YACC_RULES YACC_PROLOGUE BRACED_CODE
%option 8bit noyywrap noyy_top_state stack never-interactive prefix="c_"
%%
 /* Backslash-newline */
\\\n		DEFAULT_BACKSLASH_NEWLINE_ACTION

 /* Comment */
<C,PREPROCESSOR_LINE,YACC_DECLARATIONS,YACC_RULES,YACC_PROLOGUE,BRACED_CODE>{
	"/*"	{ echos(comment_begin); ECHO; yy_push_state(C_COMMENT); }
}
<C_COMMENT>"*/"	{ ECHO; echos(comment_end); yy_pop_state(); }
<C_COMMENT>.	{ put_char(LEXTEXT[0]); }
<C_COMMENT><<EOF>> {
		if (wflag)
			unexpected_eof(LINENO);
		yyterminate();
	}
<C,PREPROCESSOR_LINE,YACC_DECLARATIONS,YACC_RULES,YACC_PROLOGUE,BRACED_CODE>{
	"//"	{ echos(comment_begin); ECHO; yy_push_state(CPP_COMMENT); }
}

 /* String */
<C,PREPROCESSOR_LINE,YACC_DECLARATIONS,YACC_RULES,YACC_PROLOGUE,BRACED_CODE>{
	\"	{ ECHO; yy_push_state(STRING); }
}
<STRING>\"	{ ECHO; yy_pop_state(); }
<STRING>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Literal */
<C,PREPROCESSOR_LINE,YACC_DECLARATIONS,YACC_RULES,YACC_PROLOGUE,BRACED_CODE>{
	\'	{ ECHO; yy_push_state(LITERAL); }
}
<LITERAL>\'	{ ECHO; yy_pop_state(); }
<LITERAL>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Preprocessing directive */
<C,YACC_PROLOGUE,BRACED_CODE>{
	^[ \t]*\#[ \t]*(import|include|include_next) {
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
	^[ \t]*\#[ \t]*{WORD} {
		if ((last_directive = c_reserved_sharp(LEXTEXT, LEXLENG)) != 0)
			put_macro(LEXTEXT);
		else {
			ECHO;
			if (wflag)
				unknown_preprocessing_directive(LEXTEXT, LINENO);
		}
		yy_push_state(PREPROCESSOR_LINE);
	}
	/* Null directive */
	^[ \t]*\#	{ put_macro(LEXTEXT); }
}

 /* Yacc directive */
<YACC_DECLARATIONS,YACC_RULES>{
	{YACC_DIRECTIVE} {
		if (c_reserved_yacc(LEXTEXT, LEXLENG))
			put_reserved_word(LEXTEXT);
		else {
			ECHO;
			if (wflag)
				unknown_yacc_directive(LEXTEXT, LINENO);
		}
	}
	{YACC_IDENTIFIER} {
		struct anchor *a = anchor_get(LEXTEXT, LEXLENG, 0, LINENO);

		if (a) {
			put_anchor(gettag(a), a->type, LINENO);
			a->done = 1;
		} else {
			ECHO;
		}
	}
	"{"	{ put_brace(LEXTEXT); brace_level = 1; yy_push_state(BRACED_CODE); }
	"}"	{ put_brace(LEXTEXT); if (wflag) missing_left("{", LINENO); }
}
<BRACED_CODE>"{"	{ put_brace(LEXTEXT); brace_level++; }
<BRACED_CODE>"}"	{ put_brace(LEXTEXT); if (--brace_level == 0) yy_pop_state(); }
<YACC_DECLARATIONS>"%{"	{ put_reserved_word(LEXTEXT); yy_push_state(YACC_PROLOGUE); }
<YACC_PROLOGUE>"%}"	{ put_reserved_word(LEXTEXT); yy_pop_state(); }
<YACC_DECLARATIONS>"%%"	{ put_reserved_word(LEXTEXT); BEGIN YACC_RULES; }
<YACC_RULES>"%%"	{ put_reserved_word(LEXTEXT); BEGIN C; }
<YACC_DECLARATIONS,YACC_PROLOGUE,BRACED_CODE><<EOF>> {
		if (wflag)
			unexpected_eof(LINENO);
		yyterminate();
	}

<C,PREPROCESSOR_LINE,YACC_DECLARATIONS,YACC_RULES,YACC_PROLOGUE,BRACED_CODE>{
	{NUMBER}	ECHO;
}
<C,PREPROCESSOR_LINE,YACC_PROLOGUE,BRACED_CODE>{
	{WORD} {
		if (c_reserved_word(LEXTEXT, LEXLENG))
			put_reserved_word(LEXTEXT);
		else if (YY_START == PREPROCESSOR_LINE
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
}
<C,PREPROCESSOR_LINE,YACC_PROLOGUE>{
	[{}]	{ put_brace(LEXTEXT); }
}

 /* New line */
\n		DEFAULT_END_OF_LINE_ACTION
.		{ put_char(LEXTEXT[0]); }

%%
void
c_parser_init(FILE *ip)
{
	newline_terminate_string = 1;
	DEFAULT_BEGIN_OF_FILE_ACTION
	BEGIN C;
}

void
yacc_parser_init(FILE *ip)
{
	newline_terminate_string = 1;
	DEFAULT_BEGIN_OF_FILE_ACTION
	BEGIN YACC_DECLARATIONS;
}
