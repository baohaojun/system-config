%{
/*
 * Copyright (c) 2002, 2004, 2005, 2006, 2008 Tama Communications Corporation
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
 * scanner for php source code.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#include "global.h"
#include "anchor.h"
#include "common.h"
#include "incop.h"
#include "htags.h"
#include "../libparser/php_res.h"

#define lex_symbol_generation_rule(x) php_ ## x
#include "lexcommon.h"

#ifdef ECHO
#undef ECHO
#endif
#define ECHO	echos(LEXTEXT)

#define YY_USER_ACTION DEFAULT_YY_USER_ACTION
#define LEFT_BRACE '('

static int pre_here_document;
static char end_of_here_document[IDENTLEN];

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

%start	PHP C_COMMENT CPP_COMMENT SHELL_COMMENT STRING LITERAL HEREDOCUMENT PREPROCESSOR_LINE
%option 8bit noyywrap noyy_top_state stack never-interactive prefix="php_"
%%
 /* Start PHP */
<INITIAL>"<?="		{ put_string(LEXTEXT); BEGIN PHP; }
<INITIAL>"<?"		{ put_string(LEXTEXT); BEGIN PHP; }
<INITIAL>"<?php"	{ put_string(LEXTEXT); BEGIN PHP; }
<INITIAL>"<%"		{ put_string(LEXTEXT); BEGIN PHP; }
<INITIAL>"<script[ \t]+language=(\")?php(\")?>" { put_string(LEXTEXT); BEGIN PHP; }
 /* Ignore HTML */
<INITIAL>.              put_string(LEXTEXT);
 /* End of PHP */
<PHP>"?>"		{ put_string(LEXTEXT); BEGIN INITIAL; }
<PHP>"%>"               { put_string(LEXTEXT); BEGIN INITIAL; }
<PHP>"</script>"        { put_string(LEXTEXT); BEGIN INITIAL; }

 /* Comment */
<PHP>"/*"	{ echos(comment_begin); ECHO; yy_push_state(C_COMMENT); }
<C_COMMENT>"*/"	{ ECHO; echos(comment_end); yy_pop_state(); }
<C_COMMENT>.	{ put_char(LEXTEXT[0]); }
<PHP>"#"        { echos(comment_begin); ECHO; yy_push_state(SHELL_COMMENT); }
<PHP>"//"	{ echos(comment_begin); ECHO; yy_push_state(CPP_COMMENT); }

 /* String */
<PHP>\"		{ ECHO; yy_push_state(STRING); }
<STRING>\"	{ ECHO; yy_pop_state(); }
<STRING>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Literal */
<PHP>\'		{ ECHO; yy_push_state(LITERAL); }
<LITERAL>\'	{ ECHO; yy_pop_state(); }
<LITERAL>\\.	{ put_char(LEXTEXT[0]); put_char(LEXTEXT[1]); }

 /* Here document */
<PHP><<<{WORD} {
		/* extract word and save */
		if (LEXLENG - 3 > IDENTLEN)
			die("Too long name '%s'.", LEXTEXT + 3);
		strcpy(end_of_here_document, LEXTEXT + 3);
		put_string("<<<");
		put_reserved_word(end_of_here_document);
		/* begin here document from the next line */
		pre_here_document = 1;
	}
<HEREDOCUMENT>^[ \t]*{WORD} {
		const char *keyword = strtrim((const char *)LEXTEXT, TRIM_HEAD, NULL);
		put_reserved_word(LEXTEXT);
		if (!strcmp(end_of_here_document, keyword)) {
			end_of_here_document[0] = '\0';
			yy_pop_state();
		}
	}
<PHP>^[ \t]*(include|require|require_once) {
		int c;

		put_reserved_word(LEXTEXT);
		/*
		 * include  ('aaa/bbb.h');
		 * include   'aaa/bbb.h';
		 *        ^
		 */
		while ((c = input()) && c != '\n' && isspace(c))
			echoc(c);
		if (c == '\n')
			unput(c);
		else if (c) {
			char path[MAXPATHLEN], *p = path;
			int sep = 0;

			if (c == LEFT_BRACE)
				c = input();
			if (c == '"' || c == '\'')
				sep = c;
			echoc(c);

			/* pick up path name */
			while ((c = input()) && c != '\n' && c != sep)
				*p++ = c;
			*p = '\0';
			if (c == sep) {
				char tmp[MAXPATHLEN];
				char normalized_path[MAXPATHLEN];

				snprintf(tmp, sizeof(tmp), "%s/%s", get_current_dir(), path);
				/*
				 * pick up the file only when it exists.
				 */
				if (normalize(tmp, get_root_with_slash(), get_cwd(), normalized_path, sizeof(normalized_path))
					&& test("f", normalized_path))
					put_include_anchor_direct(normalized_path, path);
				else
					echos(path);
				echoc(sep);
			} else {
				echos(path);
				if (c)
					unput(c);
			}
		}
	}
<PHP>{NUMBER}	ECHO;
<PHP,STRING,HEREDOCUMENT>\${WORD} |
<PHP,STRING,HEREDOCUMENT>\$\{{WORD}\} {
		struct anchor *a = NULL;
		const char *p = LEXTEXT + 1;	/* skip '$' */
		int brace = 0, i = 0;

		/*
		 * extract name.
		 */
		if (*p == '{') {
			char buf[IDENTLEN];

			brace = 1;
			for (p++; *p && *p != '}'; p++) {
				buf[i++] = *p;
				if (i >= sizeof(buf))
					die("Too long name '%s'.", LEXTEXT);
			}
			buf[i] = '\0';
			p = buf;
		} else {
			i = LEXLENG - 1;
		}
		if ((a = anchor_get(p, i, 'Y', LINENO)) != NULL) {
			echoc('$');
			if (brace)
				echoc('{');
			put_anchor(gettag(a), a->type, LINENO);
			if (brace)
				echoc('}');
			a->done = 1;
		} else {
			ECHO;
		}
	}
<PHP>{WORD} {
		struct anchor *a = NULL;
		if (php_reserved_word(LEXTEXT, LEXLENG))
			put_reserved_word(LEXTEXT);
		else {
			a = anchor_get(LEXTEXT, LEXLENG, 0, LINENO);
			if (a) {
				put_anchor(gettag(a), a->type, LINENO);
				a->done = 1;
			} else {
				ECHO;
			}
		}
	}
<PHP>[{}]	{ put_brace(LEXTEXT); }
 /* New line */
\n		{
			DEFAULT_END_OF_LINE_ACTION
			if (pre_here_document == 1) {
				pre_here_document = 0;
				yy_push_state(HEREDOCUMENT);
			}
		}
.		{ put_char(LEXTEXT[0]); }

%%
void
php_parser_init(FILE *ip)
{
	DEFAULT_BEGIN_OF_FILE_ACTION
}
