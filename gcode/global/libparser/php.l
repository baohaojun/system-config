%{
/*
 * Copyright (c) 2003, 2006, 2010 Tama Communications Corporation
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
 * scanner for PHP source code.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "internal.h"
#include "die.h"
#include "gparam.h"
#include "linetable.h"
#include "strbuf.h"
#include "php_res.h"

#define lex_symbol_generation_rule(x) php_ ## x
#define LEXLEX lex_symbol_generation_rule(lex)
#define LEXTEXT lex_symbol_generation_rule(text)
#define LEXLENG lex_symbol_generation_rule(leng)
#define LEXRESTART lex_symbol_generation_rule(restart)
#define LEXLINENO lex_symbol_generation_rule(lineno)

#define YY_DECL	int LEXLEX(const struct parser_param *param)

#define PHP_TOKEN		1
#define PHP_VARIABLE		2
#define PHP_STRING		3
#define PHP_POINTER		4
#define PHP_DOLLAR		5
#define PHP_LPAREN		'('
#define PHP_RPAREN		')'
#define PHP_LBRACE		'{'
#define PHP_RBRACE		'}'
#define PHP_LBRACK		'['
#define PHP_RBRACK		']'

static void debug_print(const char *, ...);
static int level;			/* block nest level */
static STRBUF *string;			/* string */
static char end_of_here_document[IDENTLEN];
static int pre_here_document;

/*
 * For debug.
 */
static void
debug_print(const char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	(void)vfprintf(stderr, s, ap);
	va_end(ap);
}

#undef DBG_PRINT
#define DBG_PRINT if (!(param->flags & PARSER_DEBUG));else debug_print

#undef YYLMAX
#define YYLMAX 1024

#undef ECHO
#define ECHO DBG_PRINT("%s", LEXTEXT)

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

/*
 * IO routine.
 */
#define YY_INPUT(buf,result,max_size) \
	do { \
		if ((result = linetable_read(buf, max_size)) == -1) \
			result = YY_NULL; \
	} while (0)
%}
 /* Definitions */
H		0[Xx][0-9A-Fa-f]+
N		[0-9]+
L		{N}L?
D1		{N}\.{N}([Ee][+-]?{N})?
D2		\.{N}([Ee][+-]?{N})?
NUMBER		-?({L}|{D1}|{D2})
 /* We accept multi-bytes character */
ALPHA		[a-zA-Z_\x80-\xff]
ALPHANUM	[a-zA-Z_\x80-\xff0-9]
WORD		{ALPHA}{ALPHANUM}*
%start	PHP STRING LITERAL HEREDOCUMENT
%option 8bit caseless noyywrap nounput yylineno never-interactive prefix="php_"
%%
\n			{
				ECHO;
				if (pre_here_document == 1) {
					pre_here_document = 0;
					DBG_PRINT("[BEGIN HEREDOCUMENT:%s]\n", end_of_here_document);
					BEGIN HEREDOCUMENT;
				}
			}
 /* Start PHP */
<INITIAL>"<?="		ECHO; BEGIN PHP;
<INITIAL>"<?"		ECHO; BEGIN PHP;
<INITIAL>"<?php"	ECHO; BEGIN PHP;
<INITIAL>"<%"		ECHO; BEGIN PHP;
<INITIAL>"<script[ \t]+language=(\")?php(\")?>"	ECHO; BEGIN PHP;
 /* Ignore HTML */
<INITIAL>.		ECHO;
 /* End of PHP */
<PHP>"?>"		ECHO; BEGIN INITIAL;
<PHP>"%>"		ECHO; BEGIN INITIAL;
<PHP>"</script>"	ECHO; BEGIN INITIAL;

 /* Comment */
<PHP>"/*"		{
				int c;

				DBG_PRINT("</*");
				while ((c = input()) != EOF) {
					DBG_PRINT("%c", c);
					if (c == '*') {
						while ((c = input()) != EOF && c == '*')
							DBG_PRINT("%c", c);
						DBG_PRINT("%c", c);
						if (c == EOF || c == '/')
							break;
					}
				}
				if (c == EOF)
					die("unexpected end of comment.");
				DBG_PRINT(">");
			}
<PHP>"//".*		DBG_PRINT("<%s>", LEXTEXT);
<PHP>"#".*		DBG_PRINT("<%s>", LEXTEXT);

 /* String */
<PHP>\"			{ strbuf_reset(string); BEGIN STRING; }
<STRING>\"		{
				DBG_PRINT("<S:%s>", strbuf_value(string));
				BEGIN PHP;
					
				return PHP_STRING;
			}
<STRING>\\.		strbuf_puts(string, LEXTEXT);
<STRING>.		strbuf_putc(string, LEXTEXT[0]);

 /* Literal */
<PHP>\'			{ strbuf_reset(string); BEGIN LITERAL; }
<LITERAL>\'		{
				DBG_PRINT("<L:%s>", strbuf_value(string));
				BEGIN PHP;
					
				return PHP_STRING;
			}
<LITERAL>\\.		strbuf_puts(string, LEXTEXT);
<LITERAL>.		strbuf_putc(string, LEXTEXT[0]);

 /* Here document */
<PHP><<<{WORD}		{
				DBG_PRINT("<L:%s>", LEXTEXT);
				/* extract word and save */
				if (LEXLENG - 3 > IDENTLEN)
					die("Too long name '%s'.", LEXTEXT + 3);
				strcpy(end_of_here_document, LEXTEXT + 3);
				/* begin here document from the next line */
				pre_here_document = 1;
			}
<HEREDOCUMENT>^[ \t]*{WORD} {
				const char *keyword = strtrim((const char *)LEXTEXT, TRIM_HEAD, NULL);
				ECHO;
				if (!strcmp(end_of_here_document, keyword)) {
					DBG_PRINT("[END HEREDOCUMENT]");
					end_of_here_document[0] = '\0';
					BEGIN PHP;
				}
			}

 /* Cast */
<PHP>\([ \t]*(bool|boolean|int|integer|real|double|float|string|array|object)[ \t]*\)
			;
<PHP,STRING,HEREDOCUMENT>$\{{WORD}\} {
				/*
				 * 0123456	yyleng = 6
				 * ${abc}\0
				 */
				if (YY_START == STRING)
					strbuf_puts(string, LEXTEXT);
				memcpy(LEXTEXT, &LEXTEXT[2], LEXLENG - 3);
				LEXTEXT[LEXLENG - 3] = '\0';
				LEXLENG = LEXLENG - 3;
				DBG_PRINT("<V:%s>", LEXTEXT);

				return PHP_VARIABLE;
			}
<PHP,STRING,HEREDOCUMENT>${WORD} {
				/*
				 * 01234	yyleng = 4
				 * $abc\0
				 */
				if (YY_START == STRING)
					strbuf_puts(string, LEXTEXT);
				memcpy(LEXTEXT, &LEXTEXT[1], LEXLENG - 1);
				LEXTEXT[LEXLENG - 1] = '\0';
				LEXLENG = LEXLENG - 1;
				DBG_PRINT("<V:%s>", LEXTEXT);

				return PHP_VARIABLE;
			}
<HEREDOCUMENT>.		ECHO;

<PHP>{NUMBER}		DBG_PRINT("<N:%s>", LEXTEXT);
<PHP>{WORD}		{
				int id = php_reserved_word(LEXTEXT, LEXLENG);
				if (id) {
					DBG_PRINT("<Reserved:%s>", LEXTEXT);
					return id;
				} else {
					DBG_PRINT("<T:%s>", LEXTEXT);
					return PHP_TOKEN;
				}
			}
 /* Operator */
<PHP>[{}]		{
				int c = LEXTEXT[0];
				if (c == PHP_LBRACE)
					level++;
				else
					level--;
				DBG_PRINT("%c[%d]", c, level);

				return c;
			}
<PHP>[][()]		{
				return LEXTEXT[0];
			}
<PHP>[-+*/%&~^]=?	ECHO;
<PHP>[=><!]=		ECHO;
<PHP>[-+&|<>]{2}=?	ECHO;
<PHP>"<>"		ECHO;
<PHP>"$"		{ ECHO; return PHP_DOLLAR; }
<PHP>.			ECHO;

%%
/*
 * php: read PHP file and pickup tag entries.
 */
void
php(const struct parser_param *param)
{
	int token;

	level = 0;
	string = strbuf_open(0);
	if (linetable_open(param->file) == -1)
		die("'%s' cannot open.", param->file);

	LEXRESTART(NULL);
	LEXLINENO = 1;
	while ((token = LEXLEX(param)) != 0) {
		switch (token) {
		case PHP_DEFINE:
			if (LEXLEX(param) != PHP_LPAREN)
				break;
			if (LEXLEX(param) != PHP_STRING)
				break;
			PUT(PARSER_DEF, strbuf_value(string), LEXLINENO);
			break;
		case PHP_CLASS:
			if (LEXLEX(param) != PHP_TOKEN)
				break;
			PUT(PARSER_DEF, LEXTEXT, LEXLINENO);
			break;
		case PHP_FUNCTION:
		case PHP_CFUNCTION:
		case PHP_OLD_FUNCTION:
			if (LEXLEX(param) != PHP_TOKEN)
				break;
			PUT(PARSER_DEF, LEXTEXT, LEXLINENO);
			break;
		case PHP_VARIABLE:
			if (php_reserved_variable(LEXTEXT, LEXLENG)) {
				PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
				if (LEXLEX(param) == PHP_LBRACK && LEXLEX(param) == PHP_STRING && LEXLEX(param) == PHP_RBRACK) {
					const char *str = strbuf_value(string);

					if (strchr(str, '$') == NULL)
						PUT(PARSER_REF_SYM, str, LEXLINENO);
				}
			} else if (!strcmp(LEXTEXT, "this")) {
				;
			} else {
				PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
			}
			break;
		case PHP_POINTER:
			if (LEXLEX(param) != PHP_TOKEN)
				break;
			/* FALLTHROUGH */
		case PHP_TOKEN:
			PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
			break;
		case PHP_NEW:
			if (LEXLEX(param) != PHP_TOKEN)
				break;
			PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
			break;
		/*
		 * ${x->y}
		 */
		case PHP_DOLLAR:
			if (LEXLEX(param) != PHP_LBRACE)
				break;
			while ((token = LEXLEX(param)) != PHP_RBRACE) {
				if (token == PHP_TOKEN) {
					PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
				}
			}
			break;
		default:
			break;
		}
	}
	linetable_close();
	strbuf_close(string);
}
