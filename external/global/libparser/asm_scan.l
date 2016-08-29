%{
/*
 * Copyright (c) 2002, 2004, 2010 Tama Communications Corporation
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
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#define YYLTYPE		int

#include "internal.h"
#include "asm_parse.h"
#include "asm_res.h"
#include "die.h"
#include "linetable.h"
#include "strbuf.h"

#define lex_symbol_generation_rule(x) asm_ ## x
#define LEXLEX lex_symbol_generation_rule(lex)
#define LEXTEXT lex_symbol_generation_rule(text)
#define LEXLENG lex_symbol_generation_rule(leng)
#define LEXLINENO lex_symbol_generation_rule(lineno)
#define LEXRESTART lex_symbol_generation_rule(restart)
#define LEXLVAL lex_symbol_generation_rule(lval)
#define LEXLLOC lex_symbol_generation_rule(lloc)

#define YY_DECL	int LEXLEX(const struct parser_param *param)

#define YY_INPUT(buf, result, max_size) do {				\
	if ((result = linetable_read(buf, max_size)) == -1)		\
		result = YY_NULL;					\
} while (0)

#define ADD_SYM(tag, lno) do {						\
	LEXLVAL = strbuf_getlen(asm_symtable);				\
	LEXLLOC = (lno);						\
	strbuf_puts0(asm_symtable, tag);				\
} while (0)

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

static int last_directive;

%}

H		0[Xx][0-9A-Fa-f]+
N		[0-9]+
L		{N}L?
D1		{N}\.{N}([Ee][+-]?{N})?
D2		\.{N}([Ee][+-]?{N})?
NUMBER		-?({L}|{D1}|{D2})
ALPHA		[a-zA-Z_\x80-\xff]
ALPHANUM	[a-zA-Z_\x80-\xff0-9]
WORD		{ALPHA}{ALPHANUM}*

%x C_COMMENT CPP_COMMENT STRING LITERAL
%s PREPROCESSOR_LINE

%option 8bit yylineno stack noyywrap noyy_top_state never-interactive prefix="asm_"

%%

 /* Ignore spaces */
[ \f\t\v]+

 /* C style comment */
"/*"		{ yy_push_state(C_COMMENT); }
<C_COMMENT>{
	[^*\n]*
	[^*\n]*\n
	"*"+[^*/\n]*
	"*"+[^*/\n]*\n
	"*"+"/"		{ yy_pop_state(); }
	<<EOF>> {
		if (param->flags & PARSER_WARNING)
			warning("unexpected eof. [+%d %s]", LEXLINENO, param->file);
		yyterminate();
	}
}

 /* C++ style line comment */
"//"		{ yy_push_state(CPP_COMMENT); }
<CPP_COMMENT>{
	(\\.|[^\\\n])+
	\\\n
	\n		{ yy_pop_state(); unput('\n'); }
}

 /* String */
\"		{ yy_push_state(STRING); }
<STRING>{
	(\\.|[^\"\\\n])+
	\\\n
	\n		{ yy_pop_state(); unput('\n'); return ASM_CONST; }
	\"		{ yy_pop_state(); return ASM_CONST; }
}

 /* Character */
\'		{ yy_push_state(LITERAL); }
<LITERAL>{
	(\\.|[^\'\\\n])+
	\\\n
	\n		{ yy_pop_state(); unput('\n'); return ASM_CONST; }
	\'		{ yy_pop_state(); return ASM_CONST; }
}

 /* Number */
{NUMBER}	{ return ASM_CONST; }

<INITIAL>{
	^[ \t]*\#[ \t]*{WORD} {
		last_directive = asm_reserved_sharp(LEXTEXT, LEXLENG);
		switch (last_directive) {
		case 0:
			yy_push_state(CPP_COMMENT);
			break;
		case SHARP_DEFINE:
			yy_push_state(PREPROCESSOR_LINE);
			return ASM_DEFINE;
		case SHARP_UNDEF:
			yy_push_state(PREPROCESSOR_LINE);
			return ASM_UNDEF;
		default:
			yy_push_state(PREPROCESSOR_LINE);
			return ASM_DIRECTIVE;
		}
	}
	^[ \t]*\# {
		last_directive = 0;
		yy_push_state(PREPROCESSOR_LINE);
		return ASM_DIRECTIVE;
	}
	call|jsr	{ return ASM_CALL; }
	\.macro|macro	{ return ASM_MACRO; }
	\.equ|equ	{ return ASM_EQU; }
	^(ENTRY|ALTENTRY|NENTRY|GLOBAL_ENTRY|JSBENTRY|C_SYMBOL_NAME|C_ENTRY) {
		ADD_SYM(LEXTEXT, LEXLINENO); 
		return ASM_ENTRY;
	}
	EXT|SYMBOL_NAME|C_LABEL {
		ADD_SYM(LEXTEXT, LEXLINENO);
		return ASM_EXT;
	}
	^{WORD}		{ ADD_SYM(LEXTEXT, LEXLINENO); return ASM_LABEL; }
}

<PREPROCESSOR_LINE>{
	{WORD} {
		switch (last_directive) {
		case SHARP_IF:
		case SHARP_ELIF:
			if (strcmp(LEXTEXT, "defined") == 0)
				break;
			/* FALLTHROUGH */
		case SHARP_IFDEF:
		case SHARP_IFNDEF:
			PUT(PARSER_REF_SYM, LEXTEXT, LEXLINENO);
			break;
		default:
			ADD_SYM(LEXTEXT, LEXLINENO);
			return ASM_SYMBOL;
		}
	}
	\n		{ yy_pop_state(); return '\n'; }
}

{WORD}		{ ADD_SYM(LEXTEXT, LEXLINENO); return ASM_SYMBOL; }

\\\n
\n		{ return '\n'; }
.		{ return LEXTEXT[0]; }

%%

void
asm_initscan(void)
{
	LEXRESTART(NULL);
	LEXLINENO = 1;
}
