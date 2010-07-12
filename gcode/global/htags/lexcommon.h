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
#ifndef _LEXCOMMON_H
#define _LEXCOMMON_H

#include "incop.h"
#include "tab.h"

/*
 * Definition of LEXTEXT, LEXLENG, LEXIN and LEXRESTART.
 *
 * These symbols are substitutions of yytext, yyleng, yyin and yyrestart.
 * You should write lex code using them.
 * The usage of this file is, for instance, in c.l:
 *
 *	#define lex_symbol_generation_rule(x) c_ ## x
 *	#include "lexcommon.h"
 */
#ifndef lex_symbol_generation_rule
ERROR: lex_symbol_generation_rule(x) macro not defined.
lexcommon.h requires the lex_symbol_generation_rule(x) macro for each language
to generate language specific symbols.
#endif
#define LEXTEXT lex_symbol_generation_rule(text)
#define LEXLENG lex_symbol_generation_rule(leng)
#define LEXIN lex_symbol_generation_rule(in)
#define LEXRESTART lex_symbol_generation_rule(restart)

/*
 * The default action for line control.
 * These can be applicable to most languages.
 * You must define C_COMMENT, CPP_COMMENT SHELL_COMMENT, LITERAL, STRING
 * and PREPROCESSOR_LINE as %start values, even if they are not used.
 * It assumed that CPP_COMMENT and SHELL_COMMENT is one line comment.
 */
static int lexcommon_lineno;
static int begin_line;
/*
 * If you want newline to terminate string, set this variable to 1.
 */
static int newline_terminate_string = 0;

/*
 * Variables for converting tabs to spaces.
 */
static int dest_column;
static int left_spaces;

#define YY_INPUT(buf, result, max_size) do {				\
	result = read_file_detabing(buf, max_size, LEXIN,		\
			&dest_column, &left_spaces);			\
} while (0)

#define LINENO lexcommon_lineno

#define DEFAULT_BEGIN_OF_FILE_ACTION {					\
        LEXIN = ip;							\
        LEXRESTART(LEXIN);						\
        LINENO = 1;							\
        begin_line = 1;							\
	dest_column = 0;						\
	left_spaces = 0;						\
}

#define DEFAULT_YY_USER_ACTION {					\
	if (begin_line) {						\
		put_begin_of_line(LINENO);				\
		switch (YY_START) {					\
		case C_COMMENT:						\
		case CPP_COMMENT:					\
		case SHELL_COMMENT:					\
			echos(comment_begin);				\
			break;						\
		}							\
		begin_line = 0;						\
	}								\
}

#define DEFAULT_END_OF_LINE_ACTION {					\
	switch (YY_START) {						\
	case CPP_COMMENT:						\
	case SHELL_COMMENT:						\
		yy_pop_state();						\
		/* FALLTHROUGH */					\
	case C_COMMENT:							\
		echos(comment_end);					\
		break;							\
	case STRING:							\
	case LITERAL:							\
		if (newline_terminate_string)				\
			yy_pop_state();					\
		break;							\
	}								\
	if (YY_START == PREPROCESSOR_LINE)				\
		yy_pop_state();						\
	put_end_of_line(LINENO);					\
	/* for the next line */						\
	LINENO++;							\
	begin_line = 1;							\
}

#define DEFAULT_BACKSLASH_NEWLINE_ACTION {				\
	echoc('\\');							\
	switch (YY_START) {						\
	case CPP_COMMENT:						\
	case C_COMMENT:							\
	case SHELL_COMMENT:						\
		echos(comment_end);					\
		break;							\
	}								\
	put_end_of_line(LINENO);					\
	/* for the next line */						\
	LINENO++;							\
	begin_line = 1;							\
}

/*
 * Output routine.
 */
extern void echoc(int);
extern void echos(const char *);
extern const char *generate_guide(int);
extern void put_anchor(char *, int, int);
extern void put_include_anchor(struct data *, const char *);
extern void put_include_anchor_direct(const char *, const char *);
extern void put_reserved_word(const char *);
extern void put_macro(const char *);
extern void unknown_preprocessing_directive(const char *, int);
extern void unexpected_eof(int);
extern void unknown_yacc_directive(const char *, int);
extern void missing_left(const char *, int);
extern void put_char(int);
extern void put_string(const char *);
extern void put_brace(const char *);
extern void put_begin_of_line(int);
extern void put_end_of_line(int);

#endif /* ! _LEXCOMMON_H */
