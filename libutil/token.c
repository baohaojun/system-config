/*
 * Copyright (c) 1998, 1999, 2000, 2002, 2003, 2008
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <ctype.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "die.h"
#include "gparam.h"
#include "strlimcpy.h"
#include "token.h"

/*
 * File input method.
 */
int lineno;
const char *sp, *cp, *lp;
int crflag;			/* 1: return '\n', 0: doesn't return */
int cmode;			/* allow token which start with '#' */
int cppmode;			/* allow '::' as a token */
int ymode;			/* allow token which start with '%' */
char token[MAXTOKEN];
char curfile[MAXPATHLEN];
int continued_line;		/* previous line ends with '\\' */

static char ptok[MAXTOKEN];
static int lasttok;
static FILE *ip;
static STRBUF *ib;

#define tlen	(p - &token[0])
static void pushbackchar(void);

/*
 * opentoken:
 */
int
opentoken(const char *file)
{
	/*
	 * b flag is needed for WIN32 environment. Almost unix ignore it.
	 */
	if ((ip = fopen(file, "rb")) == NULL)
		return 0;
	ib = strbuf_open(MAXBUFLEN);
	strlimcpy(curfile, file, sizeof(curfile));
	sp = cp = lp = NULL; ptok[0] = '\0'; lineno = 0;
	crflag = cmode = cppmode = ymode = 0;
	continued_line = 0;
	return 1;
}
/*
 * closetoken:
 */
void
closetoken(void)
{
	strbuf_close(ib);
	fclose(ip);
}

/*
 * nexttoken: get next token
 *
 *	i)	interested	interested special character
 *				if NULL then all character.
 *	i)	reserved	converter from token to token number
 *				if this is specified, nexttoken() return
 *				word number, else return symbol.
 *	r)	EOF(-1)	end of file
 *		c ==0		symbol ('tok' has the value.)
 *		c < 256		interested special character
 *		c > 1000	reserved word
 *
 * nexttoken() doesn't return followings.
 *
 * o comment
 * o space (' ', '\t', '\f', '\v', '\r')
 * o quoted string ("...", '.')
 * o number
 */

int
nexttoken(const char *interested, int (*reserved)(const char *, int))
{
	int c;
	char *p;
	int sharp = 0;
	int percent = 0;

	/* check push back buffer */
	if (ptok[0]) {
		strlimcpy(token, ptok, sizeof(token));
		ptok[0] = '\0';
		return lasttok;
	}

	for (;;) {
		/* skip spaces */
		if (!crflag)
			while ((c = nextchar()) != EOF && isspace(c))
				;
		else
			while ((c = nextchar()) != EOF && isspace(c) && c != '\n')
				;
		if (c == EOF || c == '\n')
			break;

		if (c == '"' || c == '\'') {	/* quoted string */
			int quote = c;

			while ((c = nextchar()) != EOF) {
				if (c == quote)
					break;
				if (quote == '\'' && c == '\n')
					break;
				if (c == '\\' && (c = nextchar()) == EOF)
					break;
			}
		} else if (c == '/') {			/* comment */
			if ((c = nextchar()) == '/') {
				while ((c = nextchar()) != EOF)
					if (c == '\n') {
						pushbackchar();
						break;
					}
			} else if (c == '*') {
				while ((c = nextchar()) != EOF) {
					if (c == '*') {
						if ((c = nextchar()) == '/')
							break;
						pushbackchar();
					}
				}
			} else
				pushbackchar();
		} else if (c == '\\') {
			if (nextchar() == '\n')
				continued_line = 1;
		} else if (isdigit(c)) {		/* digit */
			while ((c = nextchar()) != EOF && (c == '.' || isalnum(c)))
				;
			pushbackchar();
		} else if (c == '#' && cmode) {
			/* recognize '##' as a token if it is reserved word. */
			if (peekc(1) == '#') {
				p = token;
				*p++ = c;
				*p++ = nextchar();
				*p   = 0;
				if (reserved && (c = (*reserved)(token, tlen)) == 0)
					break;
			} else if (!continued_line && atfirst_exceptspace()) {
				sharp = 1;
				continue;
			}
		} else if (c == ':' && cppmode) {
			if (peekc(1) == ':') {
				p = token;
				*p++ = c;
				*p++ = nextchar();
				*p   = 0;
				if (reserved && (c = (*reserved)(token, tlen)) == 0)
					break;
			}
		} else if (c == '%' && ymode) {
			/* recognize '%%' as a token if it is reserved word. */
			if (atfirst) {
				p = token;
				*p++ = c;
				if ((c = peekc(1)) == '%' || c == '{' || c == '}') {
					*p++ = nextchar();
					*p   = 0;
					if (reserved && (c = (*reserved)(token, tlen)) != 0)
						break;
				} else if (!isspace(c)) {
					percent = 1;
					continue;
				}
			}
		} else if (c & 0x80 || isalpha(c) || c == '_') {/* symbol */
			p = token;
			if (sharp) {
				sharp = 0;
				*p++ = '#';
			} else if (percent) {
				percent = 0;
				*p++ = '%';
			} else if (c == 'L') {
				int tmp = peekc(1);

				if (tmp == '\"' || tmp == '\'')
					continue;
			}
			for (*p++ = c; (c = nextchar()) != EOF && (c & 0x80 || isalnum(c) || c == '_');) {
				if (tlen < sizeof(token))
					*p++ = c;
			}
			if (tlen == sizeof(token)) {
				warning("symbol name is too long. (Ignored)[+%d %s]", lineno, curfile);
				continue;
			}
			*p = 0;
	
			if (c != EOF)
				pushbackchar();
			/* convert token string into token number */
			c = SYMBOL;
			if (reserved)
				c = (*reserved)(token, tlen);
			break;
		} else {				/* special char */
			if (interested == NULL || strchr(interested, c))
				break;
			/* otherwise ignore it */
		}
		sharp = percent = 0;
	}
	return lasttok = c;
}
/*
 * pushbacktoken: push back token
 *
 *	following nexttoken() return same token again.
 */
void
pushbacktoken(void)
{
	strlimcpy(ptok, token, sizeof(ptok));
}
/*
 * peekc: peek next char
 *
 *	i)	immediate	0: ignore blank, 1: include blank
 *
 * Peekc() read ahead following blanks but doesn't change line.
 */
int
peekc(int immediate)
{
	int c;
	long pos;
    int comment = 0;

	if (cp != NULL) {
		if (immediate)
			c = nextchar();
		else
            while ((c = nextchar()) != EOF && c != '\n') {
                if (c == '/') {			/* comment */
                    if ((c = nextchar()) == '/') {
                        while ((c = nextchar()) != EOF)
                            if (c == '\n') {
                                pushbackchar();
                                break;
                            }
                    } else if (c == '*') {
                        comment = 1;
                        while ((c = nextchar()) != EOF) {
                            if (c == '*') {
                                if ((c = nextchar()) == '/')
                                {
                                    comment = 0;
                                    break;
                                }
                            }
                            else if (c == '\n')
                            {
                                pushbackchar();
                                break;
                            }
                        }
                    } else
                        pushbackchar();
                }
                else if (!isspace(c))
                    break;
            }
		if (c != EOF)
			pushbackchar();
		if (c != '\n' || immediate)
			return c;
	}
	pos = ftell(ip);
	if (immediate)
		c = getc(ip);
	else
        while ((c = getc(ip)) != EOF) {
            if (comment) {
                while ((c = getc(ip)) != EOF) {
                    if (c == '*') {
                        if ((c = getc(ip)) == '/')
                        {
                            comment = 0;
                            break;
                        }
                    }
                }
            }
            else if (c == '/') {			/* comment */
                if ((c = getc(ip)) == '/') {
                    while ((c = getc(ip)) != EOF)
                        if (c == '\n') {
                            break;
                        }
                } else if (c == '*') {
                    while ((c = getc(ip)) != EOF) {
                        if (c == '*') {
                            if ((c = getc(ip)) == '/')
                                break;
                        }
                    }
                } else
                    break;
            }
            else if (!isspace(c))
                break;
        }

	(void)fseek(ip, pos, SEEK_SET);

	return c;
}
/*
 * throwaway_nextchar: throw away next character
 */
void
throwaway_nextchar()
{
	nextchar();
}
/*
 * atfirst_exceptspace: return if current position is the first column
 *			except for space.
 *	|      1 0
 *      |      v v
 *	|      # define
 */
int
atfirst_exceptspace(void)
{
	const char *start = sp;
	const char *end = cp ? cp - 1 : lp;

	while (start < end && *start && isspace(*start))
		start++;
	return (start == end) ? 1 : 0;
}
/*
 * pushbackchar: push back character.
 *
 *	following nextchar() return same character again.
 * 
 */
static void
pushbackchar(void)
{
        if (sp == NULL)
                return;         /* nothing to do */
        if (cp == NULL)
                cp = lp;
        else
                --cp;
}
