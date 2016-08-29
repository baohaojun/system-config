/*
 * Copyright (c) 2006
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
#include <string.h>

#include "compress.h"
#include "die.h"
#include "gtagsop.h"
#include "strbuf.h"
#include "strlimcpy.h"
#include "varray.h"

/*
 * Compress module
 *
 * Function compress() reduces the size of GTAGS by about 10-20% average.
 *
 * PROTOCOL:
 *
 *	meta record: " __.COMPRESS ddefine ttypedef"
 *
 *	'ddefine' means	d => define
 *	'ttypedef' means t => typedef
 *
 *	source		abbreviation
 *	-----------------------------------------
 *	@		@@
 *	<tag name>	@n
 *	"define"	@d
 *	"typedef"	@t
 *	<spaces>	@<digit> or @{<number>}
 *
 * EXAMPLE OF COMPRESS:
 *
 *	100 macro 23 #define macro(c) a;      b;
 *	             ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *	             | [compress]   ^ [uncompress]
 *	             v              |
 *	100 macro 23 #@d @n(c) a;@6b;
 *	             ~~~~~~~~~~~~~~~~
 * DATA STRUCTURE
 *
 *	o Ab2name table is used to convert from abbreviation character
 *	  to the string value.
 *	o Name2ab table is used to convert from string value to the
 *	  abbreviation character.
 *	  
 *	ab2name = ('a' => NULL, ... , 'd' => "define", ... 'z' => NULL)
 *	name2ab = ("define" => 'a', "typdef" => 't')
 */
struct abbrmap {
	int c;
	char *name;
	int length;
};
static struct abbrmap ab2name[26];
static VARRAY *name2ab;
static char abbrev_string[1024];
/*
 * setup two internal tables for abbreviation.
 *
 *	i)	abbrev	abbreviation string
 */
void
abbrev_open(const char *abbrev)
{
	int i, limit;
	struct abbrmap *ab;
	char *p;

	/*
	 * abbrev string: "ddefine ttypedef"
	 */
	/* copy abbrev string to static area */
	strlimcpy(abbrev_string, abbrev, sizeof(abbrev_string));
	p = abbrev_string;

	/* initialize ab2name table */
	limit = sizeof(ab2name) / sizeof(struct abbrmap);
	for (i = 0; i < limit; i++) {
		ab2name[i].c = 0;
		ab2name[i].name = NULL;
	}
	name2ab = varray_open(sizeof(struct abbrmap), 5);
	while (*p) {
		ab = (struct abbrmap *)varray_append(name2ab);
		ab->c = *p++;
		ab->name = p;
		for (; *p && *p != ' '; p++)
			;
		if (*p == ' ')
			*p++ = '\0';
		ab->length = strlen(ab->name);
		if (ab->c < 'a' || ab->c > 'z')
			die("Abbrev character must be a lower alphabetic character. (%s)", abbrev);
		i = ab->c - 'a';
		ab2name[i].c = ab->c;
		ab2name[i].name = ab->name;
		ab2name[i].length = ab->length;
	}
}
/*
 * free allocated memory.
 */
void
abbrev_close(void)
{
	if (name2ab)
		varray_close(name2ab);
	name2ab = NULL;
}
/*
 * for debugging.
 */
void
abbrev_dump(void)
{
	struct abbrmap *ab;
	int i, limit = sizeof(ab2name) / sizeof(struct abbrmap);

	if (!name2ab) {
		fprintf(stderr, "name2ab is NULL.\n");
		return;
	}
	fprintf(stderr, "ab2name: %d entries\n", limit);
	for (i = 0; i < limit; i++) {
		if (ab2name[i].c != 0) {
			fprintf(stderr, "ab2name[%d].c    = %c\n", i, ab2name[i].c);
			fprintf(stderr, "ab2name[%d].name = %s\n", i, ab2name[i].name);
		}
	}
	ab = (struct abbrmap *)varray_assign(name2ab, 0, 0);
	limit = name2ab->length;
	fprintf(stderr, "name2ab: %d entries\n", limit);
	for (i = 0; i < limit; i++) {
		if (ab[i].c != 0) {
			fprintf(stderr, "name2ab[%d].c    = %c\n", i, ab[i].c);
			fprintf(stderr, "name2ab[%d].name = %s\n", i, ab[i].name);
		}
	}
}
/*
 * compress source line.
 *
 *	i)	in	source line
 *	i)	name	replaced string
 *	r)		compressed string
 */
char *
compress(const char *in, const char *name)
{
	STATIC_STRBUF(sb);
	const char *p = in;
	int length = strlen(name);
	int spaces = 0;

	strbuf_clear(sb);
	while (*p) {
		if (*p == ' ') {
			spaces++;
			p++;
			continue;
		}
		if (spaces > 0) {
			if (spaces >= 10) {
				strbuf_putc(sb, '@');
				strbuf_putc(sb, '{');
				strbuf_putn(sb, spaces);
				strbuf_putc(sb, '}');
			} else if (spaces > 3) {
				strbuf_putc(sb, '@');
				strbuf_putn(sb, spaces);
			} else {
				strbuf_nputc(sb, ' ', spaces);
			}
		}
		spaces = 0;
		if (*p == '@') {
			strbuf_puts(sb, "@@");
			p++;
		} else if (!strncmp(p, name, length)) {
			strbuf_puts(sb, "@n");
			p += length;
		} else if (name2ab) {
			int i, limit = name2ab->length;
			struct abbrmap *ab = (struct abbrmap *)varray_assign(name2ab, 0, 0);

			for (i = 0; i < limit; i++) {
				if (!strncmp(p, ab[i].name, ab[i].length)) {
					strbuf_putc(sb, '@');
					strbuf_putc(sb, ab[i].c);
					p += ab[i].length;
					break;
				}
			}
			if (i >= limit) {
				strbuf_putc(sb, *p);
				p++;
			}
		} else {
			strbuf_putc(sb, *p);
			p++;
		}
	}
	if (spaces > 0) {
		if (spaces < 4) {
			strbuf_nputc(sb, ' ', spaces);
		} else if (spaces < 10) {
			strbuf_putc(sb, '@');
			strbuf_putn(sb, spaces);
		} else {
			strbuf_putc(sb, '@');
			strbuf_putc(sb, '{');
			strbuf_putn(sb, spaces);
			strbuf_putc(sb, '}');
		}
	}
	return strbuf_value(sb);
}

/*
 * uncompress source line.
 *
 *	i)	in	compressed string
 *	i)	name	replaced string
 *	r)		uncompressed string
 */
char *
uncompress(const char *in, const char *name)
{
	STATIC_STRBUF(sb);
	const char *p;
	int i;

	strbuf_clear(sb);
	for (p = in;  *p; p++) {
		if (*p == '@') {
			int spaces = 0;

			switch (*++p) {
			case '@':
				strbuf_putc(sb, '@');
				break;
			case 'n':
				strbuf_puts(sb, name);
				break;
			case '{':	/* } */
				for (p++; *p && isdigit((unsigned char)*p); p++)
					spaces = spaces * 10 + *p - '0';
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				spaces = *p - '0';
				break;
			default:
				if (*p < 'a' || *p > 'z')
					die("Abbrev character must be a lower alphabetic character. (%c)", *p);
				i = *p - 'a';
				if (ab2name[i].name)
					strbuf_puts(sb, ab2name[i].name);
				break;
                        }
			strbuf_nputc(sb, ' ', spaces);
                } else {
			strbuf_putc(sb, *p);
		}
	}
	return strbuf_value(sb);
}
