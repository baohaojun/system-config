/*
 * Copyright (c) 2003 Tama Communications Corporation
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

#ifndef _CHAR_H_
#define _CHAR_H_

extern const unsigned char chartype[256];

#define REGEXCHAR		1
#define URLCHAR			2
#define BINARYCHAR		4
#define test_chartype(c, t)	(chartype[(unsigned char)(c)] & (t))

/* test whether or not regular expression char. */
#define isregexchar(c)		test_chartype(c, REGEXCHAR)

/* test whether can be included in URL without escaping. */
#define isurlchar(c)		test_chartype(c, URLCHAR)

/* test whether or not cahr included in binary file. */
#define isbinarychar(c)		test_chartype(c, BINARYCHAR)

int isregex(const char *);
const char *quote_string(const char *);

#endif /* ! _CHAR_H_ */
