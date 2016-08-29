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

#ifndef _COMPRESS_H
#define _COMPRESS_H

/*
 * Please list words which appear in definition line.
 *
 * 'ddefine' means 'd => define'.
 */
#define DEFAULT_ABBREVIATION	"ddefine ttypedef"

void abbrev_open(const char *);
void abbrev_close(void);
void abbrev_dump(void);
char *compress(const char *text, const char *name);
char *uncompress(const char *text, const char *name);

#endif /* ! _COMPRESS_H */
