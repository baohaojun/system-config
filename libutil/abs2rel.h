/*
 * Copyright (c) 2002, 2008 Tama Communications Corporation
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

#ifndef _RELATIVE_H_
#define _RELATIVE_H_

char *normalize(const char *, const char *, const char *, char *, const int);
char *normalize_pathname(const char *, char *, const int);
char *abs2rel(const char *, const char *, char *, const int);
char *rel2abs(const char *, const char *, char *, const int);

#endif /* ! _RELATIVE_H_ */
