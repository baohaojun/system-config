/*
 * Copyright (c) 1998, 1999, 2000, 2001, 2002, 2003
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

#ifndef _CONF_H_
#define _CONF_H_

#include "strbuf.h"
/*
 * Access library for gtags.conf (.globalrc).
 * File format is a subset of XXXcap (termcap, printcap) file.
 */
#define GTAGSCONF       "/etc/gtags.conf"
#define OLD_GTAGSCONF   "/etc/global.conf"	/* for compatibility */
#define DEBIANCONF      "/etc/gtags/gtags.conf"
#define OLD_DEBIANCONF  "/etc/gtags/global.conf"/* for compatibility */
#define GTAGSRC 	".globalrc"
#ifdef __DJGPP__
#define DOS_GTAGSRC	"_globalrc"
#endif
#define DEFAULTLABEL    "default"

void openconf(void);
int getconfn(const char *, int *);
int getconfs(const char *, STRBUF *);
int getconfb(const char *);
const char *getconfline(void);
void closeconf(void);

#endif /* ! _CONF_H_ */
