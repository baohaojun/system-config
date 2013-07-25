/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2007, 2008
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

#ifndef _GPARAM_H_
#define _GPARAM_H_
#ifndef __BORLANDC__
#include <sys/param.h>
#endif

#define MAXFILLEN	1024		/* max length of filter		*/
#define IDENTLEN	512		/* max length of ident		*/
#define MAXBUFLEN	1024		/* max length of buffer		*/
#define MAXPROPLEN	1024		/* max length of property	*/
#define MAXARGLEN	512		/* max length of argument	*/
#define MAXTOKEN	512		/* max length of token		*/
#define MAXFIDLEN	32		/* max length of fid		*/
#ifndef MAXPATHLEN
#define MAXPATHLEN	1024		/* max length of path		*/
#endif
#define MAXKEYLEN	MAXPATHLEN	/* max length of record key	*/
#define MAXURLLEN	1024		/* max length of URL		*/
/*
 * The default cache size of db library is 50MB.
 * The minimum size is 500KB.
 */
#define GTAGSCACHE	50000000	/* default cache size 50MB	*/
#define GTAGSMINCACHE	500000		/* minimum cache size 500KB	*/

#endif /* ! _GPARAM_H_ */
