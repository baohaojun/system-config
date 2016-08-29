/*
 * Copyright (c) 2009
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

#ifndef _STATISTICS_H_
#define _STATISTICS_H_

#ifndef __attribute__
/* This feature is available in gcc versions 2.5 and later.  */
# if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 5) || __STRICT_ANSI__
#  define __attribute__(x)
# endif
/* The __-protected variants of `format' and `printf' attributes
   are accepted by gcc versions 2.6.4 (effectively 2.7) and later.  */
# if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
#  define __format__ format
#  define __printf__ printf
# endif
#endif

/*
 * STATISTICS_TIME
 *
 * Usage:
 *     main()
 *     {
 *         STATISTICS_TIME *tim;
 *
 *         init_statistics();
 *         tim = statistics_time_start("Time of making foo");
 *         makefoo();
 *         statistics_time_end(tim);
 *         for (i = 0; i < 3; i++) {
 *             tim = statistics_time_start("Time of making bar%d", i);
 *             makebar(i);
 *             statistics_time_end(tim);
 *         }
 *         print_statistics(style);
 *         exit(0);
 *     }
 */
struct statistics_time;
typedef struct statistics_time STATISTICS_TIME;

/*
 * STATISTICS_STYLE_NONE:
 *    Resource deallocation only.
 *
 * STATISTICS_STYLE_LIST:
 *    Print statistics information like following, and deallocate resource.
 *
 *     - Time of making foo .... user  2.016s system 0.128s elapsed  1.437s 149.0%
 *     - Time of making bar0 ... user  0.268s system 0.040s elapsed  0.282s 109.1%
 *     - Time of making bar1 ... user  1.084s system 0.120s elapsed  1.208s  99.2%
 *     - Time of making bar2 ... user 18.325s system 2.112s elapsed 16.010s 127.3%
 *     - The entire time ....... user 21.721s system 2.420s elapsed 18.989s 127.4%
 *
 * STATISTICS_STYLE_TABLE:
 *    Print statistics information like following, and deallocate resource.
 *
 *     period              user[sec] system[sec] elapsed[sec]  %CPU
 *     ------------------- --------- ----------- ------------ -----
 *     Time of making foo      2.016       0.128        1.437 149.0
 *     Time of making bar0     0.268       0.040        0.282 109.1
 *     Time of making bar1     1.084       0.120        1.208  99.2
 *     Time of making bar2    18.325       2.112       16.010 127.3
 *     ------------------- --------- ----------- ------------ -----
 *     The entire time        21.721       2.420       18.989 127.4
 */
enum {
	STATISTICS_STYLE_NONE,
	STATISTICS_STYLE_LIST,
	STATISTICS_STYLE_TABLE
};

void init_statistics(void);
STATISTICS_TIME *statistics_time_start(const char *, ...)
	__attribute__ ((__format__ (__printf__, 1, 2)));
void statistics_time_end(STATISTICS_TIME *);
void print_statistics(int);

#endif
