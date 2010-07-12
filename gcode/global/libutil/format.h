/*
 * Copyright (c) 2005, 2006 Tama Communications Corporation
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
#ifndef _FORMAT_H_
#define _FORMAT_H_

/*
 * (1) ctags -x format (FORMAT_CTAGS_X)
 *
 * PART_TAG     PART_LNO PART_PATH      PART_LINE
 * +----------------------------------------------
 * |main             227 ./src/main.c   main()
 *
 * (2) ctags -x + file id format (FORMAT_CTAGS_XID)
 *
 * 0    PART_TAG+1 PART_LNO+1 PART_PATH+1   PART_LINE+1
 * +----------------------------------------------
 * |110	main             227 ./src/main.c   main()
 *
 * (3) ctags format (FORMAT_CTAGS)
 *
 * +----------------------------------------------
 * |main        ./src/main.c       227
 *
 * (4) path name format (FORMAT_PATH)
 *
 * +----------------------------------------------
 * |./src/main.c
 *
 * (5) grep format (FORMAT_GREP)
 *
 * +----------------------------------------------
 * |./src/main.c:227:main()
 *
 * (6) cscope line mode format (FORMAT_CSCOPE)
 *
 * +----------------------------------------------
 * |./src/main.c main 227 main()
 */
#define FORMAT_CTAGS		1
#define FORMAT_CTAGS_X		2
#define FORMAT_CTAGS_XID	3
#define FORMAT_PATH		4
#define FORMAT_GREP		5
#define FORMAT_CSCOPE		6

/*
 * FORMAT_CTAGS_X
 */
#define PART_TAG  0
#define PART_LNO  1
#define PART_PATH 2
#define PART_LINE 3

/*
 * Path name type
 */
#define PATH_RELATIVE	1
#define PATH_ABSOLUTE	2
#define PATH_THROUGH	3

#endif /* ! _FORMAT_H_ */
