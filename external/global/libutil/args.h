/*
 * Copyright (c) 2010 Tama Communications Corporation
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

#ifndef _ARGS_H_
#define _ARGS_H_
#include "gpathop.h"

void args_open(char *const *);
void args_open_filelist(const char *);
void args_open_gfind(GFIND *gp);
void args_open_nop();
void args_open_both(char *const *, const char *);
const char *args_read(void);
void args_close(void);

#endif /* ! _ARGS_H_ */
