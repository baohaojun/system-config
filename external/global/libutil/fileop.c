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
#include <stdio.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "checkalloc.h"
#include "die.h"
#include "fileop.h"
#include "makepath.h"
#include "strlimcpy.h"

/*

File operation: usage

	[WRITE]
	int compress = cflag ? 1 : 0;

	FILEOP *fileop = open_output_file(path, compress);
	FILE *op = get_descripter(fileop);

	fputs("Hello", op);
	...
	close_file(fileop);

	[READ]
	FILEOP *fileop = open_input_file(path);
	FILE *ip = get_descripter(fileop);

	fgets(buf, sizeof(buf), ip);
	...
	close_file(fileop);
*/
/*
 * open input file.
 *
 *	i)	path	path name
 *	r)		file descripter
 */
FILEOP *
open_input_file(const char *path)
{
	FILE *fp = fopen(path, "r");
	FILEOP *fileop;

	if (fp == NULL)
		die("cannot open file '%s'.", path);
	fileop = check_calloc(sizeof(FILEOP), 1);
	fileop->fp = fp;
	strlimcpy(fileop->path, path, sizeof(fileop->path));
	fileop->type = FILEOP_INPUT;
	return fileop;
}
/*
 * open output file
 *
 *	i)	path	path name
 *	i)	compress 0: normal, 1: compress
 *	r)		file descripter
 */
FILEOP *
open_output_file(const char *path, int compress)
{
	FILEOP *fileop;
	FILE *fp;
	char command[MAXFILLEN];

	if (compress) {
		snprintf(command, sizeof(command), "gzip -c >\"%s\"", path);
		fp = popen(command, "w");
		if (fp == NULL)
			die("cannot create pipe.");
	} else {
		fp = fopen(path, "w");
		if (fp == NULL)
			die("cannot create file '%s'.", path);
	}
	fileop = check_calloc(sizeof(FILEOP), 1);
	strlimcpy(fileop->path, path, sizeof(fileop->path));
	if (compress)
		strlimcpy(fileop->command, command, sizeof(fileop->command));
	fileop->type = FILEOP_OUTPUT;
	if (compress)
		fileop->type |= FILEOP_COMPRESS;
	fileop->fp = fp;
	return fileop;
}
/*
 * get UNIX file descripter
 */
FILE *
get_descripter(FILEOP *fileop)
{
	return fileop->fp;
}
/*
 * close_file: close file
 *
 *	i)	fileop	file descripter
 */
void
close_file(FILEOP *fileop)
{
	if (fileop->type & FILEOP_COMPRESS) {
		if (pclose(fileop->fp) != 0)
			die("terminated abnormally. '%s'", fileop->command);
	} else
		fclose(fileop->fp);
	free(fileop);
}
