/*
 * Copyright (c) 2010
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
#include "config.h"
#endif
#include <ctype.h>
#include <stdio.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#include "global.h"
#include "htags.h"
#include "path2url.h"
#include "common.h"

/*
 * makecflowindex: make call-tree based on cflow's output
 *
 *	i)	output		output file name
 *	i)	cflow_file	input file which is the output of Cflow with --format=posix
 */
int
makecflowindex(const char *output, const char *cflow_file)
{
	STRBUF *input = strbuf_open(0);
	FILE *ip, *op;
	char *cflow_posix, *p;
	const char *m0 = "Gave up making call-tree because of illegal POSIX cflow format.";
	const char *m1 = "";
	int line = 0;
	int status = 0;
#define ERROR	do { warning("%s\n%s:%d %s.", m0, cflow_file, line, m1); status = -1; goto finish; } while(0)

	/*
	 * If syntax error occured then stop the jobs and return error status.
	 * Don't die() because htags has already done a lot of work.
	 */
	if ((ip = fopen(cflow_file, "r")) == NULL) {
		warning("cannot open cflow file '%s'.", cflow_file);
		return -1;
	}
	if ((op = fopen(makepath(distpath, output, NULL), "w")) == NULL) {
		warning("cannot create file '%s'.", output);
		fclose(ip);
		return -1;
	}
	fputs_nl(gen_page_begin(title_call_tree, TOPDIR), op);
        fputs_nl(body_begin, op);
        fputs(header_begin, op);
        fputs(gen_href_begin(NULL, "cflow", normal_suffix, NULL), op);
        fputs(title_call_tree, op);
        fputs(gen_href_end(), op);
        fputs_nl(header_end, op);
        fputs_nl(verbatim_begin, op);
	/*
	 * Cflow's output format (with the --format=posix)
	 * +----------------------------------------------------------------
	 * |   25     isregex: int (const char *s), <libutil/char.c 54>...
	 * |   32     func: 10
	 * +----------------------------------------------------------------
	 * cflow_posix
	 * v
	 *     25     isregex: int ...           , <libutil/char.c 54>...
	 *     ^      ^                             ^              ^
	 * anchor     name                         path           lineno
	 *
	 * cflow_posix
	 * v
	 *     32     func: 10
	 *     ^      ^     ^
	 * anchor     name  lineno
	 */
	while ((cflow_posix = strbuf_fgets(input, ip, STRBUF_NOCRLF)) != NULL) {
		char *anchor, *name, *path, *lineno;
		char *anchor_end, *name_end, *path_end, *lineno_end;

		anchor = name = path = lineno = anchor_end = name_end = path_end = lineno_end = NULL;
		line++;

		for (p = cflow_posix; *p && isspace(*p); p++)
			;
		m1 = "line number at the head not found";
		if (!*p || !isdigit(*p))
			ERROR;
		anchor = p;					/* anchor */
		for (; *p && isdigit(*p); p++)
			;
		if (!*p || !isspace(*p))
			ERROR;
		anchor_end = p;
		for (; *p && !isalpha(*p); p++)
			;
		m1 = "function name not found";
		if (!*p || !isalpha(*p))
			ERROR;
		name = p;					/* name */
		for (; *p && *p != ':'; p++)
			;
		if (*p != ':')
			ERROR;
		name_end = p++;
		if (*p++ != ' ')
			ERROR;
		if (isdigit(*p)) {				/* (1) name: 999 */
			lineno = p;				/* lineno */
			for (; *p && isdigit(*p); p++)
				;
			lineno_end = p;
		} else if (*p == '<' && *(p + 1) == '>') {	/* (2) name: <> */
			;
		} else {					/* (3) name: ... <path lineno> */
			m1 = "<path lineno> not found";
			for (; *p && *p != '<'; p++)
				;
			if (!*p++)
				ERROR;
			path = p;
			m1 = "path not found";
			for (; *p && !isspace(*p); p++)
				if (*p == '>')
					ERROR;
			if (!*p || *p != ' ')
				ERROR;
			path_end = p++;
			m1 = "lineno not found";
			if (!isdigit(*p))
				ERROR;
			lineno = p;
			for (; *p && isdigit(*p); p++)
				;
			if (*p != '>')
				ERROR;
			lineno_end = p;
		}
		/*
		 * print anchor
		 */
		fprintf(op, gen_name_number(atoi(anchor)));
		/*
		 * print until name
		 */
		fwrite(cflow_posix, name - cflow_posix, 1, op);
		/*
		 * print name
		 */
		if (path) {
			const char *fid = NULL;
			int path_save = *path_end;
			int lineno_save = *lineno_end;

			*path_end = *lineno_end = 0;
			if (test("f", path) && (fid = path2fid_readonly(path)) != NULL)
				fprintf(op, gen_href_begin(SRCS, fid, HTML, lineno));
			else
				path = lineno = NULL;		/* not to print </a> */
			*path_end = path_save;
			*lineno_end = lineno_save;
		} else if (lineno) {
			int lineno_save = *lineno_end;

			*lineno_end = 0;
			fprintf(op, gen_href_begin(NULL, NULL, NULL, lineno));
			*lineno_end = lineno_save;
		}
		fwrite(name, name_end - name, 1, op);
		if (path || lineno)
			fputs(gen_href_end(), op);
		/*
		 * print the rest
		 */
		for (p = name_end; *p; p++) {
			if (*p == '<')
				fputs(quote_little, op);
			else if (*p == '>')
				fputs(quote_great, op);
			else
				fputc(*p, op);
		}
		fputc('\n', op);
	}
finish:
        fputs_nl(verbatim_end, op);
        fputs_nl(body_end, op);
        fputs_nl(gen_page_end(), op);
	strbuf_close(input);
	fclose(ip);
	fclose(op);
	return status;
}
