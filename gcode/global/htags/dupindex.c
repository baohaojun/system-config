/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2010
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
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#include "cache.h"
#include "common.h"
#include "global.h"
#include "htags.h"

/*
 * Data for each tag file.
 *
 *				      GTAGS         GRTAGS       GSYMS
 */
static const char *dirs[]    = {NULL, DEFS,         REFS,        SYMS};
static const char *kinds[]   = {NULL, "definition", "reference", "symbol"};
static const char *options[] = {NULL, "",           "r",         "s"};

/*
 * Make duplicate object index.
 *
 * If referred tag is only one, direct link which points the tag is generated.
 * Else if two or more tag exists, indirect link which points the tag list
 * is generated.
 */
int
makedupindex(void)
{
	STRBUF *sb = strbuf_open(0);
	STRBUF *tmp = strbuf_open(0);
	STRBUF *command = strbuf_open(0);
	int definition_count = 0;
	char srcdir[MAXPATHLEN];
	int db;
	FILEOP *fileop = NULL;
	FILE *op = NULL;
	FILE *ip = NULL;

	snprintf(srcdir, sizeof(srcdir), "../%s", SRCS);
	for (db = GTAGS; db < GTAGLIM; db++) {
		const char *kind = kinds[db];
		const char *option = options[db];
		int writing = 0;
		int count = 0;
		int entry_count = 0;
		const char *ctags_xid, *ctags_x;
		char tag[IDENTLEN], prev[IDENTLEN], first_line[MAXBUFLEN];

		if (gtags_exist[db] == 0)
			continue;
		prev[0] = 0;
		first_line[0] = 0;
		/*
		 * construct command line.
		 */
		strbuf_reset(command);
		strbuf_sprintf(command, "%s -x%s --result=ctags-xid --encode-path=\" \t\" --nofilter=path", global_path, option);
		/*
		 * Optimization when the --dynamic option is specified.
		 */
		if (dynamic) {
			strbuf_puts(command, " --nosource");
			if (db != GSYMS)
				strbuf_puts(command, " --nofilter=sort");
		}
		strbuf_puts(command, " \".*\"");
		if ((ip = popen(strbuf_value(command), "r")) == NULL)
			die("cannot execute command '%s'.", strbuf_value(command));
		while ((ctags_xid = strbuf_fgets(sb, ip, STRBUF_NOCRLF)) != NULL) {
			char fid[MAXFIDLEN];

			ctags_x = parse_xid(ctags_xid, fid, NULL);
			/* tag name */
			(void)strcpy_withterm(tag, ctags_x, sizeof(tag), ' ');
			if (strcmp(prev, tag)) {
				count++;
				if (vflag)
					fprintf(stderr, " [%d] adding %s %s\n", count, kind, tag);
				if (writing) {
					if (!dynamic) {
						fputs_nl(gen_list_end(), op);
						fputs_nl(body_end, op);
						fputs_nl(gen_page_end(), op);
						close_file(fileop);
						html_count++;
					}
					writing = 0;
					/*
					 * cache record: " <fid>\0<entry number>\0"
					 */
					strbuf_reset(tmp);
					strbuf_putc(tmp, ' ');
					strbuf_putn(tmp, count - 1);
					strbuf_putc(tmp, '\0');
					strbuf_putn(tmp, entry_count);
					cache_put(db, prev, strbuf_value(tmp), strbuf_getlen(tmp) + 1);
				}				
				/* single entry */
				if (first_line[0]) {
					char fid[MAXFIDLEN];
					const char *ctags_x = parse_xid(first_line, fid, NULL);
					const char *lno = nextelement(ctags_x);

					strbuf_reset(tmp);
					strbuf_puts_withterm(tmp, lno, ' ');
					strbuf_putc(tmp, '\0');
					strbuf_puts(tmp, fid);
					cache_put(db, prev, strbuf_value(tmp), strbuf_getlen(tmp) + 1);
				}
				/*
				 * Chop the tail of the line. It is not important.
				 * strlimcpy(first_line, ctags_x, sizeof(first_line));
				 */
				strncpy(first_line, ctags_xid, sizeof(first_line));
				first_line[sizeof(first_line) - 1] = '\0';
				strlimcpy(prev, tag, sizeof(prev));
				entry_count = 0;
			} else {
				/* duplicate entry */
				if (first_line[0]) {
					char fid[MAXFIDLEN];
					const char *ctags_x = parse_xid(first_line, fid, NULL);

					if (!dynamic) {
						char path[MAXPATHLEN];

						snprintf(path, sizeof(path), "%s/%s/%d.%s", distpath, dirs[db], count, HTML);
						fileop = open_output_file(path, cflag);
						op = get_descripter(fileop);
						fputs_nl(gen_page_begin(tag, SUBDIR), op);
						fputs_nl(body_begin, op);
						fputs_nl(gen_list_begin(), op);
						fputs_nl(gen_list_body(srcdir, ctags_x, fid), op);
					}
					writing = 1;
					entry_count++;
					first_line[0] = 0;
				}
				if (!dynamic) {
					fputs_nl(gen_list_body(srcdir, ctags_x, fid), op);
				}
				entry_count++;
			}
		}
		if (db == GTAGS)
			definition_count = count;
		if (pclose(ip) != 0)
			die("'%s' failed.", strbuf_value(command));
		if (writing) {
			if (!dynamic) {
				fputs_nl(gen_list_end(), op);
				fputs_nl(body_end, op);
				fputs_nl(gen_page_end(), op);
				close_file(fileop);
				html_count++;
			}
			/*
			 * cache record: " <fid>\0<entry number>\0"
			 */
			strbuf_reset(tmp);
			strbuf_putc(tmp, ' ');
			strbuf_putn(tmp, count);
			strbuf_putc(tmp, '\0');
			strbuf_putn(tmp, entry_count);
			cache_put(db, prev, strbuf_value(tmp), strbuf_getlen(tmp) + 1);
		}
		if (first_line[0]) {
			char fid[MAXFIDLEN];
			const char *ctags_x = parse_xid(first_line, fid, NULL);
			const char *lno = nextelement(ctags_x);

			strbuf_reset(tmp);
			strbuf_puts_withterm(tmp, lno, ' ');
			strbuf_putc(tmp, '\0');
			strbuf_puts(tmp, fid);
			cache_put(db, prev, strbuf_value(tmp), strbuf_getlen(tmp) + 1);
		}
	}
	strbuf_close(sb);
	strbuf_close(tmp);
	strbuf_close(command);
	return definition_count;
}
