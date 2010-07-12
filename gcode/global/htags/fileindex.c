/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
 *		2006
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
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "global.h"
#include "incop.h"
#include "htags.h"
#include "path2url.h"
#include "common.h"

/*----------------------------------------------------------------------*/
/* Find list procedures							*/
/*----------------------------------------------------------------------*/
static const char *getpath(void);
static void ungetpath(void);
static GFIND *gp;
static int retry;
/*
 * get a path from input stream.
 *
 * Each path name must start with "./".
 */
static const char *
getpath(void)
{
	static const char *buff;

	if (!retry) {
		/* skip README or ChangeLog unless the -o option specified. */
		do {
			buff = gfind_read(gp);
		} while (buff && gp->type == GPATH_OTHER && !other_files);
	}
	retry = 0;
	return buff;
}
/*
 * push back a path name.
 */
static void
ungetpath(void)
{
	retry = 1;
}
/*----------------------------------------------------------------------*/
/* Path name operation procedures					*/
/*----------------------------------------------------------------------*/
static const char *extract_lastname(const char *, int);
static const char *lastpart(const char *);
static const char *dirpart(const char *, char *);
static const char *localpath(const char *, char *);
static const char *appendslash(const char *);
static const char *insert_comma(unsigned int);

/*
 * extract_lastname: extract the last name of include line.
 *
 *	i)	image	source image of include
 *	i)	is_php	1: is PHP source
 *	r)		last name
 */
static const char *
extract_lastname(const char *image, int is_php)
{
	static char buf[MAXBUFLEN];
	const char *p;
	char *q;
	int sep;

	/*
	 * C:	#include <xxx/yyy/zzz.h>
	 *	#include "xxx/yyy/zzz.h"
	 * PHP: include('xxx/yyy/zzz');
	 */
	p = image;
	while (*p && isspace((unsigned char)*p))		/* skip space */
		p++;
	if (!*p)
		return NULL;
	if (*p == '#') {
		if (is_php)
			return NULL;
		p++;
		while (*p && isspace((unsigned char)*p))	/* skip space */
			p++;
		if (!*p)
			return NULL;
	}
	/*
	 * If match to one of the include keywords then points
	 * the following character of the keyword.
	 *            p
	 *            v
	 * ... include ....
	 */
	if (is_php) {
		if ((p = locatestring(p, "include", MATCH_AT_FIRST)) == NULL)
			return NULL;
	} else {
		char *q;

		if (((q = locatestring(p, "include_next", MATCH_AT_FIRST)) == NULL) &&
		    ((q = locatestring(p, "import", MATCH_AT_FIRST)) == NULL) &&
		    ((q = locatestring(p, "include", MATCH_AT_FIRST)) == NULL))
			return NULL;
		p = q;
	}
	while (*p && isspace((unsigned char)*p))		/* skip space */
		p++;
	if (is_php && *p == '(') {
		p++;
		while (*p && isspace((unsigned char)*p))	/* skip space */
			p++;
	}
	sep = *p;
	if (is_php) {
		if (sep != '\'' && sep != '"')
			return NULL;
	} else {
		if (sep != '<' && sep != '"')
			return NULL;
	}
	if (sep == '<')
		sep = '>';
	p++;
	if (!*p)
		return NULL;
	q = buf; 
	while (*p && *p != '\n' && *p != sep)
		*q++ = *p++;
	*q = '\0';
	if (*p == sep) {
		q = locatestring(buf, "/", MATCH_LAST);
		if (q)
			q++;
		else
			q = buf;
		return q;
	}
	return NULL;
}
/*
 * get the last part of the path.
 *
 *	i)	path	path name
 *	r)		last part
 * Ex.
 * lastpart("a/b")	=> "b"
 * lastpart("a")	=> "a"
 */
static const char *
lastpart(const char *path)
{
	const char *p = strrchr(path, '/');

	return p ? p + 1 : path;
}
/*
 * get the directory part of the path.
 *
 *	i)	path	path name
 *	o)	result	result buffer
 *	r)		directory part
 * Ex.
 * dirpart("a/b/c")	=> "a/b"
 */
static const char *
dirpart(const char *path, char *result)
{
	char *p = result;
	const char *q = path, *limit = strrchr(path, '/');

	while (q < limit)
		*p++ = *q++; 
	*p = '\0';
	return result;
}
/*
 * get the local path name if the path is under the dir.
 *
 *	i)	path	path name
 *	i)	dir	directory name
 *	r)		local path name
 *
 * Ex.
 * localpath("a/b/c", "a/b")	=> "c"
 * localpath("a/b/c/d", "a/b")	=> "c/d"
 * localpath("a/b/c", "a/d")	=> NULL
 */
static const char *
localpath(const char *path, char *dir)
{
	int length = strlen(dir);

	if (!strncmp(path, dir, length) && *(path + length) == '/')
		return path + length + 1;
	return NULL;
}
/*
 * append '/' after the path name
 *
 *	i)	path	path name
 *	r)		appended path name
 *
 * Ex.
 * appendslash("a")	=> "a/"
 */
static const char *
appendslash(const char *path)
{
	STATIC_STRBUF(sb);

	strbuf_clear(sb);
	strbuf_puts(sb, path);
	strbuf_putc(sb, '/');
	return strbuf_value(sb);
}
/*
 * remove './' at the head of the path name
 *
 *	i)	path	path name
 *	r)		removed path name
 *
 * Ex.
 * removedotslash("./a") => "a"
 */
static const char *
removedotslash(const char *path)
{
	return (*path == '.' && *(path + 1) == '/') ? path + 2 : path;
}
/*
 * insert_comma: insert comma to the number.
 *
 *	i)	n	number
 *	r)		edited string
 *
 * Ex.
 * 10000 => 10,000
 */
static const char *
insert_comma(unsigned int n)
{
#define RESULTSIZE 15
#define INTERVAL 3
	static char result[RESULTSIZE];
	int i = RESULTSIZE;
	int count = 0;

	if (n == 0)
		return (const char *)"0";
	if (n > 1000000000)
		goto giveup;
	result[--i] = '\0';
	for (; n > 0; n = n / 10) {
		/*
		 * Buffer overflow. This is not important even if occurring.
		 */
		if (i <= 0)
			goto giveup;
		if (count && count % INTERVAL == 0)
			result[--i] = ',';
		result[--i] = n % 10 + '0';
		count++;
	}
	return (const char *)&result[i];
giveup:
	snprintf(result, sizeof(result), "*****");
	return (const char *)result;
}
/*----------------------------------------------------------------------*/
/* Generating file index						*/
/*----------------------------------------------------------------------*/
static int print_directory(int, char *);
static void print_directory_header(FILE *, int, const char *);
static void print_directory_footer(FILE *, int, const char *);
static const char *print_file_name(int, const char *);
static const char *print_directory_name(int, const char *, int);

FILE *FILEMAP;
STRBUF *files;
const char *indexlink;
regex_t is_include_file;
int src_count;

/*
 * print contents of one directory and the descendant. (recursively called)
 *
 *	i)	level	directory nest level
 *	io)	basedir	current directory
 *
 * This function read find style records, and print directory tree.
 */
/*
 * File list of the top level directory (when level == 0) is not written
 * to a file directly. Instead, it is written to string buffer, because
 * it appears in some places.
 */
#define PUT(s) do {						\
		if (level == 0)					\
			 strbuf_puts(files, s);			\
		 else						\
			 fputs(s, op);				\
} while (0)

static int
print_directory(int level, char *basedir)
{
	const char *path;
	FILEOP *fileop = NULL;
	FILE *op = NULL;
	int flist_items = 0;
	int count = 0;

	if (level > 0) {
		char name[MAXPATHLEN];

		snprintf(name, sizeof(name), "%s/files/%s.%s", distpath, path2fid(basedir), HTML);
		fileop = open_output_file(name, cflag);
		op = get_descripter(fileop);
		print_directory_header(op, level, basedir);
	}
	while ((path = getpath()) != NULL) {
		const char *p, *local = localpath(path, basedir);

		/*
		 * Path is outside of basedir.
		 */
		if (local == NULL) {
			ungetpath();	/* read again by upper level print_directory(). */
			break;
		}
		/*
		 * Path is inside of basedir.
		 */
		else {
			char *slash = strchr(local, '/');

			if (table_flist && flist_items++ % flist_fields == 0)
				PUT(fline_begin);
			/*
			 * Print directory.
			 */
			if (slash) {
				int baselen = strlen(basedir);
				char *q, *last = basedir + baselen;
				int subcount;

				if (baselen + 1 + (slash - local)  > MAXPATHLEN) {
					fprintf(stderr, "Too long path name.\n");
					exit(1);
				}
				/*
				 * Append new directory to the basedir.
				 */
				p = local;
				q = last;
				*q++ = '/';
				while (p < slash)
					*q++ = *p++;
				*q = '\0';
				/*
				 * print tree for this directory.
				 */
				ungetpath();	/* read again by lower level print_directory(). */
				subcount = print_directory(level + 1, basedir);
				PUT(print_directory_name(level, basedir, subcount));
				count += subcount;
				/*
				 * Shrink the basedir.
				 */
				*last = '\0';
			}
			/*
			 * Print file.
			 */
			else {
				PUT(print_file_name(level, path));
				count++;
			}
			if (table_flist && flist_items % flist_fields == 0)
				PUT(fline_end);
		}
	}
	if (flist_items % flist_fields != 0)
		PUT(fline_end);
	if (level > 0) {
		print_directory_footer(op, level, basedir);
		close_file(fileop);
	}
	html_count++;
	return count;
}
/*
 * print directory header.
 *
 *	i)	op	file index
 *	i)	level	1,2...
 *	i)	dir	directory name
 */
static void
print_directory_header(FILE *op, int level, const char *dir)
{
	STATIC_STRBUF(sb);

	if (level == 0)
		die("print_directory_header: internal error.");
	strbuf_clear(sb);
	strbuf_puts(sb, removedotslash(dir));
	strbuf_putc(sb, '/');
	fputs_nl(gen_page_begin(strbuf_value(sb), SUBDIR), op);
	fputs_nl(body_begin, op);

	strbuf_clear(sb);
 	strbuf_sprintf(sb, "%s%sroot%s/", header_begin, gen_href_begin(NULL, indexlink, normal_suffix, NULL), gen_href_end());
	fputs(strbuf_value(sb), op);
	{
		char path[MAXPATHLEN];
		char *p, *q;

		strlimcpy(path, dir, sizeof(path));
		for (p = path + 1; p != NULL; p = strchr(p, '/')) {
			int save = 0;

			q = ++p;
			while (*q && *q != '/')
				q++;
			save = *q;
			if (*q == '/')
				*q = '\0';
			if (save == '/')
				fputs(gen_href_begin(NULL, path2fid(path), HTML, NULL), op);
			fputs(p, op);
			if (save == '/')
				fputs(gen_href_end(), op);
			*q = save;
			fputc('/', op);
		}
	}
	fputs_nl(header_end, op);
	{
		char parentdir[MAXPATHLEN];
		const char *suffix, *parent;

		(void)dirpart(dir, parentdir);
		if (level == 1) {
			parent = indexlink;
			suffix = normal_suffix;
		} else {
			parent = path2fid(parentdir);
			suffix = HTML;
		}
		fputs(gen_href_begin_with_title(NULL, parent, suffix, NULL, "Parent Directory"), op);
	}
	if (Iflag)
		fputs(gen_image(PARENT, back_icon, ".."), op);
	else
		fputs("[..]", op);
	fputs_nl(gen_href_end(), op);
	if (table_flist)
		fputs_nl(flist_begin, op);
	else if (!no_order_list)
		fputs_nl(list_begin, op);
	else {
		fputs(br, op);
		fputs_nl(br, op);
	}
}
/*
 * print directory footer.
 *
 *	i)	op	file index
 *	i)	level	1,2...
 *	i)	dir	directory name
 */
static void
print_directory_footer(FILE *op, int level, const char *dir)
{
	const char *parent, *suffix;
	char parentdir[MAXPATHLEN];

	if (level == 0)
		die("print_directory_footer: internal error.");
	(void)dirpart(dir, parentdir);
	if (level == 1) {
		parent = indexlink;
		suffix = normal_suffix;
	} else {
		parent = path2fid(parentdir);
		suffix = HTML;
	}
	if (table_flist)
		fputs_nl(flist_end, op);
	else if (!no_order_list)
		fputs_nl(list_end, op);
	else
		fputs_nl(br, op);
	fputs(gen_href_begin_with_title(NULL, parent, suffix, NULL, "Parent Directory"), op);
	if (Iflag)
		fputs(gen_image(PARENT, back_icon, ".."), op);
	else
		fputs("[..]", op);
	fputs_nl(gen_href_end(), op);
	fputs_nl(body_end, op);
	fputs_nl(gen_page_end(), op);
}
/*
 * print file name.
 *
 *	i)	level	0,1,2...
 *	i)	path	path of the file
 */
static const char *
print_file_name(int level, const char *path)
{
	STATIC_STRBUF(sb);
	char *target = (Fflag) ? "mains" : "_top";
	int size = filesize(path);
	char tips[80];

	message(" [%d] adding %s", ++src_count, removedotslash(path));
	/*
	 * We assume the file which has one of the following suffixes
	 * as a candidate of include file.
	 *
	 * C: .h
	 * C++: .hxx, .hpp, .H
	 * PHP: .inc.php
	 */
	if (regexec(&is_include_file, path, 0, 0, 0) == 0)
		put_inc(lastpart(path), path, src_count);
	strbuf_clear(sb);
	if (table_flist)
		strbuf_puts(sb, fitem_begin);
	else if (!no_order_list)
		strbuf_puts(sb, item_begin);
	if (size > 1)
		snprintf(tips, sizeof(tips), "%s bytes", insert_comma(size));
	else
		snprintf(tips, sizeof(tips), "%s byte", insert_comma(size));
	strbuf_puts(sb, gen_href_begin_with_title_target(level == 0 ? SRCS: upperdir(SRCS),
			path2fid(path), HTML, NULL, tips, target));
	if (Iflag) {
		const char *lang, *suffix, *text_icon;

		if ((suffix = locatestring(path, ".", MATCH_LAST)) != NULL
		    && (lang = decide_lang(suffix)) != NULL
		    && (strcmp(lang, "c") == 0 || strcmp(lang, "cpp") == 0
		       || strcmp(lang, "yacc") == 0))
			text_icon = c_icon;
		else
			text_icon = file_icon;
		strbuf_puts(sb, gen_image(level == 0 ? CURRENT : PARENT, text_icon, removedotslash(path)));
		strbuf_puts(sb, quote_space);
	}
	strbuf_puts(sb, full_path ? removedotslash(path) : lastpart(path));
	strbuf_puts(sb, gen_href_end());
	if (table_flist)
		strbuf_puts(sb, fitem_end);
	else if (!no_order_list)
		strbuf_puts(sb, item_end);
	else
		strbuf_puts(sb, br);
	strbuf_putc(sb, '\n');
	if (map_file)
		fprintf(FILEMAP, "%s\t%s/%s.%s\n", removedotslash(path), SRCS, path2fid(path), HTML);
	return (const char *)strbuf_value(sb);
}
/*
 * print directory name.
 *
 *	i)	level	0,1,2...
 *	i)	path	path of the directory
 *	i)	count	number of files in this directory
 */
static const char *
print_directory_name(int level, const char *path, int count)
{
	STATIC_STRBUF(sb);
	char tips[80];

	if (count > 1)
		snprintf(tips, sizeof(tips), "%d files", count);
	else
		snprintf(tips, sizeof(tips), "%d file", count);
	path = removedotslash(path);
	strbuf_clear(sb);
	if (table_flist)
		strbuf_puts(sb, fitem_begin);
	else if (!no_order_list)
		strbuf_puts(sb, item_begin);
	strbuf_puts(sb, gen_href_begin_with_title(level == 0 ? "files" : NULL,
			path2fid(path), HTML, NULL, tips));
	if (Iflag) {
		strbuf_puts(sb, gen_image(level == 0 ? CURRENT : PARENT, dir_icon, appendslash(path)));
		strbuf_puts(sb, quote_space);
	}
	strbuf_sprintf(sb, "%s/%s", lastpart(path), gen_href_end());
	if (table_flist)
		strbuf_puts(sb, fitem_end);
	else if (!no_order_list)
		strbuf_puts(sb, item_end);
	else
		strbuf_puts(sb, br);
	strbuf_putc(sb, '\n');
	return (const char *)strbuf_value(sb);
}
/*
 * makefileindex: make file index.
 *
 *	i)	file		output file name
 *	o)	files		top level file index
 */
int
makefileindex(const char *file, STRBUF *a_files)
{
	STATIC_STRBUF(sb);
	FILE *filesop;
	int flags = REG_EXTENDED;
	/*
	 * Basedir is a directory to which we are paying attention on each
	 * occasion. It starts with ".", grows and shrink according to the
	 * progress of processing. It isn't copied each every recursive call
	 * not to waste the stack.
	 */
	char basedir[MAXPATHLEN];

	/*
	 * Initialize data.
	 */
	indexlink = (Fflag) ? "../files" : "../mains";
	src_count = 0;

	gp = gfind_open(dbpath, NULL, other_files ? GPATH_BOTH : GPATH_SOURCE);
	/*
	 * for collecting include files.
	 */
	if (w32)
		flags |= REG_ICASE;
	strbuf_clear(sb);
	strbuf_puts(sb, "\\.(");
	{
		const char *p = include_file_suffixes;
		int c;

		while ((c = (unsigned char)*p++) != '\0') {
			if (isregexchar(c))
				strbuf_putc(sb, '\\');
			else if (c == ',')
				c = '|';
			strbuf_putc(sb, c);
		}
	}
	strbuf_puts(sb, ")$");
	if (regcomp(&is_include_file, strbuf_value(sb), flags) != 0)
		die("cannot compile regular expression '%s'.", strbuf_value(sb));

	/*
	 * Write to files.html.
	 */
	if ((filesop = fopen(makepath(distpath, file, NULL), "w")) == NULL)
		die("cannot open file '%s'.", file);
	fputs_nl(gen_page_begin(title_file_index, TOPDIR), filesop);
	fputs_nl(body_begin, filesop);
	fputs(header_begin, filesop);
	fputs(gen_href_begin(NULL, "files", normal_suffix, NULL), filesop);
	fputs(title_file_index, filesop);
	fputs(gen_href_end(), filesop);
	fputs_nl(header_end, filesop);
	if (table_flist) {
		fputs_nl(flist_begin, filesop);
	} else if (!no_order_list) {
		fputs_nl(list_begin, filesop);
	}
	FILEMAP = NULL;
	if (map_file) {
		if (!(FILEMAP = fopen(makepath(distpath, "FILEMAP", NULL), "w")))
                        die("cannot open '%s'.", makepath(distpath, "FILEMAP", NULL));
	}
	/*
	 * print whole directory tree.
	 */
	files = a_files;
	strcpy(basedir, ".");
	(void)print_directory(0, basedir);

	if (map_file)
		fclose(FILEMAP);
	gfind_close(gp);
	regfree(&is_include_file);

	fputs(strbuf_value(files), filesop);
	if (table_flist)
		fputs_nl(flist_end, filesop);
	else if (!no_order_list)
		fputs_nl(list_end, filesop);
	else
		fputs_nl(br, filesop);
	fputs_nl(body_end, filesop);
	fputs_nl(gen_page_end(), filesop);
	fclose(filesop);
	html_count++;
	return src_count;
}
/*----------------------------------------------------------------------*/
/* Main body of generating include file index				*/
/*----------------------------------------------------------------------*/
void
makeincludeindex(void)
{
	FILE *PIPE;
	STRBUF *input = strbuf_open(0);
	char *ctags_x;
	struct data *inc;
	char *target = (Fflag) ? "mains" : "_top";
	char command[MAXFILLEN];

	/*
	 * Pick up include pattern.
	 *
	 * C: #include "xxx.h"
	 * PHP: include("xxx.inc.php");
	 */
	/*
	 * Unlike Perl regular expression, POSIX regular expression doesn't support C-style escape sequence.
	 * Therefore, we can not use "\\t" here.
	 */
	snprintf(command, sizeof(command), "%s -gnx --encode-path=\" \t\" \"^[ \t]*(#[ \t]*(import|include)|include[ \t]*\\()\"", global_path);
	if ((PIPE = popen(command, "r")) == NULL)
		die("cannot fork.");
	strbuf_reset(input);
	while ((ctags_x = strbuf_fgets(input, PIPE, STRBUF_NOCRLF)) != NULL) {
		SPLIT ptable;
		char buf[MAXBUFLEN];
		int is_php = 0;
		const char *last, *lang, *suffix;

		if (split(ctags_x, 4, &ptable) < 4) {
			recover(&ptable);
			die("too small number of parts in makefileindex().");
		}
		if ((suffix = locatestring(ptable.part[PART_PATH].start, ".", MATCH_LAST)) != NULL
		    && (lang = decide_lang(suffix)) != NULL
		    && strcmp(lang, "php") == 0)
			is_php = 1;
		last = extract_lastname(ptable.part[PART_LINE].start, is_php);
		if (last == NULL || (inc = get_inc(last)) == NULL)
			continue;
		recover(&ptable);
		/*
		 * s/^[^ \t]+/$last/;
		 */
		{
			const char *p;
			char *q = buf;

			for (p = last; *p; p++)
				*q++ = *p;
			for (p = ctags_x; *p && *p != ' ' && *p != '\t'; p++)
				;
			for (; *p; p++)
				*q++ = *p;
			*q = '\0';
		}
		put_included(inc, buf);
	}
	if (pclose(PIPE) != 0)
		die("terminated abnormally.");

	for (inc = first_inc(); inc; inc = next_inc()) {
		const char *last = inc->name;
		int no = inc->id;
		FILEOP *fileop_INCLUDE;
		FILE *INCLUDE;

		if (inc->count > 1) {
			char path[MAXPATHLEN];

			snprintf(path, sizeof(path), "%s/%s/%d.%s", distpath, INCS, no, HTML);
			fileop_INCLUDE = open_output_file(path, cflag);
			INCLUDE = get_descripter(fileop_INCLUDE);
			fputs_nl(gen_page_begin(last, SUBDIR), INCLUDE);
			fputs_nl(body_begin, INCLUDE);
			fputs_nl(verbatim_begin, INCLUDE);
			{
				const char *filename = strbuf_value(inc->contents);
				int count = inc->count;

				for (; count; filename += strlen(filename) + 1, count--) {
					fputs(gen_href_begin_with_title_target(upperdir(SRCS), path2fid(filename), HTML, NULL, NULL, target), INCLUDE);
					fputs(removedotslash(filename), INCLUDE);
					fputs_nl(gen_href_end(), INCLUDE);
				}
			}
			fputs_nl(verbatim_end, INCLUDE);
			fputs_nl(body_end, INCLUDE);
			fputs_nl(gen_page_end(), INCLUDE);
			close_file(fileop_INCLUDE);
			html_count++;
			/*
			 * inc->contents == NULL means that information already
			 * written to file.
			 */
			strbuf_close(inc->contents);
			inc->contents = NULL;
		}
		if (!inc->ref_count)
			continue;
		if (inc->ref_count == 1) {
			SPLIT ptable;
			char buf[1024];

			if (split(strbuf_value(inc->ref_contents), 4, &ptable) < 4) {
				recover(&ptable);
				die("too small number of parts in makefileindex().");
			}
			snprintf(buf, sizeof(buf), "%s %s", ptable.part[PART_LNO].start, decode_path((unsigned char *)ptable.part[PART_PATH].start));
			recover(&ptable);
			strbuf_reset(inc->ref_contents);
			strbuf_puts(inc->ref_contents, buf);
		} else {
			char path[MAXPATHLEN];

			snprintf(path, sizeof(path), "%s/%s/%d.%s", distpath, INCREFS, no, HTML);
			fileop_INCLUDE = open_output_file(path, cflag);
			INCLUDE = get_descripter(fileop_INCLUDE);
			fputs_nl(gen_page_begin(last, SUBDIR), INCLUDE);
			fputs_nl(body_begin, INCLUDE);
			fputs_nl(gen_list_begin(), INCLUDE);
			{
				const char *line = strbuf_value(inc->ref_contents);
				int count = inc->ref_count;

				for (; count; line += strlen(line) + 1, count--)
					fputs_nl(gen_list_body(upperdir(SRCS), line, NULL), INCLUDE);
			}
			fputs_nl(gen_list_end(), INCLUDE);
			fputs_nl(body_end, INCLUDE);
			fputs_nl(gen_page_end(), INCLUDE);
			close_file(fileop_INCLUDE);
			html_count++;
			/*
			 * inc->ref_contents == NULL means that information already
			 * written to file.
			 */
			strbuf_close(inc->ref_contents);
			inc->ref_contents = NULL;
		}
	}
	strbuf_close(input);
}
