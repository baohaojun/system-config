/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
 *		2006, 2008, 2010
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
#include <ctype.h>

#include "global.h"
#include "anchor.h"
#include "cache.h"
#include "common.h"
#include "incop.h"
#include "path2url.h"
#include "htags.h"

/*----------------------------------------------------------------------*/
/* Parser switch							*/
/*----------------------------------------------------------------------*/
/*
 * This is the linkage section of each parsers.
 * If you want to support new language, you must define two procedures:
 *	1. Initializing procedure.
 *		Called once first with an input file descripter.
 *	2. Executing procedure.
 *		Called repeatedly until returning EOF.
 *		It should read from above descripter and write HTML
 *		using output procedures in this module.
 */
struct lang_entry {
	const char *lang_name;
	void (*init_proc)(FILE *);		/* initializing procedure */
	int (*exec_proc)(void);			/* executing procedure */
};

/* initializing procedures */
void c_parser_init(FILE *);
void yacc_parser_init(FILE *);
void cpp_parser_init(FILE *);
void java_parser_init(FILE *);
void php_parser_init(FILE *);
void asm_parser_init(FILE *);

/* executing procedures */
int c_lex(void);
int cpp_lex(void);
int java_lex(void);
int php_lex(void);
int asm_lex(void);

/*
 * The first entry is default language.
 */
struct lang_entry lang_switch[] = {
	/* lang_name	init_proc 		exec_proc */
	{"c",		c_parser_init,		c_lex},		/* DEFAULT */
	{"yacc",	yacc_parser_init,	c_lex},
	{"cpp",		cpp_parser_init,	cpp_lex},
	{"java",	java_parser_init,	java_lex},
	{"php",		php_parser_init,	php_lex},
	{"asm",		asm_parser_init,	asm_lex}
};
#define DEFAULT_ENTRY &lang_switch[0]

/*
 * get language entry.
 *
 *	i)	lang	language name (NULL means 'not specified'.)
 *	r)		language entry
 */
static struct lang_entry *
get_lang_entry(const char *lang)
{
	int i, size = sizeof(lang_switch) / sizeof(struct lang_entry);

	/*
	 * if language not specified, it assumes default language.
	 */
	if (lang == NULL)
		return DEFAULT_ENTRY;
	for (i = 0; i < size; i++)
		if (!strcmp(lang, lang_switch[i].lang_name))
			return &lang_switch[i];
	/*
	 * if specified language not found, it assumes default language.
	 */
	return DEFAULT_ENTRY;
}
/*----------------------------------------------------------------------*/
/* Input/Output								*/
/*----------------------------------------------------------------------*/
/*
 * Input/Output descriptor.
 */
static FILEOP *fileop_out;
static FILEOP *fileop_in;
static FILE *out;
static FILE *in;

STATIC_STRBUF(outbuf);
static const char *curpfile;
static int warned;
static int last_lineno;

/*
 * Put a character to HTML as is.
 *
 * You should use this function to put a control character.
 */
void
echoc(int c)
{
        strbuf_putc(outbuf, c);
}
/*
 * Put a string to HTML as is.
 *
 * You should use this function to put a control sequence.
 */
void
echos(const char *s)
{
        strbuf_puts(outbuf, s);
}
/*----------------------------------------------------------------------*/
/* HTML output								*/
/*----------------------------------------------------------------------*/
/*
 * Quote character with HTML's way.
 */
static const char *
HTML_quoting(int c)
{
	if (c == '<')
		return quote_little;
	else if (c == '>')
		return quote_great;
	else if (c == '&')
		return quote_amp;
	return NULL;
}
/*
 * fill_anchor: fill anchor into file name
 *
 *       i)      $root   root or index page
 *       i)      $path   path name
 *       r)              hypertext file name string
 */
const char *
fill_anchor(const char *root, const char *path)
{
	STATIC_STRBUF(sb);
	char buf[MAXBUFLEN], *limit, *p;

	strbuf_clear(sb);
	strlimcpy(buf, path, sizeof(buf));
	for (p = buf; *p; p++)
		if (*p == sep)
			*p = '\0';
	limit = p;

	strbuf_sprintf(sb, "%sroot%s/", gen_href_begin_simple(root), gen_href_end());
	{
		const char *next;

		for (p = buf; p < limit; p += strlen(p) + 1) {
			const char *path = buf;
			const char *unit = p;

			next = p + strlen(p) + 1;
			if (next > limit) {
				strbuf_puts(sb, unit);
				break;
			}
			if (p > buf)
				*(p - 1) = sep;
			strbuf_puts(sb, gen_href_begin("../files", path2fid(path), HTML, NULL));
			strbuf_puts(sb, unit);
			strbuf_puts(sb, gen_href_end());
			strbuf_putc(sb, '/');
		}
	}
        return strbuf_value(sb);
}

/*
 * link_format: make hypertext from anchor array.
 *
 *	i)	(previous, next, first, last, top, bottom)
 *		-1: top, -2: bottom, other: line number
 *	r)	HTML
 */
const char *
link_format(int ref[A_SIZE])
{
	STATIC_STRBUF(sb);
	const char **label = Iflag ? anchor_comment : anchor_label;
	const char **icons = anchor_icons;
	int i;

	strbuf_clear(sb);
	for (i = 0; i < A_LIMIT; i++) {
		if (i == A_INDEX) {
			strbuf_puts(sb, gen_href_begin("..", "mains", normal_suffix, NULL));
		} else if (i == A_HELP) {
			strbuf_puts(sb, gen_href_begin("..", "help", normal_suffix, NULL));
		} else if (ref[i]) {
			char tmp[32], *key = tmp;

			if (ref[i] == -1)
				key = "TOP";
			else if (ref[i] == -2)
				key = "BOTTOM";
			else
				snprintf(tmp, sizeof(tmp), "%d", ref[i]);
			strbuf_puts(sb, gen_href_begin(NULL, NULL, NULL, key));
		}
		if (Iflag) {
			char tmp[MAXPATHLEN];
			snprintf(tmp, sizeof(tmp), "%s%s", (i != A_INDEX && i != A_HELP && ref[i] == 0) ? "n_" : "", icons[i]);
			strbuf_puts(sb, gen_image(PARENT, tmp, label[i]));
		} else {
			strbuf_sprintf(sb, "[%s]", label[i]);
		}
		if (i == A_INDEX || i == A_HELP || ref[i] != 0)
			strbuf_puts(sb, gen_href_end());
	}
        return strbuf_value(sb);
}
/*
 * generate_guide: generate guide string for definition line.
 *
 *	i)	lineno	line number
 *	r)		guide string
 */
const char *
generate_guide(int lineno)
{
	STATIC_STRBUF(sb);
	int i = 0;

	strbuf_clear(sb);
	if (definition_header == RIGHT_HEADER)
		i = 4;
	else if (nflag)
		i = ncol + 1;
	if (i > 0)
		for (; i > 0; i--)
			strbuf_putc(sb, ' ');
	strbuf_sprintf(sb, "%s/* ", comment_begin);
	strbuf_puts(sb, link_format(anchor_getlinks(lineno)));
	if (show_position)
		strbuf_sprintf(sb, "%s%s[+%d %s]%s",
			quote_space, position_begin, lineno, curpfile, position_end);
	strbuf_sprintf(sb, " */%s", comment_end);

	return strbuf_value(sb);
}
/*
 * tooltip: generate tooltip string
 *
 *	i)	type	I,R,Y,D,M
 *	i)	lno	line number
 *	i)	opt	
 *	r)		tooltip string
 */
const char *
tooltip(int type, int lno, const char *opt)
{
	STATIC_STRBUF(sb);

	strbuf_clear(sb);
	if (lno > 0) {
		if (type == 'I')
			strbuf_puts(sb, "Included from");
		else if (type == 'R')
			strbuf_puts(sb, "Defined at");
		else if (type == 'Y')
			strbuf_puts(sb, "Used at");
		else
			strbuf_puts(sb, "Refered from");
		strbuf_putc(sb, ' ');
		strbuf_putn(sb, lno);
		if (opt) {
			strbuf_puts(sb, " in ");
			strbuf_puts(sb, opt);
		}
	} else {
		strbuf_puts(sb, "Multiple ");
		if (type == 'I')
			strbuf_puts(sb, "included from");
		else if (type == 'R')
			strbuf_puts(sb, "defined in");
		else if (type == 'Y')
			strbuf_puts(sb, "used in");
		else
			strbuf_puts(sb, "refered from");
		strbuf_putc(sb, ' ');
		strbuf_puts(sb, opt);
		strbuf_putc(sb, ' ');
		strbuf_puts(sb, "places");
	}
	strbuf_putc(sb, '.');
	return strbuf_value(sb);
}
/*
 * put_anchor: output HTML anchor.
 *
 *	i)	name	tag
 *	i)	type	tag type
 *	i)	lineno	current line no
 */
void
put_anchor(char *name, int type, int lineno)
{
	const char *line;
	int db;

	if (type == 'R')
		db = GTAGS;
	else if (type == 'Y')
		db = GSYMS;
	else	/* 'D', 'M' or 'T' */
		db = GRTAGS;
	line = cache_get(db, name);
	if (line == NULL) {
		if ((type == 'R' || type == 'Y') && wflag) {
			warning("%s %d %s(%c) found but not defined.",
				curpfile, lineno, name, type);
			if (colorize_warned_line)
				warned = 1;
		}
		strbuf_puts(outbuf, name);
	} else {
		/*
		 * About the format of 'line', please see the head comment of cache.c.
		 */
		if (*line == ' ') {
			const char *fid = line + 1;
			const char *count = nextstring(fid);
			const char *dir, *file, *suffix = NULL;

			if (dynamic) {
				STATIC_STRBUF(sb);

				strbuf_clear(sb);
				strbuf_puts(sb, action);
				strbuf_putc(sb, '?');
				strbuf_puts(sb, "pattern=");
				strbuf_puts(sb, name);
				strbuf_puts(sb, quote_amp);
				if (Sflag) {
					strbuf_puts(sb, "id=");
					strbuf_puts(sb, sitekey);
					strbuf_puts(sb, quote_amp);
				}
				strbuf_puts(sb, "type=");
				if (db == GTAGS)
					strbuf_puts(sb, "definitions");
				else if (db == GRTAGS)
					strbuf_puts(sb, "reference");
				else
					strbuf_puts(sb, "symbol");
				file = strbuf_value(sb);
				dir = (*action == '/') ? NULL : "..";
			} else {
				if (type == 'R')
					dir = upperdir(DEFS);
				else if (type == 'Y')
					dir = upperdir(SYMS);
				else	/* 'D', 'M' or 'T' */
					dir = upperdir(REFS);
				file = fid;
				suffix = HTML;
			}
			strbuf_puts(outbuf, gen_href_begin_with_title(dir, file, suffix, NULL, tooltip(type, -1, count)));
			strbuf_puts(outbuf, name);
			strbuf_puts(outbuf, gen_href_end());
		} else {
			const char *lno = line;
			const char *fid = nextstring(line);
			const char *path = gpath_fid2path(fid, NULL);

			path += 2;              /* remove './' */
			/*
			 * Don't make a link which refers to itself.
			 * Being used only once means that it is a self link.
			 */
			if (db == GSYMS) {
				strbuf_puts(outbuf, name);
				return;
			}
			strbuf_puts(outbuf, gen_href_begin_with_title(upperdir(SRCS), fid, HTML, lno, tooltip(type, atoi(lno), path)));
			strbuf_puts(outbuf, name);
			strbuf_puts(outbuf, gen_href_end());
		}
	}
}
/*
 * put_include_anchor: output HTML anchor.
 *
 *	i)	inc	inc structure
 *	i)	path	path name for display
 */
void
put_include_anchor(struct data *inc, const char *path)
{
	if (inc->count == 1)
		strbuf_puts(outbuf, gen_href_begin(NULL, path2fid(strbuf_value(inc->contents)), HTML, NULL));
	else {
		char id[32];
		snprintf(id, sizeof(id), "%d", inc->id);
		strbuf_puts(outbuf, gen_href_begin(upperdir(INCS), id, HTML, NULL));
	}
	strbuf_puts(outbuf, path);
	strbuf_puts(outbuf, gen_href_end());
}
/*
 * put_include_anchor_direct: output HTML anchor.
 *
 *	i)	file	normalized path
 *	i)	path	path name for display
 */
void
put_include_anchor_direct(const char *file, const char *path)
{
	strbuf_puts(outbuf, gen_href_begin(NULL, path2fid(file), HTML, NULL));
	strbuf_puts(outbuf, path);
	strbuf_puts(outbuf, gen_href_end());
}
/*
 * Put a reserved word. (if, while, ...)
 */
void
put_reserved_word(const char *word)
{
	strbuf_puts(outbuf, reserved_begin);
	strbuf_puts(outbuf, word);
	strbuf_puts(outbuf, reserved_end);
}
/*
 * Put a macro (#define,#undef,...) 
 */
void
put_macro(const char *word)
{
	strbuf_puts(outbuf, sharp_begin);
	strbuf_puts(outbuf, word);
	strbuf_puts(outbuf, sharp_end);
}
/*
 * Print warning message when unknown preprocessing directive is found.
 */
void
unknown_preprocessing_directive(const char *word, int lineno)
{
	word = strtrim(word, TRIM_ALL, NULL);
	warning("unknown preprocessing directive '%s'. [+%d %s]", word, lineno, curpfile);
	if (colorize_warned_line)
		warned = 1;
}
/*
 * Print warning message when unexpected eof.
 */
void
unexpected_eof(int lineno)
{
	warning("unexpected eof. [+%d %s]", lineno, curpfile);
	if (colorize_warned_line)
		warned = 1;
}
/*
 * Print warning message when unknown yacc directive is found.
 */
void
unknown_yacc_directive(const char *word, int lineno)
{
	warning("unknown yacc directive '%s'. [+%d %s]", word, lineno, curpfile);
	if (colorize_warned_line)
		warned = 1;
}
/*
 * Print warning message when unmatched brace is found.
 */
void
missing_left(const char *word, int lineno)
{
	warning("missing left '%s'. [+%d %s]", word, lineno, curpfile);
	if (colorize_warned_line)
		warned = 1;
}
/*
 * Put a character with HTML quoting.
 *
 * If you want to put '<', '>' and '&', you should echoc() instead.
 */
void
put_char(int c)
{
	const char *quoted = HTML_quoting(c);

	if (quoted)
		strbuf_puts(outbuf, quoted);
	else
		strbuf_putc(outbuf, c);
}
/*
 * Put a string with HTML quoting.
 *
 * If you want to put HTML tag itself, you should echoc() instead.
 */
void
put_string(const char *s)
{
	for (; *s; s++)
		put_char(*s);
}
/*
 * Put brace ('{', '}')
 */
void
put_brace(const char *text)
{
	strbuf_puts(outbuf, brace_begin);
	strbuf_puts(outbuf, text);
	strbuf_puts(outbuf, brace_end);
}

/*
 * common procedure for line control.
 */
static char lineno_format[32];
static const char *guide = NULL;

/*
 * Begin of line processing.
 */
void
put_begin_of_line(int lineno)
{
        if (definition_header != NO_HEADER) {
                if (define_line(lineno))
                        guide = generate_guide(lineno);
                else
                        guide = NULL;
        }
        if (guide && definition_header == BEFORE_HEADER) {
		fputs_nl(guide, out);
                guide = NULL;
        }
}
/*
 * End of line processing.
 *
 *	i)	lineno	current line number
 *	gi)	outbuf	HTML line image
 *
 * The outbuf(string buffer) has HTML image of the line.
 * This function flush and clear it.
 */
void
put_end_of_line(int lineno)
{
	fputs(gen_name_number(lineno), out);
        if (nflag)
                fprintf(out, lineno_format, lineno);
	if (warned)
		fputs(warned_line_begin, out);

	/* flush output buffer */
	fputs(strbuf_value(outbuf), out);
	strbuf_reset(outbuf);

	if (warned)
		fputs(warned_line_end, out);
	if (guide == NULL)
		fputc('\n', out);
	else {
		if (definition_header == RIGHT_HEADER)
			fputs(guide, out);
		fputc('\n', out);
		if (definition_header == AFTER_HEADER) {
			fputs_nl(guide, out);
		}
		guide = NULL;
	}
	warned = 0;

	/* save for the other job in this module */
	last_lineno = lineno;
}
/*
 * Encode URL.
 *
 *	o)	sb	encoded URL
 *	i)	url	URL
 */
static void
encode(STRBUF *sb, const char *url)
{
	int c;

	while ((c = (unsigned char)*url++) != '\0') {
		if (isurlchar(c)) {
			strbuf_putc(sb, c);
		} else {
			strbuf_putc(sb, '%');
			strbuf_putc(sb, "0123456789abcdef"[c >> 4]);
			strbuf_putc(sb, "0123456789abcdef"[c & 0x0f]);
		}
	}
}
/*
 * get_cvs_module: return CVS module of source file.
 *
 *	i)	file		source path
 *	o)	basename	If basename is not NULL, store pointer to
 *				the last component of source path.
 *	r)		!=NULL : relative path from repository top
 *			==NULL : CVS/Repository is not readable.
 */
static const char *
get_cvs_module(const char *file, const char **basename)
{
	const char *p;
	STATIC_STRBUF(dir);
	static char prev_dir[MAXPATHLEN];
	STATIC_STRBUF(module);
	FILE *ip;

	strbuf_clear(dir);
	p = locatestring(file, "/", MATCH_LAST);
	if (p != NULL) {
		strbuf_nputs(dir, file, p - file);
		p++;
	} else {
		strbuf_putc(dir, '.');
		p = file;
	}
	if (basename != NULL)
		*basename = p;
	if (strcmp(strbuf_value(dir), prev_dir) != 0) {
		strlimcpy(prev_dir, strbuf_value(dir), sizeof(prev_dir));
		strbuf_clear(module);
		strbuf_puts(dir, "/CVS/Repository");
		ip = fopen(strbuf_value(dir), "r");
		if (ip != NULL) {
			strbuf_fgets(module, ip, STRBUF_NOCRLF);
			fclose(ip);
		}
	}
	if (strbuf_getlen(module) > 0)
		return strbuf_value(module);
	return NULL;
}
/*
 *
 * src2html: convert source code into HTML
 *
 *       i)      src   source file     - Read from
 *       i)      html  HTML file       - Write to
 *       i)      notsource 1: isn't source, 0: source.
 */
void
src2html(const char *src, const char *html, int notsource)
{
	char indexlink[128];

	/*
	 * setup lineno format.
	 */
	snprintf(lineno_format, sizeof(lineno_format), "%%%dd ", ncol);

	fileop_in  = open_input_file(src);
	in = get_descripter(fileop_in);
        curpfile = src;
        warned = 0;

	fileop_out = open_output_file(html, cflag);
	out = get_descripter(fileop_out);
	strbuf_clear(outbuf);

	if (Fflag)
		snprintf(indexlink, sizeof(indexlink), "../files.%s", normal_suffix);
	else
		snprintf(indexlink, sizeof(indexlink), "../mains.%s", normal_suffix);
	fputs_nl(gen_page_begin(src, SUBDIR), out);
	fputs_nl(body_begin, out);
	/*
         * print the header
         */
	if (insert_header)
		fputs(gen_insert_header(SUBDIR), out);
	fputs(gen_name_string("TOP"), out);
	fputs(header_begin, out);
	fputs(fill_anchor(indexlink, src), out);
	if (cvsweb_url) {
		STATIC_STRBUF(sb);
		const char *module, *basename;

		strbuf_clear(sb);
		strbuf_puts(sb, cvsweb_url);
		if (use_cvs_module
		 && (module = get_cvs_module(src, &basename)) != NULL) {
			encode(sb, module);
			strbuf_putc(sb, '/');
			encode(sb, basename);
		} else {
			encode(sb, src);
		}
		if (cvsweb_cvsroot) {
			strbuf_puts(sb, "?cvsroot=");
			strbuf_puts(sb, cvsweb_cvsroot);
		}
		fputs(quote_space, out);
		fputs(gen_href_begin_simple(strbuf_value(sb)), out);
		fputs(cvslink_begin, out);
		fputs("[CVS]", out);
		fputs(cvslink_end, out);
		fputs_nl(gen_href_end(), out);
		/* doesn't close string buffer */
	}
	fputs_nl(header_end, out);
	fputs(comment_begin, out);
	fputs("/* ", out);

	fputs(link_format(anchor_getlinks(0)), out);
	if (show_position)
		fprintf(out, "%s%s[+1 %s]%s", quote_space, position_begin, src, position_end);
	fputs(" */", out);
	fputs_nl(comment_end, out);
	fputs_nl(hr, out);
        /*
         * It is not source file.
         */
        if (notsource) {
		STRBUF *sb = strbuf_open(0);
		const char *_;

		fputs_nl(verbatim_begin, out);
		last_lineno = 0;
		while ((_ = strbuf_fgets(sb, in, STRBUF_NOCRLF)) != NULL) {
			fputs(gen_name_number(++last_lineno), out);
			detab_replacing(out, _, HTML_quoting);
		}
		fputs_nl(verbatim_end, out);
		strbuf_close(sb);
        }
	/*
	 * It's source code.
	 */
	else {
		const char *basename;
		struct data *incref;
		struct anchor *ancref;
		STATIC_STRBUF(define_index);

                /*
                 * INCLUDED FROM index.
                 */
		basename = locatestring(src, "/", MATCH_LAST);
		if (basename)
			basename++;
		else
			basename = src;
		incref = get_included(basename);
		if (incref) {
			char s_id[32];
			const char *dir, *file, *suffix, *key, *title;

			fputs(header_begin, out);
			if (incref->ref_count > 1) {
				char s_count[32];

				snprintf(s_count, sizeof(s_count), "%d", incref->ref_count);
				snprintf(s_id, sizeof(s_id), "%d", incref->id);
				dir = upperdir(INCREFS);
				file = s_id;
				suffix = HTML;
				key = NULL;
				title = tooltip('I', -1, s_count);
			} else {
				const char *p = strbuf_value(incref->ref_contents);
				const char *lno = strmake(p, " ");
				const char *filename;

				p = locatestring(p, " ", MATCH_FIRST);
				if (p == NULL)
					die("internal error.(incref->ref_contents)");
				filename = p + 1;
				if (filename[0] == '.' && filename[1] == '/')
					filename += 2;
				dir = NULL;
				file = path2fid(filename);
				suffix = HTML;
				key = lno;
				title = tooltip('I', atoi(lno), filename);
			}
			fputs(gen_href_begin_with_title(dir, file, suffix, key, title), out);
			fputs(title_included_from, out);
			fputs(gen_href_end(), out);
			fputs_nl(header_end, out);
			fputs_nl(hr, out);
		}
		/*
		 * DEFINITIONS index.
		 */
		strbuf_clear(define_index);
		for (ancref = anchor_first(); ancref; ancref = anchor_next()) {
			if (ancref->type == 'D') {
				char tmp[32];
				snprintf(tmp, sizeof(tmp), "%d", ancref->lineno);
				strbuf_puts(define_index, item_begin);
				strbuf_puts(define_index, gen_href_begin_with_title(NULL, NULL, NULL, tmp, tooltip('R', ancref->lineno, NULL)));
				strbuf_puts(define_index, gettag(ancref));
				strbuf_puts(define_index, gen_href_end());
				strbuf_puts_nl(define_index, item_end);
			}
		}
		if (strbuf_getlen(define_index) > 0) {
			fputs(header_begin, out);
			fputs(title_define_index, out);
			fputs_nl(header_end, out);
			fputs_nl("This source file includes following definitions.", out);
			fputs_nl(list_begin, out);
			fputs(strbuf_value(define_index), out);
			fputs_nl(list_end, out);
			fputs_nl(hr, out);
		}
		/*
		 * print source code
		 */
		fputs_nl(verbatim_begin, out);
		{
			const char *suffix = locatestring(src, ".", MATCH_LAST);
			const char *lang = NULL;
			struct lang_entry *ent;

			/*
			 * Decide language.
			 */
			if (suffix)
				lang = decide_lang(suffix);
			/*
			 * Select parser.
			 * If lang == NULL then default parser is selected.
			 */
			ent = get_lang_entry(lang);
			/*
			 * Initialize parser.
			 */
			ent->init_proc(in);
			/*
			 * Execute parser.
			 * Exec_proc() is called repeatedly until returning EOF.
			 */
			while (ent->exec_proc())
				;
		}
		fputs_nl(verbatim_end, out);
	}
	fputs_nl(hr, out);
	fputs_nl(gen_name_string("BOTTOM"), out);
	fputs(comment_begin, out);
	fputs("/* ", out);
	fputs(link_format(anchor_getlinks(-1)), out);
	if (show_position)
		fprintf(out, "%s%s[+%d %s]%s", quote_space, position_begin, last_lineno, src, position_end);
	fputs(" */", out);
	fputs_nl(comment_end, out);
	if (insert_footer) {
		fputs(br, out);
		fputs(gen_insert_footer(SUBDIR), out);
	}
	fputs_nl(body_end, out);
	fputs_nl(gen_page_end(), out);
	if (!notsource)
		anchor_unload();
	close_file(fileop_out);
	close_file(fileop_in);
}
