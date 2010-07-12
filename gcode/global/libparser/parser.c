/*
 * Copyright (c) 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2010
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
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#include <ltdl.h>

#include "parser.h"
#include "internal.h"
#include "checkalloc.h"
#include "die.h"
#include "langmap.h"
#include "locatestring.h"
#include "queue.h"
#include "strbuf.h"
#include "test.h"

#define NOTFUNCTION	".notfunction"
#ifdef __DJGPP__
#define DOS_NOTFUNCTION	"_notfunction"
#endif

struct words {
	const char *name;
};
static struct words *words;
static int tablesize;

static int
cmp(const void *s1, const void *s2)
{
	return strcmp(((struct words *)s1)->name, ((struct words *)s2)->name);
}

static void
load_notfunction(const char *filename)
{
	FILE *ip;
	STRBUF *sb = strbuf_open(0);
	STRBUF *ib = strbuf_open(0);
	char *p;
	int i;

	if ((ip = fopen(filename, "r")) == NULL)
		die("'%s' cannot read.", filename);
	for (tablesize = 0; (p = strbuf_fgets(ib, ip, STRBUF_NOCRLF)) != NULL; tablesize++)
		strbuf_puts0(sb, p);
	fclose(ip);
	words = (struct words *)check_malloc(sizeof(struct words) * tablesize);
	/*
	 * Don't free *p.
	 */
	p = (char *)check_malloc(strbuf_getlen(sb) + 1);
	memcpy(p, strbuf_value(sb), strbuf_getlen(sb) + 1);
	for (i = 0; i < tablesize; i++) {
		words[i].name = p;
		p += strlen(p) + 1;
	}
	qsort(words, tablesize, sizeof(struct words), cmp);
	strbuf_close(sb);
	strbuf_close(ib);
}

static int
isnotfunction(const char *name)
{
	struct words tmp;
	struct words *result;

	if (words == NULL)
		return 0;
	tmp.name = name;
	result = (struct words *)bsearch(&tmp, words, tablesize, sizeof(struct words), cmp);
	return (result != NULL) ? 1 : 0;
}

/*----------------------------------------------------------------------*/
/* Parser switch                                                        */
/*----------------------------------------------------------------------*/
/*
 * This is the linkage section of each parsers.
 * If you want to support new language, you must define parser procedure
 * which requires file name as an argument.
 */
struct lang_entry {
	const char *lang_name;
	void (*parser)(const struct parser_param *);	/* parser procedure */
};

struct plugin_entry {
	SLIST_ENTRY(plugin_entry) next;
	lt_dlhandle handle;
	struct lang_entry entry;
};

static SLIST_HEAD(plugin_list, plugin_entry)
	plugin_list = SLIST_HEAD_INITIALIZER(plugin_list);
static char *langmap_saved, *pluginspec_saved;

/*
 * load_plugin_parser: Load plug-in parsers.
 *
 *	i)	pluginspec	described below
 *
 * Syntax:
 *   <pluginspec> ::= <map> | <map>","<pluginspec>
 *   <map>        ::= <language name>":"<shared object path>
 *                  | <language name>":"<shared object path>":"<function name>
 */
static void
load_plugin_parser(const char *pluginspec)
{
	char *p, *q;
	const char *dso_name, *func;
	struct plugin_entry *pent;

	pluginspec_saved = check_strdup(pluginspec);
	if (lt_dlinit() != 0)
		die("cannot initialize libltdl.");
	p = pluginspec_saved;
	while (*p != '\0') {
		pent = check_malloc(sizeof(*pent));
		pent->entry.lang_name = p;
		p = strchr(p, ':');
		if (p == NULL)
			die_with_code(2, "syntax error in pluginspec '%s'.", pluginspec);
		*p++ = '\0';
		if (*p == '\0')
			die_with_code(2, "syntax error in pluginspec '%s'.", pluginspec);
		dso_name = p;
		p = strchr(p, ',');
		if (p != NULL)
			*p++ = '\0';
		q = strchr(dso_name, ':');
		if (q == NULL) {
			func = "parser";
		} else {
			*q++ = '\0';
			if (*q == '\0')
				die_with_code(2, "syntax error in pluginspec '%s'.", pluginspec);
			func = q;
		}
		pent->handle = lt_dlopen(dso_name);
		if (pent->handle == NULL)
			die_with_code(2, "cannot open shared object '%s'.", dso_name);
		pent->entry.parser = lt_dlsym(pent->handle, func);
		if (pent->entry.parser == NULL)
			die_with_code(2, "cannot find symbol '%s' in '%s'.", func, dso_name);
		SLIST_INSERT_HEAD(&plugin_list, pent, next);
		if (p == NULL)
			break;
	}
}

/*
 * unload_plugin_parser: Unload plug-in parsers.
 */
static void
unload_plugin_parser(void)
{
	struct plugin_entry *pent;

	if (pluginspec_saved == NULL)
		return;
	while (!SLIST_EMPTY(&plugin_list)) {
		pent = SLIST_FIRST(&plugin_list);
		lt_dlclose(pent->handle);
		SLIST_REMOVE_HEAD(&plugin_list, next);
		free(pent);
	}
	lt_dlexit();
	free(pluginspec_saved);
}

/*
 * The first entry is default language.
 */
static const struct lang_entry lang_switch[] = {
	/* lang_name    parser_proc	*/
	{"c",		C},			/* DEFAULT */
	{"yacc",	yacc},
	{"cpp",		Cpp},
	{"java",	java},
	{"php",		php},
	{"asm",		assembly}
};
#define DEFAULT_ENTRY &lang_switch[0]
/*
 * get language entry.
 *
 *      i)      lang    language name (NULL means 'not specified'.)
 *      r)              language entry
 */
static const struct lang_entry *
get_lang_entry(const char *lang)
{
	int i, size = sizeof(lang_switch) / sizeof(struct lang_entry);
	struct plugin_entry *pent;

	/*
	 * if language not specified, it assumes default language.
	 */
	if (lang == NULL)
		return DEFAULT_ENTRY;
	SLIST_FOREACH(pent, &plugin_list, next) {
		if (strcmp(lang, pent->entry.lang_name) == 0)
			return &pent->entry;
	}
	for (i = 0; i < size; i++)
		if (!strcmp(lang, lang_switch[i].lang_name))
			return &lang_switch[i];
	/*
	 * if specified language not found, it assumes default language.
	 */
	return DEFAULT_ENTRY;
}

/*
 * Usage:
 * [gtags.conf]
 * +----------------------------
 * |...
 * |gtags_parser=<pluginspec>
 * |langmap=<langmap>
 *
 * 1. Load langmap and pluginspec, and initialize parsers.
 *
 *	parser_init(langmap, plugin_parser);
 *
 * 2. Execute parsers
 *
 *	parse_file(...);
 *
 * 3. Unload parsers.
 *
 *	parser_exit();
 */
/*
 * parser_init: load langmap and shared libraries.
 *
 *	i)	langmap		the value of langmap=<langmap>
 *	i)	pluginspec	the value of gtags_parser=<pluginspec>
 */
void
parser_init(const char *langmap, const char *pluginspec)
{
	/* setup language mapping. */
	if (langmap == NULL)
		langmap = DEFAULTLANGMAP;
	setup_langmap(langmap);
	langmap_saved = check_strdup(langmap);

	/* load shared objects. */
	if (pluginspec != NULL)
		load_plugin_parser(pluginspec);

	/*
	 * This is a hack for FreeBSD.
	 * In the near future, it will be removed.
	 */
	if (test("r", NOTFUNCTION))
		load_notfunction(NOTFUNCTION);
#ifdef __DJGPP__
	else if (test("r", DOS_NOTFUNCTION))
		load_notfunction(DOS_NOTFUNCTION);
#endif
}

/*
 * parser_exit: unload shared libraries.
 */
void
parser_exit(void)
{
	unload_plugin_parser();
	free(langmap_saved);
}

/*
 * parse_file: select and execute a parser.
 *
 *	i)	path	path name
 *	i)	flags	PARSER_WARNING: print warning messages
 *	i)	put	callback routine
 *			each parser use this routine for output
 *	i)	arg	argument for callback routine
 */
void
parse_file(const char *path, int flags, PARSER_CALLBACK put, void *arg)
{
	const char *lang, *suffix;
	const struct lang_entry *ent;
	struct parser_param param;

	/* get suffix of the path. */
	suffix = locatestring(path, ".", MATCH_LAST);
	if (suffix == NULL)
		return;
	lang = decide_lang(suffix);
	if (lang == NULL)
		return;
	if (flags & PARSER_VERBOSE)
		fprintf(stderr, "suffix '%s' assumed language '%s'.\n", suffix, lang);
	/*
	 * Select parser.
	 * If lang == NULL then default parser is selected.
	 */
	ent = get_lang_entry(lang);
	/*
	 * call language specific parser.
	 */
	param.size = sizeof(param);
	param.flags = flags;
	param.file = path;
	param.put = put;
	param.arg = arg;
	param.isnotfunction = isnotfunction;
	param.langmap = langmap_saved;
	param.die = die;
	ent->parser(&param);
}

void
dbg_print(int level, const char *s)
{
	fprintf(stderr, "[%04d]", lineno);
	for (; level > 0; level--)
		fprintf(stderr, "    ");
	fprintf(stderr, "%s\n", s);
}
