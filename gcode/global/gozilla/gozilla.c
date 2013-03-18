/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2006
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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>
/*
 * Don't remove the following code which seems meaningless.
 * Since WIN32 has another SLIST_ENTRY, we removed the definition
 * so as not to cause the conflict.
 */
#ifdef SLIST_ENTRY
#undef SLIST_ENTRY
#endif
#endif

#include "global.h"
#include "regex.h"
#include "const.h"

static void usage(void);
static void help(void);

const char *gozillarc = ".gozillarc";
#ifdef __DJGPP__
const char *dos_gozillarc = "_gozillarc";
#endif
STRHASH *sh;

static void load_alias(void);
static const char *alias(const char *);
int main(int, char **);
void getdefinitionURL(const char *, STRBUF *);
void getURL(const char *, STRBUF *);
int isprotocol(const char *);
int convertpath(const char *, const char *, const char *, STRBUF *);
void makefileurl(const char *, int, STRBUF *);
void show_page_by_url(const char *, const char *);
#ifndef isblank
#define isblank(c)	((c) == ' ' || (c) == '\t')
#endif

char cwd[MAXPATHLEN];
char root[MAXPATHLEN];
char dbpath[MAXPATHLEN];

int bflag;
int pflag;
int qflag;
int Cflag;
int vflag;
int show_version;
int linenumber = 0;
int debug;

static void
usage(void)
{
	if (!qflag)
		fputs(usage_const, stderr);
	exit(2);
}
static void
help(void)
{
	fputs(usage_const, stdout);
	fputs(help_const, stdout);
	exit(0);
}

/*
 * load_alias: load alias value.
 *
 * [$HOME/.gozillarc]
 * +-----------------------
 * |a:http://www.gnu.org
 * |f = file:/usr/share/xxx.html
 * |www	http://www.xxx.yyy/
 */
static void
load_alias(void)
{
	FILE *ip;
	STRBUF *sb = strbuf_open(0);
	char *p;
	int flag = STRBUF_NOCRLF;
	struct sh_entry *ent;

	sh = strhash_open(10);
	if (!(p = get_home_directory()))
		goto end;
	if (!test("r", makepath(p, gozillarc, NULL)))
#ifdef __DJGPP__
		if (!test("r", makepath(p, dos_gozillarc, NULL)))
#endif
			goto end;
	if (!(ip = fopen(makepath(p, gozillarc, NULL), "r")))
#ifdef __DJGPP__
		if (!(ip = fopen(makepath(p, dos_gozillarc, NULL), "r")))
#endif
			goto end;
	while ((p = strbuf_fgets(sb, ip, flag)) != NULL) {
		char *name, *value;

		flag &= ~STRBUF_APPEND;
		if (*p == '#')
			continue;
		if (strbuf_unputc(sb, '\\')) {
			flag |= STRBUF_APPEND;
			continue;
		}
		while (*p && isblank(*p))	/* skip spaces */
			p++;
		name = p;
		while (*p && isalnum(*p))	/* get name */
			p++;
		*p++ = 0;
		while (*p && isblank(*p))	/* skip spaces */
			p++;
		if (*p == '=' || *p == ':') {
			p++;
			while (*p && isblank(*p))/* skip spaces */
				p++;
		}
		value = p;
		while (*p && !isblank(*p))	/* get value */
			p++;
		*p = 0;
		ent = strhash_assign(sh, name, 1);
		if (ent->value)
			(void)free(ent->value);
		ent->value = check_strdup(value);
	}
	fclose(ip);
end:
	strbuf_close(sb);
}
/*
 * alias: get alias value.
 *
 *	i)	alias_name	alias name
 *	r)			its value
 */
static const char *
alias(const char *alias_name)
{
	struct sh_entry *ent = strhash_assign(sh, alias_name, 0);
	return ent ? ent->value : NULL;
}

/*
 * locate_HTMLdir: locate HTML directory made by htags(1).
 *
 *	r)		HTML directory
 */
static const char *
locate_HTMLdir(void)
{
	static char htmldir[MAXPATHLEN];

	if (test("d", makepath(dbpath, "HTML", NULL)))
		strlimcpy(htmldir, makepath(dbpath, "HTML", NULL), sizeof(htmldir));
	else if (test("d", makepath(root, "HTML", NULL)))
		strlimcpy(htmldir, makepath(root, "HTML", NULL), sizeof(htmldir));
	else if (test("d", makepath(root, "html/HTML", NULL)))
		/* Doxygen makes HTML in doxygen's html directory. */
		strlimcpy(htmldir, makepath(root, "html/HTML", NULL), sizeof(htmldir));
	else
		die("hypertext not found. See htags(1).");
	if (vflag)
		fprintf(stdout, "HTML directory '%s'.\n", htmldir);
	return (const char *)htmldir;
}
int
main(int argc, char **argv)
{
	char c;
	const char *p, *browser = NULL, *definition = NULL;
	STRBUF *arg = strbuf_open(0);
	STRBUF *URL = strbuf_open(0);

	while (--argc > 0 && ((c = (++argv)[0][0]) == '-' || c == '+')) {
		if (argv[0][1] == '-') {
			if (!strcmp("--help", argv[0]))
				help();
			else if (!strcmp("--version", argv[0]))
				show_version++;
			else if (!strcmp("--quiet", argv[0])) {
				qflag++;
				vflag = 0;
			} else if (!strcmp("--verbose", argv[0])) {
				vflag++;
				qflag = 0;
			} else
				usage();
			continue;
		}
		if (c == '+') {
			linenumber = atoi(argv[0] + 1);
			continue;
		}
		p = argv[0] + 1;
		switch (*p) {
		case 'b':
			browser = argv[1];
			--argc; ++argv;
			break;
		case 'd':
			definition = argv[1];
			--argc; ++argv;
			break;
		case 'p':
			pflag++;
			break;
		case 'q':
			qflag++;
			setquiet();
			break;
		case 'v':
			vflag++;
			break;
		default:
			usage();
		}
	}
	if (show_version)
		version(progname, vflag);
	/*
	 * Load aliases from .gozillarc.
	 */
	load_alias();

	/*
	 * Decide browser.
	 */
	if (!browser && getenv("BROWSER"))
		browser = getenv("BROWSER");
	if (!browser && alias("BROWSER"))
		browser = alias("BROWSER");
	if (!browser)
		browser = "mozilla";

	/*
	 * Replace alias name.
	 */
	if (definition == NULL) {
		if (argc == 0)
			usage();
		strbuf_puts(arg, argv[0]);
		/*
		 * Replace with alias value.
		 */
		if ((p = alias(strbuf_value(arg))) != NULL) {
			strbuf_reset(arg);
			strbuf_puts(arg, p);
		}
	}
	/*
	 * Get URL.
	 */
	if (!definition && isprotocol(strbuf_value(arg))) {
		strbuf_puts(URL, strbuf_value(arg));
	} else {
		getdbpath(cwd, root, dbpath, 0);
		if (definition)
			getdefinitionURL(definition, URL);
		else
			getURL(strbuf_value(arg), URL);
	}
	if (pflag) {
		fprintf(stdout, "%s\n", strbuf_value(URL));
		if (vflag)
			fprintf(stdout, "using browser '%s'.\n", browser);
		exit(0);
	}
	/*
	 * Show URL's page.
	 */
	show_page_by_url(browser, strbuf_value(URL));
	exit(0);
}

/*
 * getdefinitionURL: get URL includes specified definition.
 *
 *	i)	arg	definition name
 *	o)	URL	URL begin with 'file:'
 */
void
getdefinitionURL(const char *arg, STRBUF *URL)
{
	FILE *fp;
	char *p;
	SPLIT ptable;
	int status = -1;
	STRBUF *sb = strbuf_open(0);
	const char *htmldir = locate_HTMLdir();
	const char *path = makepath(htmldir, "MAP", NULL);

	if (!test("f", path))
		die("'%s' not found. Please reconstruct hypertext using the latest htags(1).", path);
	fp = fopen(path, "r");
	if (!fp)
		die("cannot open '%s'.", path);
	while ((p = strbuf_fgets(sb, fp, STRBUF_NOCRLF)) != NULL) {
		if (split(p, 2, &ptable) != 2)
			die("illegal format.");
		if (!strcmp(arg, ptable.part[0].start)) {
			status = 0;
			break;
		}
	}
	fclose(fp);
	if (status == -1)
		die("definition %s not found.", arg);
	strbuf_reset(URL);
	/*
	 * convert path into URL.
	 */
	makefileurl(makepath(htmldir, ptable.part[1].start, NULL), 0, URL);
	recover(&ptable);
	strbuf_close(sb);
}
/*
 * getURL: get URL of the specified file.
 *
 *	i)	file	file name
 *	o)	URL	URL begin with 'file:'
 */
void
getURL(const char *file, STRBUF *URL)
{
	char *p;
	char buf[MAXPATHLEN];
	STRBUF *sb = strbuf_open(0);
	const char *htmldir = locate_HTMLdir();

	if (!test("f", file) && !test("d", file))
		die("path '%s' not found.", file);
	p = normalize(file, get_root_with_slash(), cwd, buf, sizeof(buf));
	if (convertpath(dbpath, htmldir, p, sb) == 0)
		makefileurl(strbuf_value(sb), linenumber, URL);
	else
		makefileurl(realpath(file, buf), 0, URL);
	strbuf_close(sb);
}
/*
 * isprotocol: return 1 if url has a procotol.
 *
 *	i)	url	URL
 *	r)		1: protocol, 0: file
 */
int
isprotocol(const char *url)
{
	const char *p;

	if (locatestring(url, "file:", MATCH_AT_FIRST))
		return 1;
	/*
	 * protocol's style is like http://xxx.
	 */
	for (p = url; *p && *p != ':'; p++)
		if (!isalnum(*p))
			return 0;
	if (!*p)
		return 0;
	if (*p++ == ':' && *p++ == '/' && *p == '/')
		return 1;
	return 0;
}
/*
 * convertpath: convert source file into hypertext path.
 *
 *	i)	dbpath	dbpath
 *	i)	htmldir	HTML directory made by htags(1)
 *	i)	path	source file path
 *	o)	sb	string buffer
 *	r)		0: normal, -1: error
 */
int
convertpath(const char *dbpath, const char *htmldir, const char *path, STRBUF *sb)
{
	static const char *suffix[] = {".html", ".htm"};
	static const char *gz = ".gz";
	int i, lim = sizeof(suffix)/sizeof(char *);
	const char *p;

	strbuf_reset(sb);
	strbuf_puts(sb, htmldir);
	strbuf_puts(sb, "/S/");
	/*
	 * new style.
	 */
	if (gpath_open(dbpath, 0) == 0) {
		int tag1 = strbuf_getlen(sb);

		p = gpath_path2fid(path, NULL);
		if (p == NULL) {
			gpath_close();
			return -1;
		}
		gpath_close();
		strbuf_puts(sb, p);
		for (i = 0; i < lim; i++) {
			int tag2 = strbuf_getlen(sb);
			strbuf_puts(sb, suffix[i]);
			if (test("f", strbuf_value(sb)))
				return 0;
			strbuf_puts(sb, gz);
			if (test("f", strbuf_value(sb)))
				return 0;
			strbuf_setlen(sb, tag2);
		}
		strbuf_setlen(sb, tag1);
	}
	/*
	 * old style.
	 */
	for (p = path + 1; *p; p++)
		strbuf_putc(sb, (*p == '/') ? ' ' : *p);
	for (i = 0; i < lim; i++) {
		int tag = strbuf_getlen(sb);
		strbuf_puts(sb, suffix[i]);
		if (test("f", strbuf_value(sb)))
			return 0;
		strbuf_puts(sb, gz);
		if (test("f", strbuf_value(sb)))
			return 0;
		strbuf_setlen(sb, tag);
	}
	return -1;
}
/*
 * makefileurl: make url which start with 'file:'.
 *
 *	i)	path	path name (absolute)
 *	i)	line	!=0: line number
 *	o)	url	URL
 *
 * makefileurl('/dir/a.html', 10)   => 'file:///dir/a.html#L10'
 *
 * (Windows32 environment)
 * makefileurl('c:/dir/a.html', 10) => 'file://c|/dir/a.html#L10'
 */
void
makefileurl(const char *path, int line, STRBUF *url)
{
	strbuf_puts(url, "file://");
#if _WIN32 || __DJGPP__
	/*
	 * copy drive name. (c: -> c|)
	 */
	if (isalpha(*path) && *(path+1) == ':') {
		strbuf_putc(url, *path);
		strbuf_putc(url, '|');
		path += 2;
	}
#endif
	strbuf_puts(url, path);
	if (line) {
		strbuf_puts(url, "#L");
		strbuf_putn(url, line);
	}
}
/*
 * show_page_by_url: show page by url
 *
 *	i)	browser browser name
 *	i)	url	URL
 */
#if defined(_WIN32)
/* Windows32 version */
void
show_page_by_url(const char *browser, const char *url)
{
	if (ShellExecute(NULL, NULL, browser, url, NULL, SW_SHOWNORMAL) <= (HINSTANCE)32)
		die("Cannot load %s (error = 0x%04x).", browser, GetLastError());
}
#elif defined(__DJGPP__)
/* DJGPP version */
void
show_page_by_url(const char *browser, const char *url)
{
	char com[MAXFILLEN];
	char *path;

	/*
	 * assume a Windows browser if it's not on the path.
	 */
	if (!(path = usable(browser))) {
		/*
		 * START is an internal command in XP, external in 9X.
		 */
		if (!(path = usable("start")))
			path = "cmd /c start";
		snprintf(com, sizeof(com), "%s %s \"%s\"", path, browser, url);
	} else {
		snprintf(com, sizeof(com), "%s \"%s\"", path, url);
	}
	system(com);
}
#else
/* UNIX version */
void
show_page_by_url(const char *browser, const char *url)
{
	char com[1024];

	/*
	 * Browsers which have openURL() command.
	 */
	if (locatestring(browser, "mozilla", MATCH_AT_LAST) ||
	    locatestring(browser, "netscape", MATCH_AT_LAST) ||
	    locatestring(browser, "netscape-remote", MATCH_AT_LAST))
	{
		char *path;

		if (!(path = usable(browser)))
			die("%s not found in your path.", browser);
		snprintf(com, sizeof(com), "%s -remote \"openURL(%s)\"", path, url);
	}
	/*
	 * Generic browser.
	 */
	else {
		snprintf(com, sizeof(com), "%s \"%s\"", browser, url);
	}
	system(com);
}
#endif
