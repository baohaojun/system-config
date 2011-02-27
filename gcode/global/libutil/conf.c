/*
 * Copyright (c) 1998, 1999, 2000, 2001, 2002, 2005, 2010
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
#include <assert.h>
#include <ctype.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "gparam.h"
#include "checkalloc.h"
#include "conf.h"
#include "die.h"
#include "env.h"
#include "langmap.h"
#include "locatestring.h"
#include "makepath.h"
#include "path.h"
#include "strbuf.h"
#include "strlimcpy.h"
#include "strmake.h"
#include "test.h"
#include "usable.h"

static FILE *fp;
static STRBUF *ib;
static char *confline;
/*
 * 8 level nested tc= or include= is allowed.
 */
static int allowed_nest_level = 8;
static int opened;

static void trim(char *);
static const char *readrecord(const char *);
static void includelabel(STRBUF *, const char *, int);

#ifndef isblank
#define isblank(c)	((c) == ' ' || (c) == '\t')
#endif

/*
 * trim: trim string.
 *
 * : var1=a b :
 *	|
 *	v
 * :var1=a b :
 */
static void
trim(char *l)
{
	char *f, *b;
	int colon = 0;

	/*
	 * delete blanks.
	 */
	for (f = b = l; *f; f++) {
		if (colon && isblank(*f))
			continue;
		colon = 0;
		if ((*b++ = *f) == ':')
			colon = 1;
	}
	*b = 0;
	/*
	 * delete duplicate semi colons.
	 */
	for (f = b = l; *f;) {
		if ((*b++ = *f++) == ':') {
			while (*f == ':')
				f++;
		}
	}
	*b = 0;
}
/*
 * readrecord: read recoed indexed by label.
 *
 *	i)	label	label in config file
 *	r)		record
 *
 * Jobs:
 * o skip comment.
 * o append following line.
 * o format check.
 */
static const char *
readrecord(const char *label)
{
	char *p;
	int flag = STRBUF_NOCRLF;
	int count = 0;

	rewind(fp);
	while ((p = strbuf_fgets(ib, fp, flag)) != NULL) {
		count++;
		/*
		 * ignore \<new line>.
		 */
		flag &= ~STRBUF_APPEND;
		if (*p == '#' || *p == '\0')
			continue;
		if (strbuf_unputc(ib, '\\')) {
			flag |= STRBUF_APPEND;
			continue;
		}
		trim(p);
		for (;;) {
			const char *candidate;
			/*
			 * pick up candidate.
			 */
			if ((candidate = strmake(p, "|:")) == NULL)
				die("invalid config file format (line %d).", count);
			if (!strcmp(label, candidate)) {
				if (!(p = locatestring(p, ":", MATCH_FIRST)))
					die("invalid config file format (line %d).", count);
				return check_strdup(p);
			}
			/*
			 * locate next candidate.
			 */
			p += strlen(candidate);
			if (*p == ':')
				break;
			else if (*p == '|')
				p++;
			else
				die("invalid config file format (line %d).", count);
		}
	}
	/*
	 * config line not found.
	 */
	return NULL;
}
/*
 * includelabel: procedure for tc= (or include=)
 *
 *	o)	sb	string buffer
 *	i)	label	record label
 *	i)	level	nest level for check
 */
static void
includelabel(STRBUF *sb, const char *label, int	level)
{
	const char *savep, *p, *q;

	if (++level > allowed_nest_level)
		die("nested include= (or tc=) over flow.");
	if (!(savep = p = readrecord(label)))
		die("label '%s' not found.", label);
	while ((q = locatestring(p, ":include=", MATCH_FIRST)) || (q = locatestring(p, ":tc=", MATCH_FIRST))) {
		STRBUF *inc = strbuf_open(0);

		strbuf_nputs(sb, p, q - p);
		q = locatestring(q, "=", MATCH_FIRST) + 1;
		for (; *q && *q != ':'; q++)
			strbuf_putc(inc, *q);
		includelabel(sb, strbuf_value(inc), level);
		p = q;
		strbuf_close(inc);
	}
	strbuf_puts(sb, p);
	free((void *)savep);
}
/*
 * configpath: get path of configuration file.
 */
static char *
configpath(void)
{
	STATIC_STRBUF(sb);
	const char *p;

	strbuf_clear(sb);
	/*
	 * at first, check environment variable GTAGSCONF.
	 */
	if (getenv("GTAGSCONF") != NULL)
		strbuf_puts(sb, getenv("GTAGSCONF"));
	/*
	 * if GTAGSCONF not set then check standard config files.
	 */
	else if ((p = get_home_directory()) && test("r", makepath(p, GTAGSRC, NULL)))
		strbuf_puts(sb, makepath(p, GTAGSRC, NULL));
#ifdef __DJGPP__
	else if ((p = get_home_directory()) && test("r", makepath(p, DOS_GTAGSRC, NULL)))
		strbuf_puts(sb, makepath(p, DOS_GTAGSRC, NULL));
#endif
	else if (test("r", GTAGSCONF))
		strbuf_puts(sb, GTAGSCONF);
	else if (test("r", OLD_GTAGSCONF))
		strbuf_puts(sb, OLD_GTAGSCONF);
	else if (test("r", DEBIANCONF))
		strbuf_puts(sb, DEBIANCONF);
	else if (test("r", OLD_DEBIANCONF))
		strbuf_puts(sb, OLD_DEBIANCONF);
	else if (test("r", makepath(SYSCONFDIR, "gtags.conf", NULL)))
		strbuf_puts(sb, makepath(SYSCONFDIR, "gtags.conf", NULL));
	else
		return NULL;
	return strbuf_value(sb);
}
/*
 * openconf: load configuration file.
 *
 *	go)	line	specified entry
 */
void
openconf(void)
{
	STRBUF *sb;
	const char *config;
	extern int vflag;

	assert(opened == 0);
	opened = 1;

	/*
	 * if config file not found then return default value.
	 */
	if (!(config = configpath())) {
		if (vflag)
			fprintf(stderr, " Using default configuration.\n");
		confline = check_strdup("");
	}
	/*
	 * if it is not an absolute path then assumed config value itself.
	 */
	else if (!isabspath(config)) {
		confline = check_strdup(config);
		if (!locatestring(confline, ":", MATCH_FIRST))
			die("GTAGSCONF must be absolute path name.");
	}
	/*
	 * else load value from config file.
	 */
	else {
		const char *label;

		if (test("d", config))
			die("config file '%s' is a directory.", config);
		if (!test("f", config))
			die("config file '%s' not found.", config);
		if (!test("r", config))
			die("config file '%s' is not readable.", config);
		if ((label = getenv("GTAGSLABEL")) == NULL)
			label = "default";
	
		if (!(fp = fopen(config, "r")))
			die("cannot open '%s'.", config);
		if (vflag)
			fprintf(stderr, " Using config file '%s'.\n", config);
		ib = strbuf_open(MAXBUFLEN);
		sb = strbuf_open(0);
		includelabel(sb, label, 0);
		confline = check_strdup(strbuf_value(sb));
		strbuf_close(ib);
		strbuf_close(sb);
		fclose(fp);
	}
	/*
	 * make up required variables.
	 */
	sb = strbuf_open(0);
	strbuf_puts(sb, confline);
	strbuf_unputc(sb, ':');

	if (!getconfs("suffixes", NULL)) {
		STRBUF *tmp = strbuf_open(0);
		const char *langmap = NULL;

		/*
		 * Variable 'suffixes' is obsolete. But it is generated
		 * internally from the value of variable 'langmap'.
		 */
		if (getconfs("langmap", tmp))
			langmap = strbuf_value(tmp);
		else
			langmap = DEFAULTLANGMAP;
		strbuf_puts(sb, ":suffixes=");
		make_suffixes(langmap, sb);
		strbuf_close(tmp);
	}
	if (!getconfs("skip", NULL)) {
		strbuf_puts(sb, ":skip=");
		strbuf_puts(sb, DEFAULTSKIP);
	}
	strbuf_unputc(sb, ':');
	strbuf_putc(sb, ':');
	confline = check_strdup(strbuf_value(sb));
	strbuf_close(sb);
	trim(confline);
	return;
}
/*
 * getconfn: get property number
 *
 *	i)	name	property name
 *	o)	num	value (if not NULL)
 *	r)		1: found, 0: not found
 */
int
getconfn(const char *name, int *num)
{
	const char *p;
	char buf[MAXPROPLEN];

	if (!opened)
		openconf();
	snprintf(buf, sizeof(buf), ":%s#", name);
	if ((p = locatestring(confline, buf, MATCH_FIRST)) != NULL) {
		p += strlen(buf);
		if (num != NULL)
			*num = atoi(p);
		return 1;
	}
	return 0;
}
/*
 * getconfs: get property string
 *
 *	i)	name	property name
 *	o)	sb	string buffer (if not NULL)
 *	r)		1: found, 0: not found
 */
int
getconfs(const char *name, STRBUF *sb)
{
	const char *p;
	char buf[MAXPROPLEN];
	int all = 0;
	int exist = 0;

	if (!opened)
		openconf();
	if (!strcmp(name, "suffixes") || !strcmp(name, "skip")
	 || !strcmp(name, "gtags_parser") || !strcmp(name, "langmap"))
		all = 1;
	snprintf(buf, sizeof(buf), ":%s=", name);
	p = confline;
	while ((p = locatestring(p, buf, MATCH_FIRST)) != NULL) {
		if (exist && sb)
			strbuf_putc(sb, ',');		
		exist = 1;
		for (p += strlen(buf); *p && *p != ':'; p++) {
			if (*p == '\\')	/* quoted character */
				p++;
			if (sb)
				strbuf_putc(sb, *p);
		}
		if (!all)
			break;
	}
	/*
	 * If 'bindir' and 'datadir' are not defined then
	 * return system configuration value.
	 */
	if (!exist) {
		if (!strcmp(name, "bindir")) {
			strbuf_puts(sb, BINDIR);
			exist = 1;
		} else if (!strcmp(name, "datadir")) {
#if defined(_WIN32) && !defined(__CYGWIN__)
			/*
			 * Test if this directory exists, and if not, take the
			 * directory relative to the binary.
			 */
			if (test("d", DATADIR))
				strbuf_puts(sb, DATADIR);
			else {
				const char *name = strrchr(_pgmptr, '\\');
				if (name)
					strbuf_nputs(sb, _pgmptr, name+1 - _pgmptr);
				strbuf_puts(sb, "..\\share");
			}
#else
			strbuf_puts(sb, DATADIR);
#endif
			exist = 1;
		} else if (!strcmp(name, "localstatedir")) {
			strbuf_puts(sb, LOCALSTATEDIR);
			exist = 1;
		} else if (!strcmp(name, "sysconfdir")) {
			strbuf_puts(sb, SYSCONFDIR);
			exist = 1;
		}
	}
	return exist;
}
/*
 * getconfb: get property bool value
 *
 *	i)	name	property name
 *	r)		1: TRUE, 0: FALSE
 */
int
getconfb(const char *name)
{
	char buf[MAXPROPLEN];

	if (!opened)
		openconf();
	snprintf(buf, sizeof(buf), ":%s:", name);
	if (locatestring(confline, buf, MATCH_FIRST) != NULL)
		return 1;
	return 0;
}
/*
 * getconfline: print loaded config entry.
 */
const char *
getconfline(void)
{
	if (!opened)
		openconf();
	return confline;
}
void
closeconf(void)
{
	if (!opened)
		return;
	free(confline);
	confline = NULL;
	opened = 0;
}
