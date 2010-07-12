/*
 * Copyright (c) 2006, 2008
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
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "getopt.h"

#include "global.h"
#include "const.h"

/* static void usage(void); */
static void help(void);
static void check_dbpath(void);
static void get_global_path(void);
int main(int, char **);
static char *get_line(void);
static void update(void);
static void print_case_distinction(void);
static char *include_pattern(const char *);
static void command_help(void);
static void command_loop(void);
static int execute_command(STRBUF *, const int, const int, const char *);
static void search(int, char *);

int show_version;
int show_help;
int qflag;
int vflag;

#define NA	-1
#define FROM_HERE -2

/*
static void
usage(void)
{
	fputs(usage_const, stderr);
	exit(2);
}
*/
static void
help(void)
{
	fputs(usage_const, stdout);
	fputs(help_const, stdout);
	exit(0);
}

static struct option const long_options[] = {
	{"ignore-case", no_argument, NULL, 'C'},
	{"quiet", no_argument, NULL, 'q'},
	{"verbose", no_argument, NULL, 'v'},
	{"version", no_argument, &show_version, 1},
	{"help", no_argument, &show_help, 1},
	{ 0 }
};

char global_path[MAXFILLEN];
char *context;
int ignore_case;

/*
 * Check whether or not GTAGS exist.
 *
 * If GTAGS not found, this function abort with a message.
 */
static void
check_dbpath(void)
{
	char cwd[MAXPATHLEN];
	char root[MAXPATHLEN];
	char dbpath[MAXPATHLEN];

	getdbpath(cwd, root, dbpath, vflag);
}
/*
 * Get global(1)'s path.
 */
static void
get_global_path(void)
{
	const char *p;

	if (!(p = usable("global")))
		die("global command required but not found.");
	strlimcpy(global_path, p, sizeof(global_path));
}

int
main(int argc, char **argv)
{
	const char *av = NULL;
	int optchar;
	int option_index = 0;

	while ((optchar = getopt_long(argc, argv, "bCcdeF:f:hI:i:kLlp:qRs:TUuVv0123456789", long_options, &option_index)) != EOF) {
		switch (optchar) {
		case 0:
			break;
		case 'C':
			ignore_case = 1;
			break;
		case 'q':
			qflag++;
			break;
		case 'v':
			vflag++;
			break;
		default:
			break;
		}
	}
	if (show_help)
		help();
	argc -= optind;
	argv += optind;
	if (!av)
		av = (argc > 0) ? *argv : NULL;
	if (show_version)
		version(av, vflag);
	/*
	 * Get global(1)'s path. If not found, abort with a message.
	 */
	get_global_path();
	/*
	 * Check dbpath. If dbpath not found, abort with a message.
	 */
	check_dbpath();
	/*
	 * Command loop
	 */
	command_loop();
	return 0;
}
/*
 * Read line with a prompt.
 *
 * This is part of cscope protocol.
 */
static char *
get_line(void)
{
	STATIC_STRBUF(sb);

	/* Prompt */
	fputs(">> ", stdout);
	fflush(stdout);
	if (strbuf_fgets(sb, stdin, STRBUF_NOCRLF) == NULL)
		return NULL;
	return strbuf_value(sb);
}
/*
 * Update tag files.
 */
static void
update(void)
{
	STATIC_STRBUF(command);

	strbuf_clear(command);
	strbuf_sprintf(command, "%s -u", global_path);
	if (vflag) {
		strbuf_putc(command, 'v');
		fprintf(stderr, "gscope: %s\n", strbuf_value(command));
	}
	system(strbuf_value(command));
}
/*
 * print verbose message about case distinction.
 */
static void
print_case_distinction(void)
{
	if (vflag) {
		const char *msg = ignore_case ? 
			"ignore letter case when searching" :
			"use letter case when searching";
		fprintf(stderr, "gscope: %s\n", msg);
	}
}
/*
 * get pattern which match to #include lines.
 *
 *	i)	arg	include file name
 *	r)		pattern which match to #include lines
 */
static char *
include_pattern(const char *arg)
{
#if defined(_WIN32)
#define INCLUDE "^[ \t]*#[ \t]*include[ \t].*[\\\"\"</\\]%s[\\\"\">]"
#elif defined(__DJGPP__)
#define INCLUDE "^[ \t]*#[ \t]*include[ \t].*[\"</\\]%s[\">]"
#else
#define INCLUDE "^[ \t]*#[ \t]*include[ \t].*[\"</]%s[\">]"
#endif
	STATIC_STRBUF(pat);

	strbuf_clear(pat);
	strbuf_sprintf(pat, INCLUDE, quote_string(arg));
	return strbuf_value(pat);
}
/*
 * show help message.
 */
static void
command_help(void)
{
	fprintf(stdout, "0<arg>: Find this C symbol\n");
	fprintf(stdout, "1<arg>: Find this definition\n");
	fprintf(stdout, "2<arg>: Find functions called by this function\n");
	fprintf(stdout, "        (Not implemented yet.)\n");
	fprintf(stdout, "3<arg>: Find functions calling this function\n");
	fprintf(stdout, "4<arg>: Find this text string\n");
	fprintf(stdout, "6<arg>: Find this egrep pattern\n");
	fprintf(stdout, "7<arg>: Find this file\n");
	fprintf(stdout, "8<arg>: Find files #including this file\n");
	fprintf(stdout, "c: Toggle ignore/use letter case\n");
	fprintf(stdout, "r: Rebuild the database\n");
	fprintf(stdout, "q: Quit the session\n");
	fprintf(stdout, "h: Show help\n");
}
/*
 * command loop.
 *
 * This is the main body of gtags-cscope.
 */
void
command_loop(void)
{
	int com = 0;
	char *line;

	print_case_distinction();
	while ((line = get_line()) != NULL) {
		switch (com = *line++) {	
		/*
		 * Control command
		 */
		case 'c':		/* c: ignore case or not */
			ignore_case ^= 1;
			print_case_distinction();
			break;
		case 'q':		/* q: quit the program */
			return;
		case 'r':		/* r: update tag file */
			update();
			break;
		case 'h':		/* h: show help */
			command_help();
			break;
		/*
		 * Search command
		 */
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '6':
		case '7':
		case '8':
			search(com, line);
			break;
		default:
			fputs("cscope: 0 lines\n", stdout);
			fflush(stdout);
			break;
		}
	}
}
/*
 * Execute global(1) and write the output to the 'sb' string buffer.
 *
 *	o)	sb	output
 *	i)	com	cscope command (0-8)
 *	i)	opt	option for global(1)
 *	i)	arg	argument for global(1)
 *	r)		number of output
 */
static int
execute_command(STRBUF *sb, const int com, const int opt, const char *arg)
{
#ifdef _WIN32
#define QUOTE '"'
#else
#define QUOTE '\''
#endif
	STATIC_STRBUF(command);
	STATIC_STRBUF(ib);
	FILE *ip;
	int count = 0;

	strbuf_clear(command);
	strbuf_puts(command, global_path);
	strbuf_puts(command, " --result=cscope");
	if (opt == FROM_HERE) {
		strbuf_puts(command, " --from-here=");
		strbuf_puts(command, context);
	} else if (opt) {
		strbuf_puts(command, " -");
		strbuf_putc(command, opt);
	}
	if (ignore_case)
		strbuf_puts(command, " --ignore-case");
	strbuf_putc(command, ' ');
	strbuf_putc(command, QUOTE);
	strbuf_puts(command, arg);
	strbuf_putc(command, QUOTE);
	if (!(ip = popen(strbuf_value(command), "r")))
		die("cannot execute '%s'.", strbuf_value(command));
	if (vflag)
		fprintf(stderr, "gscope: %s\n", strbuf_value(command));
	/*
	 * Copy records with little modification.
	 */
	strbuf_clear(ib);
	while (strbuf_fgets(ib, ip, 0)) {
		count++;
		if (opt == 0) {
			strbuf_puts(sb, strbuf_value(ib));
		} else {
			char *p = strbuf_value(ib);

			/* path name */
			while (*p && *p != ' ')
				strbuf_putc(sb, *p++);
			if (*p != ' ')
				die("illegal cscope format. (%s)", strbuf_value(ib));
			strbuf_putc(sb, *p++);

			/* replace pattern with "<unknown>" or "<global>" */
			while (*p && *p != ' ')
				p++;
			if (*p != ' ')
				die("illegal cscope format. (%s)", strbuf_value(ib));
			if (com == '8')
				strbuf_puts(sb, "<global>");
			else
				strbuf_puts(sb, "<unknown>");
			strbuf_putc(sb, *p++);

			/* line number */
			while (*p && *p != ' ')
				strbuf_putc(sb, *p++);
			if (*p != ' ')
				die("illegal cscope format. (%s)", strbuf_value(ib));
			strbuf_putc(sb, *p++);

			/* line image */
			if (*p == '\n')
				strbuf_puts(sb, "<unknown>\n");
			else
				strbuf_puts(sb, p);
		}
	}
	if (pclose(ip) < 0)
		die("terminated abnormally.");
	return count;
}
/*
 * Execute retrieval command.
 *
 *	i)	command		0: Find this C symbol
 *				1: Find this definition
 *				2: Find functions called by this function (not supported)
 *				3: Find functions calling this function
 *				4: Find this text string
 *				6: Find this egrep pattern
 *				7: Find this file
 *				8: Find files #including this file
 *	i)	arg		argument
 *
 * Unsupported command prints "cscope: 0 lines\n".
 */
static void
search(int com, char *arg)
{
	static STRBUF *sb;
	char buf[1024], *p;
	int count = 0, opt = 0;

	if (sb == NULL)
		sb = strbuf_open(1024 * 1024);
	else
		strbuf_reset(sb);
	/*
	 * Convert from cscope command to global command.
	 */
	switch (com) {
	case '0':		/* Find this C symbol */
	case '1':		/* Find this definition */
		break;
	case '2':		/* Find functions called by this function */
		/*
		 * <symbol>:<line number>:<path>
		 */
		for (p = arg; *p && *p != ':'; p++)
			;
		*p++ = '\0';
		context = p;
		opt = FROM_HERE;
		break;
	case '3':		/* Find functions calling this function */
		opt = 'r';
		break;
	case '4':		/* Find this text string */
		opt = 'g';
		strlimcpy(buf, quote_string(arg), sizeof(buf));
		arg = buf;
		break;
	case '5':		/* Change this text string */
		opt = NA;
		break;
	case '6':		/* Find this egrep pattern */
		opt = 'g';
		break;
	case '7':		/* Find this file */
		opt = 'P';
		break;
	case '8':		/* Find files #including this file */
		opt = 'g';
		arg = include_pattern(arg);
		break;
	}
	/*
	 * Execute global(1).
	 */
	if (opt == NA) {
		fprintf(stdout, "cscope: 0 lines\n");
		return;
	}
	if (com == '0') {
		count += execute_command(sb, com, 0, arg);
		count += execute_command(sb, com, (count > 0) ? 'r' : 's', arg);
	} else {
		count += execute_command(sb, com, opt, arg);
	}
	/*
	 * Output format:
	 * cscope: <n> lines
	 * ******************		... 1
	 * ******************		... 2
	 * ...
	 * ******************		... n
	 * 
	 * Example:
	 * cscope: 3 lines
	 * global/global.c main 158 main(int argc, char **argv)
	 * gozilla/gozilla.c main 155 main(int argc, char **argv)
	 * gscope/gscope.c main 108 main(int argc, char **argv)
	 */
	fprintf(stdout, "cscope: %d lines\n", count);
	if (count > 0)
		fwrite(strbuf_value(sb), 1, strbuf_getlen(sb), stdout);
	fflush(stdout);
}
