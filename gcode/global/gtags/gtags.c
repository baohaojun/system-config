/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2006, 2008,
 *	2009, 2010 Tama Communications Corporation
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

#include <ctype.h>
#include <utime.h>
#include <signal.h>
#include <stdio.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
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
#include "parser.h"
#include "const.h"

static void usage(void);
static void help(void);
int main(int, char **);
int incremental(const char *, const char *);
void updatetags(const char *, const char *, IDSET *, STRBUF *);
void createtags(const char *, const char *);
int printconf(const char *);

int cflag;					/* compact format */
int iflag;					/* incremental update */
int Iflag;					/* make  idutils index */
int Oflag;					/* use objdir */
int qflag;					/* quiet mode */
int wflag;					/* warning message */
int vflag;					/* verbose mode */
int show_version;
int show_help;
int show_config;
char *gtagsconf;
char *gtagslabel;
int debug;
const char *config_name;
const char *file_list;
const char *dump_target;
char *single_update;
int statistics = STATISTICS_STYLE_NONE;

#define GTAGSFILES "gtags.files"

/*
 * Path filter
 */
int do_path;
int convert_type = PATH_RELATIVE;

int extractmethod;
int total;

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

static struct option const long_options[] = {
	/*
	 * These options have long name and short name.
	 * We throw them to the processing of short options.
	 *
	 * Though the -o(--omit-gsyms) was removed, this code
	 * is left for compatibility.
	 */
	{"compact", no_argument, NULL, 'c'},
	{"dump", required_argument, NULL, 'd'},
	{"file", required_argument, NULL, 'f'},
	{"idutils", no_argument, NULL, 'I'},
	{"incremental", no_argument, NULL, 'i'},
	{"max-args", required_argument, NULL, 'n'},
	{"omit-gsyms", no_argument, NULL, 'o'},		/* removed */
	{"objdir", no_argument, NULL, 'O'},
	{"quiet", no_argument, NULL, 'q'},
	{"verbose", no_argument, NULL, 'v'},
	{"warning", no_argument, NULL, 'w'},

	/*
	 * The following are long name only.
	 */
	/* flag value */
	{"debug", no_argument, &debug, 1},
	{"statistics", no_argument, &statistics, STATISTICS_STYLE_TABLE},
	{"version", no_argument, &show_version, 1},
	{"help", no_argument, &show_help, 1},

	/* accept value */
#define OPT_CONFIG		128
#define OPT_GTAGSCONF		129
#define OPT_GTAGSLABEL		130
#define OPT_PATH		131
#define OPT_SINGLE_UPDATE	132
#define OPT_ENCODE_PATH		133
	{"config", optional_argument, NULL, OPT_CONFIG},
	{"encode-path", required_argument, NULL, OPT_ENCODE_PATH},
	{"gtagsconf", required_argument, NULL, OPT_GTAGSCONF},
	{"gtagslabel", required_argument, NULL, OPT_GTAGSLABEL},
	{"path", required_argument, NULL, OPT_PATH},
	{"single-update", required_argument, NULL, OPT_SINGLE_UPDATE},
	{ 0 }
};

static const char *langmap = DEFAULTLANGMAP;
static const char *gtags_parser;

int
main(int argc, char **argv)
{
	char dbpath[MAXPATHLEN];
	char cwd[MAXPATHLEN];
	STRBUF *sb = strbuf_open(0);
	int optchar;
	int option_index = 0;
	STATISTICS_TIME *tim;

	while ((optchar = getopt_long(argc, argv, "cd:f:iIn:oOqvwse", long_options, &option_index)) != EOF) {
		switch (optchar) {
		case 0:
			/* already flags set */
			break;
		case OPT_CONFIG:
			show_config = 1;
			if (optarg)
				config_name = optarg;
			break;
		case OPT_GTAGSCONF:
			gtagsconf = optarg;
			break;
		case OPT_GTAGSLABEL:
			gtagslabel = optarg;
			break;
		case OPT_PATH:
			do_path = 1;
			if (!strcmp("absolute", optarg))
				convert_type = PATH_ABSOLUTE;
			else if (!strcmp("relative", optarg))
				convert_type = PATH_RELATIVE;
			else if (!strcmp("through", optarg))
				convert_type = PATH_THROUGH;
			else
				die("Unknown path type.");
			break;
		case OPT_SINGLE_UPDATE:
			iflag++;
			single_update = optarg;
			break;
		case OPT_ENCODE_PATH:
			if (strlen(optarg) > 255)
				die("too many encode chars.");
			if (strchr(optarg, '/') || strchr(optarg, '.'))
				die("cannot encode '/' and '.' in the path.");
			set_encode_chars((unsigned char *)optarg);
			break;
		case 'c':
			cflag++;
			break;
		case 'd':
			dump_target = optarg;
			break;
		case 'f':
			file_list = optarg;
			break;
		case 'i':
			iflag++;
			break;
		case 'I':
			Iflag++;
			break;
		case 'o':
			/*
			 * Though the -o(--omit-gsyms) was removed, this code
			 * is left for compatibility.
			 */
			break;
		case 'O':
			Oflag++;
			break;
		case 'q':
			qflag++;
			setquiet();
			break;
		case 'w':
			wflag++;
			break;
		case 'v':
			vflag++;
			break;
		default:
			usage();
			break;
		}
	}
	if (gtagsconf) {
		char path[MAXPATHLEN];

		if (realpath(gtagsconf, path) == NULL)
			die("%s not found.", gtagsconf);
		set_env("GTAGSCONF", path);
	}
	if (gtagslabel) {
		set_env("GTAGSLABEL", gtagslabel);
	}
	if (qflag)
		vflag = 0;
	if (show_version)
		version(NULL, vflag);
	if (show_help)
		help();

	argc -= optind;
        argv += optind;

	/* If dbpath is specified, -O(--objdir) option is ignored. */
	if (argc > 0)
		Oflag = 0;
	if (show_config) {
		if (config_name)
			printconf(config_name);
		else
			fprintf(stdout, "%s\n", getconfline());
		exit(0);
	} else if (do_path) {
		/*
		 * This is the main body of path filter.
		 * This code extract path name from tag line and
		 * replace it with the relative or the absolute path name.
		 *
		 * By default, if we are in src/ directory, the output
		 * should be converted like follws:
		 *
		 * main      10 ./src/main.c  main(argc, argv)\n
		 * main      22 ./libc/func.c   main(argc, argv)\n
		 *		v
		 * main      10 main.c  main(argc, argv)\n
		 * main      22 ../libc/func.c   main(argc, argv)\n
		 *
		 * Similarly, the --path=absolute option specified, then
		 *		v
		 * main      10 /prj/xxx/src/main.c  main(argc, argv)\n
		 * main      22 /prj/xxx/libc/func.c   main(argc, argv)\n
		 */
		STRBUF *ib = strbuf_open(MAXBUFLEN);
		CONVERT *cv;
		char *ctags_x;

		if (argc < 3)
			die("gtags --path: 3 arguments needed.");
		cv = convert_open(convert_type, FORMAT_CTAGS_X, argv[0], argv[1], argv[2], stdout);
		while ((ctags_x = strbuf_fgets(ib, stdin, STRBUF_NOCRLF)) != NULL)
			convert_put(cv, ctags_x);
		convert_close(cv);
		strbuf_close(ib);
		exit(0);
	} else if (dump_target) {
		/*
		 * Dump a tag file.
		 */
		DBOP *dbop = NULL;
		const char *dat = 0;
		int is_gpath = 0;

		if (!test("f", dump_target))
			die("file '%s' not found.", dump_target);
		if ((dbop = dbop_open(dump_target, 0, 0, DBOP_RAW)) == NULL)
			die("file '%s' is not a tag file.", dump_target);
		/*
		 * The file which has a NEXTKEY record is GPATH.
		 */
		if (dbop_get(dbop, NEXTKEY))
			is_gpath = 1;
		for (dat = dbop_first(dbop, NULL, NULL, 0); dat != NULL; dat = dbop_next(dbop)) {
			const char *flag = is_gpath ? dbop_getflag(dbop) : "";

			if (*flag)
				printf("%s\t%s\t%s\n", dbop->lastkey, dat, flag);
			else
				printf("%s\t%s\n", dbop->lastkey, dat);
		}
		dbop_close(dbop);
		exit(0);
	} else if (Iflag) {
		if (!usable("mkid"))
			die("mkid not found.");
	}

	/*
	 * If 'gtags.files' exists, use it as a file list.
	 * If the file_list other than "-" is given, it must be readable file.
	 */
	if (file_list == NULL && test("f", GTAGSFILES))
		file_list = GTAGSFILES;
	if (file_list && strcmp(file_list, "-")) {
		if (test("d", file_list))
			die("'%s' is a directory.", file_list);
		else if (!test("f", file_list))
			die("'%s' not found.", file_list);
		else if (!test("r", file_list))
			die("'%s' is not readable.", file_list);
	}
	/*
	 * Regularize the path name for single updating (--single-update).
	 */
	if (single_update) {
		static char regular_path_name[MAXPATHLEN];
		char *p = single_update;
		
		if (!test("f", p))
			die("'%s' not found.", p);
		if (isabspath(p))
			die("--single-update requires relative path name.");
		if (!(p[0] == '.' && p[1] == '/')) {
			snprintf(regular_path_name, MAXPATHLEN, "./%s", p);
			p = regular_path_name;
		}
		single_update = p;
	}
	if (!getcwd(cwd, MAXPATHLEN))
		die("cannot get current directory.");
	canonpath(cwd);
	/*
	 * Decide directory (dbpath) in which gtags make tag files.
	 *
	 * Gtags create tag files at current directory by default.
	 * If dbpath is specified as an argument then use it.
	 * If the -i option specified and both GTAGS and GRTAGS exists
	 * at one of the candidate directories then gtags use existing
	 * tag files.
	 */
	if (iflag) {
		if (argc > 0)
			realpath(*argv, dbpath);
		else if (!gtagsexist(cwd, dbpath, MAXPATHLEN, vflag))
			strlimcpy(dbpath, cwd, sizeof(dbpath));
	} else {
		if (argc > 0)
			realpath(*argv, dbpath);
		else if (Oflag) {
			char *objdir = getobjdir(cwd, vflag);

			if (objdir == NULL)
				die("Objdir not found.");
			strlimcpy(dbpath, objdir, sizeof(dbpath));
		} else
			strlimcpy(dbpath, cwd, sizeof(dbpath));
	}
	if (iflag && (!test("f", makepath(dbpath, dbname(GTAGS), NULL)) ||
		!test("f", makepath(dbpath, dbname(GRTAGS), NULL)) ||
		!test("f", makepath(dbpath, dbname(GPATH), NULL)))) {
		if (wflag)
			warning("GTAGS, GRTAGS or GPATH not found. -i option ignored.");
		iflag = 0;
	}
	if (!test("d", dbpath))
		die("directory '%s' not found.", dbpath);
	if (vflag)
		fprintf(stderr, "[%s] Gtags started.\n", now());
	/*
	 * load configuration file.
	 */
	openconf();
	if (getconfb("extractmethod"))
		extractmethod = 1;
	strbuf_reset(sb);
	if (getconfs("langmap", sb))
		langmap = check_strdup(strbuf_value(sb));
	strbuf_reset(sb);
	if (getconfs("gtags_parser", sb))
		gtags_parser = check_strdup(strbuf_value(sb));
	/*
	 * initialize parser.
	 */
	if (vflag && gtags_parser)
		fprintf(stderr, " Using plug-in parser.\n");
	parser_init(langmap, gtags_parser);
	if (vflag && file_list)
		fprintf(stderr, " Using '%s' as a file list.\n", file_list);
	/*
	 * Start statistics.
	 */
	init_statistics();
	/*
	 * incremental update.
	 */
	if (iflag) {
		/*
		 * Version check. If existing tag files are old enough
		 * gtagsopen() abort with error message.
		 */
		GTOP *gtop = gtags_open(dbpath, cwd, GTAGS, GTAGS_MODIFY, 0);
		gtags_close(gtop);
		/*
		 * GPATH is needed for incremental updating.
		 * Gtags check whether or not GPATH exist, since it may be
		 * removed by mistake.
		 */
		if (!test("f", makepath(dbpath, dbname(GPATH), NULL)))
			die("Old version tag file found. Please remake it.");
		(void)incremental(dbpath, cwd);
		print_statistics(statistics);
		exit(0);
	}
	/*
	 * create GTAGS and GRTAGS
	 */
	createtags(dbpath, cwd);
	/*
	 * create idutils index.
	 */
	if (Iflag) {
		tim = statistics_time_start("Time of creating ID");
		if (vflag)
			fprintf(stderr, "[%s] Creating indexes for idutils.\n", now());
		strbuf_reset(sb);
		strbuf_puts(sb, "mkid");
		if (vflag)
			strbuf_puts(sb, " -v");
		strbuf_sprintf(sb, " --file='%s/ID'", dbpath);
		if (vflag) {
#ifdef __DJGPP__
			if (is_unixy())	/* test for 4DOS as well? */
#endif
			strbuf_puts(sb, " 1>&2");
		} else {
			strbuf_puts(sb, " >/dev/null");
		}
		if (debug)
			fprintf(stderr, "executing mkid like: %s\n", strbuf_value(sb));
		if (system(strbuf_value(sb)))
			die("mkid failed: %s", strbuf_value(sb));
		if (chmod(makepath(dbpath, "ID", NULL), 0644) < 0)
			die("cannot chmod ID file.");
		statistics_time_end(tim);
	}
	if (vflag)
		fprintf(stderr, "[%s] Done.\n", now());
	closeconf();
	strbuf_close(sb);
	print_statistics(statistics);

	return 0;
}
/*
 * incremental: incremental update
 *
 *	i)	dbpath	dbpath directory
 *	i)	root	root directory of source tree
 *	r)		0: not updated, 1: updated
 */
int
incremental(const char *dbpath, const char *root)
{
	STATISTICS_TIME *tim;
	struct stat statp;
	time_t gtags_mtime;
	STRBUF *addlist = strbuf_open(0);
	STRBUF *deletelist = strbuf_open(0);
	STRBUF *addlist_other = strbuf_open(0);
	IDSET *deleteset, *findset;
	int updated = 0;
	const char *path;
	unsigned int id, limit;

	tim = statistics_time_start("Time of inspecting %s and %s.", dbname(GTAGS), dbname(GRTAGS));
	if (vflag) {
		fprintf(stderr, " Tag found in '%s'.\n", dbpath);
		fprintf(stderr, " Incremental updating.\n");
	}
	/*
	 * get modified time of GTAGS.
	 */
	path = makepath(dbpath, dbname(GTAGS), NULL);
	if (stat(path, &statp) < 0)
		die("stat failed '%s'.", path);
	gtags_mtime = statp.st_mtime;

	if (gpath_open(dbpath, 0) < 0)
		die("GPATH not found.");
	/*
	 * deleteset:
	 *	The list of the path name which should be deleted from GPATH.
	 * findset:
	 *	The list of the path name which exists in the current project.
	 *	A project is limited by the --file option.
	 */
	deleteset = idset_open(gpath_nextkey());
	findset = idset_open(gpath_nextkey());
	total = 0;
	/*
	 * Make add list and delete list for update.
	 */
	if (single_update) {
		int type;
		const char *fid = gpath_path2fid(single_update, &type);
		/*
		 * The --single-update=file supports only updating.
		 * If it is new file, this option is ignored, and the processing is
		 * automatically switched to the normal procedure.
		 */
		if (fid == NULL) {
			if (vflag)
				fprintf(stderr, " --single-update option ignored, because '%s' is new file.\n", single_update);
			goto normal_update;
		}
		/*
		 * If type != GPATH_SOURCE then we have nothing to do, and you will see
		 * a message 'Global databases are up to date.'.
		 */
		if (type == GPATH_SOURCE) {
			strbuf_puts0(addlist, single_update);
			idset_add(deleteset, atoi(fid));
			total++;
		}
	} else {
normal_update:
		if (file_list)
			find_open_filelist(file_list, root);
		else
			find_open(NULL);
		while ((path = find_read()) != NULL) {
			const char *fid;
			int n_fid = 0;
			int other = 0;

			/* a blank at the head of path means 'NOT SOURCE'. */
			if (*path == ' ') {
				if (test("b", ++path))
					continue;
				other = 1;
			}
			if (stat(path, &statp) < 0)
				die("stat failed '%s'.", path);
			fid = gpath_path2fid(path, NULL);
			if (fid) { 
				n_fid = atoi(fid);
				idset_add(findset, n_fid);
			}
			if (other) {
				if (fid == NULL)
					strbuf_puts0(addlist_other, path);
			} else {
				if (fid == NULL) {
					strbuf_puts0(addlist, path);
					total++;
				} else if (gtags_mtime < statp.st_mtime) {
					strbuf_puts0(addlist, path);
					total++;
					idset_add(deleteset, n_fid);
				}
			}
		}
		find_close();
		/*
		 * make delete list.
		 */
		limit = gpath_nextkey();
		for (id = 1; id < limit; id++) {
			char fid[MAXFIDLEN];
			int type;

			snprintf(fid, sizeof(fid), "%d", id);
			/*
			 * This is a hole of GPATH. The hole increases if the deletion
			 * and the addition are repeated.
			 */
			if ((path = gpath_fid2path(fid, &type)) == NULL)
				continue;
			/*
			 * The file which does not exist in the findset is treated
			 * assuming that it does not exist in the file system.
			 */
			if (type == GPATH_OTHER) {
				if (!idset_contains(findset, id) || !test("f", path) || test("b", path))
					strbuf_puts0(deletelist, path);
			} else {
				if (!idset_contains(findset, id) || !test("f", path)) {
					strbuf_puts0(deletelist, path);
					idset_add(deleteset, id);
				}
			}
		}
	}
	gpath_close();
	statistics_time_end(tim);
	/*
	 * execute updating.
	 */
	if ((!idset_empty(deleteset) || strbuf_getlen(addlist) > 0) ||
	    (strbuf_getlen(deletelist) + strbuf_getlen(addlist_other) > 0))
	{
		int db;

		updated = 1;
		tim = statistics_time_start("Time of updating %s and %s.", dbname(GTAGS), dbname(GRTAGS));
		if (!idset_empty(deleteset) || strbuf_getlen(addlist) > 0)
			updatetags(dbpath, root, deleteset, addlist);
		if (strbuf_getlen(deletelist) + strbuf_getlen(addlist_other) > 0) {
			const char *start, *end, *p;

			if (vflag)
				fprintf(stderr, "[%s] Updating '%s'.\n", now(), dbname(GPATH));
			gpath_open(dbpath, 2);
			if (strbuf_getlen(deletelist) > 0) {
				start = strbuf_value(deletelist);
				end = start + strbuf_getlen(deletelist);

				for (p = start; p < end; p += strlen(p) + 1)
					gpath_delete(p);
			}
			if (strbuf_getlen(addlist_other) > 0) {
				start = strbuf_value(addlist_other);
				end = start + strbuf_getlen(addlist_other);

				for (p = start; p < end; p += strlen(p) + 1)
					gpath_put(p, GPATH_OTHER);
			}
			gpath_close();
		}
		/*
		 * Update modification time of tag files
		 * because they may have no definitions.
		 */
		for (db = GTAGS; db < GTAGLIM; db++)
			utime(makepath(dbpath, dbname(db), NULL), NULL);
		statistics_time_end(tim);
	}
	if (vflag) {
		if (updated)
			fprintf(stderr, " Global databases have been modified.\n");
		else
			fprintf(stderr, " Global databases are up to date.\n");
		fprintf(stderr, "[%s] Done.\n", now());
	}
	strbuf_close(addlist);
	strbuf_close(deletelist);
	strbuf_close(addlist_other);
	idset_close(deleteset);
	idset_close(findset);

	return updated;
}
/*
 * callback functions for built-in parser
 */
struct put_func_data {
	GTOP *gtop[GTAGLIM];
	const char *fid;
};
static void
put_syms(int type, const char *tag, int lno, const char *path, const char *line_image, void *arg)
{
	const struct put_func_data *data = arg;
	GTOP *gtop;

	switch (type) {
	case PARSER_DEF:
		gtop = data->gtop[GTAGS];
		break;
	case PARSER_REF_SYM:
		gtop = data->gtop[GRTAGS];
		if (gtop == NULL)
			return;
		break;
	default:
		return;
	}
	gtags_put_using(gtop, tag, lno, data->fid, line_image);
}
/*
 * updatetags: update tag file.
 *
 *	i)	dbpath		directory in which tag file exist
 *	i)	root		root directory of source tree
 *	i)	deleteset	bit array of fid of deleted or modified files 
 *	i)	addlist		\0 separated list of added or modified files
 */
void
updatetags(const char *dbpath, const char *root, IDSET *deleteset, STRBUF *addlist)
{
	struct put_func_data data;
	int seqno, flags;
	const char *path, *start, *end;

	if (vflag)
		fprintf(stderr, "[%s] Updating '%s' and '%s'.\n", now(), dbname(GTAGS), dbname(GRTAGS));
	/*
	 * Open tag files.
	 */
	data.gtop[GTAGS] = gtags_open(dbpath, root, GTAGS, GTAGS_MODIFY, 0);
	if (test("f", makepath(dbpath, dbname(GRTAGS), NULL))) {
		data.gtop[GRTAGS] = gtags_open(dbpath, root, GRTAGS, GTAGS_MODIFY, 0);
	} else {
		/*
		 * If you set NULL to data.gtop[GRTAGS], parse_file() doesn't write to
		 * GRTAGS. See put_syms().
		 */
		data.gtop[GRTAGS] = NULL;
	}
	/*
	 * Delete tags from GTAGS.
	 */
	if (!idset_empty(deleteset)) {
		if (vflag) {
			char fid[MAXFIDLEN];
			int total = idset_count(deleteset);
			unsigned int id;

			seqno = 1;
			for (id = idset_first(deleteset); id != END_OF_ID; id = idset_next(deleteset)) {
				snprintf(fid, sizeof(fid), "%d", id);
				path = gpath_fid2path(fid, NULL);
				if (path == NULL)
					die("GPATH is corrupted.");
				fprintf(stderr, " [%d/%d] deleting tags of %s\n", seqno++, total, path + 2);
			}
		}
		gtags_delete(data.gtop[GTAGS], deleteset);
		if (data.gtop[GRTAGS] != NULL)
			gtags_delete(data.gtop[GRTAGS], deleteset);
	}
	/*
	 * Set flags.
	 */
	data.gtop[GTAGS]->flags = 0;
	if (extractmethod)
		data.gtop[GTAGS]->flags |= GTAGS_EXTRACTMETHOD;
	data.gtop[GRTAGS]->flags = data.gtop[GTAGS]->flags;
	flags = 0;
	if (debug)
		flags |= PARSER_DEBUG;
	if (wflag)
		flags |= PARSER_WARNING;
	/*
	 * Add tags to GTAGS and GRTAGS.
	 */
	start = strbuf_value(addlist);
	end = start + strbuf_getlen(addlist);
	seqno = 0;
	for (path = start; path < end; path += strlen(path) + 1) {
		gpath_put(path, GPATH_SOURCE);
		data.fid = gpath_path2fid(path, NULL);
		if (data.fid == NULL)
			die("GPATH is corrupted.('%s' not found)", path);
		if (vflag)
			fprintf(stderr, " [%d/%d] extracting tags of %s\n", ++seqno, total, path + 2);
		if (debug)
			fprintf(stderr, "[%s]\n", path + 2);
		parse_file(path, flags, put_syms, &data);
		gtags_flush(data.gtop[GTAGS], data.fid);
		if (data.gtop[GRTAGS] != NULL)
			gtags_flush(data.gtop[GRTAGS], data.fid);
	}
	parser_exit();
	gtags_close(data.gtop[GTAGS]);
	if (data.gtop[GRTAGS] != NULL)
		gtags_close(data.gtop[GRTAGS]);
}
/*
 * createtags: create tags file
 *
 *	i)	dbpath	dbpath directory
 *	i)	root	root directory of source tree
 */
void
createtags(const char *dbpath, const char *root)
{
	STATISTICS_TIME *tim;
	STRBUF *sb = strbuf_open(0);
	struct put_func_data data;
	int openflags, flags, seqno;
	const char *path;

	tim = statistics_time_start("Time of creating %s and %s.", dbname(GTAGS), dbname(GRTAGS));
	if (vflag)
		fprintf(stderr, "[%s] Creating '%s' and '%s'.\n", now(), dbname(GTAGS), dbname(GRTAGS));
	openflags = cflag ? GTAGS_COMPACT : 0;
	data.gtop[GTAGS] = gtags_open(dbpath, root, GTAGS, GTAGS_CREATE, openflags);
	data.gtop[GTAGS]->flags = 0;
	if (extractmethod)
		data.gtop[GTAGS]->flags |= GTAGS_EXTRACTMETHOD;
	data.gtop[GRTAGS] = gtags_open(dbpath, root, GRTAGS, GTAGS_CREATE, openflags);
	data.gtop[GRTAGS]->flags = data.gtop[GTAGS]->flags;
	flags = 0;
	if (debug)
		flags |= PARSER_DEBUG;
	if (wflag)
		flags |= PARSER_WARNING;
	/*
	 * Add tags to GTAGS and GRTAGS.
	 */
	if (file_list)
		find_open_filelist(file_list, root);
	else
		find_open(NULL);
	seqno = 0;
	while ((path = find_read()) != NULL) {
		if (*path == ' ') {
			path++;
			if (!test("b", path))
				gpath_put(path, GPATH_OTHER);
			continue;
		}
		gpath_put(path, GPATH_SOURCE);
		data.fid = gpath_path2fid(path, NULL);
		if (data.fid == NULL)
			die("GPATH is corrupted.('%s' not found)", path);
		seqno++;
		if (vflag)
			fprintf(stderr, " [%d] extracting tags of %s\n", seqno, path + 2);
		if (debug)
			fprintf(stderr, "[%s]\n", path + 2);
		parse_file(path, flags, put_syms, &data);
		gtags_flush(data.gtop[GTAGS], data.fid);
		gtags_flush(data.gtop[GRTAGS], data.fid);
	}
	total = seqno;
	parser_exit();
	find_close();
	statistics_time_end(tim);
	tim = statistics_time_start("Time of flushing B-tree cache");
	gtags_close(data.gtop[GTAGS]);
	gtags_close(data.gtop[GRTAGS]);
	statistics_time_end(tim);
	strbuf_reset(sb);
	if (getconfs("GTAGS_extra", sb)) {
		tim = statistics_time_start("Time of executing GTAGS_extra command");
		if (system(strbuf_value(sb)))
			fprintf(stderr, "GTAGS_extra command failed: %s\n", strbuf_value(sb));
		statistics_time_end(tim);
	}
	strbuf_reset(sb);
	if (getconfs("GRTAGS_extra", sb)) {
		tim = statistics_time_start("Time of executing GRTAGS_extra command");
		if (system(strbuf_value(sb)))
			fprintf(stderr, "GRTAGS_extra command failed: %s\n", strbuf_value(sb));
		statistics_time_end(tim);
	}
	strbuf_close(sb);
}
/*
 * printconf: print configuration data.
 *
 *	i)	name	label of config data
 *	r)		exit code
 */
int
printconf(const char *name)
{
	int num;
	int exist = 1;

	if (getconfn(name, &num))
		fprintf(stdout, "%d\n", num);
	else if (getconfb(name))
		fprintf(stdout, "1\n");
	else {
		STRBUF *sb = strbuf_open(0);
		if (getconfs(name, sb))
			fprintf(stdout, "%s\n", strbuf_value(sb));
		else
			exist = 0;
		strbuf_close(sb);
	}
	return exist;
}
