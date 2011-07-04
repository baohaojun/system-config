/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
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
#ifndef _HTAGS_H_
#define _HTAGS_H_

#include "gparam.h"
#include "gtagsop.h"

#if defined(_WIN32) || defined(__DJGPP__)
#define W32	1
#define NULL_DEVICE	"NUL"
#else
#define W32	0
#define NULL_DEVICE	"/dev/null"
#endif
#define SITEKEYDIRNAME "sitekeys"

/*
 * definition_header
 */
#define NO_HEADER	0
#define BEFORE_HEADER	1
#define RIGHT_HEADER	2
#define AFTER_HEADER	3

/*
 * Directory names.
 */
#define SRCS	"S"
#define DEFS	"D"
#define REFS	"R"
#define INCS	"I"
#define INCREFS "J"
#define SYMS	"Y"

extern int w32;
extern const char *www;
extern int html_count;
extern int sep;
extern const char *save_config;
extern const char *save_argv;

extern char cwdpath[MAXPATHLEN];
extern char dbpath[MAXPATHLEN];
extern char distpath[MAXPATHLEN];
extern char gtagsconf[MAXPATHLEN];

extern char gtags_path[MAXFILLEN];
extern char global_path[MAXFILLEN];
extern int gtags_exist[GTAGLIM];
extern const char *null_device;
extern const char *tmpdir;
extern const char *tree_view_type;
extern const char *sitekey;

extern int aflag;
extern int cflag;
extern int fflag;
extern int Fflag;
extern int gflag;
extern int Iflag;
extern int nflag;
extern int Sflag;
extern int qflag;
extern int vflag;
extern int wflag;
extern int debug;

extern int show_help;
extern int show_version;
extern int caution;
extern int auto_completion;
extern int tree_view;
extern int dynamic;
extern int symbol;
extern int statistics;

extern int no_order_list;
extern int other_files;
extern int enable_grep;
extern int enable_idutils;
extern int enable_xhtml;

extern const char *main_func;
extern const char *cvsweb_url;
extern int use_cvs_module;
extern const char *cvsweb_cvsroot;
extern const char *gtagslabel;
extern const char *title;
extern const char *xhtml_version;
extern const char *insert_header;
extern const char *insert_footer;
extern const char *html_header;
extern const char *jscode;

extern const char *title_define_index;
extern const char *title_file_index;
extern const char *title_call_tree;
extern const char *title_included_from;

extern const char *anchor_label[];
extern const char *anchor_icons[];
extern const char *anchor_comment[];
extern const char *anchor_msg[];
extern const char *back_icon;
extern const char *dir_icon;
extern const char *c_icon;
extern const char *file_icon;

extern int ncol;
extern int tabs;
extern int flist_fields;
extern int full_path;
extern int map_file;
extern const char *icon_suffix;
extern const char *icon_spec;
extern const char *prolog_script;
extern const char *epilog_script;
extern int show_position;
extern int table_list;
extern int table_flist;
extern int colorize_warned_line;
extern const char *script_alias;
extern const char *gzipped_suffix;
extern const char *normal_suffix;
extern const char *HTML;
extern const char *action;
extern int definition_header;
extern const char *htags_options;
extern const char *include_file_suffixes;

#endif /* _HTAGS_H_ */
