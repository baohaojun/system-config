/* This file is generated automatically by convert.pl from gtags/manual.in. */
const char *progname = "gtags";
const char *usage_const = "Usage: gtags [-ciIOqvw][-d tag-file][-f file][dbpath]\n";
const char *help_const = "Options:\n\
-c, --compact\n\
       Make GTAGS in compact format.\n\
       This option does not influence GRTAGS,\n\
       because they are always made in compact format.\n\
--config[=name]\n\
       Print the value of config variable name.\n\
       If name is not specified then print all names and values.\n\
-d, --dump tag-file\n\
       Dump a tag file. The output format is 'key<tab>data'.\n\
       This is for debugging.\n\
-f, --file file\n\
       Browse through all source files whose names are listed in file.\n\
       The argument file can  be set to - to accept a list of\n\
       files from the standard input.\n\
       File names must be separated by newline.\n\
--gtagsconf file\n\
       Set the GTAGSCONF environment variable to file.\n\
--gtagslabel label\n\
       Set the GTAGSLABEL environment variable to label.\n\
-I, --idutils\n\
       Also make the ID database file for idutils(1).\n\
-i, --incremental\n\
       Update tag files incrementally. You had better use\n\
       global(1) with the -u option.\n\
-O, --objdir\n\
       Use BSD-style objdir as the location of tag files.\n\
       If $MAKEOBJDIRPREFIX directory exists, gtags creates\n\
       $MAKEOBJDIRPREFIX/<current directory> directory and makes\n\
       tag files in it.\n\
       If dbpath is specified, this options is ignored.\n\
--single-update file\n\
       Update tag files for single file.\n\
       It is considered that file was updated, and other files were not\n\
       updated.  The file must be relative path name from the current directory.\n\
       This option implies the -i option.\n\
       If the file is new then --single-update is ignored,\n\
       and the processing is automatically switched to normal incremental updating.\n\
--statistics\n\
       Print statistics information.\n\
       This option is valid only for normal creation of tag files.\n\
-q, --quiet\n\
       Quiet mode.\n\
-v, --verbose\n\
       Verbose mode.\n\
-w, --warning\n\
       Print warning messages.\n\
dbpath\n\
       The directory in which tag files are generated.\n\
       The default is the current directory.\n\
See also:\n\
       GNU GLOBAL web site: http://www.gnu.org/software/global/\n\
";
