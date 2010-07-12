dnl
dnl Configuration script for GLOBAL.
dnl Process this file with autoconf to produce a configure script.
dnl
dnl Copyright (c) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2010
dnl	Tama Communications Corporation
dnl
dnl This file is part of GNU GLOBAL.
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl 
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl
AC_INIT([GNU GLOBAL],[5.9])
AC_CONFIG_SRCDIR(global/global.c)
AC_CONFIG_HEADERS([config.h:config-h.in])
AC_PREREQ(2.59)
AM_INIT_AUTOMAKE([1.9.3 gnu])

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AM_PATH_LISPDIR
dnl
dnl Currently, the absolute path name of perl is required only for CGI script.
dnl So, if you don't use the -f(--form) option of htags(1), it is not required.
dnl
AC_PATH_PROG(PERL, perl, no)
if test ${ac_cv_path_PERL} = no; then
	PERL=SPECIFY_ABSOLUTE_PATH_OF_PERL
fi
AC_CHECK_TOOL(AR, ar)

AC_GNU_SOURCE

LT_PREREQ([2.2.6b])
LT_CONFIG_LTDL_DIR([libltdl])
dnl
dnl Because autoreconf up to version 2.59 doesn't know LT_INIT,
dnl we use the old name AC_PROG_LIBTOOL.
dnl
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
LTDL_INIT([recursive])

dnl Checks for libraries.

dnl Checks for header files.
AC_CHECK_HEADERS(limits.h string.h unistd.h stdarg.h sys/time.h fcntl.h)
AC_CHECK_HEADERS(sys/resource.h)
AC_HEADER_DIRENT
if test ${ac_header_dirent} = no; then
        AC_MSG_ERROR([dirent(3) is required but not found.])
fi
AC_HEADER_STDC
AC_HEADER_STAT
AC_HEADER_TIME

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_CHECK_MEMBERS([struct stat.st_blksize])
AC_C_BIGENDIAN
AC_CHECK_TYPE([int8_t],,[AC_DEFINE_UNQUOTED([int8_t], [signed char],
		[Define to `signed char' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([int16_t],,[AC_DEFINE_UNQUOTED([int16_t], [short],
		[Define to `short' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([int32_t],,[AC_DEFINE_UNQUOTED([int32_t], [int],
		[Define to `int' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([u_int8_t],,[AC_DEFINE_UNQUOTED([u_int8_t], [unsigned char],
		[Define to `unsigned char' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([u_int16_t],,[AC_DEFINE_UNQUOTED([u_int16_t], [unsigned short],
		[Define to `unsigned short' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([u_int32_t],,[AC_DEFINE_UNQUOTED([u_int32_t], [unsigned int],
		[Define to `unsigned int' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([ssize_t],,[AC_DEFINE_UNQUOTED([ssize_t], [int],
		[Define to `int' if <sys/types.h> does not define.])])
AC_CHECK_TYPE([caddr_t],,[AC_DEFINE_UNQUOTED([caddr_t], [char *],
		[Define to `char *' if <sys/types.h> does not define.])])
AC_CHECK_SIZEOF(int)
if test ${ac_cv_sizeof_int} != 4; then
	AC_MSG_ERROR([Integer size isn't 32 bit.])
fi
AC_CHECK_SIZEOF(short)
if test ${ac_cv_sizeof_short} != 2; then
	AC_MSG_ERROR([Short integer size isn't 16 bit.])
fi
AC_CHECK_SIZEOF(char)
if test ${ac_cv_sizeof_char} != 1; then
	AC_MSG_ERROR([Char size isn't 8 bit.])
fi

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP
AC_TYPE_SIGNAL
AC_FUNC_STRFTIME
AC_CHECK_FUNCS(getcwd putenv lstat snprintf)
AC_CHECK_FUNCS(index rindex bzero bcmp bcopy strchr strrchr memset memcmp memmove)
AC_CHECK_FUNCS(putc_unlocked getc_unlocked)
AC_CHECK_FUNCS(gettimeofday getrusage)
AG_DJGPP

INCLUDES='-I$(top_srcdir)/libparser $(LTDLINCL) -I$(top_srcdir)/libutil -I$(top_srcdir)/libdb -I$(top_srcdir)/libglibc -I../libutil'
DBLIBRARY='../libdb/libglodb.a'

dnl
dnl This option is for debugging GLOBAL. Not for normal use.
dnl
AC_ARG_WITH(db185-compat,
[  --with-db185-compat[[=DIR]] use DB1.85 compatible API included in DB2 or later ],
[
	if test "$withval" != 'no'; then
		case "$withval" in
		''|yes)
			;;
		*)
			if ! test -d $withval; then
				AC_MSG_ERROR([directory $withval not found.])
			fi
			if ! test -r $withval/include/db_185.h; then
				AC_MSG_ERROR([header $withval/include/db_185.h not found.])
			fi
			INCLUDES="$INCLUDES -I$withval/include"
			LDFLAGS="$LDFLAGS -L$withval/lib"
			;;
		esac
		DBLIBRARY='-ldb'
		AC_DEFINE(USE_DB185_COMPAT,1,[Define if you use DB1.85 compatible API.])
	fi
],[ with_db185_compat='no' ])
AM_CONDITIONAL([USE_DB185_COMPAT], [test "$with_db185_compat" != no])

LDADD='../libparser/libgloparser.a ../libutil/libgloutil.a '$DBLIBRARY' ../libglibc/libgloglibc.a'

dnl
dnl for home-etc support
dnl
AC_MSG_CHECKING(for home-etc support)
AC_ARG_WITH(home-etc,
[  --with-home-etc[[=DIR]]   include home-etc support [[DIR=/usr/local]] ],
[
        if test "$withval" = 'no'; then
                AC_MSG_RESULT(no)
        else
                case $withval in
                ''|yes) HOMEETC=/usr/local;;
                *)      HOMEETC=$withval;;
                esac
                if ! test -d $HOMEETC; then
                        AC_MSG_ERROR([directory '$HOMEETC' not found.])
                fi
		if ! test -f $HOMEETC/include/home_etc.h; then
                        AC_MSG_ERROR([header '$HOMEETC/include/home_etc.h' not found.])
                fi
                INCLUDES="$INCLUDES -I$HOMEETC/include"
                LDFLAGS="$LDFLAGS -L$HOMEETC/lib -lhome_etc"
                AC_DEFINE(HAVE_HOME_ETC_H,1,[Define if you use home-etc facility.])
                AC_MSG_RESULT([yes, using $HOMEETC])
        fi
],[
        AC_MSG_RESULT(no)
])

dnl
dnl for pread/pwrite support
dnl
AC_MSG_CHECKING(for pread/pwrite support)
AC_ARG_WITH(pread-pwrite,
[  --with-pread-pwrite     include pread/pwrite support for BSD db library ],
[
        if test "$withval" = 'no'; then
                AC_MSG_RESULT(no)
        else
		AC_DEFINE(HAVE_PREAD, 1, [Define to 1 if you have and use pread.])
		AC_DEFINE(HAVE_PWRITE, 1, [Define to 1 if you have and use pwrite.])
                AC_MSG_RESULT([yes])
        fi
],[
        AC_MSG_RESULT(no)
])

dnl
dnl for gtags.conf and function layer plugin parser sample.
dnl
AC_MSG_CHECKING(for exuberant ctags program)
AC_ARG_WITH(exuberant-ctags,
[  --with-exuberant-ctags=PROGRAM  specify Exuberant Ctags program ],
[ EXUBERANT_CTAGS="$withval" ],
[ EXUBERANT_CTAGS=/usr/local/bin/ctags-exuberant ])
AC_DEFINE_UNQUOTED(EXUBERANT_CTAGS, "$EXUBERANT_CTAGS", [Exuberant Ctags program.])
AC_MSG_RESULT("$EXUBERANT_CTAGS")
AC_SUBST(EXUBERANT_CTAGS)

dnl
dnl for POSIX sort program. By default, "/usr/bin/sort" is used.
dnl
AC_MSG_CHECKING(for POSIX sort program)
AC_ARG_WITH(posix-sort,
[  --with-posix-sort=PROGRAM  specify POSIX sort program ],
[ POSIX_SORT="$withval" ],
[ POSIX_SORT=/usr/bin/sort ])
AC_DEFINE_UNQUOTED(POSIX_SORT, "$POSIX_SORT", [POSIX sort program.])
AC_MSG_RESULT("$POSIX_SORT")
AC_SUBST(POSIX_SORT)

AC_SUBST(INCLUDES)
AC_SUBST(LDADD)
AC_SUBST(LDFLAGS)

dnl
dnl The definition of config variable skip and suffixes.
dnl
dnl DEFAULTSKIP: You need not list files whoes first character of name is '.'.
dnl		Such files are skipped even in case of not being in the list.
dnl
DEFAULTSKIP='GPATH,GTAGS,GRTAGS,GSYMS,HTML/,HTML.pub/,html/,tags,TAGS,ID,y.tab.c,y.tab.h,cscope.out,cscope.po.out,cscope.in.out,SCCS/,RCS/,CVS/,CVSROOT/,{arch}/,autom4te.cache/'
DEFAULTLANGMAP='c:.c.h,yacc:.y,asm:.s.S,java:.java,cpp:.c++.cc.cpp.cxx.hxx.hpp.C.H,php:.php.php3.phtml'
AC_SUBST(DEFAULTSKIP)
AC_SUBST(DEFAULTLANGMAP)

dnl
dnl Quoted langmap. It is only for gtags.conf.
dnl
DEFAULTLANGMAP_QUOTED=`echo $DEFAULTLANGMAP | sed 's/:/\\\\:/g'`
AC_SUBST(DEFAULTLANGMAP_QUOTED)

AC_CONFIG_FILES([Makefile
	gtags.conf
	libutil/langmap.h
	htags/global.cgi.tmpl
	htags/completion.cgi.tmpl
	libutil/Makefile
	gtags/Makefile
	htags/Makefile
	libdb/Makefile
	global/Makefile
	gozilla/Makefile
	gtags-cscope/Makefile
	globash/Makefile
	htags-refkit/Makefile
	libglibc/Makefile
	doc/Makefile
	icons/Makefile
	jquery/Makefile
	libparser/Makefile
	plugin-example/Makefile
	libltdl/Makefile
])
AC_OUTPUT
