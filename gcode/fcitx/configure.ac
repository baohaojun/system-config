# Process this file with autoconf to produce a configure script.
AC_INIT(scim-fcitx, 3.1.1, baohaojun@yahoo.com)
AC_CONFIG_SRCDIR([src/scim_fcitx_imengine.h])

# Save this value here, since automake will set cflags and cxxflags later
cflags_set=${CFLAGS+set}
cppflags_set=${CPPFLAGS+set}

SCIM_FCITX_MAJOR_VERSION=0
SCIM_FCITX_MINOR_VERSION=1
SCIM_FCITX_MICRO_VERSION=3

SCIM_FCITX_VERSION=$SCIM_FCITX_MAJOR_VERSION.$SCIM_FCITX_MINOR_VERSION.$SCIM_FCITX_MICRO_VERSION

AC_SUBST(SCIM_FCITX_MAJOR_VERSION)
AC_SUBST(SCIM_FCITX_MINOR_VERSION)
AC_SUBST(SCIM_FCITX_MICRO_VERSION)
AC_SUBST(SCIM_FCITX_VERSION)

# Init automake stuff
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER([config.h])

# Init libtool
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
AC_LIB_LTDL
AC_SUBST(LIBTOOL_DEPS)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_LANG(C++)

AC_C_CONST
AC_TYPE_SIZE_T

# Checks for libraries.

SCIM_VERSION=1.2.0


PKG_CHECK_MODULES(SCIM,[scim >= $SCIM_VERSION])

# Checks for header files.
AC_HEADER_STDC

# Checks for library functions.
AC_SUBST(ac_aux_dir)

# libtool option to control which symbols are exported
# right now, symbols starting with _ are not exported
LIBTOOL_EXPORT_OPTIONS='-export-symbols-regex "^[[^_]].*"'

# Extra args.
AC_ARG_ENABLE(debug,
	      [  --enable-debug     Turn on debugging],
	      [case "${enableval}" in
	       yes) debug=yes ;;
	       no)  debug=no ;;
	       *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
       	      esac],[debug=no])

if test "x$debug" = "xyes"; then
  test "$cflags_set" = set || CFLAGS="$CFLAGS -g"
  test "$cppflags_set" = set || CPPFLAGS="$CXXFLAGS -g"
  SCIM_DEBUG_FLAGS="-DENABLE_DEBUG=1"
else
  SCIM_DEBUG_FLAGS=""
fi
AC_SUBST(SCIM_DEBUG_FLAGS)

SCIM_ICONDIR=`$PKG_CONFIG --variable=icondir scim`/fcitx
SCIM_DATADIR=`$PKG_CONFIG --variable=scimdatadir scim`
SCIM_MODULEDIR=`$PKG_CONFIG --variable=moduledir scim`

if test "x$SCIM_ICONDIR" = "x"; then
  SCIM_ICONDIR=${datadir}/scim/icons
fi

if test "x$SCIM_DATADIR" = "x"; then
  SCIM_DATADIR=${datadir}/scim
fi

if test "x$SCIM_MODULEDIR" = "x"; then
  SCIM_MODULEDIR=${libdir}/scim-1.0
fi

AC_SUBST(SCIM_ICONDIR)
AC_SUBST(SCIM_DATADIR)
AC_SUBST(SCIM_MODULEDIR)
SCIM_FCITXDATADIR=$SCIM_DATADIR/fcitx
AC_SUBST(SCIM_FCITXDATADIR)

AC_CONFIG_FILES([Makefile
		 src/Makefile
		 data/Makefile
		 tools/Makefile
		 icons/Makefile
		 scim-fcitx.spec])
AC_OUTPUT
