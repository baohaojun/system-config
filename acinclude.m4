# Local additions to Autoconf macros.
#
# Copyright (c) 2000, 2001 Tama Communications Corporation
#
# This file is part of GNU GLOBAL.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
AC_DEFUN([AG_DJGPP],
[AC_CACHE_CHECK([whether we are using the GNU DJGPP compiler], ac_cv_djgpp,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[#ifdef __DJGPP__
int a;
#else
XXXXXX
#endif]])],[ac_cv_djgpp=yes],[ac_cv_djgpp=no])])
AM_CONDITIONAL(DJGPP, test $ac_cv_djgpp = yes)
])
