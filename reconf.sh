#!/bin/sh
#
# Copyright (c) 2001, 2003 Tama Communications Corporation
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
# Usage:
#
#	% sh reconf.sh [--configure|--make|--install]
#
case $1 in
--help)	echo "Usage: sh reconf.sh [--configure|--make|--install]"
	exit 0;;
esac
prog='autoreconf flex gperf perl bison'	# required programs
file='convert.pl configure.ac Makefile.am libparser/reserved.pl'	# required files

echo "- File existent checking..."
for f in `echo $file`; do
	if [ ! -f $f ]; then
		echo "*** File '$f' not found."
		echo "You must execute this command at the root of GLOBAL source directory."
		exit 1
	fi
	echo "+ $f"
done

echo "- Program existent checking..."
for p in `echo $prog`; do
	found=0
	for d in `echo $PATH | sed -e 's/^:/.:/' -e 's/::/:.:/g' -e 's/:$/:./' -e 's/:/ /g'`
	do
		if [ -x $d/$p ]; then
			#echo "Found at $d/$p."
			found=1
			echo "+ $d/$p"
			break
		fi
	done
	case $found in
	0)	echo "*** Program '$p' not found."
		echo "Please install `echo $p | sed 's/autoreconf/automake and autoconf/'`."
		exit 1;;
	esac
done

#
# We should do this before packaging so that user can build it without
# flex, bison and gperf.
#
echo "- Preparing parser source ..."
(cd libparser; set -x
for lang in c cpp java php asm; do
	name=${lang}_res
	perl ./reserved.pl --prefix=$lang ${lang}_res.in > ${name}.gpf
	option=`perl ./reserved.pl --prefix=$lang --option`
	gperf $option < ${name}.gpf > ${name}.h
	if [ -f $lang.l ]; then
		flex -o$lang.c $lang.l
	fi
	if [ -f ${lang}_scan.l ]; then
		flex -o${lang}_scan.c ${lang}_scan.l
	fi
	if [ -f ${lang}_parse.y ]; then
		bison -d -o ${lang}_parse.c ${lang}_parse.y
	fi
done
)
(cd htags; set -x
for lang in c cpp java php asm; do
	flex -o$lang.c $lang.l
done
)

echo "- Collecting reference manuals ..."
commands="global gtags htags gozilla gtags-cscope globash";
perl ./convert.pl --menu $commands > doc/reference.texi
for d in `echo $commands`; do
	perl ./convert.pl --info $d/manual.in > doc/$d.ref
	echo "+ doc/$d.ref"
	perl ./convert.pl --man  $d/manual.in > $d/$d.1
	echo "+ $d/$d.1"
	perl ./convert.pl --c $d/manual.in > $d/const.h
	echo "+ $d/const.h"
done

echo "- Clean up config.cache..."
rm -f config.cache

echo "- Generating configure items..."
(set -x; autoreconf --symlink --verbose --install) &&
case $1 in
'')	echo "You are ready to execute ./configure"
	;;
--debug)
	./configure CFLAGS='-g -p -Wall -DDEBUG'
	make -s
	;;
--warn)
	./configure CFLAGS='-g -O2 -Wall'
	make -s
	;;
-c|--configure|--make|--install)
	./configure
	;;
esac && case $1 in
--make)	make
	;;
--install)
	make install
	;;
esac
