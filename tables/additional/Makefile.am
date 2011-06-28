# vim:set noet ts=4
#
# ibus-table - The Tables engine for IBus
#
# Copyright (c) 2008-2009 Yu Yuwei <acevery@gmail.com>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# $Id: $
#

createdb = \
	$(PYTHON) $(abs_top_srcdir)/engine/tabcreatedb.py 

createdb_index = \
	$(PYTHON) $(abs_top_srcdir)/engine/tabcreatedb.py -i

tables_DATA =

if IBUS_TABLE_BUILD_ADDITIONAL
tables_DATA += compose.db \
	latex.db 
endif

tables_DATA += $(NULL)


tablesdir = $(pkgdatadir)/tables

EXTRA_DIST = \
	compose.txt \
	latex.txt \
	$(NULL)

DISTCLEANFILES = \
	*.db \
	$(NULL)

CLEANFILES = \
	$(DISTCLEANFILES) \
	$(NULL)

MAINTAINERCLEANFILES = \
	Makefile.in \
	$(NULL)


.txt.db:
	$(createdb) -n $@ -s $<

install-data-hook:
	@(if test x"$$NO_INDEX" = x""; then \
		cd $(DESTDIR)$(tablesdir); \
		for db in ${tables_DATA}; do \
		echo "Create Index for $$db"; \
			$(createdb_index) -n $$db; \
		done \
	fi)
