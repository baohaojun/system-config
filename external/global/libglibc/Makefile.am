## Process this file with automake to create Makefile.in
#
# Copyright (c) 2000, 2001 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
noinst_LIBRARIES = libgloglibc.a

noinst_HEADERS = regex.h getopt.h obstack.h hash-string.h getopt_int.h

libgloglibc_a_SOURCES = regex.c getopt.c getopt1.c snprintf.c obstack.c \
		hash-string.c

libgloglibc_a_DEPENDENCIES = $(libgloglibc_a_LIBADD)
