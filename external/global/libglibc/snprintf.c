/* Copyright (C) 1991, 1995, 1997, 1998 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
   Boston, MA  02110-1301  USA. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdarg.h>
#include <stdio.h>

/*
 * drived from GNU C Library and modified at 2001/10/09.
 *
 * This is the simplest version of snprintf, that is, it just exits after
 * writing over MAXLEN characters. It is used in the system which doesn't
 * have snprintf(3).
 */

#ifndef HAVE_SNPRINTF
/* Write formatted output into S, according to the format
   string FORMAT, writing no more than MAXLEN characters.  */
/* VARARGS3 */
int
snprintf (char *s, size_t maxlen, const char *format, ...)
{
  va_list arg;
  int done;

  va_start (arg, format);
/*  done = __vsnprintf (s, maxlen, format, arg); */
  done = vsprintf (s, format, arg);
  if (done >= maxlen) {
    fprintf(stderr, "This program exit because vsprintf(3) destroy memory.\n");
    fprintf(stderr, "You should install snprintf(3) instead.\n");
    exit(1);
  }
  va_end (arg);

  return done;
}
#endif /* HAVE_SNPRINTF */
