/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef	_COMPAT_H_
#define	_COMPAT_H_

#include <sys/types.h>

#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__DJGPP__)
typedef unsigned char	u_char;
typedef unsigned int	u_int;
typedef unsigned long	u_long;
typedef unsigned short	u_short;
#endif /* _WIN32 || __DJGPP__ */

#ifdef	NO_POSIX_SIGNALS
#define	sigemptyset(set)	(*(set) = 0)
#define	sigfillset(set)		(*(set) = ~(sigset_t)0, 0)
#define	sigaddset(set,signo)	(*(set) |= sigmask(signo), 0)
#define	sigdelset(set,signo)	(*(set) &= ~sigmask(signo), 0)
#define	sigismember(set,signo)	((*(set) & sigmask(signo)) != 0)

#define	SIG_BLOCK	1
#define	SIG_UNBLOCK	2
#define	SIG_SETMASK	3

static int __sigtemp;		/* For the use of sigprocmask */

/* Repeated test of oset != NULL is to avoid "*0". */
#define	sigprocmask(how, set, oset)					\
	((__sigtemp =							\
	(((how) == SIG_BLOCK) ?						\
		sigblock(0) | *(set) :					\
	(((how) == SIG_UNBLOCK) ?					\
		sigblock(0) & ~(*(set)) :				\
	((how) == SIG_SETMASK ?						\
		*(set) : sigblock(0))))),				\
	((oset) ? (*(oset ? oset : set) = sigsetmask(__sigtemp)) :	\
		sigsetmask(__sigtemp)), 0)
#endif

/*
 * If your system doesn't have an include file with the appropriate
 * byte order set, make sure you specify the correct one.
 */
#ifndef BYTE_ORDER
#define LITTLE_ENDIAN   1234
#define BIG_ENDIAN      4321
#ifdef WORDS_BIGENDIAN
#define BYTE_ORDER BIG_ENDIAN
#else
#define BYTE_ORDER LITTLE_ENDIAN
#endif
#endif

/*
 * Old definitions were rewritten using 'HAVE_XXX' macros.
 *
 * #if defined(SYSV) || defined(SYSTEM5)
 * #define	index(a, b)		strchr(a, b)
 * #define	rindex(a, b)		strrchr(a, b)
 * #define	bzero(a, b)		memset(a, 0, b)
 * #define	bcmp(a, b, n)		memcmp(a, b, n)
 * #define	bcopy(a, b, n)		memmove(b, a, n)
 * #endif
 * 
 * #if defined(BSD) || defined(BSD4_3)
 * #define	strchr(a, b)		index(a, b)
 * #define	strrchr(a, b)		rindex(a, b)
 * #define	memcmp(a, b, n)		bcmp(a, b, n)
 * #define	memmove(a, b, n)	bcopy(b, a, n)
 * #endif
*/
#if !defined (HAVE_INDEX) && defined (HAVE_STRCHR)
#define	index(a, b)		strchr(a, b)
#endif
#if !defined (HAVE_RINDEX) && defined (HAVE_STRRCHR)
#define	rindex(a, b)		strrchr(a, b)
#endif
#if !defined (HAVE_BZERO) && defined (HAVE_MEMSET)
#define	bzero(a, b)		memset(a, 0, b)
#endif
#if !defined (HAVE_BCMP) && defined (HAVE_MEMCMP)
#define	bcmp(a, b, n)		memcmp(a, b, n)
#endif
#if !defined (HAVE_BCOPY) && defined (HAVE_MEMMOVE)
#define	bcopy(a, b, n)		memmove(b, a, n)
#endif

#if !defined (HAVE_STRCHR) && defined (HAVE_INDEX)
#define	strchr(a, b)		index(a, b)
#endif
#if !defined (HAVE_STRRCHR) && defined (HAVE_RINDEX)
#define	strrchr(a, b)		rindex(a, b)
#endif
#if !defined (HAVE_MEMCMP) && defined (HAVE_BCMP)
#define	memcmp(a, b, n)		bcmp(a, b, n)
#endif
#if !defined (HAVE_MEMMOVE) && defined (HAVE_BCOPY)
#define	memmove(a, b, n)	bcopy(b, a, n)
#endif

/*
 * 32-bit machine.  The db routines are theoretically independent of
 * the size of u_shorts and u_longs, but I don't know that anyone has
 * ever actually tried it.  At a minimum, change the following #define's
 * if you are trying to compile on a different type of system.
 */
#ifndef O_ACCMODE			/* POSIX 1003.1 access mode mask. */
#define	O_ACCMODE	(O_RDONLY|O_WRONLY|O_RDWR)
#endif

#ifndef	_POSIX2_RE_DUP_MAX		/* POSIX 1003.2 RE limit. */
#define	_POSIX2_RE_DUP_MAX	255
#endif

/*
 * If you can't provide lock values in the open(2) call.  Note, this
 * allows races to happen.
 */
#ifndef O_EXLOCK			/* 4.4BSD extension. */
#define	O_EXLOCK	0
#endif

#ifndef O_SHLOCK			/* 4.4BSD extension. */
#define	O_SHLOCK	0
#endif

#ifndef O_BINARY	/* UNIX systems don't often have or need this */
#define O_BINARY 0
#endif

#ifndef O_NONBLOCK	/* Win32 systems doesn't have or need this */
#define	O_NONBLOCK	0
#endif

#ifndef EFTYPE
#define	EFTYPE		EINVAL		/* POSIX 1003.1 format errno. */
#endif

#ifndef SEEK_END
#define	SEEK_SET	0		/* POSIX 1003.1 seek values */
#define	SEEK_CUR	1
#define	SEEK_END	2
#endif

#ifndef _POSIX2_RE_DUP_MAX		/* POSIX 1003.2 values. */
#define	_POSIX2_RE_DUP_MAX	255
#endif

#ifndef NULL				/* ANSI C #defines NULL everywhere. */
#define	NULL		0
#endif

#ifndef	MAX				/* Usually found in <sys/param.h>. */
#define	MAX(_a,_b)	((_a)<(_b)?(_b):(_a))
#endif
#ifndef	MIN				/* Usually found in <sys/param.h>. */
#define	MIN(_a,_b)	((_a)<(_b)?(_a):(_b))
#endif

#ifndef S_ISDIR				/* POSIX 1003.1 file type tests. */
#define	S_ISDIR(m)	((m & 0170000) == 0040000)	/* directory */
#define	S_ISCHR(m)	((m & 0170000) == 0020000)	/* char special */
#define	S_ISBLK(m)	((m & 0170000) == 0060000)	/* block special */
#define	S_ISREG(m)	((m & 0170000) == 0100000)	/* regular file */
#define	S_ISFIFO(m)	((m & 0170000) == 0010000)	/* fifo */
#endif

#ifndef HAVE_LSTAT
#define lstat	stat
#endif
#ifndef HAVE_GETCWD
#define getcwd(buf, max) getwd (buf)
#endif

#endif /* !_COMPAT_H_ */
