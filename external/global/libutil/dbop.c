/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2006,
 *	2009, 2010
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include "char.h"
#include "checkalloc.h"
#include "dbop.h"
#include "die.h"
#include "env.h"
#include "locatestring.h"
#include "strbuf.h"
#include "strlimcpy.h"
#include "test.h"

/*
 * Though the prefix of the key of meta record is currently only a ' ',
 * this will be enhanced in the future.
 */
#define ismeta(p)	(*((char *)(p)) <= ' ')

/*
 * Stuff for DBOP_SORTED_WRITE
 */
#define SORT_SEP '\t'

/*
 * Two functions required for sorted writing.
 *
 * (1) start_sort_process: start sort process for sorted writing
 *
 *	i)	dbop	DBOP descriptor
 *
 * (2) terminate_sort_process: terminate sort process
 *
 *	i)	dbop	DBOP descriptor
 */
static void start_sort_process(DBOP *);
static void terminate_sort_process(DBOP *);
/*
 * 1. DJGPP
 */
#if defined(__DJGPP__)
/*
 * Just ignored. DJGPP version doesn't use sorted writing.
 */
static void
start_sort_process(DBOP *dbop) {
	return;
}
static void
terminate_sort_process(DBOP *dbop) {
	return;
}
/*
 * 2. WIN32
 */
#elif defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/*
 * sort is included with the binary distribution
 */
static char argv[] = "sort -k 1,1";
static void
start_sort_process(DBOP *dbop) {
	HANDLE opipe[2], ipipe[2];
	SECURITY_ATTRIBUTES sa;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	const char* lc_all;
	char sort[MAX_PATH];
	char* path;
	static int informed;

	if (informed)
		return;
	/*
	 * force using sort in the same directory as the program, to avoid
	 * using the Windows one
	 */
	path = strrchr(_pgmptr, '\\');
	sprintf(sort, "%.*s\\sort.exe", path - _pgmptr, _pgmptr);
	if (!test("fx", sort)) {
		warning("POSIX sort program not found. If available, the program will be speed up.");
		informed = 1;
		return;
	}

	sa.nLength = sizeof(sa);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;
	if (!CreatePipe(&opipe[0], &opipe[1], &sa, 0) ||
	    !CreatePipe(&ipipe[0], &ipipe[1], &sa, 0))
		die("cannot create pipe.");
	SetHandleInformation(opipe[1], HANDLE_FLAG_INHERIT, 0);
	SetHandleInformation(ipipe[0], HANDLE_FLAG_INHERIT, 0);
	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	si.hStdInput = opipe[0];
	si.hStdOutput = ipipe[1];
	si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	si.dwFlags = STARTF_USESTDHANDLES;
	lc_all = getenv("LC_ALL");
	if (lc_all == NULL)
		lc_all = "";
	set_env("LC_ALL", "C");
	CreateProcess(sort, argv, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi);
	set_env("LC_ALL", lc_all);
	CloseHandle(opipe[0]);
	CloseHandle(ipipe[1]);
	CloseHandle(pi.hThread);
	dbop->pid = pi.hProcess;
	dbop->sortout = fdopen(_open_osfhandle((long)opipe[1], _O_WRONLY), "w");
	dbop->sortin = fdopen(_open_osfhandle((long)ipipe[0], _O_RDONLY), "r");
	if (dbop->sortout == NULL || dbop->sortin == NULL)
		die("fdopen failed.");
}
static void
terminate_sort_process(DBOP *dbop) {
	WaitForSingleObject(dbop->pid, INFINITE);
	CloseHandle(dbop->pid);
}
/*
 * 3. UNIX and CYGWIN
 */
#else
#include <sys/wait.h>
/*
 * Though it doesn't understand why, GNU sort with no option is faster
 * than 'sort -k 1,1'. But we should use '-k 1,1' here not to rely on
 * a specific command.
 */
static char *argv[] = {
	POSIX_SORT,
	"-k",
	"1,1",
	NULL
};
static void
start_sort_process(DBOP *dbop) {
	int opipe[2], ipipe[2];

	if (!test("fx", POSIX_SORT)) {
		static int informed;

		if (!informed) {
			warning("POSIX sort program not found. If available, the program will be speed up.");
			informed = 1;
		}
		return;
	}
	/*
	 * Setup pipe for two way communication
	 *
	 *	Parent(gtags)				Child(sort)
	 *	---------------------------------------------------
	 *	(dbop->sortout) opipe[1] =====> opipe[0] (stdin)
	 *	(dbop->sortin)  ipipe[0] <===== ipipe[1] (stdout)
	 */
	if (pipe(opipe) < 0 || pipe(ipipe) < 0)
		fprintf(stderr, "cannot create pipe.");
	dbop->pid = fork();
	if (dbop->pid == 0) {
		/* child process */
		close(opipe[1]);
		close(ipipe[0]);
		if (dup2(opipe[0], 0) < 0 || dup2(ipipe[1], 1) < 0)
			die("dup2 failed.");
		close(opipe[0]);
		close(ipipe[1]);
		/*
		 * Use C locale in order to avoid the degradation of performance 	 
		 * by internationalized sort command. 	 
		 */
		set_env("LC_ALL", "C");
		execvp(POSIX_SORT, argv);
	} else if (dbop->pid < 0)
		die("fork failed.");
	/* parent process */
	close(opipe[0]);
	close(ipipe[1]);
	fcntl(ipipe[0], F_SETFD, FD_CLOEXEC);
	fcntl(opipe[1], F_SETFD, FD_CLOEXEC);
	dbop->sortout = fdopen(opipe[1], "w");
	dbop->sortin = fdopen(ipipe[0], "r");
	if (dbop->sortout == NULL || dbop->sortin == NULL)
		die("fdopen failed.");
}
static void
terminate_sort_process(DBOP *dbop) {
	while (waitpid(dbop->pid, NULL, 0) < 0 && errno == EINTR)
		;
}
#endif

/*
 * dbop_open: open db database.
 *
 *	i)	path	database name
 *	i)	mode	0: read only, 1: create, 2: modify
 *	i)	perm	file permission
 *	i)	flags
 *			DBOP_DUP: allow duplicate records.
 *			DBOP_SORTED_WRITE: use sorted writing. This requires POSIX sort.
 *	r)		descripter for dbop_xxx()
 *
 * Sorted wirting is fast because all writing is done by not insertion but addition.
 */
DBOP *
dbop_open(const char *path, int mode, int perm, int flags)
{
	DB *db;
	int rw = 0;
	DBOP *dbop;
	BTREEINFO info;

	/*
	 * setup arguments.
	 */
	switch (mode) {
	case 0:
		rw = O_RDONLY;
		break;
	case 1:
		rw = O_RDWR|O_CREAT|O_TRUNC;
		break;
	case 2:
		rw = O_RDWR;
		break;
	default:
		assert(0);
	}
	memset(&info, 0, sizeof(info));
	if (flags & DBOP_DUP)
		info.flags |= R_DUP;
	info.psize = DBOP_PAGESIZE;
	/*
	 * Decide cache size. The default value is 5MB.
	 * See libutil/gparam.h for the details.
	 */
	info.cachesize = GTAGSCACHE;
	if (getenv("GTAGSCACHE") != NULL)
		info.cachesize = atoi(getenv("GTAGSCACHE"));
	if (info.cachesize < GTAGSMINCACHE)
		info.cachesize = GTAGSMINCACHE;

	/*
	 * if unlink do job normally, those who already open tag file can use
	 * it until closing.
	 */
	if (path != NULL && mode == 1 && test("f", path))
		(void)unlink(path);
	db = dbopen(path, rw, 0600, DB_BTREE, &info);
	if (!db)
		return NULL;
	dbop = (DBOP *)check_calloc(sizeof(DBOP), 1);
	if (path == NULL)
		dbop->dbname[0] = '\0';
	else
		strlimcpy(dbop->dbname, path, sizeof(dbop->dbname));
	dbop->db	= db;
	dbop->openflags	= flags;
	dbop->perm	= (mode == 1) ? perm : 0;
	dbop->lastdat	= NULL;
	dbop->lastsize	= 0;
	dbop->sortout	= NULL;
	dbop->sortin	= NULL;
	/*
	 * Setup sorted writing.
	 */
	if (dbop->openflags & DBOP_SORTED_WRITE)
		start_sort_process(dbop);
	return dbop;
}
/*
 * dbop_get: get data by a key.
 *
 *	i)	dbop	descripter
 *	i)	name	name
 *	r)		pointer to data
 */
const char *
dbop_get(DBOP *dbop, const char *name)
{
	DB *db = dbop->db;
	DBT key, dat;
	int status;

	key.data = (char *)name;
	key.size = strlen(name)+1;

	status = (*db->get)(db, &key, &dat, 0);
	dbop->lastdat = (char *)dat.data;
	dbop->lastsize = dat.size;
	switch (status) {
	case RET_SUCCESS:
		break;
	case RET_ERROR:
		die("cannot read from database.");
	case RET_SPECIAL:
		return (NULL);
	}
	return (dat.data);
}
/*
 * dbop_put: put data by a key.
 *
 *	i)	dbop	descripter
 *	i)	name	key
 *	i)	data	data
 */
void
dbop_put(DBOP *dbop, const char *name, const char *data)
{
	DB *db = dbop->db;
	DBT key, dat;
	int status;
	int len;

	if (!(len = strlen(name)))
		die("primary key size == 0.");
	if (len > MAXKEYLEN)
		die("primary key too long.");
	/* sorted writing */
	if (dbop->sortout != NULL) {
		fputs(name, dbop->sortout);
		putc(SORT_SEP, dbop->sortout);
		fputs(data, dbop->sortout);
		putc('\n', dbop->sortout);
		return;
	}
	key.data = (char *)name;
	key.size = strlen(name)+1;
	dat.data = (char *)data;
	dat.size = strlen(data)+1;

	status = (*db->put)(db, &key, &dat, 0);
	switch (status) {
	case RET_SUCCESS:
		break;
	case RET_ERROR:
	case RET_SPECIAL:
		die(dbop->put_errmsg ? dbop->put_errmsg : "cannot write to database.");
	}
}
/*
 * dbop_put_withlen: put data by a key.
 *
 *	i)	dbop	descripter
 *	i)	name	key
 *	i)	data	data
 *	i)	length	length of data
 *
 * Note: This function doesn't support sorted writing.
 */
void
dbop_put_withlen(DBOP *dbop, const char *name, const char *data, int length)
{
	DB *db = dbop->db;
	DBT key, dat;
	int status;
	int len;

	if (!(len = strlen(name)))
		die("primary key size == 0.");
	if (len > MAXKEYLEN)
		die("primary key too long.");
	key.data = (char *)name;
	key.size = strlen(name)+1;
	dat.data = (char *)data;
	dat.size = length;

	status = (*db->put)(db, &key, &dat, 0);
	switch (status) {
	case RET_SUCCESS:
		break;
	case RET_ERROR:
	case RET_SPECIAL:
		die(dbop->put_errmsg ? dbop->put_errmsg : "cannot write to database.");
	}
}
/*
 * dbop_delete: delete record by path name.
 *
 *	i)	dbop	descripter
 *	i)	path	path name
 */
void
dbop_delete(DBOP *dbop, const char *path)
{
	DB *db = dbop->db;
	DBT key;
	int status;

	if (path) {
		key.data = (char *)path;
		key.size = strlen(path)+1;
		status = (*db->del)(db, &key, 0);
	} else
		status = (*db->del)(db, &key, R_CURSOR);
	if (status == RET_ERROR)
		die("cannot delete record.");
}
/*
 * dbop_update: update record.
 *
 *	i)	dbop	descripter
 *	i)	key	key
 *	i)	dat	data
 */
void
dbop_update(DBOP *dbop, const char *key, const char *dat)
{
	dbop_put(dbop, key, dat);
}
/*
 * dbop_first: get first record. 
 * 
 *	i)	dbop	dbop descripter
 *	i)	name	key value or prefix
 *			!=NULL: indexed read by key
 *			==NULL: sequential read
 *	i)	preg	compiled regular expression if any.
 *	i)	flags	following dbop_next call take over this.
 *			DBOP_KEY	read key part
 *			DBOP_PREFIX	prefix read
 *					only valied when sequential read
 *	r)		data
 */
const char *
dbop_first(DBOP *dbop, const char *name, regex_t *preg, int flags)
{
	DB *db = dbop->db;
	DBT key, dat;
	int status;

	dbop->preg = preg;
	if (flags & DBOP_PREFIX && !name)
		flags &= ~DBOP_PREFIX;
	if (name) {
		if (strlen(name) > MAXKEYLEN)
			die("primary key too long.");
		strlimcpy(dbop->key, name, sizeof(dbop->key));
		key.data = (char *)name;
		key.size = strlen(name);
		/*
		 * includes NULL character unless prefix read.
		 */
		if (!(flags & DBOP_PREFIX))
			key.size++;
		dbop->keylen = key.size;
		for (status = (*db->seq)(db, &key, &dat, R_CURSOR);
			status == RET_SUCCESS;
			status = (*db->seq)(db, &key, &dat, R_NEXT)) {
			if (flags & DBOP_PREFIX) {
				if (strncmp((char *)key.data, dbop->key, dbop->keylen))
					return NULL;
			} else {
				if (strcmp((char *)key.data, dbop->key))
					return NULL;
			}
			if (preg && regexec(preg, (char *)key.data, 0, 0, 0) != 0)
				continue;
			break;
		}
	} else {
		dbop->keylen = dbop->key[0] = 0;
		for (status = (*db->seq)(db, &key, &dat, R_FIRST);
			status == RET_SUCCESS;
			status = (*db->seq)(db, &key, &dat, R_NEXT)) {
			/* skip meta records */
			if (ismeta(key.data) && !(dbop->openflags & DBOP_RAW))
				continue;
			if (preg && regexec(preg, (char *)key.data, 0, 0, 0) != 0)
				continue;
			break;
		}
	}
	dbop->lastdat = (char *)dat.data;
	dbop->lastsize = dat.size;
	dbop->lastkey = (char *)key.data;
	dbop->lastkeysize = key.size;
	switch (status) {
	case RET_SUCCESS:
		break;
	case RET_ERROR:
		die("dbop_first failed.");
	case RET_SPECIAL:
		return (NULL);
	}
	dbop->ioflags = flags;
	if (flags & DBOP_KEY) {
		strlimcpy(dbop->prev, (char *)key.data, sizeof(dbop->prev));
		return (char *)key.data;
	}
	return ((char *)dat.data);
}
/*
 * dbop_next: get next record. 
 * 
 *	i)	dbop	dbop descripter
 *	r)		data
 *
 * Db_next always skip meta records.
 */
const char *
dbop_next(DBOP *dbop)
{
	DB *db = dbop->db;
	int flags = dbop->ioflags;
	DBT key, dat;
	int status;

	if (dbop->unread) {
		dbop->unread = 0;
		return dbop->lastdat;
	}
	while ((status = (*db->seq)(db, &key, &dat, R_NEXT)) == RET_SUCCESS) {
		assert(dat.data != NULL);
		/* skip meta records */
		if (!(dbop->openflags & DBOP_RAW)) {
			if (flags & DBOP_KEY && ismeta(key.data))
				continue;
			else if (ismeta(dat.data))
				continue;
		}
		if (flags & DBOP_KEY) {
			if (!strcmp(dbop->prev, (char *)key.data))
				continue;
			if (strlen((char *)key.data) > MAXKEYLEN)
				die("primary key too long.");
			strlimcpy(dbop->prev, (char *)key.data, sizeof(dbop->prev));
		}
		dbop->lastdat	= (char *)dat.data;
		dbop->lastsize	= dat.size;
		dbop->lastkey = (char *)key.data;
		dbop->lastkeysize = key.size;
		if (flags & DBOP_PREFIX) {
			if (strncmp((char *)key.data, dbop->key, dbop->keylen))
				return NULL;
		} else if (dbop->keylen) {
			if (strcmp((char *)key.data, dbop->key))
				return NULL;
		}
		if (dbop->preg && regexec(dbop->preg, (char *)key.data, 0, 0, 0) != 0)
			continue;
		return (flags & DBOP_KEY) ? (char *)key.data : (char *)dat.data;
	}
	if (status == RET_ERROR)
		die("dbop_next failed.");
	return NULL;
}
/*
 * dbop_unread: unread record to read again.
 * 
 *	i)	dbop	dbop descripter
 *
 * Dbop_next will read this record later.
 */
void
dbop_unread(DBOP *dbop)
{
	dbop->unread = 1;
}
/*
 * dbop_lastdat: get last data
 * 
 *	i)	dbop	dbop descripter
 *	r)		last data
 */
const char *
dbop_lastdat(DBOP *dbop, int *size)
{
	if (size)
		*size = dbop->lastsize;
	return dbop->lastdat;
}
/*
 * get_flag: get flag value
 */
const char *
dbop_getflag(DBOP *dbop)
{
	int size;
	const char *dat = dbop_lastdat(dbop, &size);
	const char *flag = "";
	/*
	 * Dat format is like follows.
	 * dat 'xxxxxxx\0ffff\0'
	 *      (data)   (flag)
	 */
	if (dat) {
		int i = strlen(dat) + 1;
		if (size > i)
			flag = dat + i;
	}
	return flag;
}
/*
 * dbop_getoption: get option
 */
const char *
dbop_getoption(DBOP *dbop, const char *key)
{
	static char buf[1024];
	const char *p;

	if ((p = dbop_get(dbop, key)) == NULL)
		return NULL;
	if (dbop->lastsize <= strlen(key))
		die("illegal format (dbop_getoption).");
	for (p += strlen(key); *p && isspace((unsigned char)*p); p++)
		;
	strlimcpy(buf, p, sizeof(buf));
	return buf;
}
/*
 * dbop_putoption: put option
 */
void
dbop_putoption(DBOP *dbop, const char *key, const char *string)
{
	char buf[1024];

	if (string)
		snprintf(buf, sizeof(buf), "%s %s", key, string);
	else
		snprintf(buf, sizeof(buf), "%s", key);
	dbop_put(dbop, key, buf);
}
/*
 * dbop_getversion: get format version
 */
int
dbop_getversion(DBOP *dbop)
{
	int format_version = 1;			/* default format version */
	const char *p;

	if ((p = dbop_getoption(dbop, VERSIONKEY)) != NULL)
		format_version = atoi(p);
	return format_version;
}
/*
 * dbop_putversion: put format version
 */
void
dbop_putversion(DBOP *dbop, int version)
{
	char number[32];

	snprintf(number, sizeof(number), "%d", version);
	dbop_putoption(dbop, VERSIONKEY, number);
}
/*
 * dbop_close: close db
 * 
 *	i)	dbop	dbop descripter
 */
void
dbop_close(DBOP *dbop)
{
	DB *db = dbop->db;

	/*
	 * Load sorted tag records and write them to the tag file.
	 */
	if (dbop->sortout != NULL) {
		STRBUF *sb = strbuf_open(256);
		char *p;

		/*
		 * End of the former stage of sorted writing.
		 * fclose() and sortout = NULL is important.
		 *
		 * fclose(): enables reading from sortin descriptor.
		 * sortout = NULL: makes the following dbop_put write to the tag file directly.
		 */
		fclose(dbop->sortout);
		dbop->sortout = NULL;
		/*
		 * The last stage of sorted writing.
		 */
		while (strbuf_fgets(sb, dbop->sortin, STRBUF_NOCRLF)) {
			for (p = strbuf_value(sb); *p && *p != SORT_SEP; p++)
				;
			if (!*p)
				die("unexpected end of record.");
			*p++ = '\0';
			dbop_put(dbop, strbuf_value(sb), p);
		}
		fclose(dbop->sortin);
		strbuf_close(sb);
		terminate_sort_process(dbop);
	}
#ifdef USE_DB185_COMPAT
	(void)db->close(db);
#else
	/*
	 * If dbname = NULL, omit writing to the disk in __bt_close().
	 */
	(void)db->close(db, dbop->dbname[0] == '\0' ? 1 : 0);
#endif
	if (dbop->dbname[0] != '\0') {
		if (dbop->perm && chmod(dbop->dbname, dbop->perm) < 0)
			die("cannot change file mode.");
	}
	(void)free(dbop);
}
