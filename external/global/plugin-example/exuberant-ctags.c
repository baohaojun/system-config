/*
 * Copyright (c) 2010
 *      Tama Communications Corporation
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
#if !defined(_WIN32) || defined(__CYGWIN__)
#include <sys/wait.h>
#endif
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
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
#include <fcntl.h>

#include "parser.h"

/*
 * Function layer plugin parser sample
 */

#define TERMINATOR              "###terminator###"
#define LANGMAP_OPTION          "--langmap="
#define INITIAL_BUFSIZE         1024

static char *argv[] = {
        "ctags-ajoke",
        NULL,
        "-xu",
        "--filter",
        "--filter-terminator=" TERMINATOR "\n",
        "--extra=+q",
        "--c-kinds=+p",
        NULL
};
static pid_t pid;
static FILE *ip, *op;
static char *linebuf;
static size_t bufsize;

static void
copy_langmap_converting_cpp(char *dst, const char *src)
{
        const char *p;

        if (strncmp(src, "cpp:", 4) == 0) {
                memcpy(dst, "c++:", 4);
                dst += 4;
                src += 4;
        }
        while ((p = strstr(src, ",cpp:")) != NULL) {
                memcpy(dst, src, p - src);
                dst += p - src;
                memcpy(dst, ",c++:", 5);
                dst += 5;
                src = p + 5;
        }
        strcpy(dst, src);
}

static void
start_ctags(const struct parser_param *param)
{
        int opipe[2], ipipe[2];

        argv[1] = malloc(sizeof(LANGMAP_OPTION) + strlen(param->langmap));
        if (argv[1] == NULL)
                param->die("short of memory.");
        memcpy(argv[1], LANGMAP_OPTION, sizeof(LANGMAP_OPTION) - 1);
        copy_langmap_converting_cpp(argv[1] + sizeof(LANGMAP_OPTION) - 1, param->langmap);

        if (pipe(opipe) < 0 || pipe(ipipe) < 0)
                param->die("cannot create pipe.");
        pid = fork();
        if (pid == 0) {
                /* child process */
                close(opipe[1]);
                close(ipipe[0]);
                if (dup2(opipe[0], STDIN_FILENO) < 0
                 || dup2(ipipe[1], STDOUT_FILENO) < 0)
                        param->die("dup2 failed.");
                close(opipe[0]);
                close(ipipe[1]);
                execvp("ctags-ajoke", argv);
                param->die("execvp failed: %s", strerror(errno));
        }
        /* parent process */
        if (pid < 0)
                param->die("fork failed.");
        free(argv[1]);
        close(opipe[0]);
        close(ipipe[1]);

        ipipe[1] = opipe[1]; //this is a hack!

        int i=0;
        for (; i < 2; i++) {
                int flags;
                flags = fcntl(ipipe[i], F_GETFD);
                if (flags == -1) {
                        param->die("fcntl failed.");
                }
                flags |= FD_CLOEXEC;
                if (fcntl(ipipe[i], F_SETFD, flags) == -1) {
                        param->die("fcntl failed.");
                }
        }

        ip = fdopen(ipipe[0], "r");
        op = fdopen(opipe[1], "w");
        if (ip == NULL || op == NULL)
                param->die("fdopen failed.");

        bufsize = INITIAL_BUFSIZE;
        linebuf = malloc(bufsize);
        if (linebuf == NULL)
                param->die("short of memory.");
}

#ifdef __GNUC__
static void terminate_ctags(void) __attribute__((destructor));
#endif

static void
terminate_ctags(void)
{
        if (op == NULL)
                return;
        free(linebuf);
        fclose(op);
        fclose(ip);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR)
                ;
}

static char *
get_line(const struct parser_param *param)
{
        size_t linelen = 0;

        for (;;) {
                if (fgets(linebuf + linelen, bufsize - linelen, ip) == NULL) {
                        if (linelen == 0)
                                return NULL;
                        break;
                }
                linelen += strlen(linebuf + linelen);
                if (linelen < bufsize - 1 || linebuf[linelen - 1] == '\n'
                 || feof(ip))
                        break;
                bufsize *= 2;
                linebuf = realloc(linebuf, bufsize);
                if (linebuf == NULL)
                        param->die("short of memory.");
        }
        while (linelen-- > 0
                && (linebuf[linelen] == '\n' || linebuf[linelen] == '\r'))
                linebuf[linelen] = '\0';
        return linebuf;
}

static void
put_line(char *ctags_x, const struct parser_param *param)
{
        int lineno;
        char *p, *tagname, *filename, *typename;
        int typelen;

        filename = strstr(ctags_x, param->file);
        if (filename == NULL || filename == ctags_x)
                return;
        p = filename - 1;
        if (!isspace((unsigned char)*p))
                return;
        while (p >= ctags_x && isspace((unsigned char)*p))
                *p-- = '\0';
        if (p < ctags_x)
                return;
        if (!isdigit((unsigned char)*p))
                return;
        while (p >= ctags_x && isdigit((unsigned char)*p))
                p--;
        if (p < ctags_x)
                return;
        lineno = atoi(p + 1);
        if (!isspace((unsigned char)*p))
                return;
        while (p >= ctags_x && isspace((unsigned char)*p))
                *p-- = '\0';
        if (p < ctags_x)
                return;
        while (p >= ctags_x && !isspace((unsigned char)*p))
                p--;
        if (p < ctags_x)
                return;
        typename = p + 1;
        while (p >= ctags_x && isspace((unsigned char)*p))
                *p-- = '\0';
        if (p < ctags_x)
                return;
        while (p >= ctags_x && !isspace((unsigned char)*p))
                p--;
        tagname = p + 1;

        p = filename + strlen(param->file);
        if (*p != '\0') {
                if (!isspace((unsigned char)*p))
                        return;
                *p++ = '\0';
                while (isspace((unsigned char)*p))
                        p++;
        }

        typelen = strlen(typename);

        p -= typelen + 2;
        memmove(p, typename, typelen);
        p[typelen] = ':';
        p[typelen + 1] = '\t';

        param->put(PARSER_DEF, tagname, lineno, param->file, p, param->arg);
}

static int handle_special_files(const struct parser_param *param, int filename_len, const char* ext, const char* fakeline)
{
        int extlen = strlen(ext);
        if (filename_len > extlen && ! strcmp(param->file + filename_len - extlen, ext)) {
                char filename[1024];
                char* p = param->file + filename_len - extlen;
                while (p > param->file && *--p != '/')
                        ;
                p++;
                strncpy(filename, p, sizeof filename);
                filename[param->file + filename_len - p - extlen] = '\0';
                param->put(PARSER_DEF, filename, 1, param->file, fakeline, param->arg);
                return 1;
        }
        return 0;
}

void
parser(const struct parser_param *param)
{
        char *ctags_x;
        int filename_len;

        assert(param->size >= sizeof(*param));

        if (op == NULL)
                start_ctags(param);

        filename_len = strlen(param->file);

        handle_special_files(param, filename_len, ".xml", "xml:\t***");
        if (handle_special_files(param, filename_len, ".9.png", "9png:\t***") ||
            handle_special_files(param, filename_len, ".png", "png:\t***") ||
            handle_special_files(param, filename_len, ".jpg", "jpg:\t***")) {
                return;
        }

        /* Write path of input file to pipe. */
        fputs(param->file, op);
        putc('\n', op);
        fflush(op);

        /* Read output of ctags command. */
        for (;;) {
                ctags_x = get_line(param);
                if (ctags_x == NULL)
                        param->die("unexpected EOF.");
                if (strcmp(ctags_x, TERMINATOR) == 0)
                        break;
                put_line(ctags_x, param);
        }
}
