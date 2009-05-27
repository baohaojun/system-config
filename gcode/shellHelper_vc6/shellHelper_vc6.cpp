// shellHelper_vc6.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <process.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char *strdup_quote(const char* src)
{
	int src_len = strlen(src);
	char *res = (char *)calloc(src_len*2 + 5, sizeof(char));
	res[0] = '"';
	int j = 1;
	for (int i=0; i<src_len; i++) {
		res[j++] = src[i];
		if (src[i] == '"') {
			res[j++] = src[i];
		}
	}
	res[j] = '"';
	return res;
}

int main(int argc, char* argv[])
{
	char **new_argv = (char**)calloc(argc+5, sizeof(char *));

	char bash_path[1024] = {0};

	
	FILE * fp = fopen("c:\\.bash-loc", "rb");
	if (!fp) {
		fprintf(stderr, "Error: can't open `%s'\n", "c:\\.bash-loc");
		exit(0);
	}

	fgets(bash_path, 1024, fp);
	for (int i=0; bash_path[i]; i++) {
		if (bash_path[i] == '\r' || bash_path[i] == '\n') {
			bash_path[i] = 0;
			break;
		}
	}
	
	fprintf(stderr, "Bash is found at `%s'\n", bash_path);
	new_argv[0] = bash_path;
	new_argv[1] = "~/bin/windows/shellHelper_vc6.sh";
	fprintf(stderr, "will run `%s' with args:\n", new_argv[1]);

	for (i=0; i<argc; i++) {
		new_argv[i+2] = strdup_quote(argv[i]);
		fprintf(stderr, "`%s' ", new_argv[i+2]);
	}        

	fprintf(stderr, "\n");
	fflush(stderr);
	new_argv[i+2] = NULL;
	_execvp(bash_path, new_argv);
	printf("end of shellHelper_vc6.exe\n");
	return 0;
}
