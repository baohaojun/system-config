// echo_vc6.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <process.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <windows.h>
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
	FILE * fp = fopen("q:\\echo_vc6.log", "a+t");
	if(!fp) {
		fp = fopen("q:\\echo_vc6.log", "w+t");
		if (!fp) {
			//error
			return -1;
		}
	}
	
	printf("cmd line: `%s'\n", GetCommandLine());
	fprintf(fp, "cmd line: `%s'\n", GetCommandLine());
	for (int i=0; i<argc; i++) {
		printf("`%s' ", argv[i]);
		fprintf(fp, "`%s' ", argv[i]);
	}
	printf("\n");
	fprintf(fp, "\n");
	fclose(fp);
	system("notepad q:\\echo_vc6.log");
	return 0;
}
