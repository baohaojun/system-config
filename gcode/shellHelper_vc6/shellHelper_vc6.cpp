// shellHelper_vc6.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <windows.h>
#include <process.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#define ENABLE_BHJDEBUG

char *strdup_quote(char *res, const char* src)
{
	int src_len = strlen(src);

	res[0] = '"';
	int j = 1;
	for (int i=0; i<src_len; i++) {
		if (src[i] == '"' || src[i] == '\\') {
			res[j++] = '\\';
		}
		res[j++] = src[i];
	}
	res[j] = '"';
	return res+j+1; //always return the next position to write
}

void chomp(char *line)
{
	for (int i=0; line[i]; i++) {
		if (line[i] == '\r' || line[i] == '\n') {
			line[i] = 0;
		}
	}
}

int system2(char* cmd)
{
	STARTUPINFO si = {0};
	PROCESS_INFORMATION pi = {0};

	si.cb = sizeof(si);

	// Start the child process. 
	if( !CreateProcess( NULL,   // No module name (use command line). 
			    cmd, 
			    NULL,             // Process handle not inheritable. 
			    NULL,             // Thread handle not inheritable. 
			    true,            // Set handle inheritance to FALSE. 
			    0,                // No creation flags. 
			    NULL,             // Use parent's environment block. 
			    NULL,             // Use parent's starting directory. 
			    &si,              // Pointer to STARTUPINFO structure.
			    &pi )             // Pointer to PROCESS_INFORMATION structure.
		) 
	{
		return -1;
	}

	WaitForSingleObject(pi.hProcess, INFINITE);

	DWORD exitCode = 0;
	if (!GetExitCodeProcess(pi.hProcess, &exitCode)) {
		printf(" Error: GetExitCodeProcess %d\n", GetLastError());
		fflush(stdout);
	}
	CloseHandle( pi.hProcess );
	CloseHandle( pi.hThread );

	return exitCode;

}

int main(int argc, char* argv[])
{

	char bash_path[1024] = {0};

	FILE * fp = fopen("c:\\.bash-loc", "rb");
	if (!fp) {
		fprintf(stderr, "Error: can't open `%s'\n", "c:\\.bash-loc");
		exit(0);
	}

	fgets(bash_path, 1024, fp);
	chomp(bash_path);

	fprintf(stderr, "Bash is found at `%s'\n", bash_path);
	
	char shellHerlperSh[]= "~/system-config/bin/windows/shellHelper_vc6.sh";

	int new_cmd_len = strlen(bash_path) + sizeof(shellHerlperSh) + 20; 
    fprintf(stderr, "command line is %s\n", GetCommandLine());
	for (int i=0; i<argc; i++) {
		new_cmd_len += strlen(argv[i])*2 +5;
        fprintf(stderr, "argv[%d] is '%s'\n", i, argv[i]);
	}


	char *new_cmd_str = (char *)calloc(new_cmd_len, sizeof(char));
	
	char *next_pos = new_cmd_str + sprintf(new_cmd_str, "%s %s ", bash_path, shellHerlperSh);

	for (i=0; i<argc; i++) {
		next_pos = strdup_quote(next_pos, argv[i]);
		if (i<argc-1) {
			*next_pos++ = ' ';
		}
	}        

	fprintf(stderr, "Will run `%s'\n", new_cmd_str);
	fflush(stderr);

	return system2(new_cmd_str);

}
