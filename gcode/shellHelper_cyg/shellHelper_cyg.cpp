#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
int main(int argc, char* argv[])
{
    char **new_argv = (char**)calloc(argc+5, sizeof(char *));

    char bash_path[1024] = "/bin/bash";

    fprintf(stderr, "Bash is found at `%s'\n", bash_path);
    new_argv[0] = bash_path;
    new_argv[1] = "shellHelper_cyg.sh";
    fprintf(stderr, "will run `%s' with args:\n", new_argv[1]);

    int i;
    for (i=0; i<argc; i++) {
        new_argv[i+2] = strdup(argv[i]);
        fprintf(stderr, "`%s' ", new_argv[i+2]);
    }

    fprintf(stderr, "\n");
    fflush(stderr);
    new_argv[i+2] = NULL;
    execvp(bash_path, new_argv);
    printf("end of shellHelper_cyg.exe\n");
    return 0;
}
