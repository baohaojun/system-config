#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int * init_fail_over(const char* str)
{
    int *fail_over = (int *)malloc(sizeof(int) * (strlen(str) + 1));
    int i;

    fail_over[0] = -1;
    for (i = 1; str[i]; i++) {
        if (str[i] == str[0]) {
            fail_over[i] = -1;
        } else {
            fail_over[i] = 0;
        }
    }

    for (i = 1; str[i]; i++) {
        if (str[i] == str[0]) {
            int j;

            for (j = 1; str[i + j]; j++) {
                if (str[j] != str[i + j] && fail_over[i + j] < j) {
                    fail_over[i + j] = j;
                    break;
                }
            }
        }
    }

    return fail_over;
}

int string_find(const char* t, const char*p, const int* fail_over)
{
    int i;
    int count = 0;
    for (i = 0; t[i]; i++) {
        int j;
        for (j = 0; t[i + j] && p[j]; j++) {
            count++;
            if (t[i + j] != p[j]) {
                if (fail_over[j] == -1) {
                    i = i + j;
                    break;
                }
                i = i + j - fail_over[j];
                j = fail_over[j];
                j--;
            }
        }
        if (! p[j]) {
            printf("count is %d\n", count);
            return i;
        }
    }
    printf("count is %d\n", count);
    return -1;
}

int main(int argc, char* argv[])
{
    printf("%d\n", string_find(argv[2], argv[1], init_fail_over(argv[1])));
}
