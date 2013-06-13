#include <stdio.h>

int T[1024];

int kmp_search(char W[], char S[])
{
    extern int T[];
    int m = 0;
    int i = 0;

    int cnt = 0;
    while (S[m + i] != '\0' && W[i] != '\0') {
        cnt++;
        if (S[m + i] == W[i]) {
            ++i;
        } else {
            m += i - T[i];
            if (i > 0) i = T[i];
            printf("m is %d, i is %d, m+i is %d\n", m, i, m+i);
        }
    }

    printf("cnt is %d\n", cnt);
    if (W[i] == '\0') {
        return m;
    } else {
        return -1;
    }
}

void kmp_table(char W[])
{
    extern int T[];
    int i = 2;
    int j = 0;
    T[0] = -1, T[1] = 0;
    while (W[i] != '\0') {
        if (W[i - 1] == W[j]) {
            T[i] = j + 1;
            ++j;
            ++i;
        } else if (j > 0) {
            j = T[j];
        } else {
            T[i] = 0;
            ++i;
            j = 0;
        }
    }

    return;
}

int main(int argc, char *argv[])
{
    kmp_table(argv[1]);
    printf("matched at %d\n", kmp_search(argv[1], argv[2]));
    int i;
    for (i = 0; argv[1][i]; i++) {
        printf("T[%d] is %d\n", i, T[i]);
    }
}
