#include <stdio.h>
#include <windows.h>
int main(int argc, char *argv[])
{
    TerminateProcess(OpenProcess(0x00010000L|0x1F0FFF, 0, atoi(argv[1])), 0);
}
