#include "hotkey_hook.h"
#include "bhjdebug.h"
#include <shellapi.h>

#define IS_SPC(c) ((unsigned char)(c) <= 32)
#define IS_SLASH(c) ((c) == '\\' || (c) == '/')

LPSTR NextToken(LPSTR buf, LPCSTR *string, const char *delims = NULL);

LPSTR NextToken(LPSTR buf, LPCSTR *string, const char *delims)
{
    char c, q=0;
    char* bufptr = buf;
    const char* s = *string;
    bool delim_spc = NULL == delims || strchr(delims, ' ');

    while (0 != (c=*s))
    {
        s++;
        if (0==q)
        {
            // User-specified delimiter
            if ((delims && strchr(delims, c)) || (delim_spc && IS_SPC(c)))
                break;

            if ('\"'==c || '\''==c) q=c;
            else
            if (IS_SPC(c) && bufptr==buf)
                continue;
        }
        else
        if (c==q)
        {
            q=0;
        }
        *bufptr++ = c;
    }
    while (bufptr > buf && IS_SPC(bufptr[-1])) bufptr--;
    *bufptr = '\0';
    while (IS_SPC(*s) && *s) s++;
    *string = s;
    return buf;
}



bool ShellCommand(const char *command, const char *work_dir, bool wait)
{
    SHELLEXECUTEINFO sei; char path[MAX_PATH]; BOOL r;
    NextToken(path, &command);
    ZeroMemory(&sei, sizeof sei);
    sei.cbSize = sizeof sei;
    sei.fMask = SEE_MASK_DOENVSUBST;
    if (wait) sei.fMask |= SEE_MASK_NOCLOSEPROCESS;
    sei.lpFile = path;
    sei.lpParameters = command;
    sei.lpDirectory = work_dir;
    sei.nShow = SW_SHOWNORMAL;
    r = ShellExecuteEx(&sei);
    if (r)
    {
        if (wait) {
            WaitForSingleObject(sei.hProcess, INFINITE);
        }
        CloseHandle(sei.hProcess);
    }
    return r;
}

BOOL BBExecute_string(const char *command, bool no_errors)
{
    return ShellCommand(command, NULL, false);
}
