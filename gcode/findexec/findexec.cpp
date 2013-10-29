/**************************
 * Copyright (c) 2005-2005 baohaojun <baohaojun@gmail.com>
 *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
 *
 *****************************/
#define _WIN32_WINNT 0x0500
#define WINVER 0x0500
#define _UNICODE

#include <windows.h>
#include <stdio.h>
#include <map>
#include <vector>
#include <string>
#include <psapi.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"
using namespace std;

#include "findexec.h"
/* these are global vars */
LPWSTR match_class=NULL;
LPWSTR match_program=NULL;
LPWSTR match_title=NULL;
wstring nofound_exec_program;
wstring found_exec_program;
LPWSTR match_save=NULL;
LPWSTR prog_name=NULL;
bool act_first=false;
bool minimize_others=false;
bool print_windows=false;
bool iconic_first_test=true;



BOOL BBExecute_string(const wstring& command_string, HANDLE* hProcess);

bool IsChildWnd(HWND hw)
{
    if (GetParent(hw)) return true;
    return false;
}

void debug_window(HWND hwnd)
{
    wchar_t buff[1024];
    GetWindowTextW(hwnd, buff, 1024);
    _wcslwr_s(buff, wcslen(buff)+1);
    wprintf(L"window name %s ", buff);

    GetClassNameW(hwnd, buff, 1024);
    _wcslwr_s(buff, wcslen(buff)+1);
    wprintf(L"class %s\n", buff);
    fflush(stdout);

}

void debug_window(vector<HWND>& v_win)
{
    for(unsigned int i=0; i<v_win.size(); i++) {
        debug_window(v_win[i]);
    }
}

#define debug_window(...) do {\
        BHJDEBUG("");\
        debug_window(##__VA_ARGS__);\
    } while(0)

void print_found(vector<HWND>& v_win)
{
    for (unsigned int i=0; i<v_win.size(); i++) {
        printf("%d\n", v_win[i]);
    }
}

void found_action(vector<HWND>& v_win)
{
    if (!v_win.size()) {
        return;
    }
    debug_window(v_win);
    unsigned int i;
    if (act_first) {
        i=0;
        if (IsIconic(v_win[i]) && iconic_first_test) {
            i=v_win.size()-1;
        }
    } else {
        i=v_win.size()-1;
    }

    if (print_windows) {
        print_found(v_win);
        return;
    }

    HWND hTarW=v_win[i];
    hTarW = GetLastActivePopup(hTarW);
    //debug_window(hTarW);
    if (IsChildWnd(hTarW))
        return;

    if (IsIconic(hTarW))
        PostMessage(hTarW, WM_SYSCOMMAND, SC_RESTORE, 0);

    SetForegroundWindow(hTarW);

    if (minimize_others) {
        for (unsigned int j=0; j<v_win.size(); j++) {
            if (i==j) {
                continue;
            }
            HWND hwnd=v_win[j];
            if (IsChildWnd(hwnd))
                continue;
            if (!IsIconic(hwnd)) {
                DWORD_PTR dwResult = 0;
                SendMessageTimeout(hwnd, WM_SYSCOMMAND, SC_MINIMIZE, 0, SMTO_NORMAL, 1000, &dwResult);
            }
        }
    }

    SetForegroundWindow(hTarW);

}

int run_command(wstring program_str)
{


    if (program_str.size()==0) {
        return -1;
    }

    HANDLE hProcess = NULL;
    BOOL isOK = BBExecute_string(program_str, &hProcess);
    if (!isOK || !hProcess)
    {
        return -1;
    }

    if (match_save) {
        FILE *fp = _wfopen(match_save, L"w");
        DWORD pid = GetProcessId(hProcess);
        fwrite(&pid, sizeof(DWORD), 1, fp);
        fclose(fp);
    }

    CloseHandle( hProcess );
    return 0;
}

bool my_get_window_module_file_name(HWND hwnd, wchar_t buff[], unsigned int size)
{
    DWORD proc_id;
    GetWindowThreadProcessId(hwnd, &proc_id);
    HANDLE proc_handle = OpenProcess(PROCESS_QUERY_INFORMATION |
                                     PROCESS_VM_READ,
                                     FALSE, proc_id);
    if(!GetProcessImageFileNameW(proc_handle,
                             buff,
                             size)) {
        CloseHandle(proc_handle);
        printf("hello world for pid %d %s %s %d\n", (int)proc_id, __FILE__, __FUNCTION__, __LINE__);
        return false;
    }
    CloseHandle(proc_handle);

    _wcslwr_s(buff, wcslen(buff)+1);
    return true;
}

bool window_match(HWND wnd)
{

    if (match_class) {
        wchar_t buff[1024];
        GetClassNameW(wnd, buff, 1024);
        _wcslwr_s(buff, wcslen(buff)+1);
        if (!wcsstr(buff, match_class))
            return false;
    }

    if (match_program) {
        wchar_t buff[1024];

        if (!my_get_window_module_file_name(wnd, buff, 1024))
            return false;

        if (!wcsstr(buff, match_program))
            return false;
    }

    if (match_title) {
        wchar_t buff[1024];
        GetWindowTextW(wnd, buff, 1024);
        _wcslwr_s(buff, wcslen(buff)+1);
        if (!wcsstr(buff, match_title))
            return false;
    }

    if (match_save) {
        FILE *fp = _wfopen(match_save, L"r");
        if (!fp) {
            return false;
        }
        DWORD pidsave;
        fread(&pidsave, sizeof(pidsave), 1, fp);
        fclose(fp);

        DWORD pidthis;
        GetWindowThreadProcessId(wnd, &pidthis);
        if (pidsave != pidthis) {
            return false;
        }
    }

    return true;
}

bool IsWindowSwitchable(HWND wnd)
{
    DWORD style = GetWindowLong(wnd, GWL_STYLE);

    if (style&WS_OVERLAPPED)
        return true;
    DWORD ex_style = GetWindowLong(wnd, GWL_EXSTYLE);
    if (ex_style&WS_EX_TOOLWINDOW)
        return false;
    if (ex_style & WS_EX_APPWINDOW)
        return true;
    if (style&WS_SYSMENU) {
        HWND wnd_owner = GetWindow(wnd, GW_OWNER);
        HWND wnd_shell = GetShellWindow();
        if (!wnd_owner || GetWindow(wnd_owner, GW_OWNER)==wnd_shell)
            return true;
    }
    return false;
}

BOOL CALLBACK EnumWindowsProc(HWND hTarW, LPARAM lParam)
{
    vector<HWND>* pv_win=(vector<HWND>*)lParam;

    if (IsWindowVisible(hTarW) && !IsChildWnd(hTarW) && IsWindowSwitchable(hTarW) && window_match(hTarW)) {
    pv_win->push_back(hTarW);
    }
    return TRUE;
}

int find_exec()
{
    vector<HWND> v_win;
    DWORD thread_id = GetCurrentThreadId();
    HDESK hdesk = GetThreadDesktop(thread_id);
    v_win.resize(0);
    EnumDesktopWindows(hdesk, EnumWindowsProc, (LPARAM)&v_win);

    if(v_win.size()) {
    found_action(v_win);
        run_command(found_exec_program);
        return 0;
    }
    else {
    return run_command(nofound_exec_program);
    }
}

void Usage()
{

    wprintf(L"%s, find the window or execute a program\n\n", prog_name);
    wprintf(L"Usage: %s OPTIONS EXEC\n\n", prog_name);
    wprintf(L"Main Options:\n");
    wprintf(L"    -h             :print this usage help\n");
    wprintf(L"    -c class       :the window class to match\n");
    wprintf(L"    -p program     :the program module of the window to match\n");
    wprintf(L"    -t title       :the title of the window to match\n");
    wprintf(L"    -f save        :the file to save the pid of the program\n");
    wprintf(L"    -fe exec args -fe\n");
    wprintf(L"                   :the command to exec when matches found\n");
    wprintf(L"    -F bool        :bring to top the first matching window in z-order\n");
    wprintf(L"    -mo            :minimize others\n");
    wprintf(L"    -C             :cycle through windows of the same program as the currently focused window\n");
    wprintf(L"    -P             :print the matching windows' ids\n");
    wprintf(L"    -nift          :no iconic first test, reverse meaning of -F if first is iconic\n");
    wprintf(L"    EXEC args      :the program to execute if no window is found\n");
    wprintf(L"\n");
    wprintf(L"NOTE: you can specify multiple matching criteriors\n");
    wprintf(L"      but at least one criterior is required\n");
    exit(0);

}

void check_argc(int i, int argc)
{
    if (i >= argc)
        Usage();
}
void build_exec_string(wstring& string, wchar_t *arg)
{
    string+=L"\"";

    for (int i=0; arg[i]; i++) {
        if (arg[i]==L'\"') {
            string += L"\"\"";
        } else {
            string += arg[i];
        }
    }
    string += L"\" ";
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                   int nCmdShow)
{
    int argc;
    LPWSTR pWCmdLine=GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(pWCmdLine, &argc);

    prog_name = _wcsdup(wcsrchr(argv[0], PATH_SEPW)?wcsrchr(argv[0], PATH_SEPW)+1:argv[0]);

    if (argc == 1) {
        Usage();
    }

    for (int i=1; i<argc; i++) {
        if (!wcscmp(argv[i], L"-h")) {
            Usage();
        } else if(!wcscmp(argv[i], L"-c")) {
            check_argc(++i, argc);
            match_class = _wcsdup(argv[i]);
        } else if(!wcscmp(argv[i], L"-p")) {
            check_argc(++i, argc);
            match_program = _wcsdup(argv[i]);
        } else if(!wcscmp(argv[i], L"-t")) {
            check_argc(++i, argc);
            match_title = _wcsdup(argv[i]);
        } else if(!wcscmp(argv[i], L"-f")) {
            check_argc(++i, argc);
            match_save = _wcsdup(argv[i]);
        } else if(!wcscmp(argv[i], L"-F")) {
            check_argc(++i, argc);
            act_first = _wtoi(argv[i]);
        } else if(!wcscmp(argv[i], L"-mo")) {
            minimize_others = true;
        } else if (!wcscmp(argv[i], L"-C")) {

            HWND hwnd=GetForegroundWindow();
            wchar_t buff[1024];
            GetClassNameW(hwnd, buff, 1024);
            _wcslwr_s(buff, wcslen(buff)+1);
            if (wcsstr(buff, L"emacs")) {
                match_class = _wcsdup(L"emacs");
                continue;
            }

            if (!my_get_window_module_file_name(hwnd, buff, 1024)) {
                return 0;
            }
            match_program = _wcsdup(wcsrchr(buff, PATH_SEPW)?wcsrchr(buff, PATH_SEPW)+1:buff);
        } else if (!wcscmp(argv[i], L"-P")) {
            print_windows = true;
        } else if (!wcscmp(argv[i], L"-fe")) {
            check_argc(++i, argc);
            if (!wcscmp(argv[i], L"-fe")) {
                Usage();
            } else {
                build_exec_string(found_exec_program, argv[i]);
            }

            while(++i<argc && wcscmp(argv[i], L"-fe")) {
                build_exec_string(found_exec_program, argv[i]);
            }
            wprintf(L"found_exec_program is %s\n", found_exec_program.c_str());
        } else if (!wcscmp(argv[i], L"-nift")) {
            iconic_first_test = false;
        }else {
            build_exec_string(nofound_exec_program, argv[i]);
            while(++i < argc) { //all the rest is for nofound_exec_program
                build_exec_string(nofound_exec_program, argv[i]);
            }
            wprintf(L"nofound_exec_program is %s\n", nofound_exec_program.c_str());
        }
    }

    if (match_class)
        _wcslwr_s(match_class, wcslen(match_class)+1);

    if (match_program)
        _wcslwr_s(match_program, wcslen(match_program)+1);

    if (match_title)
        _wcslwr_s(match_title, wcslen(match_title)+1);

    if (!match_title && !match_program && !match_class)
        Usage();

    return find_exec();
}

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



BOOL ShellCommand(const char *command, const char *work_dir, bool wait, HANDLE* hProcess)
{
    SHELLEXECUTEINFO sei; char path[MAX_PATH]; BOOL r;
    NextToken(path, &command);
    ZeroMemory(&sei, sizeof sei);
    sei.cbSize = sizeof sei;
    sei.fMask = SEE_MASK_DOENVSUBST;
    sei.fMask |= SEE_MASK_NOCLOSEPROCESS;
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
        *hProcess = sei.hProcess;
        return TRUE;
    }
    return FALSE;
}

BOOL BBExecute_string(const wstring& command_string, HANDLE* hProcess)
{
    size_t buf_size = command_string.size()*sizeof(wchar_t) + 0xff;
    char *command = (char *)malloc(buf_size);
    wcstombs(command, command_string.c_str(), buf_size);
    BOOL ret = ShellCommand(command, NULL, false, hProcess);
    free(command);
    return ret;
}
