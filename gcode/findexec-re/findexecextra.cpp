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
#include <windows.h>
#include <stdio.h>
#include <map>
#include <vector>
#include <string>

using namespace std;

#using <system.dll>

using namespace System;
using namespace System::Text::RegularExpressions;

wstring g_wndclass;
wstring g_appclass;
wstring g_appname;


bool IsChildWnd(HWND hw)
{
    if (GetParent(hw)) return true; 
    return false;
}

void found_action(vector<HWND>& v_win)
{
    for (int i=0; i<v_win.size(); i++) {

        HWND hTarW=v_win[i];
        if (IsChildWnd(hTarW))continue;

        WINDOWPLACEMENT wp;
        wp.length = sizeof(wp);
        GetWindowPlacement(hTarW, &wp);
        RECT r = wp.rcNormalPosition;
        if (r.top >700) continue;
        

        if (IsIconic(hTarW))
            PostMessage(hTarW, WM_SYSCOMMAND, SC_RESTORE, 0);
    
        if (r.top <700)
            SetForegroundWindow(hTarW);
    }
}

void run_command()
{
    wstring cmd = g_appname;
    
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );

    // Start the child process. 
    if( !CreateProcess( NULL,   // No module name (use command line). 
                        (LPWSTR)cmd.c_str(), // Command line. 
                        NULL,             // Process handle not inheritable. 
                        NULL,             // Thread handle not inheritable. 
                        FALSE,            // Set handle inheritance to FALSE. 
                        0,                // No creation flags. 
                        NULL,             // Use parent's environment block. 
                        NULL,             // Use parent's starting directory. 
                        &si,              // Pointer to STARTUPINFO structure.
                        &pi )             // Pointer to PROCESS_INFORMATION structure.
        ) 
    {
        return;
    }

    FILE *fp = _wfopen(g_appclass.c_str(), TEXT("w"));
    
    fwrite(&pi.dwProcessId, sizeof(DWORD), 1, fp);
    fclose(fp);
    // Wait until child process exits.
    //WaitForSingleObject( pi.hProcess, INFINITE );

    // Close process and thread handles. 
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );
}


BOOL CALLBACK EnumWindowsProc(HWND hTarW, LPARAM lParam)
{
    vector<HWND>* pv_win=(vector<HWND>*)lParam;
  
    if (!IsChildWnd(hTarW) && IsWindowVisible(hTarW)) {
        pv_win->push_back(hTarW);
    }
    return TRUE;
}

void find_exec()
{

    vector<HWND> v_win;
    v_win.resize(0);
    vector<HWND> v_matched_win;
    EnumWindows(EnumWindowsProc, (LPARAM)&v_win);
    unsigned int i;
    if(i = v_win.size()) {
        int words = 0;
        if (g_wndclass.length() == 0) {
            return;
        }

        String^ pattern = gcnew String (g_wndclass.c_str());

        //Console::WriteLine( "pattern : '{0}'", pattern );

   
        Regex^ regex = gcnew Regex( pattern );

        for (int n=0; n<i; n++) {
            HWND hwnd = v_win[n];
            wchar_t buff_class_name[1024];
            GetClassName(hwnd, buff_class_name, 1024);
            wstring buff = _wcslwr(buff_class_name);

            String^ buff_str = gcnew String (buff.c_str());
        
            Match^ match = regex->Match( buff_str ); 
   
            if ( match->Success )
            {
                FILE *fp = _wfopen(g_appclass.c_str(), TEXT("r"));
                if (!fp) {
                    break;
                }
                DWORD pidsave;
                fread(&pidsave, sizeof(pidsave), 1, fp);
                fclose(fp);
    
                DWORD pidthis;
                GetWindowThreadProcessId(hwnd, &pidthis);
                if (pidsave == pidthis) {            
                    v_matched_win.push_back(hwnd);
                }
            }
        }
    }
    
    if (v_matched_win.size()) {
        found_action(v_matched_win);
    } else {
        run_command();
    }
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                   int nCmdShow)
{
    int argc;
    LPWSTR pWCmdLine=GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(pWCmdLine, &argc);

    if (argc!=4) return -1;
    g_wndclass = argv[1];
    g_appclass = argv[2];
    g_appname = argv[3];

    find_exec();
    return 0;
}


