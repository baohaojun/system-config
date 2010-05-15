/*-------------------------------------------------------
  higher_lower.cpp -- find window by it's class/title
  (c) Bao Haojun, 2007
  -------------------------------------------------------*/

/*system headers*/
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#define _WIN32_WINNT 0x0501
#define _WIN32_IE 0x0600

#include <windows.h>
#include <shellapi.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <bhjdebug.h>
#include <psapi.h>

using namespace std;

/*project headers*/
#include "higher_lower.h"


/*global definitions*/
wstring looking_for = L"";
TCHAR szAppName[] = TEXT ("higher_lower");
vector<WINFO> v_winfo;
unsigned int idx_selected;
unsigned int idx_first_visible;
RECT main_window_rect;
bool control_down;
HWND main_window;
bool start_as_daemon = true;
int up_button_top;
int down_button_top;
int first_item_top;
int last_item_bottom;
int maximun_shown_items;
int main_window_visible;

/*function definitions*/

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
    int argc;
    LPWSTR pWCmdLine=GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(pWCmdLine, &argc);

    bool set_top = false;
    if (wcsstr(argv[0], L"puthigh")) {
        BHJDEBUG("puthigh is true");
        set_top = true;
    } else {
        BHJDEBUG("puthigh is false\n");
    }

    if (argc == 1) {
        set_window_topmost(GetForegroundWindow(), set_top);
        return 0;
    }

    for (int i=1; i<argc; i++) {
        BHJDEBUG("argv[i] is %d", argv[i]);
        set_window_topmost((HWND)_wtoi(argv[i]), set_top);
    }


}

void looking_for_foreground()
{
            HWND hwnd=GetForegroundWindow();
            wchar_t buff[1024];
            GetClassNameW(hwnd, buff, 1024);
            _wcslwr_s(buff, wcslen(buff)+1);
            if (wcsstr(buff, L"emacs")) {
                looking_for=L"emacs";
                return;
            }

            if (!my_get_window_module_file_name(hwnd, buff, 1024)) {
                return;
            }
            looking_for=wcsrchr(buff, PATH_SEPW)?wcsrchr(buff, PATH_SEPW)+1:buff;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{

    switch (message)
    {
    case WM_SETFOCUS:
        initialize_metrics();
        update_found_list();
        SetWindowPos(hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE|SWP_NOMOVE);
        break;
    case WM_KILLFOCUS:
        //hide_or_quit();
        break;
    case WM_LBUTTONDBLCLK:
        if (v_winfo.size()<1)
            return 0;
        handle_wm_lbuttondblclk(wParam, lParam);
        break;
    case WM_LBUTTONDOWN:
        if (v_winfo.size()<1)
            return 0;
        handle_wm_lbuttondown(wParam, lParam);
        break;
    case WM_MOUSEWHEEL:
        if (v_winfo.size()<1)
            return 0;
        handle_mouse_wheel(wParam, lParam);
        break;

    case WM_KEYDOWN:

        if (v_winfo.size()<1)
            return 0;

        if (wParam==VK_UP) {
            change_selected(hwnd, idx_selected + v_winfo.size() -1);
        } else if (wParam==VK_DOWN) {
            change_selected(hwnd, idx_selected+1);
        } 

        break;
    case WM_HOTKEY:
        if (HIWORD(lParam) == VK_ESCAPE) {
            hide_or_quit();
            return 0;
        }
        if (!main_window_visible) {
            if (HIWORD(lParam) == 'S') {
                looking_for.clear();
            } else if(HIWORD(lParam) == 'O') {
                looking_for_foreground();
            } 
            update_found_list();
            ShowWindow(hwnd, SW_SHOWNORMAL);
            UpdateWindow(hwnd);
            restore_window(hwnd);
            main_window_visible = true;
        } else {
            if (v_winfo.size()<1)
                return 0;
            change_selected(hwnd, idx_selected+1);
        }
        break;
        
    case WM_CHAR:
        if (wParam == L'\b' ) {
            if (looking_for.size()) {
                looking_for.erase(looking_for.end()-1);
                idx_selected = 0;
            }
        } else if (iswprint(wParam)) {
            idx_selected = 0;
            looking_for.push_back(wParam);
        } else if (wParam == VK_RETURN) {
            found_action();
            hide_or_quit();
            return 0;
        } else if (wParam == VK_ESCAPE) {
            if (looking_for.size()) {
                looking_for.clear();
            } else {
                hide_or_quit();
                return 0;
            }
        } else {
            wParam += 0x40;  //in case the control-r/s/n/p is pressed:-)
            if (v_winfo.size()<1) {
                return 0;
            } else if (wParam=='R' || wParam=='P') {
                change_selected(hwnd, idx_selected + v_winfo.size() -1);
            } else if (wParam=='S' || wParam=='N') {
                change_selected(hwnd, idx_selected + 1);
            }
            return 0;
        }

        update_found_list();
        InvalidateRect(hwnd, NULL, true);

        return 0;
        
    case WM_PAINT:
        draw_window(hwnd);
        return 0;

    case WM_DESTROY:
        hide_or_quit();
        return 0;
    }
    return DefWindowProc (hwnd, message, wParam, lParam);
}

void draw_window(HWND wnd)
{

    HDC hdc;
    PAINTSTRUCT ps;
    RECT rect;

    GetClientRect(wnd, &main_window_rect);
    get_looking_for_rect(rect);
    hdc = BeginPaint(wnd, &ps);

    DrawText (hdc, looking_for.c_str(), -1, &rect,
              DT_SINGLELINE | DT_CENTER | DT_VCENTER);
    

    for (unsigned int i=idx_first_visible; i <v_winfo.size(); i++) {
        if (!get_item_rect(rect, false, i)) {

            continue;
        }
        DrawText(hdc, v_winfo[i].title_class.c_str(), -1, &rect,
                 DT_SINGLELINE | DT_LEFT | DT_VCENTER);
        get_item_rect(rect, true, i);
        DrawIcon(hdc, rect.left, rect.top, v_winfo[i].icon);
    }
    if (v_winfo.size())
        draw_mask_on_selected(wnd, hdc);
    
    if (idx_first_visible > 0) {
        draw_button(hdc, 0);
    } 
    
    if (v_winfo.size()-idx_first_visible > maximun_shown_items) {
        draw_button(hdc, 1);
    }

    EndPaint (wnd, &ps);
}

void draw_button(HDC hdc, int down)
{
    POINT p[3];
    int n;
    if (down) {
        p[0].y = down_button_top+BUTTON_HEIGHT;
        n=-1;
    } else {
        p[0].y = up_button_top;
        n=+1;
    }
    p[1].y = p[2].y = p[0].y + BUTTON_HEIGHT*n;
    
    p[0].x = (main_window_rect.right-main_window_rect.left)/2;
    p[1].x = p[0].x + BUTTON_HEIGHT/3;
    p[2].x = p[0].x - BUTTON_HEIGHT/3;

    HRGN rgn = CreatePolygonRgn(p, 3, WINDING);
    HBRUSH brush = CreateSolidBrush(0xff0000);
    FillRgn(hdc, rgn, brush);
    DeleteObject(rgn);
    DeleteObject(brush);
}

void change_selected(HWND wnd, int idx)
{
    if (v_winfo.size()<1) 
        return;
    RECT rect;
    idx %= v_winfo.size();
    place_back_selected();
    if (get_item_rect(rect, false, idx)) {
        draw_mask_on_selected(wnd);
        idx_selected = idx;
        draw_mask_on_selected(wnd);
    } else {
        if (idx<idx_first_visible) {
            idx_first_visible=idx;
        } else {
            idx_first_visible = idx-maximun_shown_items+1;
        }

        idx_selected = idx;
        if (wnd)
            InvalidateRect(wnd, NULL, true);
    }
    tmp_foreput_selected();
}

void draw_mask_on_selected(HWND wnd, HDC hdc)
{
    if (!wnd)
        return;
    bool hdc_null = (hdc == NULL);
    if (hdc_null) {
        hdc = GetDC(wnd);
    }
    GetClientRect(wnd, &main_window_rect);
    RECT rect;
    get_item_rect(rect, false, idx_selected);
    
    HDC hdc_mem = CreateCompatibleDC(hdc); 
 
    HBITMAP hbitmap = CreateCompatibleBitmap(hdc, 
                                             rect.right-rect.left,
                                             rect.bottom-rect.top);
    HBRUSH hbrush = CreateSolidBrush(0x2837df);
    
    RECT rect_mem = rect;
    rect_offset(rect_mem, -rect.left, -rect.top);

    HBITMAP hbitmap_old = (HBITMAP)SelectObject(hdc_mem, hbitmap);
 
    FillRect(hdc_mem, &rect_mem, hbrush);

    BitBlt(hdc, 
           rect.left, rect.top,
           rect.right-rect.left, rect.bottom-rect.top,
           hdc_mem,
           0,0, 
           SRCINVERT);

    DeleteObject(hbrush);
    SelectObject(hdc_mem, hbitmap_old);
    DeleteObject(hbitmap);
    DeleteObject(hdc_mem);

    if (hdc_null) {
        ReleaseDC(wnd, hdc);
    }
}

void rect_offset(RECT &rect, int x, int y)
{
    rect.top += y;
    rect.bottom += y;
    rect.left += x;
    rect.right += x;
}

void update_found_list()
{
    change_selected(NULL, 0);
    v_winfo.resize(0);
    EnumDesktopWindows(NULL, EnumWindowsProc, (LPARAM)&v_winfo);
}

bool my_get_window_module_file_name(HWND hwnd, wchar_t buff[], unsigned int size)
{
    DWORD proc_id;
    GetWindowThreadProcessId(hwnd, &proc_id);
    HANDLE proc_handle = OpenProcess(PROCESS_QUERY_INFORMATION |
                                     PROCESS_VM_READ,
                                     FALSE, proc_id);
    if(!GetModuleFileNameExW(proc_handle,
                             NULL,
                             buff,
                             size)) {
        CloseHandle(proc_handle);
        memset(buff, 0, size*sizeof(buff[0]));
        return false;
    }
    CloseHandle(proc_handle);
    
    return true;
}


BOOL CALLBACK EnumWindowsProc(HWND wnd, LPARAM lParam)
{
    vector<WINFO> *pv_winfo = (vector<WINFO>*) lParam;
    if (wnd == main_window)
        return TRUE;
	
    if (IsWindowVisible(wnd) && IsWindowSwitchable(wnd)) {
        wchar_t buff[1024];
        WINFO winfo;
        winfo.wnd=wnd;

        wsprintf(buff, L"%02d: ", v_winfo.size());
        winfo.title_class = buff;

        if (IsIconic(wnd)) {
            winfo.title_class += L"-";
        } else {
            winfo.title_class += L"+";
        }

        if (!is_window_topmost(wnd)) {
            winfo.title_class += L"-";
        } else {
            winfo.title_class += L"+";
        }

    	::GetWindowTextW(wnd, buff, 1024);
        winfo.title_class += _wcslwr(buff);

        ::GetClassName(wnd, buff, 1024);
	winfo.title_class += L" : ";
	winfo.title_class += _wcslwr(buff);

        my_get_window_module_file_name(wnd, buff, 1024);
        winfo.title_class += L" : ";
        winfo.title_class += _wcslwr(wcsrchr(buff, PATH_SEPW)?wcsrchr(buff, PATH_SEPW)+1:buff);
        

	if (window_match(winfo.title_class)) {

            HICON icon=NULL;
            
            icon = (HICON)(UINT_PTR)GetClassLongPtr(wnd, GCLP_HICON);
            if (!icon) {
                SendMessageTimeout(wnd, WM_GETICON, ICON_BIG, 0, 
                                   SMTO_ABORTIFHUNG, HUNG_TIMEOUT, (PDWORD_PTR)&icon);
            }
            
            if (!icon) {
                HWND wnd_owner = GetWindow(wnd, GW_OWNER);
                if (wnd_owner)
                    icon = (HICON)(UINT_PTR)GetClassLongPtr(wnd_owner, GCLP_HICON);
            }
            if (!icon) {
                icon = LoadIcon(NULL, IDI_APPLICATION);

            }

            winfo.icon = icon;
            winfo.iconized = (bool)IsIconic(wnd);
            winfo.topmost = is_window_topmost(wnd);
	    pv_winfo->insert(pv_winfo->begin(), winfo);
	} 
    }
    return TRUE;
}

bool window_match(wstring& title_class)
{
    return title_class.find(looking_for) != wstring::npos;
}

bool IsChildWnd(HWND wnd)
{
    if (GetParent(wnd)) return true; 
    return false;
}

bool IsWindowSwitchable(HWND wnd)
{
    DWORD style = GetWindowLong(wnd, GWL_STYLE);

    if (style&WS_OVERLAPPED)
        return true;
    DWORD ex_style = GetWindowLong(wnd, GWL_EXSTYLE);
    if (ex_style&WS_EX_TOOLWINDOW)
        return false;
    if (style&WS_SYSMENU) {
        HWND wnd_owner = GetWindow(wnd, GW_OWNER);
        HWND wnd_shell = GetShellWindow();
        if (!wnd_owner || GetWindow(wnd_owner, GW_OWNER)==wnd_shell)
            return true;
    }
    return false;
}

/*return true if item is visible, false if not*/
int get_item_rect(RECT &rect, bool icon, unsigned int idx)
{
    if (idx < idx_first_visible)
        return 0;
    if (idx-idx_first_visible >= maximun_shown_items)
        return 0;

    rect = main_window_rect;
    rect.top = first_item_top + (idx-idx_first_visible)*ITEM_HEIGHT + (ITEM_HEIGHT-ICON_SIZE)/2;
    rect.bottom = rect.top + ICON_SIZE;

    if (icon) {
        rect.left = 15;
        rect.right = rect.left + ICON_SIZE;
    } else {
        rect.left = 15+ITEM_HEIGHT;
        rect.right -=15;
    }
    return 1;

}

int get_item_idx_from_xy(LPARAM l)
{
    int y = HIWORD(l);
    if (y<first_item_top || y>=last_item_bottom)
        return -1;

    return (y-first_item_top)/ITEM_HEIGHT + idx_first_visible;
}

void get_looking_for_rect(RECT &rect)
{
    rect = main_window_rect;
    rect.bottom = rect.top+LOOKING_FOR_HEIGHT;
}

void found_action()
{
    if (v_winfo.size()<1) {
        return;
    }

    idx_selected %= v_winfo.size();
    HWND wnd=v_winfo[idx_selected].wnd;

    restore_window(wnd);
}

void restore_window(HWND wnd)
{
    if (::IsIconic(wnd))
        ::PostMessage(wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
    ::SetForegroundWindow(wnd);
}

void handle_wm_lbuttondown(WPARAM w, LPARAM l)
{
    int i = get_item_idx_from_xy(l);
    if (i!=-1 && i!=idx_selected) {
        change_selected(main_window, i);
    }
}

void handle_wm_lbuttondblclk(WPARAM w, LPARAM l)
{
    int i = get_item_idx_from_xy(l);

    if (i!=-1) {
        idx_selected = i;
        found_action();
        hide_or_quit();
    }
}

void handle_mouse_wheel(WPARAM w, LPARAM l)
{
    short i = HIWORD(w);
    if (i>0) {
        change_selected(main_window, idx_selected+v_winfo.size()-1);
    } else {
        change_selected(main_window, idx_selected+1);
    }
}

void parse_command_line()
{
    int nNumArgs;
    PWSTR *ppArgv = CommandLineToArgvW(GetCommandLineW(), &nNumArgs);

    for (int i = 1; i < nNumArgs; i++) {
        if (ppArgv[i][0] == L'/' || ppArgv[i][0] == L'-') {
            if (!lstrcmpi(ppArgv[i] + 1, L"nodaemon")) {
                start_as_daemon = false;
            }
	}
    }
    HeapFree(GetProcessHeap(), 0, ppArgv);
}

void hide_or_quit()
{
    if (start_as_daemon) {
        ShowWindow(main_window, SW_HIDE);
        v_winfo.clear();
    }
    else 
        PostQuitMessage(0);
    main_window_visible = false;
}

int number_of_visible_items()
{
    int n = (main_window_rect.bottom-LOOKING_FOR_HEIGHT)/ITEM_HEIGHT;

    return n;
}

void initialize_metrics()
{    
    up_button_top = LOOKING_FOR_HEIGHT+2;
    RECT rect;
    GetClientRect(main_window, &rect);
    down_button_top = rect.bottom-2-BUTTON_HEIGHT;
    first_item_top = up_button_top+BUTTON_HEIGHT;
    last_item_bottom = down_button_top-(down_button_top-first_item_top)%ITEM_HEIGHT;
    maximun_shown_items = (last_item_bottom-first_item_top)/ITEM_HEIGHT;
}

void tmp_foreput_selected()
{
    if (v_winfo[idx_selected].iconized)
        ::PostMessage(v_winfo[idx_selected].wnd, WM_SYSCOMMAND, SC_RESTORE, 0);

    HWND hwnd = v_winfo[idx_selected].wnd;
    HWND ins_after = main_window;

    if (is_window_topmost(hwnd)) {
        ins_after = main_window;
    }
    SetWindowPos(hwnd, ins_after, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|SWP_ASYNCWINDOWPOS);
    restore_window(main_window);
    BHJDEBUGW(L"%s\n", v_winfo[idx_selected].title_class.c_str());
}

void place_back_selected()
{
    if (idx_selected >= v_winfo.size()-1)
        return;

    WINDOWPLACEMENT wp;
    wp.length = sizeof(wp);
    GetWindowPlacement(v_winfo[idx_selected].wnd,
                       &wp);

    
    if (v_winfo[idx_selected].iconized)
        ::PostMessage(v_winfo[idx_selected].wnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    
    HWND ins_after = v_winfo[idx_selected+1].wnd;

    if (!is_window_topmost(v_winfo[idx_selected].wnd) &&
        is_window_topmost(v_winfo[idx_selected+1].wnd)) {

        ins_after = HWND_TOP;
                          
    }
    SetWindowPos(v_winfo[idx_selected].wnd, ins_after, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|SWP_ASYNCWINDOWPOS);
    BHJDEBUGW(L"%s\n", v_winfo[idx_selected].title_class.c_str());
}

bool is_window_topmost(HWND hwnd) 
{
    LONG ex_style = GetWindowLong(hwnd, GWL_EXSTYLE);
    return ex_style&WS_EX_TOPMOST ? true : false;
}

void set_window_topmost(HWND hwnd, bool set)
{
    HWND ins_after = set ? HWND_TOPMOST : HWND_NOTOPMOST;

    LONG res = SetWindowPos(hwnd, ins_after, 0, 0, 0, 0, SWP_NOSIZE|SWP_NOMOVE);
    BHJDEBUGW(L"res is %x", res);
}
