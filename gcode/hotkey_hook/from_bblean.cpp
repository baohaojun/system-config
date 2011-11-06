#include "hotkey_hook.h"
#include "bhjdebug.h"

typedef void (*window_cmd_func_t)(HWND);
void (WINAPI *pRunDlg)(HWND, HICON, LPCSTR, LPCSTR, LPCSTR, int);

////////////////////////////////////////////////////////
static const char shell_lib   [] =  "SHELL32"   ;
static const char user_lib    [] =  "USER32"    ;
static const char kernel_lib  [] =  "KERNEL32"  ;
static const char psapi_lib   [] =  "PSAPI"     ;
static const char shdocvw_lib [] =  "SHDOCVW"   ;
static const char *rtl_libs   [] =
{
    shell_lib,
    //user_lib,
    //kernel_lib,
    //psapi_lib,
    //shdocvw_lib,
    NULL
};

//don't touch this, it's for ifind
#define WM_HH_IFIND (WM_USER+0xff)
//static routines
static bool check_for_restore(HWND hwnd);
static void grow_window(HWND hwnd, bool v);
static void WS_GrowWindowHeight(HWND hwnd);
static void WS_GrowWindowWidth(HWND hwnd);
static void WS_MaximizeWindow(HWND hwnd);
static void WS_CloseWindow(HWND hwnd);
static void WS_SwitchRAltLWin(HWND hwnd);
static void WS_NoSwitchRAltLWin(HWND hwnd);
static void WS_MinimizeWindow(HWND hwnd);
static LRESULT send_syscommand(HWND hwnd, UINT SC_XXX);
static void window_set_pos(HWND hwnd, RECT rc);
static void get_rect(HWND hwnd, RECT *rp);


////////////////////////////////////////////////////////
void init_runtime_libs(void)
{
    struct proc_info { const char *lib; char *procname; void *procadr; };

    static struct proc_info rtl_list [] =
    {
        { shell_lib, (char*)0x3D, &pRunDlg },
        { NULL }
    };

    struct proc_info *rtl_ptr = rtl_list;

    do {
        *(FARPROC*)rtl_ptr->procadr = GetProcAddress(LoadLibrary(rtl_ptr->lib), rtl_ptr->procname);
    } while ((++rtl_ptr)->lib);

}

void exit_runtime_libs(void)
{
    const char **p = rtl_libs;
    do FreeLibrary(GetModuleHandle(*p)); while (*++p);
}

//===========================================================================
static bool check_for_restore(HWND hwnd)
{
    if (FALSE == IsZoomed(hwnd))
    {
        return false;
    }
    send_syscommand(hwnd, SC_RESTORE);

    // restore the default maxPos (necessary when it was V-max'd or H-max'd)
    WINDOWPLACEMENT wp;
    wp.length = sizeof wp;
    GetWindowPlacement(hwnd, &wp);
    wp.ptMaxPosition.x =
    wp.ptMaxPosition.y = -1;
    SetWindowPlacement(hwnd, &wp);
    return true;
}

static void grow_window(HWND hwnd, bool v)
{
    if (check_for_restore(hwnd)) return;
    RECT r; get_rect(hwnd, &r);
    LockWindowUpdate(hwnd);
    send_syscommand(hwnd, SC_MAXIMIZE);
    RECT r2; get_rect(hwnd, &r2);
    if (v) r.top = r2.top, r.bottom = r2.bottom;
    else r.left = r2.left, r.right = r2.right;
    window_set_pos(hwnd, r);
    LockWindowUpdate(NULL);
}

static void WS_GrowWindowHeight(HWND hwnd)
{
    grow_window(hwnd, true);
}

static void WS_GrowWindowWidth(HWND hwnd)
{
    grow_window(hwnd, false);
}

static void WS_MaximizeWindow(HWND hwnd)
{
    if (check_for_restore(hwnd)) return;
    send_syscommand(hwnd, SC_MAXIMIZE);
}

static void WS_CloseWindow(HWND hwnd)
{
    send_syscommand(hwnd, SC_CLOSE);
}

bool g_switch_ralt_lwin;
static void WS_NoSwitchRAltLWin(HWND hwnd)
{
    g_switch_ralt_lwin = false;
}

static void WS_SwitchRAltLWin(HWND hwnd)
{
    g_switch_ralt_lwin = true;
}

static void WS_MinimizeWindow(HWND hwnd)
{
    LONG style = GetWindowLong(hwnd, GWL_STYLE);
    if (0 == (WS_MINIMIZEBOX & style)) return;
    BHJDEBUG("");
    send_syscommand(hwnd, SC_MINIMIZE);
    BHJDEBUG("");
    return;
}

static LRESULT send_syscommand(HWND hwnd, UINT SC_XXX)
{
    DWORD_PTR dwResult = 0;
    SendMessageTimeout(hwnd, WM_SYSCOMMAND, SC_XXX, 0, SMTO_NORMAL, 1000, &dwResult);
    return dwResult;
}

bool core_window_cmd(const string& cmd_str)
{
    static struct {
        char *cmd_name;
        window_cmd_func_t func;
    } window_cmd_map[] = {
        {"CloseWindow", WS_CloseWindow},
	{"SwitchRAltLWin", WS_SwitchRAltLWin},
	{"NoSwitchRAltLWin", WS_NoSwitchRAltLWin},
        {"MinimizeWindow", WS_MinimizeWindow},
        {"MaximizeVertical", WS_GrowWindowHeight},
        {"MaximizeHorizontal", WS_GrowWindowWidth},
        {NULL, NULL},
    };

    unsigned int i=0;
    while(window_cmd_map[i].cmd_name) {
        if (cmd_str == window_cmd_map[i].cmd_name) {
            HWND hwnd = GetForegroundWindow();
            window_cmd_map[i].func(hwnd);
            return true;
        }
        i++;
    }
    return false;
}

bool core_misc_cmd(const string& cmd_str)
{
    bool misc_cmd_match = false;
    if (cmd_str == "Run") {
        pRunDlg(NULL, NULL, NULL, NULL, NULL, 0 );
        misc_cmd_match = true;
    } else if (cmd_str == "Quit") {
        extern HWND g_main_wnd;
        send_syscommand(g_main_wnd, SC_CLOSE);
        misc_cmd_match = true;
    } else if (cmd_str == "ifinds") {
        HWND hwnd = FindWindow("ifind", NULL);
        if (!hwnd) {
            BHJDEBUG("can't find ifind window");
            return true;
        }
        BHJDEBUG("post S to ifind");
        DWORD ifindProcId=0;
        GetWindowThreadProcessId(hwnd, &ifindProcId);
        AllowSetForegroundWindow(ifindProcId);
        PostMessage(hwnd, WM_HH_IFIND, 0, MAKELPARAM('S', 'S'));
        misc_cmd_match = true;
    } else if (cmd_str == "ifindo") {
        HWND hwnd = FindWindow("ifind", NULL);
        if (!hwnd) {
            BHJDEBUG("can't find ifind window");
            return true;
        }
        BHJDEBUG("post O to ifind");
        DWORD ifindProcId=0;
        GetWindowThreadProcessId(hwnd, &ifindProcId);
        AllowSetForegroundWindow(ifindProcId);
        PostMessage(hwnd, WM_HH_IFIND, 0, MAKELPARAM('O', 'O'));
        misc_cmd_match = true;
    } else if (cmd_str == "LockWorkStation") {
        LockWorkStation();
        extern mask_key_state_t g_mask_key_state;
        g_mask_key_state = NOTHING_IS_ACTIVE;
        misc_cmd_match = true;
    }
    return misc_cmd_match;
}

void core_cmd(const string& cmd_str)
{
    simple_debug(cmd_str.c_str(), %s);
    if (core_window_cmd(cmd_str)) return;
    if (core_misc_cmd(cmd_str)) return;

    MessageBox(NULL, "core_cmd not found", cmd_str.c_str(), 0);
    
}


void exec_cmd(const string& cmd_str)
{
    simple_debug(cmd_str.c_str(), %s);
    //run_command(cmd_str);
    extern BOOL BBExecute_string(const char *command, bool no_errors);
    BBExecute_string(cmd_str.c_str(), false);
}



static void window_set_pos(HWND hwnd, RECT rc)
{
    int width = rc.right - rc.left;
    int height = rc.bottom - rc.top;
    SetWindowPos(hwnd, NULL,
        rc.left, rc.top, width, height,
        SWP_NOZORDER|SWP_NOACTIVATE);
}

static void get_rect(HWND hwnd, RECT *rp)
{
    GetWindowRect(hwnd, rp);
    if (WS_CHILD & GetWindowLong(hwnd, GWL_STYLE))
    {
        HWND pw = GetParent(hwnd);
        ScreenToClient(pw, (LPPOINT)&rp->left);
        ScreenToClient(pw, (LPPOINT)&rp->right);
    }
}
