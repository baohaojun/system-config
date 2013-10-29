
#include "hotkey_hook.h"
#include "bhjdebug.h"
#include <assert.h>
#include <string.h>
#include <unistd.h>

//global vars
const char *g_app_name = "hotkey_hook";
HWND g_main_wnd = NULL;
HHOOK g_kbd_hook = NULL;
HINSTANCE g_instance = NULL;
mask_key_state_t g_mask_key_state;
vector<hotkey_action_t> g_hotkey_action_vecarr[0xff];
static mask_key_state_t vk_code_mask[0xff]={0};

//static routines
static int CleanUp();
static void UnLoadHotKeys();
static bool vkCodeValid(DWORD vkCode);
static bool vkCodeInvalid(DWORD);

#define ARR_SIZE(a) (sizeof(a)/sizeof(a[0]))

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    //Do initialization
    g_instance = hInst;
    Initialize(hInst, lpCmdLine);
    //Enter message loop
    MSG msg;

    while (GetMessage (&msg, NULL, 0, 0))
    {
        TranslateMessage (&msg);
        DispatchMessage (&msg);
    }

    CleanUp();
    return msg.wParam;
}

void DebugHotkeyActionVecArr()
{
    for (unsigned int key=0; key<ARR_SIZE(g_hotkey_action_vecarr); key++) {
        for (unsigned int idx=0; idx<g_hotkey_action_vecarr[key].size(); idx++) {
            simple_debug(g_hotkey_action_vecarr[key][idx].mask, %02x);
            simple_debug(g_hotkey_action_vecarr[key][idx].action.c_str(), %s);
        }
    }
}
DWORD WINAPI KbdHookThreadFunc(LPVOID)
{
    g_kbd_hook = SetWindowsHookEx(WH_KEYBOARD_LL,
                                  (HOOKPROC)KbdHookProc, g_instance, 0);
    MSG msg;
    while (GetMessage (&msg, NULL, 0, 0))
    {
        TranslateMessage (&msg);
        DispatchMessage (&msg);
    }
    return 0;
}

int Initialize(HINSTANCE hInst, LPSTR lpCmdLine)
{
    //Create window
    g_main_wnd = CreateMainWindow(hInst);
    InitVkCodeMask();
    init_runtime_libs();
    LoadHotkeys();
    CreateThread(NULL, 0, KbdHookThreadFunc, NULL, 0, NULL);
    //DebugHotkeyActionVecArr();


    return 0;
    //With a proper windows message handler routine
    //Set the low level keyboard hook
}

int CleanUp()
{
    //do it in reverse order as Initialize
    UnhookWindowsHookEx(g_kbd_hook);
    UnLoadHotKeys();
    exit_runtime_libs();
    return 0;
}

void UnLoadHotKeys()
{
    for (unsigned int i=0; i<ARR_SIZE(g_hotkey_action_vecarr); i++) {
        g_hotkey_action_vecarr[i].clear();
    }
}

LRESULT CALLBACK WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{

    switch (message)
    {

    case WM_HH_EXEC_CMD:
        exec_cmd(g_hotkey_action_vecarr[wParam][lParam].action);
        break;

    case WM_HH_CORE_CMD:
        core_cmd(g_hotkey_action_vecarr[wParam][lParam].action);
        break;

    case WM_CREATE:
        BHJDEBUG("");
        break;

    case WM_DESTROY:
        PostQuitMessage(0);
        break;

    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
    return 0;

}


int MapKeys(WPARAM wParam, PKBDLLHOOKSTRUCT hook_struct)
{
    // if key is synthesized (by ourself?), then don't do it, and let the hook handle it (return 0 here)

    if (hook_struct->flags & LLKHF_INJECTED) {
        return 0;
    }

    // else if key is maped, then kbdhookproc must not handle it, also must not pass it to the next hook (return 1 here)
    DWORD vkCode = hook_struct->vkCode;

    static struct KEY_MAP {
        DWORD from;
        DWORD to;
    } exchange_table [] = {
        {VK_RMENU, VK_LWIN},
        {VK_LWIN, VK_RMENU},
        {0, 0},
    };

    if (wParam==WM_SYSKEYUP||wParam==WM_KEYUP) {
        xsimple_debug(GetAsyncKeyState(VK_LMENU));
        xsimple_debug(GetAsyncKeyState(VK_RMENU));
    }

    if (vkCode == VK_RMENU) {
        if (wParam==WM_SYSKEYDOWN || wParam==WM_KEYDOWN) {
            INPUT input = {0};

            input.type = INPUT_KEYBOARD;
            input.ki.wVk = VK_RMENU;
            input.ki.dwFlags = 0;
            SendInput(1, &input, sizeof(INPUT));

            memset(&input, 0, sizeof(INPUT));
            input.type = INPUT_KEYBOARD;
            input.ki.wVk = VK_RMENU;
            input.ki.dwFlags = KEYEVENTF_KEYUP;
            SendInput(1, &input, sizeof(INPUT));

            memset(&input, 0, sizeof(INPUT));
            input.type = INPUT_KEYBOARD;
            input.ki.wVk = VK_RMENU;
            input.ki.dwFlags = 0;
            SendInput(1, &input, sizeof(INPUT));

            memset(&input, 0, sizeof(INPUT));
            input.type = INPUT_KEYBOARD;
            input.ki.wVk = VK_RMENU;
            input.ki.dwFlags = KEYEVENTF_KEYUP;
            SendInput(1, &input, sizeof(INPUT));

        }
    }

    for (int i=0; exchange_table[i].to; i++) {
        if (vkCode==exchange_table[i].from) {
            INPUT input = {0};
            input.type = INPUT_KEYBOARD;
            input.ki.wVk = exchange_table[i].to;
            input.ki.dwFlags = KEYEVENTF_KEYUP * (wParam==WM_SYSKEYUP || wParam==WM_KEYUP);

            SendInput(1, &input, sizeof(INPUT));
            return 1;
        }
    }




    // key is not maped, then kbdhookproc should handle it like normal. (return 0 here)
    return 0;
}

LRESULT CALLBACK KbdHookProc(int nCode, WPARAM wParam, LPARAM lParam)
{

    // By returning a non-zero value from the hook procedure, the
    // message does not get passed to the target window


    //Mask keys are control, alt, shift, win
    //if win key pressed/released, don't call next hook, for other mask keys, call next hook
    //the mask keys should be recorded, so that we have a consistent state of which is down/up.
    //if nonmask key touched, then check if we are interested in it associated with the current state of mask keys

    PKBDLLHOOKSTRUCT hook_struct = (PKBDLLHOOKSTRUCT)lParam;
    DWORD vkCode = hook_struct->vkCode;


    if (nCode != HC_ACTION)
        goto out;
    if (vkCodeInvalid(vkCode))
        goto out;

    if (wParam==WM_KEYDOWN || wParam==WM_SYSKEYDOWN) {
        SetMaskKeyActiveOn(vkCode);
    } else {
        SetMaskKeyActiveOff(vkCode);
    }

    if (IsPrimaryKey(vkCode)) {

        //simple_debug((int)vkCode, %02x);
        unsigned int nth_ha=0;
        if (HotKeyIsTrigged(vkCode, g_mask_key_state, &nth_ha)) {
            if (wParam==WM_KEYUP || wParam==WM_SYSKEYUP) {
                return 1;
            }
            //simple_debug(g_hotkey_action_vecarr[vkCode][nth_ha].action.c_str(), %s);
            if (g_hotkey_action_vecarr[vkCode][nth_ha].is_exec_cmd) {
                PostMessage(g_main_wnd, WM_HH_EXEC_CMD, vkCode, nth_ha);
            } else {
                PostMessage(g_main_wnd, WM_HH_CORE_CMD, vkCode, nth_ha);
            }
            return 1;
        }
    }

    if ((vkCode==VK_LWIN || vkCode==VK_RWIN)) {
        return 1;
    }



out:
    return CallNextHookEx (g_kbd_hook, nCode, wParam, lParam);
}

HWND CreateMainWindow(HINSTANCE hInst)
{
    WNDCLASS wndclass;

    wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
    wndclass.lpfnWndProc   = WndProc;
    wndclass.cbClsExtra    = 0;
    wndclass.cbWndExtra    = 0;
    wndclass.hInstance     = hInst;
    wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION);
    wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW);
    wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
    wndclass.lpszMenuName  = NULL;
    wndclass.lpszClassName = g_app_name;

    if (!RegisterClass (&wndclass))
    {
        MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                    g_app_name, MB_ICONERROR);
        exit(-1);
    }

    return CreateWindowEx (WS_EX_TOOLWINDOW|WS_EX_TOPMOST, g_app_name, g_app_name,
                                 0,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 HWND_MESSAGE, NULL, hInst, NULL);
}


void LoadHotkeys()
{
    char rcpath[MAX_PATH];
    GetModuleFileName(g_instance, rcpath, sizeof(rcpath));
    int nLen = strlen(rcpath);
    while (nLen && rcpath[nLen-1] != '\\') nLen--;
    rcpath[nLen]='\0';
    chdir(rcpath);
    strcpy(rcpath+nLen, "hotkey.rc");
    if (!FileExists(rcpath)) {
        BHJDEBUG(".rc file not found!");
        exit(-1);
    }

    char buffer[1024], keytograb[120], modifier[120], action[1024];

    FILE *fp = fopen(rcpath, "rt");

    // KeyToGrab(), WithModifier(), WithAction(), DoThis()
    while (ReadNextCommand(fp, buffer, sizeof (buffer)))
    {
        if (0==getvalue(buffer, "KEYTOGRAB", keytograb, false))
        {
            continue;
        }
        getvalue(buffer, "WITHMODIFIER", modifier, false);
        if (0==getvalue(buffer, "WITHACTION", action, false))
            continue;

        strupr(keytograb);

        unsigned ch = (unsigned char)keytograb[0];
        mask_key_state_t mask;
        bool is_ExecCommand = false;

        if (0 == stricmp(action, "ExecCommand"))
        {
            is_ExecCommand = true;
            getvalue(buffer, "DOTHIS", action, true);
        }

        //dbg_printf(buffer, "<%s>\n<%s>\n<%s>\n", keytograb, modifier, action);

        if (keytograb[1])
        {
            static const struct vkTable
            {
                const char* key;
                int vKey;
            } vkTable[] =
                  {
                      {"F1", VK_F1},
                      {"F2", VK_F2},
                      {"F3", VK_F3},
                      {"F4", VK_F4},
                      {"F5", VK_F5},
                      {"F6", VK_F6},
                      {"F7", VK_F7},
                      {"F8", VK_F8},
                      {"F9", VK_F9},
                      {"F10", VK_F10},
                      {"F11", VK_F11},
                      {"F12", VK_F12},
                      {"PRTSCN", VK_SNAPSHOT},
                      {"PAUSE", VK_PAUSE},
                      {"SCRLCK", VK_SCROLL},
                      {"INSERT", VK_INSERT},
                      {"DELETE", VK_DELETE},
                      {"HOME", VK_HOME},
                      {"END", VK_END},
                      {"PAGEUP", VK_PRIOR},
                      {"PAGEDOWN", VK_NEXT},
                      {"LEFT", VK_LEFT},
                      {"RIGHT", VK_RIGHT},
                      {"DOWN", VK_DOWN},
                      {"UP", VK_UP},
                      {"TAB", VK_TAB},
                      {"BACKSPACE", VK_BACK},
                      {"SPACEBAR", VK_SPACE},
                      {"APPS", VK_APPS},
                      {"ENTER", VK_RETURN},
                      {"NUM0", VK_NUMPAD0},
                      {"NUM1", VK_NUMPAD1},
                      {"NUM2", VK_NUMPAD2},
                      {"NUM3", VK_NUMPAD3},
                      {"NUM4", VK_NUMPAD4},
                      {"NUM5", VK_NUMPAD5},
                      {"NUM6", VK_NUMPAD6},
                      {"NUM7", VK_NUMPAD7},
                      {"NUM8", VK_NUMPAD8},
                      {"NUM9", VK_NUMPAD9},
                      {"MUL", VK_MULTIPLY},
                      {"DIV", VK_DIVIDE},
                      {"ADD", VK_ADD},
                      {"SUB", VK_SUBTRACT},
                      {"DEC", VK_DECIMAL},
                      {"ESCAPE", VK_ESCAPE},
                      {"LWIN", VK_LWIN},
                      {"RWIN", VK_RWIN},
                      {"PLUS", VK_OEM_PLUS},
                      {"MINUS", VK_OEM_MINUS},
                      { NULL, 0 }

                  };
            const struct vkTable *v = vkTable;
            do if (!strcmp(v->key, keytograb))
               {
                   ch = v->vKey;
                   goto found;
               }
            while ((++v)->key);
            if (keytograb[0] == 'V' && keytograb[1] == 'K'
                && keytograb[2] >= '0' && keytograb[2] <= '9')
            {
                ch = atoi(keytograb+2);
                goto found;
            }
            continue;
        }

    found:
        assert(0<ch && ch<0xff);

        mask = GetMaskCode(modifier);

        hotkey_action_t ha_tmp;
        ha_tmp.vk_code = ch;
        ha_tmp.mask = mask;
        ha_tmp.action = action;
        ha_tmp.is_exec_cmd = is_ExecCommand;

        g_hotkey_action_vecarr[ch].push_back(ha_tmp);
/*
        if (stristr(modifier, "WIN"))   sub |= MOD_WIN;
        if (stristr(modifier, "ALT"))   sub |= MOD_ALT;
        if (stristr(modifier, "CTRL"))  sub |= MOD_CONTROL;
        if (stristr(modifier, "SHIFT")) sub |= MOD_SHIFT;
*/

    }
    fclose(fp);
}

mask_key_state_t GetMaskCode(char *mod_str)
{
    mask_key_state_t ret = NOTHING_IS_ACTIVE;
    if (stristr(mod_str, "lwin")) ret |= LEFT_WIN_ACTIVE;
    if (stristr(mod_str, "rwin")) ret |= RIGHT_WIN_ACTIVE;
    if (stristr(mod_str, "lalt")) ret |= LEFT_ALT_ACTIVE;
    if (stristr(mod_str, "ralt")) ret |= RIGHT_ALT_ACTIVE;
    if (stristr(mod_str, "lctrl")) ret |= LEFT_CONTROL_ACTIVE;
    if (stristr(mod_str, "rctrl")) ret |= RIGHT_CONTROL_ACTIVE;
    if (stristr(mod_str, "lshift")) ret |= LEFT_SHIFT_ACTIVE;
    if (stristr(mod_str, "rshift")) ret |= RIGHT_SHIFT_ACTIVE;
    return ret;
}

void SetMaskKeyActiveOn(DWORD vkCode)
{
    if (!vkCodeValid(vkCode)) {
        return;
    }
    assert(0<vkCode && vkCode<0xff);


    g_mask_key_state |= vk_code_mask[vkCode];
}

void SetMaskKeyActiveOff(DWORD vkCode)
{
    if (!vkCodeValid(vkCode)) {
        return;
    }
    assert(0<vkCode && vkCode<0xff);
    g_mask_key_state &= ~vk_code_mask[vkCode];
}

bool IsPrimaryKey(DWORD vkCode)
{
    if (vkCodeInvalid(vkCode)) {
        return false;
    }
    return vk_code_mask[vkCode]==0;
}

bool IsMaskKey(DWORD vkCode)
{
    return !IsPrimaryKey(vkCode);
}

void InitVkCodeMask()
{
    vk_code_mask[VK_LWIN]=LEFT_WIN_ACTIVE;
    vk_code_mask[VK_RWIN]=RIGHT_WIN_ACTIVE;
    vk_code_mask[VK_LSHIFT]=LEFT_SHIFT_ACTIVE;
    vk_code_mask[VK_RSHIFT]=RIGHT_SHIFT_ACTIVE;
    vk_code_mask[VK_LCONTROL]=LEFT_CONTROL_ACTIVE;
    vk_code_mask[VK_RCONTROL]=RIGHT_CONTROL_ACTIVE;
    vk_code_mask[VK_LMENU]=LEFT_ALT_ACTIVE;
    vk_code_mask[VK_RMENU]=RIGHT_ALT_ACTIVE;
}

bool ReadNextCommand(FILE *fp, LPSTR szBuffer, DWORD dwLength)
{
    while (read_next_line(fp, szBuffer, dwLength))
    {
        char c = szBuffer[0];
        if (c && '#' != c && '!' != c) return true;
    }
    return false;
}

//===========================================================================

bool read_next_line(FILE *fp, LPSTR szBuffer, DWORD dwLength)
{
#define IS_SPC(c) ((unsigned char)(c) <= 32)
    if (fp && fgets(szBuffer, dwLength, fp))
    {
        char *p, *q, c; p = q = szBuffer;
        while (0 != (c = *p) && IS_SPC(c)) p++;
        while (0 != (c = *p)) *q++ = IS_SPC(c) ? ' ' : c, p++;
        while (q > szBuffer && IS_SPC(q[-1])) q--;
        *q = 0;
        return true;
    }
    szBuffer[0] = 0;
    return false;
}

const char* stristr(const char *aa, const char *bb)
{
    do {
        const char *a, *b; char c, d;
        for (a = aa, b = bb;;++a, ++b)
        {
            if (0 == (c = *b)) return aa;
            if (0 != (d = *a^c))
                if (d != 32 || (c |= 32) < 'a' || c > 'z')
                    break;
        }
    } while (*aa++);
    return NULL;
}

int getvalue(char *from, const char *token, char *to, bool from_right)
{
    typedef char *(*strchr_func_t)(const char*, int);
    strchr_func_t func_p = from_right?(strchr_func_t)strrchr:(strchr_func_t)strchr;

    const char *p, *q; int l=0;
    if (NULL!=(p=stristr(from, token)))
    {
        p += strlen(token);
        while (' ' == *p) ++p;
        if ('(' == *p && NULL != (q = (func_p)(++p,')')))
            memcpy(to, p, l = q-p);
    }
    to[l]=0;
    return l;
}

bool FileExists(LPCSTR szFileName)
{
    DWORD a = GetFileAttributes(szFileName);
    return (DWORD)-1 != a && 0 == (a & FILE_ATTRIBUTE_DIRECTORY);
}

bool HotKeyIsTrigged(DWORD vkCode, mask_key_state_t mask, unsigned int* idx)
{

    for (unsigned int i=0; i<g_hotkey_action_vecarr[vkCode].size(); i++) {
        if (mask == g_hotkey_action_vecarr[vkCode][i].mask) {
            *idx=i;
            return true;
        }
    }
    return false;
}

static bool vkCodeValid(DWORD vkCode)
{
    return 0<vkCode && vkCode<0xff;
}

static bool vkCodeInvalid(DWORD vkCode)
{
    return !vkCodeValid(vkCode);
}
