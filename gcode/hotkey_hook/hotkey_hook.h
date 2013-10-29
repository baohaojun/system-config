#ifndef __HOTKEY_HOOK_H__
#define __HOTKEY_HOOK_H__

#ifndef WINVER                          // Allow use of features specific to Windows XP or later.
#define WINVER 0x0501           // Change this to the appropriate value to target other versions of Windows.
#endif

#ifndef _WIN32_WINNT            // Allow use of features specific to Windows XP or later.
#define _WIN32_WINNT 0x0501     // Change this to the appropriate value to target other versions of Windows.
#endif

#ifndef _WIN32_WINDOWS          // Allow use of features specific to Windows 98 or later.
#define _WIN32_WINDOWS 0x0410 // Change this to the appropriate value to target Windows Me or later.
#endif

#ifndef _WIN32_IE                       // Allow use of features specific to IE 6.0 or later.
#define _WIN32_IE 0x0600        // Change this to the appropriate value to target other versions of IE.
#endif

#define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <string>
#include <vector>

using namespace std;



typedef UINT message_number_t;
const message_number_t WM_HH_EXEC_CMD = WM_USER+1;
const message_number_t WM_HH_CORE_CMD = WM_HH_EXEC_CMD+1;

typedef unsigned int mask_key_state_t;
const mask_key_state_t LEFT_CONTROL_ACTIVE=1<<0;
const mask_key_state_t RIGHT_CONTROL_ACTIVE=1<<1;
const mask_key_state_t LEFT_ALT_ACTIVE=1<<2;
const mask_key_state_t RIGHT_ALT_ACTIVE=1<<3;
const mask_key_state_t LEFT_SHIFT_ACTIVE=1<<4;
const mask_key_state_t RIGHT_SHIFT_ACTIVE=1<<5;
const mask_key_state_t LEFT_WIN_ACTIVE=1<<6;
const mask_key_state_t RIGHT_WIN_ACTIVE=1<<7;
const mask_key_state_t NOTHING_IS_ACTIVE=0;

extern bool g_switch_ralt_lwin;
typedef struct
{
    unsigned char vk_code; //this is a bit redundant
    mask_key_state_t mask;
    string action;
    bool is_exec_cmd;
} hotkey_action_t;




int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow);
int Initialize(HINSTANCE hInst, LPSTR lpCmdLine);
LRESULT CALLBACK WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK KbdHookProc(int nCode, WPARAM wParam, LPARAM lParam);
HWND CreateMainWindow(HINSTANCE hInst);
void LoadHotkeys();
mask_key_state_t GetMaskCode(char *mod_str);
void SetMaskKeyActiveOn(DWORD vkCode);
void SetMaskKeyActiveOff(DWORD vkCode);
bool IsPrimaryKey(DWORD vkCode);
bool IsMaskKey(DWORD vkCode);
void InitVkCodeMask();
bool ReadNextCommand(FILE *fp, LPSTR szBuffer, DWORD dwLength);
bool read_next_line(FILE *fp, LPSTR szBuffer, DWORD dwLength);
const char* stristr(const char *aa, const char *bb);
int getvalue(char *from, const char *token, char *to, bool from_right);
bool FileExists(LPCSTR szFileName);
bool HotKeyIsTrigged(DWORD vkCode, mask_key_state_t mask, unsigned int* idx);

void core_cmd(const string&);
void exec_cmd(const string&);

bool core_window_cmd(const string&);
bool core_misc_cmd(const string&);

void init_runtime_libs(void);
void exit_runtime_libs(void);


#endif
