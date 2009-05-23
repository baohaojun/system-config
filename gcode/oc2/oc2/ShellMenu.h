/*
Copyright (C) 2004 Jacquelin POTIER <jacquelin.potier@free.fr>
Dynamic aspect ratio code Copyright (C) 2004 Jacquelin POTIER <jacquelin.potier@free.fr>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 2 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

//-----------------------------------------------------------------------------
// Object: class helper shell context menu
//-----------------------------------------------------------------------------

#pragma once

#ifndef _WIN32_WINNT
    #define _WIN32_WINNT 0x0501
#endif

#include <windows.h>
#pragma warning (push)
#pragma warning(disable : 4005)// for '_stprintf' : macro redefinition in tchar.h
#include <TCHAR.h>
#pragma warning (pop)
#include <shlobj.h>
#include <commctrl.h>
#pragma comment (lib,"comctl32.lib")

#define CShellMenu_idCmdFirst 0xF000// Cmd must be on WORD see CMINVOKECOMMANDINFO.lpVerb
#define CShellMenu_idCmdLast  0xFFFF// Cmd must be on WORD see CMINVOKECOMMANDINFO.lpVerb

class CShellMenu
{
private:
    BOOL bFolderBackgroundMenu;
    BOOL bFillMenuSuccess;
    HWND hWndDialog;
    HMENU hPopUpMenu;
    IContextMenu* pContextMenu;
    IContextMenu2* pContextMenu2;
    IContextMenu3* pContextMenu3;

    void CommonConstructor(HWND hWndDialog);
    BOOL SubClassWindowProc();
    BOOL UnSubClassWindowProc();
    BOOL FillMenu(int SpecialFolderCSIDL);
    BOOL FillMenu(TCHAR* FileOrDirectoryPath);
    BOOL FillMenu(IShellFolder* pShellFolder,LPCITEMIDLIST pItemIdList);
    BOOL FillContextMenuMember(IShellFolder* pShellFolder);
    static LRESULT CALLBACK HookWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,UINT_PTR uIdSubclass,DWORD_PTR dwRefData);
public:
    CShellMenu(HWND hWndDialog,int SpecialFolderCSIDL);
    CShellMenu(HWND hWndDialog,int SpecialFolderCSIDL,BOOL bBackgroundMenu);
    CShellMenu(HWND hWndDialog,TCHAR* FileOrDirectoryPath);
    CShellMenu(HWND hWndDialog,TCHAR* FileOrDirectoryPath,BOOL bBackgroundMenu);
    ~CShellMenu(void);

    BOOL IsCreationSuccessful();

    //////////////////////////////////////////////////////////////////////////////
    // The following method should be called only if Shellmenu is NOT used as submenu
    //////////////////////////////////////////////////////////////////////////////
    BOOL Show(int x,int y);// must be called only if no parent menu, else menu will be shown for the parent menu TrackPopupMenu call


    //////////////////////////////////////////////////////////////////////////////
    // The following method should be called only if Shellmenu is used as submenu
    //////////////////////////////////////////////////////////////////////////////
    BOOL BeforeParentMenuShow();// must be called only if used as submenu, before the parent menu TrackPopupMenu call
    BOOL AfterParentMenuShow();// must be called only if used as submenu, after the parent menu TrackPopupMenu call
    HMENU GetControlHandle();// return the shell menu HMENU (can be called to use Shell menu as a submenu)
    HRESULT InvokeCommand(CMINVOKECOMMANDINFO* pCmdInfo);// must be called only if used as submenu, after the parent menu TrackPopupMenu call
};
