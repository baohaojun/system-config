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

#include "stdafx.h"
#include <windows.h>
#include <commctrl.h>
#include "shellmenu.h"

//-----------------------------------------------------------------------------
// Name: CShellMenu
// Object: Constructor
//         WARNING : CoInitialize or OleInitialize must have been called before creating object
// Parameters :
//     in  : HWND hWndDialog : handle of window that owns the menu
//           int SpecialFolderCSIDL : CSIDL of special folder (CSIDL_DESKTOP, ...)
//     out :
//     return : 
//-----------------------------------------------------------------------------
CShellMenu::CShellMenu(HWND hWndDialog,int SpecialFolderCSIDL)
{
    this->CommonConstructor(hWndDialog);
    this->bFillMenuSuccess=this->FillMenu(SpecialFolderCSIDL);
}

//-----------------------------------------------------------------------------
// Name: CShellMenu
// Object: Constructor
//         WARNING : CoInitialize or OleInitialize must have been called before creating object
// Parameters :
//     in  : HWND hWndDialog : handle of window that owns the menu
//           int SpecialFolderCSIDL : CSIDL of special folder (CSIDL_DESKTOP, ...)
//     out :
//     return : 
//-----------------------------------------------------------------------------
CShellMenu::CShellMenu(HWND hWndDialog,int SpecialFolderCSIDL,BOOL bFolderBackgroundMenu)
{
    this->CommonConstructor(hWndDialog);
    this->bFolderBackgroundMenu=bFolderBackgroundMenu;
    this->bFillMenuSuccess=this->FillMenu(SpecialFolderCSIDL);
}

//-----------------------------------------------------------------------------
// Name: CShellMenu
// Object: Constructor.
//         WARNING : CoInitialize or OleInitialize must have been called before creating object
// Parameters :
//     in  : HWND hWndDialog : handle of window that owns the menu
//           TCHAR* FileOrDirectoryPath : folder or file path
//     out :
//     return : 
//-----------------------------------------------------------------------------
CShellMenu::CShellMenu(HWND hWndDialog,TCHAR* FileOrDirectoryPath)
{
    this->CommonConstructor(hWndDialog);
    this->bFillMenuSuccess=this->FillMenu(FileOrDirectoryPath);
}

//-----------------------------------------------------------------------------
// Name: CShellMenu
// Object: Constructor.
//         WARNING : CoInitialize or OleInitialize must have been called before creating object
// Parameters :
//     in  : HWND hWndDialog : handle of window that owns the menu
//           TCHAR* FileOrDirectoryPath : folder or file path
//     out :
//     return : 
//-----------------------------------------------------------------------------
CShellMenu::CShellMenu(HWND hWndDialog,TCHAR* FileOrDirectoryPath,BOOL bFolderBackgroundMenu)
{
    this->CommonConstructor(hWndDialog);
    this->bFolderBackgroundMenu=bFolderBackgroundMenu;
    this->bFillMenuSuccess=this->FillMenu(FileOrDirectoryPath);
}

//-----------------------------------------------------------------------------
// Name: CommonConstructor
// Object: Common Constructor.
// Parameters :
//     in  : HWND hWndDialog : handle of window that owns the menu
//     out :
//     return : 
//-----------------------------------------------------------------------------
void CShellMenu::CommonConstructor(HWND hWndDialog)
{
    this->hWndDialog=hWndDialog;
    this->pContextMenu=NULL;
    this->pContextMenu2=NULL;
    this->pContextMenu3=NULL;
    this->bFolderBackgroundMenu=FALSE;

    // create menu
    this->hPopUpMenu=::CreatePopupMenu();
    MENUINFO mnfo;
    mnfo.cbSize = sizeof(mnfo);
    mnfo.fMask = MIM_STYLE;
    mnfo.dwStyle = MNS_CHECKORBMP; //| MNS_AUTODISMISS can make menu to disappear when used on toolbar
    ::SetMenuInfo(this->hPopUpMenu, &mnfo);
}

//-----------------------------------------------------------------------------
// Name: ~CShellMenu
// Object: Destructor
// Parameters :
//     in  : 
//     out :
//     return : 
//-----------------------------------------------------------------------------
CShellMenu::~CShellMenu(void)
{
    if (this->pContextMenu3)
        this->pContextMenu3->Release();
    if (this->pContextMenu2)
        this->pContextMenu2->Release();
    if (this->pContextMenu)
        this->pContextMenu->Release();

    // destroy menu
    ::DestroyMenu(this->hPopUpMenu);
}

//-----------------------------------------------------------------------------
// Name: IsCreationSuccessful
// Object: check menu creation success
// Parameters :
//     in  : 
//     out :
//     return : TRUE if menu has been created successfully
//-----------------------------------------------------------------------------
BOOL CShellMenu::IsCreationSuccessful()
{
    return this->bFillMenuSuccess;
}

//-----------------------------------------------------------------------------
// Name: SubClassWindowProc
// Object: subclass window proc of owning window
// Parameters :
//     in  : 
//     out :
//     return : TRUE
//-----------------------------------------------------------------------------
BOOL CShellMenu::SubClassWindowProc()
{
    UINT_PTR uIdSubclass=0;
    DWORD_PTR dwRefData=(DWORD_PTR)this;
    return SetWindowSubclass(this->hWndDialog,CShellMenu::HookWndProc,uIdSubclass,dwRefData);
}

//-----------------------------------------------------------------------------
// Name: UnSubClassWindowProc
// Object: remove window proc subclassing of owning window
// Parameters :
//     in  : 
//     out :
//     return : TRUE
//-----------------------------------------------------------------------------
BOOL CShellMenu::UnSubClassWindowProc()
{
    UINT_PTR uIdSubclass=0;
    // remove subclassing wndproc
    return RemoveWindowSubclass(this->hWndDialog,CShellMenu::HookWndProc,uIdSubclass);
}

//-----------------------------------------------------------------------------
// Name: BeforeParentMenuShow
// Object: MUST BE CALLED ONLY IF USED AS SUBMENU, BEFORE THE PARENT MENU TrackPopupMenu CALL
// Parameters :
//     in  : 
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::BeforeParentMenuShow()
{
    return this->SubClassWindowProc();
}

//-----------------------------------------------------------------------------
// Name: AfterParentMenuShow
// Object: MUST BE CALLED ONLY IF USED AS SUBMENU, AFTER THE PARENT MENU TrackPopupMenu CALL
// Parameters :
//     in  : 
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::AfterParentMenuShow()
{
    return this->UnSubClassWindowProc();
}

//-----------------------------------------------------------------------------
// Name: InvokeCommand
// Object: MUST BE CALLED ONLY IF USED AS SUBMENU, AFTER THE PARENT MENU TrackPopupMenu CALL
// Parameters :
//     in  : 
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
HRESULT CShellMenu::InvokeCommand(CMINVOKECOMMANDINFO* pCmdInfo)
{
    if (!this->pContextMenu)
        return E_FAIL;

    return this->pContextMenu->InvokeCommand(pCmdInfo);
}

//-----------------------------------------------------------------------------
// Name: FillMenu
// Object: fill the created menu for special folder
// Parameters :
//     in  : int SpecialFolderCSIDL : CSIDL of special folder (CSIDL_DESKTOP, ...)
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::FillMenu(int SpecialFolderCSIDL)
{
    BOOL bRet=FALSE;
    HRESULT hResult;
    ITEMIDLIST* pFullpidl=NULL;
    ITEMIDLIST* pFolderId=NULL;
    IShellFolder* pShellFolder=NULL;
    IMalloc* pMalloc=NULL;

    // get pointer to malloc interface
    hResult = ::SHGetMalloc(&pMalloc);
    if (FAILED(hResult)||(!pMalloc))
        goto CleanUp;

    // get folder full idl list
    hResult = ::SHGetFolderLocation(this->hWndDialog,SpecialFolderCSIDL,NULL,NULL,&pFullpidl);
    if ( FAILED(hResult) || (!pFullpidl) )
        goto CleanUp;

    // get upper folder and object id from full pidl
    hResult = ::SHBindToParent(pFullpidl, IID_IShellFolder, (void**)&pShellFolder,(LPCITEMIDLIST*)&pFolderId);
    if ( FAILED(hResult) || (!pFolderId) || (!pShellFolder) )
        goto CleanUp;

    if (this->bFolderBackgroundMenu)
    {
        // for shellview background menu
        //IShellView* pShellView=NULL;
        //pShellFolder->CreateViewObject(this->hWndDialog,IID_IShellView,(void**)&pShellView);
        //pShellView->GetItemObject(SVGIO_BACKGROUND,IID_IContextMenu,(void**)&this->pContextMenu);

        hResult = pShellFolder->CreateViewObject(this->hWndDialog,IID_IContextMenu,(void**)&this->pContextMenu);
        if ( FAILED(hResult) || (!this->pContextMenu) )
            goto CleanUp;
        bRet=this->FillContextMenuMember(pShellFolder);
    }
    else
    {
        // fill the menu from matching IShellFolder and folder id
        bRet=this->FillMenu(pShellFolder,pFolderId);
    }

    if (!bRet)
        goto CleanUp;

    bRet=TRUE;

CleanUp:

    if (pFullpidl)
        pMalloc->Free(pFullpidl);

    if (pMalloc)
        pMalloc->Release();

    if (pShellFolder)
        pShellFolder->Release();

    return bRet;
}

//-----------------------------------------------------------------------------
// Name: FillMenu
// Object: fill the created menu for folder or file
// Parameters :
//     in  : TCHAR* FileOrDirectoryPath : folder or file path
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::FillMenu(TCHAR* FileOrDirectoryPath)
{
    BOOL bRet=FALSE;
    HRESULT hResult;
    WCHAR* pwscPath;
    IShellFolder* pDesktopFolder=NULL;
    IShellFolder* pShellFolder=NULL;
    IMalloc* pMalloc=NULL;
    ITEMIDLIST* pFullpidl=NULL;
    ITEMIDLIST* pFolderId=NULL;

    // check parameter
    if (::IsBadReadPtr(FileOrDirectoryPath,sizeof(TCHAR)))
        return FALSE;

    // convert ansi to unicode if needed
#if ( defined(UNICODE) || defined(_UNICODE))
    pwscPath=FileOrDirectoryPath;
#else
    size_t NbCharacters = strlen(FileOrDirectoryPath)+1;

    pwscPath = (WCHAR*) malloc(NbCharacters*sizeof(WCHAR));
    if (!pwscPath)
        return FALSE;

    // Convert to Unicode.
    if (::MultiByteToWideChar(CP_ACP, 0, FileOrDirectoryPath, (int)NbCharacters,pwscPath, (int)NbCharacters)==0)
    {
        free(pwscPath);
        return FALSE;
    }
#endif

    // get pointer to malloc interface
    hResult = ::SHGetMalloc(&pMalloc);
    if (FAILED(hResult)||(!pMalloc))
        goto CleanUp;

    // get root folder
    hResult= ::SHGetDesktopFolder(&pDesktopFolder);
    if (FAILED(hResult)||(!pDesktopFolder))
        goto CleanUp;

    // get object full pidl from path name
    hResult = pDesktopFolder->ParseDisplayName(NULL,
                                               NULL,
                                               pwscPath,
                                               NULL,
                                               &pFullpidl,
                                               NULL);
    if ( FAILED(hResult) || (!pFullpidl) )
        goto CleanUp;

    // get upper folder and object id from full pidl
    hResult = ::SHBindToParent(pFullpidl, IID_IShellFolder, (void**)&pShellFolder,(LPCITEMIDLIST*)&pFolderId);
    if ( FAILED(hResult) || (!pFolderId) || (!pShellFolder) )
        goto CleanUp;

    if (this->bFolderBackgroundMenu)
    {
        // for shellview background menu
        //IShellView* pShellView=NULL;
        //pShellFolder->CreateViewObject(this->hWndDialog,IID_IShellView,(void**)&pShellView);
        //pShellView->GetItemObject(SVGIO_BACKGROUND,IID_IContextMenu,(void**)&this->pContextMenu);

        hResult = pShellFolder->CreateViewObject(this->hWndDialog,IID_IContextMenu,(void**)&this->pContextMenu);
        if ( FAILED(hResult) || (!this->pContextMenu) )
            goto CleanUp;
        bRet=this->FillContextMenuMember(pShellFolder);
    }
    else
    {
        // fill the menu from matching IShellFolder and item (folder or file) id
        bRet=this->FillMenu(pShellFolder,pFolderId);
    }

    if (!bRet)
        goto CleanUp;

    bRet=TRUE;

CleanUp:

#if ( (!defined(UNICODE)) && (!defined(_UNICODE)) )
    if (pwscPath)
        free(pwscPath);
#endif

    if (pFullpidl)
        pMalloc->Free(pFullpidl);

    if (pMalloc)
        pMalloc->Release();

    if (pShellFolder)
        pShellFolder->Release();

    if (pDesktopFolder)
        pDesktopFolder->Release();

    return bRet;
}

//-----------------------------------------------------------------------------
// Name: FillMenu
// Object: fill the created menu for specified item
// Parameters :
//     in  : IShellFolder* pShellFolder : IShellFolder object
//           LPCITEMIDLIST pItemIdList : id of folder or file
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::FillMenu(IShellFolder* pShellFolder,LPCITEMIDLIST pItemIdList)
{
    HRESULT hResult;
    // get IContextMenu interface for specified item
    hResult = pShellFolder->GetUIObjectOf(hWndDialog,1,&pItemIdList,IID_IContextMenu,NULL,(void**)&this->pContextMenu);
    if (FAILED(hResult)||(!this->pContextMenu))
        return FALSE;

    return this->FillContextMenuMember(pShellFolder);
}

//-----------------------------------------------------------------------------
// Name: FillMenu
// Object: fill the created menu for specified item
// Parameters :
//     in  : IShellFolder* pShellFolder : IShellFolder object
//           LPCITEMIDLIST pItemIdList : id of folder or file
//     out :
//     return : TRUE on success
//-----------------------------------------------------------------------------
BOOL CShellMenu::FillContextMenuMember(IShellFolder* pShellFolder)
{
    HRESULT hResult;

    // try to get IContextMenu3 or at least IContextMenu2 interface for specified item
    hResult = this->pContextMenu->QueryInterface (IID_IContextMenu3,(void**)&this->pContextMenu3);
    if (FAILED(hResult) || (!this->pContextMenu3) )
        this->pContextMenu->QueryInterface (IID_IContextMenu2,(void**)&this->pContextMenu2);

    // get popupmenu content
    hResult = this->pContextMenu->QueryContextMenu(this->hPopUpMenu,
                                                    0,
                                                    CShellMenu_idCmdFirst,
                                                    CShellMenu_idCmdLast,
                                                    CMF_NORMAL
                                                    );

    return SUCCEEDED(hResult);
}


//-----------------------------------------------------------------------------
// Name: Show
// Object: MUST BE CALLED ONLY IF NO PARENT MENU (else menu will be shown for the parent menu TrackPopupMenu call)
// Parameters :
//     in  : int x : x menu position in screen coordinates
//           int y : y menu position in screen coordinates
//     out :
//     return : TRUE on success, FALSE on error or if user has cancel operation
//-----------------------------------------------------------------------------
BOOL CShellMenu::Show(int x,int y)
{
    if ( (!this->pContextMenu) || (!this->hPopUpMenu) )
        return FALSE;

    UINT CmdId;

    // add window subclassing
    this->SubClassWindowProc();

    // show popupmenu
    CmdId=(UINT)::TrackPopupMenuEx(this->hPopUpMenu,TPM_LEFTALIGN|TPM_RETURNCMD,x,y,this->hWndDialog,NULL);

    if (CmdId==0)
    {
        if (::GetLastError()==ERROR_POPUP_ALREADY_ACTIVE)
            CmdId=(UINT)::TrackPopupMenuEx(this->hPopUpMenu,TPM_LEFTALIGN|TPM_RETURNCMD|TPM_RECURSE,x,y,this->hWndDialog,NULL);
    }

    // remove subclassing
    this->UnSubClassWindowProc();

    // if one of our cmd id
    if ( (CShellMenu_idCmdFirst<=CmdId) && (CmdId<=CShellMenu_idCmdLast) )
    {
        CMINVOKECOMMANDINFO CmdInfo;
        memset(&CmdInfo,0,sizeof(CMINVOKECOMMANDINFO));
        CmdInfo.cbSize=sizeof(CMINVOKECOMMANDINFO);
        CmdInfo.lpVerb=MAKEINTRESOURCEA (CmdId - CShellMenu_idCmdFirst);
        CmdInfo.nShow=SW_SHOWNORMAL;
        CmdInfo.hwnd=hWndDialog;
        
        // invoke the required command
        if (FAILED(this->pContextMenu->InvokeCommand(&CmdInfo)))
            return FALSE;
    }
    return (CmdId!=0);
}


//-----------------------------------------------------------------------------
// Name: HookWndProc
// Object: sub classing window proc
//         REQUIRED TO DISPLAY ALL CONTEXT MENU ITEMS (plugins items and "Send To" content)
// Parameters :
//     in  : WndProc params
//     out :
//     return : WndProc return
//-----------------------------------------------------------------------------
LRESULT CALLBACK CShellMenu::HookWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,UINT_PTR uIdSubclass,DWORD_PTR dwRefData)
{
    UNREFERENCED_PARAMETER(uIdSubclass);

    CShellMenu* pShellMenu=(CShellMenu*)dwRefData;
    if (!pShellMenu)
        return TRUE;

    switch (uMsg)
    { 
    case WM_MENUCHAR:	// only supported by IContextMenu3
        if (pShellMenu->pContextMenu3)
        {
            LRESULT lResult = 0;
            pShellMenu->pContextMenu3->HandleMenuMsg2 (uMsg, wParam, lParam, &lResult);
            return lResult;
        }
        break;

    case WM_DRAWITEM:
    case WM_MEASUREITEM:
        if (wParam) 
            break; // if wParam != 0 then the message is not menu-related

    case WM_INITMENUPOPUP:
        {
            if (pShellMenu->pContextMenu3)
            {
                LRESULT lResult = 0;
                pShellMenu->pContextMenu3->HandleMenuMsg2 (uMsg, wParam, lParam, &lResult);
                return lResult;
            }
            else if (pShellMenu->pContextMenu2)
            {
                pShellMenu->pContextMenu2->HandleMenuMsg (uMsg, wParam, lParam);
            }
            return (uMsg == WM_INITMENUPOPUP ? 0 : TRUE); // inform caller that we handled WM_INITPOPUPMENU by ourself

        }
        break;
    }

    return ::DefSubclassProc(hWnd,uMsg,wParam,lParam);
}
