/**************************************************************************
   THIS CODE AND INFORMATION IS PROVIDED 'AS IS' WITHOUT WARRANTY OF
   ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
   PARTICULAR PURPOSE.
   Author: Leon Finker  01/2001
**************************************************************************/
#include "stdafx.h"
#include "resource.h"
#include "mainfrm.h"
#include "treedroptarget.h"

LRESULT CMainFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_pShellView = NULL;

	CreateSimpleToolBar();
	CreateSimpleStatusBar();

	RECT rcSplitter;
	::GetClientRect(m_hWnd, &rcSplitter);
	m_hWndClient = m_Splitter.Create(m_hWnd, rcSplitter, NULL, WS_VISIBLE|WS_CHILD|WS_CLIPSIBLINGS,WS_EX_CLIENTEDGE);
	if(m_hWndClient == NULL)
		return FALSE;
	m_Splitter.m_cxyMin = 0;
	m_Splitter.m_cxyBarEdge=4;
	m_Splitter.m_bFullDrag = false;
	m_Splitter.SetSplitterPos(200);	

	if(!m_FileTreeBox.Create(m_Splitter, rcSplitter, NULL, WS_OVERLAPPED|WS_TABSTOP|WS_VISIBLE|WS_CHILD|TVS_HASLINES|TVS_LINESATROOT|TVS_HASBUTTONS|TVS_NOTOOLTIPS, 0, IDC_FILETREE))
		return FALSE;
	m_Splitter.SetSplitterPane(SPLIT_PANE_LEFT, m_FileTreeBox);
	
	SHFILEINFO  sfi;
	m_hImageListLarge = (HIMAGELIST)SHGetFileInfo( TEXT("C:\\"), 0,&sfi,sizeof(SHFILEINFO), SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
	m_hImageListSmall = (HIMAGELIST)SHGetFileInfo( TEXT("C:\\"),0,&sfi,sizeof(SHFILEINFO),SHGFI_SYSICONINDEX | SHGFI_SMALLICON);

	if(!InitializeTree())
		return false;
	
	if(!InitDragDrop())
		ATLTRACE("FAILED to Init Drag and Drop.\n");

	UIAddToolBar(m_hWndToolBar);
	UISetCheck(ID_VIEW_TOOLBAR, 1);
	UISetCheck(ID_VIEW_STATUS_BAR, 1);

	// register object for message filtering and idle updates
	CMessageLoop* pLoop = _Module.GetMessageLoop();
	ATLASSERT(pLoop != NULL);
	pLoop->AddMessageFilter(this);
	pLoop->AddIdleHandler(this);

	return 0;
}

bool CMainFrame::InitDragDrop()
{
		m_pDropTarget = new CTreeDropTarget(m_FileTreeBox);
		if(m_pDropTarget == NULL)
			return false;
		m_pDropTarget->AddRef();

		if(FAILED(RegisterDragDrop(m_FileTreeBox,m_pDropTarget))) //calls addref
		{
			m_pDropTarget = NULL;
			return false;
		}
		else
			m_pDropTarget->Release(); //i decided to AddRef explicitly after new

		FORMATETC ftetc={0};
		ftetc.dwAspect = DVASPECT_CONTENT;
		ftetc.lindex = -1;
		ftetc.tymed = TYMED_HGLOBAL;
		ftetc.cfFormat=CF_HDROP;
		m_pDropTarget->AddSuportedFormat(ftetc);
	return true;
}

void CMainFrame::Tree_DoItemMenu(HWND hwndTreeView, HTREEITEM hItem, LPPOINT pptScreen)
{
TVITEM   tvItem;

ZeroMemory(&tvItem, sizeof(tvItem));
tvItem.mask = TVIF_PARAM;
tvItem.hItem = hItem;

if(TreeView_GetItem(hwndTreeView, &tvItem))
   {
	HWND           hwndParent = ::GetParent(hwndTreeView);
    HRESULT        hr;
    TREEITEMINFO*     pInfo = (TREEITEMINFO*)tvItem.lParam;
    IContextMenu   *pcm;
    IShellFolder   *psfFolder = pInfo->pParentFolder;

   if(!psfFolder)
      {
      SHGetDesktopFolder(&psfFolder);
      }
   else
      {
      psfFolder->AddRef();
      }

   if(psfFolder)
      {
      hr = psfFolder->GetUIObjectOf(   hwndParent, 
                                       1, 
                                       (LPCITEMIDLIST*)&pInfo->pidlSelf,
                                       IID_IContextMenu, 
                                       NULL, 
                                       (LPVOID*)&pcm);

      if(SUCCEEDED(hr))
         {
         HMENU hPopup;

         hPopup = CreatePopupMenu();
         if(hPopup)
            {
            hr = pcm->QueryContextMenu(hPopup, 0, 1, 0x7fff, CMF_NORMAL | CMF_EXPLORE);

            if(SUCCEEDED(hr))
               {
			   IContextMenu2* pcm2;
               pcm->QueryInterface(IID_IContextMenu2, (LPVOID*)&pcm2);

               UINT  idCmd;

               idCmd = TrackPopupMenu( hPopup, 
                                       TPM_LEFTALIGN | TPM_RETURNCMD | TPM_RIGHTBUTTON, 
                                       pptScreen->x,
                                       pptScreen->y,
                                       0,
                                       hwndParent,
                                       NULL);
            
               if(pcm2)
                  {
                  pcm2->Release();
                  pcm2 = NULL;
                  }

               if(idCmd)
                  {
                  CMINVOKECOMMANDINFO  cmi;
                  cmi.cbSize = sizeof(CMINVOKECOMMANDINFO);
                  cmi.fMask = 0;
                  cmi.hwnd = hwndParent;
                  cmi.lpVerb = (LPCSTR)(INT_PTR)(idCmd - 1);
                  cmi.lpParameters = NULL;
                  cmi.lpDirectory = NULL;
                  cmi.nShow = SW_SHOWNORMAL;
                  cmi.dwHotKey = 0;
                  cmi.hIcon = NULL;
                  hr = pcm->InvokeCommand(&cmi);
                  }
               }
            }
      
         pcm->Release();
         }

      psfFolder->Release();
      }
   }
}

bool CMainFrame::InitializeTree()
{
	IShellFolder* pDesktop;
	ITEMIDLIST*   pidl;
	TV_ITEM tvItem={0};
	TV_INSERTSTRUCT   tvInsert={0};

	m_FileTreeBox.SetImageList(m_hImageListSmall,TVSIL_NORMAL);
	m_FileTreeBox.SetScrollTime(100);

	if(SUCCEEDED(SHGetSpecialFolderLocation(NULL, CSIDL_DESKTOP, &pidl)))
	{
	   if(FAILED(SHGetDesktopFolder(&pDesktop)))
	   {
		   //TODO: free the pidl through IMalloc
		   return false;
	   }

	   TREEITEMINFO* pItemInfo = new TREEITEMINFO;
	   if(pItemInfo == NULL)
	   {
		   //TODO: free the pidl through IMalloc
		   pDesktop->Release();
		   return false;
	   }
	   pItemInfo->pidlSelf = pidl;
	   pItemInfo->pidlFullyQual = pidl;
	   tvItem.mask = TVIF_PARAM | TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_CHILDREN;
	   tvItem.lParam= (LPARAM)pItemInfo;
	   tvItem.pszText = LPSTR_TEXTCALLBACK;
	   tvItem.iImage = tvItem.iSelectedImage = I_IMAGECALLBACK;
	   tvItem.cChildren = TRUE;
	   tvInsert.item = tvItem;
	   tvInsert.hInsertAfter = TVI_LAST;
	   HTREEITEM hItem = m_FileTreeBox.InsertItem(&tvInsert);
	   m_FileTreeBox.SelectItem(hItem);
	   m_FileTreeBox.Expand(hItem);
	   pDesktop->Release();
	   return true;
	}

	return false;
}

LRESULT CMainFrame::OnTreeItemRClick(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	TVHITTESTINFO  tvhti;
	GetCursorPos(&tvhti.pt);
	::ScreenToClient(m_FileTreeBox, &tvhti.pt);
	tvhti.flags = LVHT_NOWHERE;
	TreeView_HitTest(m_FileTreeBox, &tvhti);

	if(TVHT_ONITEM & tvhti.flags)
	   {
		::ClientToScreen(m_FileTreeBox, &tvhti.pt);
		Tree_DoItemMenu(m_FileTreeBox, tvhti.hItem , &tvhti.pt);
	   }

	return 0;
}

LRESULT CMainFrame::OnTreeGetDispInfo(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
      LPNMTVDISPINFO lpdi = (LPNMTVDISPINFO)pnmh;
      TREEITEMINFO* pItemInfo = (TREEITEMINFO*)lpdi->item.lParam;
	  SHFILEINFO     sfi;

      if(lpdi->item.mask & TVIF_TEXT)
         {
         if(SHGetFileInfo((LPCTSTR)pItemInfo->pidlFullyQual, 0, &sfi, sizeof(sfi), SHGFI_PIDL | SHGFI_DISPLAYNAME))
            lstrcpy(lpdi->item.pszText, sfi.szDisplayName);
         }
	  
      if(lpdi->item.mask & TVIF_IMAGE)
         {
         if(SHGetFileInfo((LPCTSTR)pItemInfo->pidlFullyQual, 0, &sfi, sizeof(sfi), SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_LINKOVERLAY))
            lpdi->item.iImage = sfi.iIcon;
         }
      if(lpdi->item.mask & TVIF_SELECTEDIMAGE)
         {
         if(SHGetFileInfo((LPCTSTR)pItemInfo->pidlFullyQual, 0, &sfi, sizeof(sfi), SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_OPENICON))
            lpdi->item.iSelectedImage = sfi.iIcon;
         }
	return 0;
}

LRESULT CMainFrame::OnTreeItemSelected(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	NM_TREEVIEW* pnmtv = (NM_TREEVIEW*) pnmh;
	TREEITEMINFO* pItemInfo = (TREEITEMINFO*)pnmtv->itemNew.lParam;	
	if (pItemInfo->dwFlags == 0)
	{
		IShellFolder* pParentFolder;
		if(pItemInfo->pParentFolder == 0)
		{
			if(FAILED(SHGetDesktopFolder(&pParentFolder)))
				return FALSE;
		}
		else if(FAILED(pItemInfo->pParentFolder->BindToObject(pItemInfo->pidlSelf, NULL, IID_IShellFolder, (void**)&pParentFolder)))
			return 0;

		FOLDERSETTINGS fs;
		IShellView* pShellView2 = m_pShellView;
		
		if(pShellView2 != NULL)
		{
			pShellView2->GetCurrentInfo(&fs);
		}
		else
		{
			fs.fFlags = FVM_DETAILS;
			fs.ViewMode = FWF_SNAPTOGRID;
		}
		HRESULT hr = pParentFolder->CreateViewObject(m_Splitter, IID_IShellView, (void**)&m_pShellView);
		if(FAILED(hr))
		{
			m_pShellView=0;
			return 0;
		}
		
		AddRef();
		m_pShellView->CreateViewWindow(pShellView2, &fs, (IShellBrowser*)this, &CWindow::rcDefault, &m_hWndListView);
		if(pShellView2 != NULL)
		{
			pShellView2->GetCurrentInfo(&fs);
			pShellView2->UIActivate(SVUIA_DEACTIVATE);
			pShellView2->DestroyViewWindow();
			pShellView2->Release();
		}
		m_Splitter.SetSplitterPane(SPLIT_PANE_RIGHT, m_hWndListView);
		m_pShellView->UIActivate(SVUIA_ACTIVATE_NOFOCUS);

		pParentFolder->Release();

		/////////////////////////////////////////
	}
	return 0;
}

LRESULT CMainFrame::OnTreeDeleteItem(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTREEVIEW   pnmtv = (LPNMTREEVIEW)pnmh;
    IMalloc* pMalloc;
	TREEITEMINFO* pItemInfo = (TREEITEMINFO*)pnmtv->itemOld.lParam;
      if(SUCCEEDED(SHGetMalloc(&pMalloc)))
         {
			 if(pItemInfo->dwFlags == 0)
			 {
				 pMalloc->Free(pItemInfo->pidlSelf);
				 pMalloc->Release();
				 if(pItemInfo->pParentFolder)
				 {
					pItemInfo->pParentFolder->Release();
					pMalloc->Free(pItemInfo->pidlFullyQual);
				 }
			 }
         }
      
	delete pItemInfo;
	return 0;
}

LRESULT CMainFrame::OnTreeItemExpanding(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTREEVIEW   pnmtv = (LPNMTREEVIEW)pnmh;
	IShellFolder *pParentFolder;

	if(pnmtv->action == TVE_COLLAPSE)
        m_FileTreeBox.Expand(pnmtv->itemNew.hItem, TVE_COLLAPSE | TVE_COLLAPSERESET);
    else if(pnmtv->action == TVE_EXPAND)
	{
		HCURSOR	 hCursor;
		TVITEM   tvItem = {0};
		tvItem.mask = TVIF_PARAM;
		tvItem.hItem = pnmtv->itemNew.hItem;
		if(!m_FileTreeBox.GetItem(&tvItem))
			return FALSE;
		TREEITEMINFO* pItemInfo = (TREEITEMINFO*)tvItem.lParam;
		hCursor = SetCursor(LoadCursor(NULL, IDC_WAIT));
		
		if(pItemInfo->pParentFolder == 0)
		{
			if(FAILED(SHGetDesktopFolder(&pParentFolder)))
				return FALSE;
		}
		else if(FAILED(pItemInfo->pParentFolder->BindToObject(pItemInfo->pidlSelf, NULL, IID_IShellFolder, (void**)&pParentFolder)))
			return 0;
	
		m_FileTreeBox.SetRedraw(FALSE);
		Tree_EnumObjects(pnmtv->itemNew.hItem, pParentFolder, pItemInfo->pidlFullyQual);
		m_FileTreeBox.SetRedraw(TRUE);
		pParentFolder->Release();
		SetCursor(hCursor);
	}
	return 0;
}

bool CMainFrame::Tree_EnumObjects(HTREEITEM hParentItem,IShellFolder* pParentFolder, ITEMIDLIST* pidlParent)
{
	IEnumIDList* pEnum;
	if(SUCCEEDED(pParentFolder->EnumObjects(NULL, SHCONTF_FOLDERS|SHCONTF_INCLUDEHIDDEN, &pEnum)))
	{
		   ITEMIDLIST* pidl;
		   DWORD  dwFetched = 1;
		   DWORD  dwAttribs;
		   TV_ITEM tvItem={0};
		   TV_INSERTSTRUCT   tvInsert={0};
		   while(SUCCEEDED(pEnum->Next(1, &pidl, &dwFetched)) && dwFetched)
		   {
				TREEITEMINFO* pItemInfo = new TREEITEMINFO;
			    pItemInfo->pidlSelf = pidl;
			    pItemInfo->pidlFullyQual = Pidl_Concatenate(pidlParent,pidl);
				pParentFolder->AddRef();
				pItemInfo->pParentFolder = pParentFolder;
				ZeroMemory(&tvItem, sizeof(tvItem));
				tvItem.mask = TVIF_PARAM | TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_CHILDREN;
				tvItem.pszText = LPSTR_TEXTCALLBACK;
				tvItem.iImage = tvItem.iSelectedImage = I_IMAGECALLBACK;
				tvItem.lParam= (LPARAM)pItemInfo;
				dwAttribs = SFGAO_HASSUBFOLDER | SFGAO_FOLDER | SFGAO_DISPLAYATTRMASK | SFGAO_CANRENAME;
				pParentFolder->GetAttributesOf(1, (LPCITEMIDLIST*)&pidl, &dwAttribs);
				tvItem.cChildren = (dwAttribs & SFGAO_HASSUBFOLDER);
		        if(dwAttribs & SFGAO_SHARE)
				{
					tvItem.mask |= TVIF_STATE;
					tvItem.stateMask |= TVIS_OVERLAYMASK;
					tvItem.state |= INDEXTOOVERLAYMASK(1);
				}
			    tvInsert.item = tvItem;
			    tvInsert.hInsertAfter = TVI_LAST;
				tvInsert.hParent = hParentItem;
				m_FileTreeBox.InsertItem(&tvInsert);
				dwFetched = 0;
		   }
		pEnum->Release();
		return true;
	}
	return false;
}

