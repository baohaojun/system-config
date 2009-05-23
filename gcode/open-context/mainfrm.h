/**************************************************************************
   THIS CODE AND INFORMATION IS PROVIDED 'AS IS' WITHOUT WARRANTY OF
   ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
   PARTICULAR PURPOSE.
   Author: Leon Finker  01/2001
**************************************************************************/
// MainFrm.h : interface of the CMainFrame class
/////////////////////////////////////////////////////////////////////////////
#ifndef __MAINFRM_H__
#define __MAINFRM_H__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define IDC_FILETREE 10001
#ifndef WM_GETISHELLBROWSER
    #define WM_GETISHELLBROWSER (WM_USER+7)
#endif
   
class IShellBrowserImpl : public IShellBrowser, public ICommDlgBrowser
{
	DWORD m_dwRef;
public:
	IShellBrowserImpl():m_dwRef(0){}

	STDMETHOD(QueryInterface)(REFIID iid, void **ppvObject)
	{
		if(ppvObject == NULL)
			return E_POINTER;

		*ppvObject = NULL;

		if(iid == IID_IUnknown)
			*ppvObject = (IUnknown*)(IShellBrowser*) this;
		else if(iid == IID_IOleWindow)
			*ppvObject = (IOleWindow*) this;			
		else if(iid == IID_IShellBrowser)
			*ppvObject = (IShellBrowser*) this;
		else if(iid == IID_ICommDlgBrowser)
			*ppvObject = (ICommDlgBrowser*) this;
		else
			return E_NOINTERFACE;
		((IUnknown*)(*ppvObject))->AddRef();
		return S_OK;
	}

	STDMETHOD_(ULONG, AddRef)() { return ++m_dwRef; }
	STDMETHOD_(ULONG, Release)(){ return --m_dwRef; }  //not heap based	
    // *** IOleWindow methods ***
    STDMETHOD(ContextSensitiveHelp)(BOOL fEnterMode){return E_NOTIMPL;}	
	// *** ICommDlgBrowser methods ***
    STDMETHOD(OnDefaultCommand) (THIS_ struct IShellView * ppshv)
	{	//handle double click and ENTER key if needed
		return E_NOTIMPL; 
	}
    STDMETHOD(OnStateChange) (THIS_ struct IShellView * ppshv,ULONG uChange)
	{	//handle selection, rename, focus if needed
		return E_NOTIMPL; 
	}
    STDMETHOD(IncludeObject) (THIS_ struct IShellView * ppshv,LPCITEMIDLIST pidl)
	{	//filter files if needed
		return S_OK;
	}
    // *** IShellBrowser methods *** (same as IOleInPlaceFrame)
    STDMETHOD(InsertMenusSB) (HMENU hmenuShared, LPOLEMENUGROUPWIDTHS lpMenuWidths) {return E_NOTIMPL;}
    STDMETHOD(SetMenuSB) (HMENU hmenuShared, HOLEMENU holemenuReserved,HWND hwndActiveObject){return E_NOTIMPL;}
    STDMETHOD(RemoveMenusSB) (HMENU hmenuShared){return E_NOTIMPL;}
    STDMETHOD(SetStatusTextSB) (LPCOLESTR lpszStatusText){return E_NOTIMPL;}
    STDMETHOD(EnableModelessSB) (BOOL fEnable){return E_NOTIMPL;}
	STDMETHOD(BrowseObject)(LPCITEMIDLIST pidl, UINT wFlags){return E_NOTIMPL; }
	STDMETHOD(GetViewStateStream)(DWORD grfMode,LPSTREAM  *ppStrm){return E_NOTIMPL;}	
	STDMETHOD(OnViewWindowActive)(struct IShellView *ppshv){return E_NOTIMPL;}
	STDMETHOD(SetToolbarItems)(LPTBBUTTON lpButtons, UINT nButtons,UINT uFlags){return E_NOTIMPL;}
	STDMETHOD(TranslateAcceleratorSB) (LPMSG lpmsg, WORD wID) { return S_OK; }
};

class CExplorerSplitter : public WTL::CSplitterWindow
{
public:
	BEGIN_MSG_MAP(CExplorerSplitter)
		MESSAGE_RANGE_HANDLER(WM_USER, 0x7FFF, OnUserMsgForward) //forward the needed WM_USER+7
		CHAIN_MSG_MAP(CSplitterWindow)
		FORWARD_NOTIFICATIONS()
	END_MSG_MAP()
	
	LRESULT OnUserMsgForward(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;
		return ::SendMessage(GetParent(), uMsg, wParam, lParam);
	}
};

class CMainFrame : public CFrameWindowImpl<CMainFrame>, public CUpdateUI<CMainFrame>,
		public CMessageFilter, public CIdleHandler, public IShellBrowserImpl
{
	CExplorerSplitter	m_Splitter;
	WTL::CTreeViewCtrl	m_FileTreeBox;

	IShellView*			m_pShellView; // current hosted shellview
	HIMAGELIST		    m_hImageListLarge; // shell image
    HIMAGELIST			m_hImageListSmall; // list
	HWND				m_hWndListView;  //handle to shellview, which encapsulates listview
	
	struct TREEITEMINFO
	{
		TREEITEMINFO() {memset(this, 0, sizeof(TREEITEMINFO));}
		ITEMIDLIST*   pidlSelf;
		ITEMIDLIST*   pidlFullyQual;
		IShellFolder* pParentFolder;
		DWORD		  dwFlags;
	};

public:
	/////////////////////////////////////
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnTreeGetDispInfo(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
	LRESULT OnTreeItemExpanding(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
	LRESULT OnTreeDeleteItem(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
	LRESULT OnTreeItemSelected(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
	LRESULT OnTreeItemRClick(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);	
	LRESULT OnDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if(m_pShellView != NULL)
		{
			::PostMessage(m_hWndListView, WM_QUIT, 0, 0);
			m_pShellView->UIActivate(SVUIA_DEACTIVATE);
			m_pShellView->DestroyViewWindow();
			m_pShellView->Release();
			::PostMessage(m_hWnd, WM_QUIT, 0, 0);
		}
		return 0;
	}
	LRESULT OnGetIShellBrowser(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;
		//AddRef(); //not addrefed
		//SetWindowLong(DWL_MSGRESULT, (LONG)(IShellBrowser*)this); //use this if dialog
		return (LRESULT)(IShellBrowser*)this;
	}
	/////////////////////////////////////

	/////////////////////////////////////
	bool InitDragDrop();
	bool InitializeTree();
	bool Tree_EnumObjects(HTREEITEM hParentItem,IShellFolder* pParentFolder,ITEMIDLIST* pidlParent);
	void Tree_DoItemMenu(HWND hwndTreeView, HTREEITEM hItem, LPPOINT pptScreen);
	/////////////////////////////////////

	BEGIN_MSG_MAP(CMainFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_GETISHELLBROWSER, OnGetIShellBrowser)

		CHAIN_MSG_MAP(CUpdateUI<CMainFrame>)
		CHAIN_MSG_MAP(CFrameWindowImpl<CMainFrame>)
	END_MSG_MAP()
	
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MAINFRAME)

	BEGIN_UPDATE_UI_MAP(CMainFrame)
		UPDATE_ELEMENT(ID_VIEW_TOOLBAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_STATUS_BAR, UPDUI_MENUPOPUP)
	END_UPDATE_UI_MAP()

	//IOleWindow
	STDMETHOD(GetWindow)(HWND * lphwnd)
	{ 
		*lphwnd = m_Splitter; 
		return S_OK; 
	}
	//IShellBrowser
	STDMETHOD(QueryActiveShellView)(struct IShellView ** ppshv)
	{
		m_pShellView->AddRef();
		*ppshv = m_pShellView;
		return S_OK; 
	}
	STDMETHOD(GetControlWindow)(UINT id, HWND * lphwnd)
	{
		if(lphwnd == NULL)
			return E_POINTER;
		if(FCW_STATUS == id)
		{
			*lphwnd = m_hWndStatusBar;
			return S_OK;
		}
		return E_NOTIMPL;
	}

	STDMETHOD(SendControlMsg)(UINT id, UINT uMsg, WPARAM wParam,LPARAM lParam, LRESULT *pret)
	{
		if(pret == NULL)
			return E_POINTER;
		if(FCW_STATUS == id)
		{
			*pret = ::SendMessage(m_hWndStatusBar, uMsg, wParam, lParam);
			return S_OK;
		}
	
		return E_NOTIMPL;
	}
	
/****************************************************************************************/
	//misc
/****************************************************************************************/	
	LRESULT OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		PostMessage(WM_CLOSE);
		return 0;
	}

	LRESULT OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		BOOL bVisible = !::IsWindowVisible(m_hWndToolBar);
		::ShowWindow(m_hWndToolBar, bVisible ? SW_SHOWNOACTIVATE : SW_HIDE);
		UISetCheck(ID_VIEW_TOOLBAR, bVisible);
		UpdateLayout();
		return 0;
	}

	LRESULT OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		BOOL bVisible = !::IsWindowVisible(m_hWndStatusBar);
		::ShowWindow(m_hWndStatusBar, bVisible ? SW_SHOWNOACTIVATE : SW_HIDE);
		UISetCheck(ID_VIEW_STATUS_BAR, bVisible);
		UpdateLayout();
		return 0;
	}

	LRESULT OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CAboutDlg dlg;
		dlg.DoModal();
		return 0;
	}
	
	virtual BOOL PreTranslateMessage(MSG* pMsg)
	{
		return CFrameWindowImpl<CMainFrame>::PreTranslateMessage(pMsg);
	}

	virtual BOOL OnIdle()
	{
		UIUpdateToolBar();
		return FALSE;
	}
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif //__MAINFRM_H__
