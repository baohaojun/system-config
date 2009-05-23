// MainFrm.h : interface of the CMainFrame class
//


#pragma once

#include "ChildView.h"

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

class CMainFrame : public CFrameWnd, public IShellBrowserImpl
{
	IShellView*			m_pShellView; // current hosted shellview	
public:
	CMainFrame();
protected: 
	DECLARE_DYNAMIC(CMainFrame)

// Attributes
public:

// Operations
public:

// Overrides
public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	CChildView    m_wndView;

 	STDMETHOD(GetWindow)(HWND * lphwnd)
 	{ 
 		*lphwnd = m_hWnd; 
 		return S_OK; 
 	}
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
		return E_NOTIMPL;
	}
	STDMETHOD(SendControlMsg)(UINT id, UINT uMsg, WPARAM wParam,LPARAM lParam, LRESULT *pret)
	{
		if(pret == NULL)
			return E_POINTER;

		return E_NOTIMPL;
	}


// Generated message map functions
protected:
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSetFocus(CWnd *pOldWnd);
	DECLARE_MESSAGE_MAP()
};


