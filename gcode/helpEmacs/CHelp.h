// Machine generated IDispatch wrapper class(es) created with Add Class from Typelib Wizard

#import "C:\\Program Files\\Common Files\\Microsoft Shared\\MSEnv\\vshelp.tlb" no_namespace
// CHelp wrapper class

class CHelp : public COleDispatchDriver
{
public:
	CHelp(){} // Calls COleDispatchDriver default constructor
	CHelp(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CHelp(const CHelp& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:

	// Operations
public:


	// Help methods
public:
	void Contents()
	{
		InvokeHelper(0x1, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void Index()
	{
		InvokeHelper(0x2, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void Search()
	{
		InvokeHelper(0x3, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void IndexResults()
	{
		InvokeHelper(0x4, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void SearchResults()
	{
		InvokeHelper(0x5, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void DisplayTopicFromId(LPCTSTR bstrFile, unsigned long Id)
	{
		static BYTE parms[] = VTS_BSTR VTS_UI4 ;
		InvokeHelper(0x6, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrFile, Id);
	}
	void DisplayTopicFromURL(LPCTSTR pszURL)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0x7, DISPATCH_METHOD, VT_EMPTY, NULL, parms, pszURL);
	}
	void DisplayTopicFromURLEx(LPCTSTR pszURL, LPUNKNOWN pIVsHelpTopicShowEvents)
	{
		static BYTE parms[] = VTS_BSTR VTS_UNKNOWN ;
		InvokeHelper(0x8, DISPATCH_METHOD, VT_EMPTY, NULL, parms, pszURL, pIVsHelpTopicShowEvents);
	}
	void DisplayTopicFromKeyword(LPCTSTR pszKeyword)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0x9, DISPATCH_METHOD, VT_EMPTY, NULL, parms, pszKeyword);
	}
	void DisplayTopicFromF1Keyword(LPCTSTR pszKeyword)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0xa, DISPATCH_METHOD, VT_EMPTY, NULL, parms, pszKeyword);
	}
	void DisplayTopicFrom_OLD_Help(LPCTSTR bstrFile, unsigned long Id)
	{
		static BYTE parms[] = VTS_BSTR VTS_UI4 ;
		InvokeHelper(0xb, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrFile, Id);
	}
	void SyncContents(LPCTSTR bstrURL)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0xc, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrURL);
	}
	void CanSyncContents(LPCTSTR bstrURL)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0xd, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrURL);
	}
	CString GetNextTopic(LPCTSTR bstrURL)
	{
		CString result;
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0xe, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms, bstrURL);
		return result;
	}
	CString GetPrevTopic(LPCTSTR bstrURL)
	{
		CString result;
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0xf, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms, bstrURL);
		return result;
	}
	void FilterUI()
	{
		InvokeHelper(0x10, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void CanShowFilterUI()
	{
		InvokeHelper(0x11, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void Close()
	{
		InvokeHelper(0x12, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
	}
	void SyncIndex(LPCTSTR bstrKeyword, long fShow)
	{
		static BYTE parms[] = VTS_BSTR VTS_I4 ;
		InvokeHelper(0x13, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrKeyword, fShow);
	}
	void SetCollection(LPCTSTR bstrCollection, LPCTSTR bstrFilter)
	{
		static BYTE parms[] = VTS_BSTR VTS_BSTR ;
		InvokeHelper(0x14, DISPATCH_METHOD, VT_EMPTY, NULL, parms, bstrCollection, bstrFilter);
	}
	CString get_Collection()
	{
		CString result;
		InvokeHelper(0x15, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
		return result;
	}
	CString get_Filter()
	{
		CString result;
		InvokeHelper(0x16, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
		return result;
	}
	void put_Filter(LPCTSTR newValue)
	{
		static BYTE parms[] = VTS_BSTR ;
		InvokeHelper(0x16, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms, newValue);
	}
	CString get_FilterQuery()
	{
		CString result;
		InvokeHelper(0x17, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
		return result;
	}
	LPDISPATCH get_HelpOwner()
	{
		LPDISPATCH result;
		InvokeHelper(0x18, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
		return result;
	}
	void put_HelpOwner(LPDISPATCH newValue)
	{
		static BYTE parms[] = VTS_DISPATCH ;
		InvokeHelper(0x18, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms, newValue);
	}
	LPDISPATCH get_HxSession()
	{
		LPDISPATCH result;
		InvokeHelper(0x19, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
		return result;
	}
	LPDISPATCH get_Help()
	{
		LPDISPATCH result;
		InvokeHelper(0x1a, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
		return result;
	}
	LPDISPATCH GetObject(LPCTSTR bstrMoniker, LPCTSTR bstrOptions)
	{
		LPDISPATCH result;
		static BYTE parms[] = VTS_BSTR VTS_BSTR ;
		InvokeHelper(0x1b, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms, bstrMoniker, bstrOptions);
		return result;
	}

	// Help properties
public:

};
