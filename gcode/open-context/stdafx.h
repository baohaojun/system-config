// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__B5466F69_68C9_43BA_BAA4_0DC717E51478__INCLUDED_)
#define AFX_STDAFX_H__B5466F69_68C9_43BA_BAA4_0DC717E51478__INCLUDED_

// Change these values to use different versions
#define WINVER		0x0501
#define _WIN32_WINNT 0x501
#define _WIN32_IE	0x0500
#define _RICHEDIT_VER	0x0100


#include <atlbase.h>
#include <atlapp.h>

extern CAppModule _Module;

#include <atlcom.h>
#include <atlhost.h>
#include <atlwin.h>
#include <atlctl.h>
#include <atlsplit.h>
#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include "aboutdlg.h"
#include <shlobj.h>

inline LPITEMIDLIST Pidl_GetNextItem(LPCITEMIDLIST);
UINT Pidl_GetSize(LPCITEMIDLIST);
LPITEMIDLIST Pidl_Create(UINT);
LPITEMIDLIST Pidl_Concatenate(LPCITEMIDLIST, LPCITEMIDLIST);


//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__B5466F69_68C9_43BA_BAA4_0DC717E51478__INCLUDED_)
