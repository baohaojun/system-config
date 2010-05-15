// RunBhjRun.h : main header file for the RUNBHJRUN application
//

#if !defined(AFX_RUNBHJRUN_H__DF220C5D_947C_4B97_B308_BEDFB557127F__INCLUDED_)
#define AFX_RUNBHJRUN_H__DF220C5D_947C_4B97_B308_BEDFB557127F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CRunBhjRunApp:
// See RunBhjRun.cpp for the implementation of this class
//

class CRunBhjRunApp : public CWinApp
{
public:
	CRunBhjRunApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRunBhjRunApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CRunBhjRunApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RUNBHJRUN_H__DF220C5D_947C_4B97_B308_BEDFB557127F__INCLUDED_)
