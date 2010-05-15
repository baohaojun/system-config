// oc2.h : main header file for the oc2 application
//
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"       // main symbols


// Coc2App:
// See oc2.cpp for the implementation of this class
//

class Coc2App : public CWinApp
{
public:
	Coc2App();


// Overrides
public:
	virtual BOOL InitInstance();

// Implementation

public:
	afx_msg void OnAppAbout();
	DECLARE_MESSAGE_MAP()
};

extern Coc2App theApp;