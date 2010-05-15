// helpEmacs.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "CHelp.h"

// ChelpEmacsApp:
// See helpEmacs.cpp for the implementation of this class
//

class ChelpEmacsApp : public CWinApp
{
public:
	ChelpEmacsApp();

// Overrides
	public:
	virtual BOOL InitInstance();
	CHelp help;

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern ChelpEmacsApp theApp;

#define PATH_SEP '\\'

#define BHJDEBUG(fmt, ...) do {                                       \
        printf("%s %s() %d: " fmt "\n", strrchr(__FILE__, PATH_SEP)?strrchr(__FILE__, PATH_SEP)+1:__FILE__,\
               __FUNCTION__, __LINE__,\
               ##__VA_ARGS__);\
    } while(0) 
