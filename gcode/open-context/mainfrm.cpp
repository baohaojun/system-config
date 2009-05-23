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
#include "shellmenu.h"
#include <windows.h>

void OnBnClickedButton1(HWND m_hWnd)
{
	

	CShellMenu ShellMenu(m_hWnd, "C:\\1.txt");
	if (ShellMenu.IsCreationSuccessful())
	{
		// get mouse position
		POINT Point;
		::GetCursorPos(&Point);

		// display shell menu
		if (ShellMenu.Show(Point.x,Point.y))
		{
			// if user has done an action on shell menu,
			// close current menu
			EndMenu();
		}
	} 

}

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
	OnBnClickedButton1(m_hWnd);
	return  -1;
}

