// stdafx.cpp : source file that includes just the standard includes
//	explorer.pch will be the pre-compiled header
//	stdafx.obj will contain the pre-compiled type information

#include "stdafx.h"
#include <atlimpl.cpp>


inline ITEMIDLIST* Pidl_GetNextItem(LPCITEMIDLIST pidl)
{
	if(pidl)
	{
	   return (ITEMIDLIST*)(BYTE*)(((BYTE*)pidl) + pidl->mkid.cb);
	}
	else
	   return NULL;
}

UINT Pidl_GetSize(LPCITEMIDLIST pidl)
{
UINT           cbTotal = 0;
ITEMIDLIST*   pidlTemp = (ITEMIDLIST*) pidl;

if(pidlTemp)
   {
   while(pidlTemp->mkid.cb)
      {
      cbTotal += pidlTemp->mkid.cb;
      pidlTemp = Pidl_GetNextItem(pidlTemp);
      }  

   // Requires a 16 bit zero value for the NULL terminator
   cbTotal += 2 * sizeof(BYTE);
   }

return cbTotal;
}

LPITEMIDLIST Pidl_Create(UINT cbSize)
{
	LPITEMIDLIST pidl = NULL;

	IMalloc* pMalloc;
	if(FAILED(SHGetMalloc(&pMalloc)))
		return false;

	pidl = (LPITEMIDLIST) pMalloc->Alloc(cbSize);
	if(pidl)
	   ZeroMemory(pidl, cbSize);

	pMalloc->Release();
return pidl;
}

LPITEMIDLIST Pidl_Concatenate(LPCITEMIDLIST pidl1, LPCITEMIDLIST pidl2)
{
	LPITEMIDLIST   pidlNew;
	UINT           cb1, 
				   cb2 = 0;
	if(pidl1)
	   cb1 = Pidl_GetSize(pidl1) - (2 * sizeof(BYTE));

	cb2 = Pidl_GetSize(pidl2);
	pidlNew = Pidl_Create(cb1 + cb2);
	if(pidlNew)
	   {
	   if(pidl1)   
		  CopyMemory(pidlNew, pidl1, cb1);

	   CopyMemory(((LPBYTE)pidlNew) + cb1, pidl2, cb2);
	   }
return pidlNew;
}
