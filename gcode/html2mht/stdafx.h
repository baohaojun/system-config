// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#ifdef _DEBUG
#define _STLP_DEBUG
#else
#undef _STLP_DEBUG
#endif

#undef UNICODE

#include <windows.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <string>
#include <list>
#include "base64.h"
#include <boost/crc.hpp>
#include <boost/regex.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <shlwapi.h>
#include "resource.h"

#pragma comment(lib, "shlwapi.lib")

using namespace std;
using namespace boost;