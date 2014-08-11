/// <summary>
/// Snarl C++ interface implementation
/// API version 42
///
/// http://sourceforge.net/apps/mediawiki/snarlwin/index.php?title=Windows_API
/// https://sourceforge.net/apps/mediawiki/snarlwin/index.php?title=Generic_API
///
/// Written and maintained by Toke Noer Nøttrup (toke@noer.it)
///
///  Please note the following changes compared to the VB6 (official API) dokumentation:
///  - Function names doesn't have the prefix "sn". Naming of constants and variables are
///    generally changed to follow Microsoft C# standard. This naming convention is kept for
///    the C++ version, to keep them alike.
///  - Grouped variables like SNARL_LAUNCHED, SNARL_QUIT is enums in SnarlEnums namespace.
///  - Message events like SNARL_NOTIFICATION_CLICKED, is found in SnarlEnums::MessageEvent.
///  - Please note that string functions return NULL when they fail and not an empty string.
///  - Some functions in the VB API takes an appToken as first parameter. This token is a
///    member variable in C++ version, so it is omitted from the functions.
///    (Always call RegisterApp as first function!)
///  - Functions manipulating messages (Update, Hide etc.) still takes a message token as
///    parameter, but you can get the last message token calling GetLastMsgToken();
///    Example: snarl.Hide(snarl.GetLastMsgToken());
///
/// The functions in SnarlInterface both have ANSI(UTF8) and UNICODE versions.
/// If the LPCWSTR (unicode) version of the functions are called, the strings
/// are converted to UTF8 by SnarlInterface before sent to Snarl. So using the
/// ANSI/UTF8/LPCSTR versions of the functions are faster!
///
/// See https://sourceforge.net/apps/mediawiki/snarlwin/index.php?title=Windows_API#C.2B.2B
/// for example code etc.
/// </summary>
///----------------------------------------------------------------------------
/// <VersionHistory>
///  2011-07-31 : General update to match VB6 SVN rev. 232
///             : Added AppFlags and MessagePriority enums.
///  2011-07-12 : MingW64 fixes by Patrick von Reth
///  2011-07-07 : Some changes to compile under VS2008
///                 - Changed vector iterators to begin/end instead of cbegin/cend
///                 - Removed const in PairType
///  2011-02-09 : Fix for wrong parameters in AddClass
///  2011-02-06 : Updated to rev. 3 of the wiki documentation
///  2011-02-02 : First release of V42 Snarl API implementation
/// </VersionHistory>
///
/// <Todo>
/// </Todo>

#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "SnarlInterface.h"


namespace Snarl {
namespace V42 {


// workaround for mingw-w64 bug
#ifdef __MINGW64_VERSION_MAJOR
    extern "C" {
        __declspec(dllimport) errno_t __cdecl strcpy_s(char * _Dst, size_t _SizeInBytes, const char *_Src);
        __declspec(dllimport) errno_t __cdecl wcscpy_s(wchar_t * _Dst, size_t _SizeInBytes, const wchar_t *_Src);
        __declspec(dllimport) errno_t __cdecl strncat_s(char *_Dst, size_t _DstSizeInChars, const char *_Src, size_t _MaxCount);
    }
#endif //__MINGW64_VERSION_MAJOR


// ----------------------------------------------------------------------------
// Static Snarl interface functions
// ----------------------------------------------------------------------------

UINT SnarlInterface::AppMsg()
{
    return RegisterWindowMessage(SnarlAppMsg);
}

UINT SnarlInterface::Broadcast()
{
    return RegisterWindowMessage(SnarlGlobalMsg);
}

LONG32 SnarlInterface::DoRequest(LPCSTR request, UINT replyTimeout)
{
    DWORD_PTR nResult = 0;

    HWND hWnd = GetSnarlWindow();
    if (!IsWindow(hWnd))
        return -SnarlEnums::ErrorNotRunning;

    // Create COPYDATASTRUCT
    COPYDATASTRUCT cds;
    cds.dwData = 0x534E4C03;           // "SNL",3
    cds.cbData = (DWORD)strlen(request);      // No knowledge of max string lenght
    cds.lpData = const_cast<char*>(request);

    // Send message
    if (SendMessageTimeout(hWnd, WM_COPYDATA, (WPARAM)GetCurrentProcessId(), (LPARAM)&cds, SMTO_ABORTIFHUNG | SMTO_NOTIMEOUTIFNOTHUNG, replyTimeout, &nResult) == 0)
    {
        // Handle error
        DWORD nError = GetLastError();
        LONG32 errorResult = 0;
        if (nError == ERROR_TIMEOUT)
            errorResult = -SnarlEnums::ErrorTimedOut;
        else
            errorResult = -SnarlEnums::ErrorFailed;

        return errorResult;
    }
    else
        return (LONG32)nResult;
}

LONG32 SnarlInterface::DoRequest(LPCWSTR request, UINT replyTimeout)
{
    LONG32 nResult = 0;

    // Convert to UTF8
    LPSTR utf8Request = WideToUTF8(request);
    if (utf8Request == NULL)
        return -SnarlEnums::ErrorCppInterface;

    nResult = DoRequest(utf8Request, replyTimeout);

    // Cleanup and return result
    FreeString(utf8Request);
    return nResult;
}

std::basic_string<char>& SnarlInterface::Escape(std::basic_string<char>& str)
{
    std::basic_string<char>::size_type strLength = str.length();
    for (std::basic_string<char>::size_type i = 0; i < strLength; ++i)
    {
        if (str.at(i) == '=') {
            str.insert(++i, "=");
            ++strLength;
        }
        else if (str[i] == '&') {
            str.insert(++i, "&");
            ++strLength;
        }
    }

    return str;
}

std::basic_string<wchar_t>& SnarlInterface::Escape(std::basic_string<wchar_t>& str)
{
    std::basic_string<wchar_t>::size_type strLength = str.length();
    for (std::basic_string<wchar_t>::size_type i = 0; i < strLength; ++i)
    {
        if (str.at(i) == L'=') {
            str.insert(++i, L"=");
            ++strLength;
        }
        else if (str[i] == L'&') {
            str.insert(++i, L"&");
            ++strLength;
        }
    }

    return str;
}

LPCTSTR SnarlInterface::GetAppPath()
{
    HWND hWnd = GetSnarlWindow();
    if (hWnd)
    {
        HWND hWndPath = FindWindowEx(hWnd, NULL, _T("static"), NULL);
        if (hWndPath)
        {
            TCHAR strTmp[MAX_PATH] = {0};
            int nReturn = GetWindowText(hWndPath, strTmp, MAX_PATH-1);
            if (nReturn > 0) {
                TCHAR* strReturn = AllocateString(nReturn + 1);
                _tcsncpy(strReturn, strTmp, nReturn + 1);
                strReturn[nReturn] = 0;
                return strReturn;
            }
        }
    }

    return NULL;
}

LPCTSTR SnarlInterface::GetIconsPath()
{
    TCHAR* szIconPath = NULL;
    LPCTSTR szPath = GetAppPath();
    if (!szPath)
        return NULL;

    size_t nLen = 0;
    nLen = _tcsnlen(szPath, MAX_PATH);
    if (nLen > 0)
    {
        nLen += 10 + 1; // etc\\icons\\ + NULL
        szIconPath = AllocateString(nLen);

        _tcsncpy(szIconPath, szPath, nLen);
        _tcsncat(szIconPath, _T("etc\\icons\\"), nLen);
    }

    FreeString(szPath);

    return szIconPath;
}

HWND SnarlInterface::GetSnarlWindow()
{
    return FindWindow(SnarlWindowClass, SnarlWindowTitle);;
}

LONG32 SnarlInterface::GetVersion()
{
    return DoRequest(Requests::VersionA());
}

BOOL SnarlInterface::IsSnarlRunning()
{
    return IsWindow(GetSnarlWindow());
}


// --------------------------------------------------------------------------------------------
// SnarlInterface member functions
// --------------------------------------------------------------------------------------------

SnarlInterface::SnarlInterface()
    : appToken(0), lastMsgToken(0), szPasswordA(NULL), szPasswordW(NULL)
{
}

SnarlInterface::~SnarlInterface()
{
    delete [] szPasswordA;
    delete [] szPasswordW;
}

//---------------------------------------------------------------------------------------------

LONG32 SnarlInterface::AddAction(LONG32 msgToken, LPCSTR label, LPCSTR cmd)
{
    // addaction?[token=<notification token>|app-sig=<signature>&uid=<uid>][&password=<password>]&label=<label>&cmd=<command>

    SnarlParameterList<char> spl(4);
    spl.Add("token", msgToken);
    spl.Add("password", szPasswordA);

    spl.Add("label", label);
    spl.Add("cmd", cmd);

    return DoRequest(Requests::AddActionA(), spl);
}

LONG32 SnarlInterface::AddAction(LONG32 msgToken, LPCWSTR label, LPCWSTR cmd)
{
    SnarlParameterList<wchar_t> spl(4);
    spl.Add(L"token", msgToken);
    spl.Add(L"password", szPasswordW);

    spl.Add(L"label", label);
    spl.Add(L"cmd", cmd);

    return DoRequest(Requests::AddActionW(), spl);
}


LONG32 SnarlInterface::AddClass(LPCSTR classId, LPCSTR name, LPCSTR title, LPCSTR text, LPCSTR icon, LPCSTR sound, LONG32 duration, LPCSTR callback, bool enabled)
{
    // addclass?[app-sig=<signature>|token=<application token>][&password=<password>]&id=<class identifier>&name=<class name>[&enabled=<0|1>][&callback=<callback>]
    //          [&title=<title>][&text=<text>][&icon=<icon>][&sound=<sound>][&duration=<duration>]

    SnarlParameterList<char> spl(11);
    spl.Add("token", appToken);
    spl.Add("password", szPasswordA);

    spl.Add("id", classId);
    spl.Add("name", name);
    spl.Add("enabled", enabled);
    spl.Add("callback", callback);
    spl.Add("title", title);
    spl.Add("text", text);
    spl.Add("icon", icon);
    spl.Add("sound", sound);
    if (duration != -1) spl.Add("duration", duration);

    return DoRequest(Requests::AddClassA(), spl);
}

LONG32 SnarlInterface::AddClass(LPCWSTR classId, LPCWSTR name, LPCWSTR title, LPCWSTR text, LPCWSTR icon, LPCWSTR sound, LONG32 duration, LPCWSTR callback, bool enabled)
{
    SnarlParameterList<wchar_t> spl(11);
    spl.Add(L"token", appToken);
    spl.Add(L"password", szPasswordW);

    spl.Add(L"id", classId);
    spl.Add(L"name", name);
    spl.Add(L"enabled", enabled);
    spl.Add(L"callback", callback);
    spl.Add(L"title", title);
    spl.Add(L"text", text);
    spl.Add(L"icon", icon);
    spl.Add(L"sound", sound);
    if (duration != -1)	spl.Add(L"duration", duration);

    return DoRequest(Requests::AddClassW(), spl);
}


LONG32 SnarlInterface::ClearActions(LONG32 msgToken)
{
    // clearactions?[token=<notification token>|app-sig=<app-sig>&uid=<uid>][&password=<password>]

    SnarlParameterList<char> spl(2);
    spl.Add("token", msgToken);
    spl.Add("password", szPasswordA);

    return DoRequest(Requests::ClearActionsA(), spl);
}


LONG32 SnarlInterface::ClearClasses()
{
    // clearclasses?[token=app-sig=<signature>|token=<application token>][&password=<password>]

    SnarlParameterList<char> spl(2);
    spl.Add("token", appToken);
    spl.Add("password", szPasswordA);

    return DoRequest(Requests::ClearClassesA(), spl);
}


LONG32 SnarlInterface::GetLastMsgToken() const
{
    return lastMsgToken;
}


LONG32 SnarlInterface::Hide(LONG32 msgToken)
{
    // hide?[token=<notification token>|app-sig=<app-sig>&uid=<uid>][&password=<password>]

    SnarlParameterList<char> spl(2);
    spl.Add("token", msgToken);
    spl.Add("password", szPasswordA);

    return DoRequest(Requests::HideA(), spl);
}


LONG32 SnarlInterface::IsVisible(LONG32 msgToken)
{
    // isvisible?[token=<notification token>|app-sig=<app-sig>&uid=<uid>][&password=<password>]

    SnarlParameterList<char> spl(2);
    spl.Add("token", msgToken);
    spl.Add("password", szPasswordA);

    return DoRequest(Requests::IsVisibleA(), spl);
}


LONG32 SnarlInterface::Notify(LPCSTR classId, LPCSTR title, LPCSTR text, LONG32 timeout, LPCSTR iconPath, LPCSTR iconBase64, SnarlEnums::MessagePriority priority, LPCSTR uid, LPCSTR callback, LPCSTR value)
{
    // notify?[app-sig=<signature>|token=<application token>][&password=<password>][&id=<class identifier>]
    //        [&title=<title>][&text=<text>][&timeout=<timeout>][&icon=<icon path>][&icon-base64=<MIME data>][&callback=<default callback>]
    //        [&priority=<priority>][&uid=<notification uid>][&value=<value>]

    SnarlParameterList<char> spl(12);
    spl.Add("token", appToken);
    spl.Add("password", szPasswordA);

    spl.Add("id", classId);
    spl.Add("title", title);
    spl.Add("text", text);
    spl.Add("icon", iconPath);
    spl.Add("icon-base64", iconBase64);
    spl.Add("uid", uid);
    spl.Add("callback", callback);
    spl.Add("value", value);
    if (timeout != -1)  spl.Add("timeout", timeout);
    if (priority != SnarlEnums::PriorityUndefined) spl.Add("priority", priority); // -1 is a legal priority

    LONG32 request = DoRequest(Requests::NotifyA(), spl);
    lastMsgToken = (request > 0) ? request : 0;

    return request;
}

LONG32 SnarlInterface::Notify(LPCWSTR classId, LPCWSTR title, LPCWSTR text, LONG32 timeout, LPCWSTR iconPath, LPCWSTR iconBase64, SnarlEnums::MessagePriority priority, LPCWSTR uid, LPCWSTR callback, LPCWSTR value)
{
    SnarlParameterList<wchar_t> spl(12);
    spl.Add(L"token", appToken);
    spl.Add(L"password", szPasswordW);

    spl.Add(L"id", classId);
    spl.Add(L"title", title);
    spl.Add(L"text", text);
    spl.Add(L"icon", iconPath);
    spl.Add(L"icon-base64", iconBase64);
    spl.Add(L"uid", uid);
    spl.Add(L"callback", callback);
    spl.Add(L"value", value);
    if (timeout != -1)  spl.Add(L"timeout", timeout);
    if (priority != SnarlEnums::PriorityUndefined) spl.Add(L"priority", priority); // -1 is a legal priority

    LONG32 request = DoRequest(Requests::NotifyW(), spl);
    lastMsgToken = (request > 0) ? request : 0;

    return request;
}


LONG32 SnarlInterface::Register(LPCSTR signature, LPCSTR title, LPCSTR icon, LPCSTR password, HWND hWndReplyTo, LONG32 msgReply, SnarlEnums::AppFlags flags)
{
    // register?app-sig=<signature>&title=<title>[&icon=<icon>][&password=<password>][&reply-to=<reply window>][&reply=<reply message>]

    SnarlParameterList<char> spl(6);
    spl.Add("app-sig", signature);
    spl.Add("title", title);
    spl.Add("icon", icon);
    spl.Add("password", password);
    spl.Add("reply-to", hWndReplyTo);
    spl.Add("reply", msgReply);
    spl.Add("flags", flags);

    // If password was given, save and use in all other functions requiring password
    if (password != NULL && strlen(password) > 0)
        SetPassword(password);

    LONG32 request = DoRequest(Requests::RegisterA(), spl);
    if (request > 0)
        appToken = request;

    return request;
}

LONG32 SnarlInterface::Register(LPCWSTR signature, LPCWSTR name, LPCWSTR icon, LPCWSTR password, HWND hWndReplyTo, LONG32 msgReply, SnarlEnums::AppFlags flags)
{
    SnarlParameterList<wchar_t> spl(7);
    spl.Add(L"app-sig", signature);
    spl.Add(L"title", name);
    spl.Add(L"icon", icon);
    spl.Add(L"password", password);
    spl.Add(L"reply-to", hWndReplyTo);
    spl.Add(L"reply", msgReply);
    spl.Add(L"flags", flags);

    // If password was given, save and use in all other functions requiring password
    if (password != NULL && wcslen(password) > 0)
        SetPassword(password);

    LONG32 request = DoRequest(Requests::RegisterW(), spl);
    if (request > 0)
        appToken = request;

    return request;
}


LONG32 SnarlInterface::RemoveClass(LPCSTR classId)
{
    // remclass?[app-sig=<signature>|token=<application token>][&password=<password>][&id=<class identifier>|&all=<0|1>]

    SnarlParameterList<char> spl(3);
    spl.Add("token", appToken);
    spl.Add("password", szPasswordA);

    spl.Add("id", classId);
    // instead of all, use ClearClasses

    return DoRequest(Requests::RemoveClassA(), spl);
}

LONG32 SnarlInterface::RemoveClass(LPCWSTR classId)
{
    SnarlParameterList<wchar_t> spl(3);
    spl.Add(L"token", appToken);
    spl.Add(L"password", szPasswordW);

    spl.Add(L"id", classId);

    return DoRequest(Requests::RemoveClassW(), spl);
}


LONG32 SnarlInterface::Unregister(LPCSTR signature)
{
    // unregister?[app-sig=<signature>|token=<application token>][&password=<password>]

    SnarlParameterList<char> spl(2);
    spl.Add("app-sig", signature);
    spl.Add("password", szPasswordA);

    appToken = 0;
    lastMsgToken = 0;
    ClearPassword();

    return DoRequest(Requests::UnregisterA(), spl);
}

LONG32 SnarlInterface::Unregister(LPCWSTR signature)
{
    SnarlParameterList<wchar_t> spl(2);
    spl.Add(L"app-sig", signature);
    spl.Add(L"password", szPasswordW);

    appToken = 0;
    lastMsgToken = 0;
    ClearPassword();

    return DoRequest(Requests::UnregisterW(), spl);
}

LONG32 SnarlInterface::Update(LONG32 msgToken, LPCSTR classId, LPCSTR title, LPCSTR text, LONG32 timeout, LPCSTR iconPath, LPCSTR iconBase64, SnarlEnums::MessagePriority priority, LPCSTR callback, LPCSTR value)
{
    // Made from best guess - no documentation available yet
    SnarlParameterList<char> spl(11);
    spl.Add("token", msgToken);
    spl.Add("password", szPasswordA);

    spl.Add("id", classId);
    spl.Add("title", title);
    spl.Add("text", text);
    spl.Add("icon", iconPath);
    spl.Add("icon-base64", iconBase64);
    spl.Add("callback", callback);
    spl.Add("value", value);
    if (timeout != -1)  spl.Add("timeout", timeout);
    if (priority != SnarlEnums::PriorityUndefined) spl.Add("priority", priority); // -1 is a legal priority

    return DoRequest(Requests::UpdateA(), spl);
}

LONG32 SnarlInterface::Update(LONG32 msgToken, LPCWSTR classId, LPCWSTR title, LPCWSTR text, LONG32 timeout, LPCWSTR iconPath, LPCWSTR iconBase64, SnarlEnums::MessagePriority priority, LPCWSTR callback, LPCWSTR value)
{
    // Made from best guess - no documentation available yet
    SnarlParameterList<wchar_t> spl(11);
    spl.Add(L"token", msgToken);
    spl.Add(L"password", szPasswordW);

    spl.Add(L"id", classId);
    spl.Add(L"title", title);
    spl.Add(L"text", text);
    spl.Add(L"icon", iconPath);
    spl.Add(L"icon-base64", iconBase64);
    spl.Add(L"callback", callback);
    spl.Add(L"value", value);
    if (timeout != -1)  spl.Add(L"timeout", timeout);
    if (priority != SnarlEnums::PriorityUndefined) spl.Add(L"priority", priority); // -1 is a legal priority

    return DoRequest(Requests::UpdateW(), spl);
}

/*
// Update app should not be used at this time, according to wiki
LONG32 SnarlInterface::UpdateApp(LPCSTR title, LPCSTR icon)
{
    SnarlParameterList<char> spl(3);
    spl.Add("token", appToken);
    spl.Add("title", title);
    spl.Add("icon", icon);

    return DoRequest(Requests::UpdateAppA(), spl);
}

LONG32 SnarlInterface::UpdateApp(LPCWSTR title, LPCWSTR icon)
{
    SnarlParameterList<wchar_t> spl(2);
    spl.Add(L"token", appToken);
    spl.Add(L"title", title);
    spl.Add(L"icon", icon);

    return DoRequest(Requests::UpdateAppW(), spl);
}*/


//-----------------------------------------------------------------------------
// Private functions
//-----------------------------------------------------------------------------

LONG32 SnarlInterface::DoRequest(LPCSTR request, SnarlParameterList<char>& spl, UINT replyTimeout)
{
    // <action>[?<data>=<value>[&<data>=<value>]]
    const std::vector<SnarlParameterList<char>::PairType>&list = spl.GetList();

    if (list.size() > 0)
    {
        std::string requestStr = request;
        requestStr.append("?");

        std::vector<SnarlParameterList<char>::PairType>::const_iterator listEnd = list.end(); // cend();
        for (std::vector<SnarlParameterList<char>::PairType>::const_iterator iter = list.begin(); // cbegin();
            iter != listEnd; ++iter)
        {
            SnarlParameterList<char>::PairType pair = *iter;

            if (iter->second.length() > 0)
            {
                std::basic_string<char>& value = const_cast<std::basic_string<char>&>(iter->second);
                requestStr.append(iter->first).append("=").append(Escape(value));
                requestStr.append("&");
            }
        }
        // Delete last &
        requestStr.erase(requestStr.size() - 1);

        return DoRequest(requestStr.c_str(), replyTimeout);
    }
    else
        return DoRequest(request, replyTimeout);
}

LONG32 SnarlInterface::DoRequest(LPCWSTR request, SnarlParameterList<wchar_t>& spl, UINT replyTimeout)
{
    // <action>[?<data>=<value>[&<data>=<value>]]
    const std::vector<SnarlParameterList<wchar_t>::PairType>&list = spl.GetList();

    if (list.size() > 0)
    {
        std::basic_string<wchar_t> requestStr = request;
        requestStr.append(L"?");

        std::vector<SnarlParameterList<wchar_t>::PairType>::const_iterator listEnd = list.end(); // cend();
        for (std::vector<SnarlParameterList<wchar_t>::PairType>::const_iterator iter = list.begin(); // cbegin();
            iter != listEnd; ++iter)
        {
            if (iter->second.length() > 0)
            {
                std::basic_string<wchar_t>& value = const_cast<std::basic_string<wchar_t>&>(iter->second);
                requestStr.append(iter->first).append(L"=").append(Escape(value));
                requestStr.append(L"&");
            }
        }
        // Delete last &
        requestStr.erase(requestStr.size() - 1);

        return DoRequest(requestStr.c_str(), replyTimeout);
    }
    else
        return DoRequest(request, replyTimeout);
}

// Remember to delete [] returned string
LPSTR SnarlInterface::WideToUTF8(LPCWSTR szWideStr)
{
    if (szWideStr == NULL)
        return NULL;

    int nSize = WideCharToMultiByte(CP_UTF8, 0, szWideStr, -1, NULL, 0, NULL, NULL);
    LPSTR szUTF8 = new char[nSize];
    WideCharToMultiByte(CP_UTF8, 0, szWideStr, -1, szUTF8, nSize, NULL, NULL);

    return szUTF8;
}

void SnarlInterface::SetPassword(LPCSTR password)
{
    ClearPassword();

    if (password != NULL)
    {
        int bufSize = (int)strlen(password) + 1;
        szPasswordA = new char[bufSize];
        szPasswordW = new wchar_t[bufSize];

        // Copy ansi string
        strcpy_s(szPasswordA, bufSize, password);

        // Copy wide string
        MultiByteToWideChar(CP_ACP, 0, password, bufSize, szPasswordW, bufSize);
    }
}

void SnarlInterface::SetPassword(LPCWSTR password)
{
    ClearPassword();

    if (password != NULL)
    {
        size_t bufSize = wcslen(password) + 1;
        szPasswordW = new wchar_t[bufSize];

        // Copy wide string
        wcscpy_s(szPasswordW, bufSize, password);

        // Copy ansi string
        szPasswordA = WideToUTF8(password);
    }
}

void SnarlInterface::ClearPassword()
{
    // Safe to delete NULL
    delete [] szPasswordA;
    delete [] szPasswordW;
    szPasswordA = NULL;
    szPasswordW = NULL;
}


}} // namespace Snarl::V42

