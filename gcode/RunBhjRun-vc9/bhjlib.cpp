#include "stdafx.h"
#include "bhjlib.h"
#include "stdafx.h"
#include <boost/regex.hpp>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#include <sys/types.h> 
#include <sys/stat.h>
#include <algorithm>


#define OPEN_NAMESPACE(n) namespace n {
#define CLOSE_NAMESPACE(n) }

OPEN_NAMESPACE(bhj)
using namespace boost;
using std::list;
using std::string;
using std::copy;
lstring_t split(const cstring& regex, const cstring& src)
{
	lstring_t ls;

	boost::regex e(regex);
	boost::sregex_token_iterator i(src.begin(), src.end(), e, -1);
	boost::sregex_token_iterator j;
	while(i != j)
	{
		ls.push_back(string(*i++));
	}

	
	return ls;
}

cstring remove_pattern(const cstring& src, const cstring& pat)
{
	return regex_replace(src, regex(pat), "", match_default | format_perl);
}

cstring::cstring(const CString& CStr) 
{
	resize(CStr.GetLength());
	std::copy((const char*)CStr, (const char*)CStr + CStr.GetLength(), begin());
}

cstring::operator CString() const
{
	CString CStr;
	CStr.GetBuffer(size());
	for (string::const_iterator i = begin(); i != end(); i++) {
		CStr += *i;
	}
	return CStr;
}

cstring::operator wstring() const
{
	wchar_t *wbuff = new wchar_t[size()+1];
	const char *str = c_str();
	int n = mbstowcs(wbuff, str, size());
	wbuff[n] = L'\0';
	wstring wstr = wbuff;
	delete []wbuff;
	return wstr;	
}

cstring::operator const char*() const
{
	return c_str();
}

bool fields_match(const cstring& src, const cstring& fstr)
{
	lstring_t tokens = split("\\s+", fstr);
	for (lstring_t::iterator i = tokens.begin(); i != tokens.end(); i++) {
		if (!string_contains(src, *i)) {
			return false;
		}
	}
	return true;
}

bool string_contains(const cstring& src, const cstring& tgt)
{
	return src.find(tgt) != std::string::npos;
}

cstring string_format(const char* fmt, ...)
{
	va_list argList;
	va_start(argList, fmt);
	CString str;
	str.FormatV(fmt, argList);
	va_end(argList);
	return str;
}

cstring dirname(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);
	while (p[p.size() -1] == '/') {
		p.erase(p.size() - 1);
	}

	if (!string_contains(p, "/")) {
		return ".";
	}

	int n = p.find_last_of("/");
	p.erase(n);
	return p;
}

cstring bce_dirname(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);

	if (!string_contains(p, "/")) {
		if (p.size()==2 && p.at(1)==':') {
			return p + "/";
		}
		return ".";
	}

	int n = p.find_last_of("/");
	p.erase(n);
	if (p.size()==2 && p.at(1)==':') {
		return p + "/";
	}
	return p;

}

cstring basename(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);
	while (p[p.size() -1] == '/') {
		p.erase(p.size() - 1);
	}

	if (!string_contains(p, "/")) {
		return p;
	}

	int n = p.find_last_of("/");
	p.erase(0, n);
	return p.empty() ? "/" : p;

}

cstring bce_basename(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);

	if (p[p.size() - 1] == '/') {
		return "";
	}

	if (!string_contains(p, "/")) { //if it is like `c:', return `c:/'
		if (p.size()>1 && p.at(1)==':') {
			return "";
		}
		return p;
	}

	int n = p.find_last_of("/");
	p.erase(0, n+1);
	return p;

}

bool is_abspath(const cstring& path)
{
	if (path.size() < 2) {
		return false;
	}
	return (path.c_str()[1] == ':');
}

lstring_t getMatchingFiles(const cstring& dir, const cstring& base)
{
	lstring_t ls_match;
	WIN32_FIND_DATA wfd;
	BHJDEBUG(" patten is %s", (dir+"/*"+base+"*").c_str());
	HANDLE hfile = FindFirstFile(cstring(dir + "/*" + base + "*"), &wfd);
	while (hfile != INVALID_HANDLE_VALUE) {
		if (dir.at(dir.size() - 1) == '/') {
			ls_match.push_back(dir + wfd.cFileName);
		} else {
			ls_match.push_back(dir + "/" + wfd.cFileName);
		}
		if (FindNextFile(hfile, &wfd) == 0) {
			break;
		}
	}
	return ls_match;
}

void debug_lstring(const lstring_t& ls)
{
	for (lstring_t::const_iterator i = ls.begin(); i != ls.end(); i++) {
		BHJDEBUG(" %s", i->c_str());
	}
}

cstring get_sh_folder(int csid)
{
	char strAppPath[MAX_PATH] = "";
	HRESULT ret = SHGetFolderPath(NULL, csid, NULL, SHGFP_TYPE_CURRENT, strAppPath);
	if (ret != S_OK) {
		BHJDEBUG(" path for %d not available", csid);
		return "";
	} else {
		BHJDEBUG(" path for %d is %s", csid, strAppPath);
		cstring path = strAppPath;
		path += '/';
		return bce_dirname(path);		
	}
}

cstring unquote(const cstring& str)
{
	cstring res;
	enum {
		in_quote,
		out_quote,
		//open_quote, // not used
		close_quote,
	};

	char cq = '"';
	int state = out_quote;
	for (cstring::const_iterator i=str.begin(); i!=str.end(); i++) {
		if (state == out_quote) {
			if (*i == cq) {
				state = in_quote;
			} else {
				res.push_back(*i);
			}
		} else if (state == in_quote) {
			if (*i == cq) {
				state = close_quote;
			} else {
				res.push_back(*i);
			}
		} else if (state == close_quote) {
			if (*i == cq) {
				state = in_quote;
			} else {
				state = out_quote;
			}
			res.push_back(*i);
		}
	}
	return res;		
}

bool file_exist(const cstring& str)
{
	struct _stat stat;
	return _stat(unquote(str), &stat) == 0;
}

static bool isquote(char c)
{
	return c == '"';
}

static bool isescape(char c)
{
	return c == '\\';
}

cmdline_parser::cmdline_parser(const cstring& str)
{
	enum {
		in_space,
		in_quote,
		out_quote,
		close_quote,
		in_escape_out_quote,
		in_escape_in_quote,
	};

	m_cmd_str = str;
	cstring token;
	int state = in_space;
	char cq = '"';
	int idx=0;
	for (cstring::const_iterator i=str.begin(); i!=str.end(); i++, idx++) {
		if (state == in_space) {

			if (isspace(*i)) {
				continue;
			}
 			
			m_lab.push_back(idx);
			token.clear();
			if (isquote(*i)) {
				state = in_quote;
				continue;
			} 

			token.push_back(*i);
			if (isescape(*i)){
				state = in_escape_out_quote;
			} else {
				state = out_quote;
			}
		} else if (state == in_quote) {
			if (isquote(*i)) {
				state = close_quote;
			} else {
				token.push_back(*i);
				if (isescape(*i)) {
				state = in_escape_in_quote;
				}
			} 
		} else if (state == close_quote) {
			if (isquote(*i)) {
				state = in_quote;
				token.push_back(*i);
			} else if (isspace(*i)) {
				state = in_space;
				m_lae.push_back(idx-1);
				m_lsa.push_back(token);
			} else if (isquote(*i)) {
				state = in_escape_out_quote;
				token.push_back(*i);
			} else {
				state = out_quote;
				token.push_back(*i);
			}
		} else if (state == out_quote) {
			if (isquote(*i)) {
				state = in_quote;
			} else if (isspace(*i)) {
				state = in_space;
				m_lae.push_back(idx-1);
				m_lsa.push_back(token);
			} else if (isescape(*i)) {
				token.push_back(*i);
				state = in_escape_out_quote;
			} else {
				token.push_back(*i);
			}
		} else if (state == in_escape_in_quote) {
			state = in_quote;
			if (isquote(*i)) {
				token.erase(token.size() - 1); //pop the escape
				token.push_back(*i);
			} else if (isescape(*i)) {
				;//escape is already pushed
			} else {
				token.push_back(*i);
			}
		} else if (state == in_escape_out_quote) {
			state = out_quote;
			if (isquote(*i)) {
				token.erase(token.size() - 1);
				token.push_back(*i);
			} else {
				if (isspace(*i)) {
					state = in_space;
					m_lae.push_back(idx-1);
				} else {
					token.push_back(*i);
				}
			}
		}
	}

	if (state != in_space) {
		m_lsa.push_back(token);
		m_lae.push_back(idx-1);
	}
}

lstring_t cmdline_parser::get_args()
{
	return m_lsa;
}

cstring cmdline_parser::get_text_of_args(unsigned int arg_start, unsigned int arg_end)
{
	arg_start = arg_start < 0 ? 0 : arg_start;
	arg_end = arg_end < 0 ? 0 : arg_end;

	if (arg_start >= m_lae.size()) {
		return "";
	}

	if (arg_end >= m_lae.size()) {
		arg_end = m_lae.size() - 1;
	}

	if (arg_end < arg_start) {
		arg_end = arg_start;
	}

	int text_start = m_lab[arg_start];
	int text_end = m_lae[arg_end];

	return m_cmd_str.substr(text_start, text_end - text_start + 1);
}

lstring_t cmdline2args(const cstring& str)
{

	cmdline_parser cp(str);
	return cp.get_args();
}

cstring quote_first_file(const cstring& str)
{
	cmdline_parser cp(str);
	lstring_t ls = cp.get_args();

	int n_args = 0;
	for (lstring_t::iterator i=ls.begin(); i!=ls.end(); n_args++, i++) {
		cstring prefix = cp.get_text_of_args(0, n_args);
		if (file_exist(prefix)) {
			return format_string("\"%s\" %s", prefix.c_str(), cp.get_text_of_args(n_args+1, ls.size()).c_str());
		}
		if (file_exist(prefix+".exe")) {
			return format_string("\"%s.exe\" %s", prefix.c_str(), cp.get_text_of_args(n_args+1, ls.size()).c_str());
		}
	}
	return str;
}

cstring format_string(const char* fmt, ...)
{
	CString res;
	va_list argList;
	va_start(argList, fmt);
	res.FormatV(fmt, argList);
	va_end(argList);
	return res;
}

program_runner::program_runner(const char* exec, const cstring& cmdline, which_output_t which)
{
	HANDLE                pipe_read, pipe_write;
    SECURITY_ATTRIBUTES   sa;
    STARTUPINFO           startup;
    PROCESS_INFORMATION   pinfo;
    char                  program_path[ MAX_PATH ];
    int                   ret;

    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;

    /* create pipe, and ensure its read handle isn't inheritable */
    ret = CreatePipe( &pipe_read, &pipe_write, &sa, 0 );
    if (!ret) {
        fprintf(stderr, "CreatePipe() failure, error %ld\n", GetLastError() );
        return;
    }

    SetHandleInformation( pipe_read, HANDLE_FLAG_INHERIT, 0 );

    ZeroMemory( &startup, sizeof(startup) );
    startup.cb = sizeof(startup);
    startup.hStdInput  = GetStdHandle( STD_INPUT_HANDLE );
	if (which == read_out) {
		startup.hStdOutput = pipe_write;
		startup.hStdError  = GetStdHandle( STD_ERROR_HANDLE );
	} else {
		startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
		startup.hStdError = pipe_write;
	}
    startup.dwFlags    = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	startup.wShowWindow = SW_HIDE;

    ZeroMemory( &pinfo, sizeof(pinfo) );

    /* get path of current program */
    GetModuleFileName( NULL, program_path, sizeof(program_path) );
	CString cl_buf = CString(cmdline);

    ret = CreateProcess(
		exec,
		cl_buf.GetBuffer(0),
		/* the fork-server argument will set the
		   debug = 2 in the child           */
		NULL,                   /* process handle is not inheritable */
		NULL,                    /* thread handle is not inheritable */
		TRUE,                          /* yes, inherit some handles */
		0, /* the new process doesn't have a console */
		NULL,                     /* use parent's environment block */
		NULL,                    /* use parent's starting directory */
		&startup,                 /* startup info, i.e. std handles */
		&pinfo );

    CloseHandle( pipe_write );

    if (!ret) {
        fprintf(stderr, "CreateProcess failure, error %ld\n", GetLastError() );
        CloseHandle( pipe_read );
        return;
    }

	char  temp[4096];
	DWORD  count;

	while (1) {
		ret = ReadFile( pipe_read, temp, 4096, &count, NULL );
		if (ret && count == 0) {
			break;
		} 
		if (ret) {
			m_str_output += string_from_buffer(temp, count);
		} else {
			BHJDEBUG(" Error: read from pipe %d", GetLastError());
			break;
		}
	}
	CloseHandle( pipe_read );

	WaitForSingleObject(pinfo.hProcess, INFINITE);
	if (!GetExitCodeProcess(pinfo.hProcess, &m_exit_code)) {
		BHJDEBUG(" Error: GetExitCodeProcess");
	}
    CloseHandle( pinfo.hProcess );
    CloseHandle( pinfo.hThread );
}

cstring program_runner::get_output()
{
	return m_str_output;
}

DWORD program_runner::exit_code()
{
	return m_exit_code;
}

cstring string_from_buffer(const char* buf, int size)
{
	cstring res;
	res.resize(size);
	using std::copy;
	copy(buf, buf+size, res.begin());
	return res;
}

void fmt_messagebox(const char* fmt, ...)
{
	va_list argList;
	va_start(argList, fmt);
	CString str;
	str.FormatV(fmt, argList);
	va_end(argList);
	
	AfxMessageBox(str);
}

CLOSE_NAMESPACE(bhj)
