#include "stdafx.h"
#include "bhjlib.h"
#include "stdafx.h"
#include <boost/regex.hpp>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#include <sys/types.h> 
#include <sys/stat.h>
#include <algorithm>
#include <map>
using std::map;


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

bool fields_match(const cstring& src, const lstring_t& tokens)
{
	for (lstring_t::const_iterator i = tokens.begin(); i != tokens.end(); i++) {
		if (i->size()>1 && (i->c_str())[0] == '!') {
			if (string_nocase_contains(src, i->c_str()+1)) {
				return false;
			}
		} else if (!string_nocase_contains(src, *i)) {
			return false;
		}
	}
	return true;
}

bool string_contains(const cstring& src, const cstring& tgt)
{
	return src.find(tgt) != std::string::npos;
}

bool string_nocase_contains(const cstring& src, const cstring& tgt)
{
	if (strcasestr(src.c_str(), tgt.c_str())) {
		return true;
	} else {
		return false;
	}
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

cstring bce_dirname(const cstring& path)
{
	int n_not_slash = path.find_first_not_of("\\/");
	cstring left = string_left_of(path, n_not_slash);
	cstring right = string_right_of(path, n_not_slash);

	left = regex_replace(left, regex("\\\\"), "/", match_default|format_perl);
	right = regex_replace(right, regex("\\\\+"), "/", match_default|format_perl);
	right = regex_replace(right, regex("/+"), "/", match_default|format_perl);
	cstring p = left + right;

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
	if (path.c_str()[1] == ':') {
		return true;
	}

	cstring p = regex_replace(path, regex("\\\\"), "/", match_default|format_perl);
	if (p[0] == '/') {
		if (p[1] != '/') {
			return true;
		} else if (p.find_last_of("/") == 1) {//the last / is the 2nd, it has no / after the beginning //!
			return false;
		} else {
			return true;
		}
	}
	return false;
}

static lstring_t getMatchingFiles(const cstring& udir, const cstring& wdir, const cstring& base)
{
	lstring_t ls_match;
	WIN32_FIND_DATA wfd;
	HANDLE hfile = FindFirstFile(cstring(wdir + "/" + base + "*").c_str(), &wfd);
	while (hfile != INVALID_HANDLE_VALUE) {
		if (1 /*string_nocase_contains(wfd.cFileName, base)*/) {
			if (udir.at(udir.size() - 1) == '/') {
				ls_match.push_back(udir + wfd.cFileName);
			} else {
				ls_match.push_back(udir + "/" + wfd.cFileName);
			}
			if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
				ls_match.back() += "/";
			}
			if (ls_match.back().find_first_of(" \x09\x0a\x0b\x0c\x0d") != cstring::npos) {
				ls_match.back() = cstring("\"")+ls_match.back()+"\"";
			}
		}
		if (FindNextFile(hfile, &wfd) == 0) {
			break;
		}
	}
	return ls_match;
}

lstring_t getMatchingFiles(const cstring& dir, const cstring& base)
{
	if (dir.size() > 1 && 
		(dir[0] == '/' && dir[1] != '/' ||
		 dir[0] != '/')) {
		cstring wdir  = get_win_path(dir);
		return getMatchingFiles(dir, wdir, base);
	}

	cstring find = getWhichFile("find.exe");
	
	program_runner pr(find, dir, "-maxdepth", "1", "-iname", base+"*", "-print0", read_out);
	cstring output = pr.get_output();
	fwrite(output.c_str(), output.size(), 1, stdout);
	fflush(stdout);

	lstring_t ls_match;
	for (unsigned int i=0, j=0; i<output.size(); i++) {
		if (output[i] == 0) {
			cstring str = output.substr(j, i-j);
			j = i+1;

			if (str.find_first_of(" \x09\x0a\x0b\x0c\x0d") != cstring::npos) {
				str = cstring("\"") + str + "\"";
			}
			ls_match.push_back(str);
		}
	}
	return ls_match;


	
}

cstring getWhichFile(const cstring& file)
{
	if (string_contains(file, "\\") || string_contains(file, "/")) {
		return file;
	}

	cstring path_env = getenv("PATH");
	lstring_t paths = split(";", path_env);
	
	paths.push_back(get_sh_folder(CSIDL_COMMON_DESKTOPDIRECTORY));
	paths.push_back(get_sh_folder(CSIDL_DESKTOPDIRECTORY));
	paths.push_back(get_sh_folder(CSIDL_APPDATA)+"/Microsoft/Internet Explorer/Quick Launch");
	paths.push_back(get_sh_folder(CSIDL_COMMON_APPDATA)+"/Microsoft/Internet Explorer/Quick Launch");

	paths = unique_ls(paths);
	for (lstring_t::iterator i = paths.begin(); i != paths.end(); i++) {
		WIN32_FIND_DATA wfd;
		HANDLE hfile = FindFirstFile(string_format("%s/%s", i->c_str(), file.c_str()).c_str(), &wfd);
		if (hfile == INVALID_HANDLE_VALUE) {
			hfile = FindFirstFile(string_format("%s/%s.exe", i->c_str(), file.c_str()).c_str(), &wfd);
		}
		if (hfile != INVALID_HANDLE_VALUE && !strncasecmp(wfd.cFileName, file.c_str(), file.size())) {
			return get_win_path(bce_dirname(*i+"/") + "/" + wfd.cFileName);
		}	
	}
	return file;
}

lstring_t getPathEnvMatchingFiles(const lstring_t& args)
{
	lstring_t res;
	if (args.size() < 1) {
		return res;
	}
	cstring front = args.front();
	lstring_t args2 = args;
	args2.pop_front();

	cstring path_env = getenv("PATH");
	lstring_t paths = split(";", path_env);

	paths.push_back(get_sh_folder(CSIDL_COMMON_DESKTOPDIRECTORY));
	paths.push_back(get_sh_folder(CSIDL_DESKTOPDIRECTORY));
	paths.push_back(get_sh_folder(CSIDL_APPDATA)+"/Microsoft/Internet Explorer/Quick Launch");
	paths.push_back(get_sh_folder(CSIDL_COMMON_APPDATA)+"/Microsoft/Internet Explorer/Quick Launch");

	paths = unique_ls(paths);
	debug_lstring(paths);
	//BHJDEBUG(" we are talking about %s", (get_sh_folder(CSIDL_COMMON_APPDATA)+"Microsoft/Internet Explorer/Quick Launch/").c_str());

	for (lstring_t::iterator i = paths.begin(); i != paths.end(); i++) {
		lstring_t files = getMatchingFiles(bce_dirname(*i + "/"), front);
		for (lstring_t::iterator i=files.begin(); i!=files.end(); i++) {
			if (fields_match(*i, args2)) {
				res.push_back(*i);
			}
		}
	}
	return res;
}

lstring_t getLocateMatchingFiles(const lstring_t& args_const, bool rerun_locate)
{
	lstring_t res;
	if (args_const.empty()) {
		return res;
	}
	lstring_t args = args_const;
	static cstring saved_str;
	static lstring_t saved_ls;
	if (rerun_locate && saved_str != args.front()) {
		saved_str = args.front();
		program_runner pr("bash", "run-locate.sh", args.front(), read_out);
		cstring output = pr.get_output();
		
		saved_ls = split("\r\n|\n", output);
	}
	args.pop_front();
	for (lstring_t::iterator i=saved_ls.begin(); i!=saved_ls.end(); i++) {
		if (fields_match(*i, args)) {
			res.push_back(*i);
		}
	}
	return res;
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

cstring quote_str(const cstring& str)
{
	cstring res = "\"";
	res.reserve(str.size());
	for (cstring::const_iterator i=str.begin(); i!=str.end(); i++) {
		if (*i == '"' || *i == '\\') {
			res.push_back('\\');
		}
		res.push_back(*i);
	}
	res.push_back('"');
	return res;
}

cstring unquote_str(const cstring& str)
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

cstring cmdline_parser::get_prefix(unsigned int argi)
{
	if (argi < 0) {
		return "";
	}

	if (argi >= m_lae.size()) {
		return m_cmd_str;
	}

	return m_cmd_str.substr(0, m_lab[argi]);	
}

cstring cmdline_parser::get_postfix(unsigned int argi)
{
	if (argi < 0) {
		return m_cmd_str;
	}

	if (argi >= m_lae.size()) {
		return "";
	}
	return m_cmd_str.substr(m_lae[argi]+1);
}

lstring_t cmdline2args(const cstring& str)
{

	cmdline_parser cp(str);
	return cp.get_args();
}

cstring get_win_path(const cstring& upath)
{
	if (upath.empty()) {
		return "";
	}

	// if (upath[0] != '/' && upath[0] != '\\') {
	// 	return upath;
	// }
	//because the file can be a softlink, and we need to get to the real one

	const char* option = "-alm";
	if (upath.size() > 1 && 
		(upath[0] == '/' || upath[0] == '\\') && 
		(upath[1] == '/' || upath[1] == '\\')) {
		option = "-alw";
	}
	program_runner pr("cygpath", "option", upath, read_out);
	return regex_replace(pr.get_output(), regex("\r|\n"), "", match_default|format_perl);
}

lstring_t unique_ls(const lstring_t& ls)
{
	lstring_t res;
	map<cstring, int> smap;
	for (lstring_t::const_iterator i=ls.begin(); i!=ls.end(); i++) {
		if (!smap[*i]) {
			smap[*i] = 1;
			res.push_back(*i);
		}
	}
	return res;
}

void cmdline_to_file_and_args(const cstring& str, cstring& file, cstring& args)
{
	file="";
	args="";
	cmdline_parser cp(str);
	lstring_t ls = cp.get_args();

	if (ls.size() > 0) {
		file = get_win_path(getWhichFile(ls.front()));
	}
	args = cp.get_text_of_args(1, ls.size());
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

void program_runner::ctor_helper(const cstring& cmdline, which_output_t which, int timeout)
{
	const char* exec = NULL;
	HANDLE                pipe_read, pipe_write;
    SECURITY_ATTRIBUTES   sa;
    STARTUPINFO           startup;
    PROCESS_INFORMATION   pinfo;
    int                   ret;

    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;

    /* create pipe, and ensure its read handle isn't inheritable */
	if (which != read_none) {
		ret = CreatePipe( &pipe_read, &pipe_write, &sa, 0 );
		if (!ret) {
			fprintf(stderr, "CreatePipe() failure, error %ld\n", GetLastError() );
			return;
		}
		SetHandleInformation( pipe_read, HANDLE_FLAG_INHERIT, 0 );
	}



    ZeroMemory( &startup, sizeof(startup) );
    startup.cb = sizeof(startup);
    startup.hStdInput  = GetStdHandle( STD_INPUT_HANDLE );
	if (which == read_out) {
		startup.hStdOutput = pipe_write;
		startup.hStdError  = GetStdHandle( STD_ERROR_HANDLE );
	} else if (which == read_err) {
		startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
		startup.hStdError = pipe_write;
	} else {
		startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
		startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	}
		
    startup.dwFlags    = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	startup.wShowWindow = SW_HIDE;

    ZeroMemory( &pinfo, sizeof(pinfo) );

    /* get path of current program */
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



    if (!ret) {
        fprintf(stderr, "CreateProcess failure, error %ld\n", GetLastError() );
        CloseHandle( pipe_read );
        return;
    }

	if (which != read_none) {
		CloseHandle( pipe_write );
		char  temp[4096];
		DWORD  count;

		if (timeout > 0) {
			DWORD res = WaitForSingleObject(pipe_read, timeout);
			if (res == WAIT_TIMEOUT) {
				m_exit_code = -1;
				BHJDEBUG(" wait for pipe timeout!");
				goto bail;
			}
		}


		while (1) {
			ret = ReadFile( pipe_read, temp, 4096, &count, NULL );
			if (ret && count == 0) {
				break;
			} 
			if (ret) {
				m_str_output += string_from_buffer(temp, count);
			} else {
				//BHJDEBUG(" Error: read from pipe %d", GetLastError());
				break;
			}
		}
	}


	if (timeout > 0) {
		DWORD res = WaitForSingleObject(pinfo.hProcess, timeout);
		if (res == WAIT_TIMEOUT) {
			m_exit_code = -1;
			BHJDEBUG(" wait for process timeout!");
			goto bail;
		}
	} else {
		WaitForSingleObject(pinfo.hProcess, INFINITE);
	}
	WaitForSingleObject(pinfo.hProcess, INFINITE);
	if (!GetExitCodeProcess(pinfo.hProcess, &m_exit_code)) {
		BHJDEBUG(" Error: GetExitCodeProcess");
	}
bail:
	if (which != read_none) {
		CloseHandle( pipe_read );
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

cstring string_right_of(const cstring& str, int point)
{
	if (point >= (int)str.size()) {
		return "";
	}

	if (point < 0) {
		return str;
	}

	return str.substr(point);
}

cstring string_left_of(const cstring& str, int point)
{
	if (point >= (int)str.size()) {
		return str;
	}

	if (point < 0) {
		return "";
	}

	return str.substr(0, point);
}

bool is_dir_cyg(const cstring& path)
{
	if (path.empty()) {
		return false;
	}

	if (path[0] != '/' && path[0] != '\\') {
		struct _stat stat;
		return _stat(path.c_str(), &stat) == 0 && (stat.st_mode|_S_IFDIR);
	}

	cstring cmd = string_format("test -d %s", quote_str(path).c_str());

	program_runner pr("bash", "-c", cmd, read_none, 500);
	return pr.exit_code() == 0;
}

bool file_exist(const cstring& path)
{
	if (path.empty()) {
		return false;
	}
	if (path[0] == '/') {
		cstring cmd = string_format("test -e %s", quote_str(path).c_str());
		program_runner pr("bash", "-c", cmd, read_none, 500);
		return pr.exit_code() == 0;
	}

	struct _stat stat;
	return _stat(unquote_str(path).c_str(), &stat) == 0;

}

program_runner::program_runner(const cstring& cmd, which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd), which, timeout);
}

program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   const cstring& arg5, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				+ " " + quote_str(arg5)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   const cstring& arg5, 
							   const cstring& arg6, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				+ " " + quote_str(arg5)
				+ " " + quote_str(arg6)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   const cstring& arg5, 
							   const cstring& arg6, 
							   const cstring& arg7, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				+ " " + quote_str(arg5)
				+ " " + quote_str(arg6)
				+ " " + quote_str(arg7)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   const cstring& arg5, 
							   const cstring& arg6, 
							   const cstring& arg7, 
							   const cstring& arg8, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				+ " " + quote_str(arg5)
				+ " " + quote_str(arg6)
				+ " " + quote_str(arg7)
				+ " " + quote_str(arg8)
				,
				which, 
				timeout);
}
program_runner::program_runner(const cstring& cmd, 
							   const cstring& arg1, 
							   const cstring& arg2, 
							   const cstring& arg3, 
							   const cstring& arg4, 
							   const cstring& arg5, 
							   const cstring& arg6, 
							   const cstring& arg7, 
							   const cstring& arg8, 
							   const cstring& arg9, 
							   which_output_t which, int timeout)
{
	ctor_helper(quote_str(cmd)
				+ " " + quote_str(arg1)
				+ " " + quote_str(arg2)
				+ " " + quote_str(arg3)
				+ " " + quote_str(arg4)
				+ " " + quote_str(arg5)
				+ " " + quote_str(arg6)
				+ " " + quote_str(arg7)
				+ " " + quote_str(arg8)
				+ " " + quote_str(arg9)
				,
				which, 
				timeout);
}

CLOSE_NAMESPACE(bhj)
