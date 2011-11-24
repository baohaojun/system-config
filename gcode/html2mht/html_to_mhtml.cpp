// html_to_mhtml.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "html_to_mhtml.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

struct FIELD
{
	string name;
	DWORD crc;
	LONGLONG length;

	FIELD(): crc(0) {}
	FIELD(const char *n): name(n), crc(0), length(0) {}
	FIELD(string &n): name(n), crc(0), length(0) {}

	void CalcAll()
	{
		crc_32_type result;

		HANDLE hFile=CreateFile(name.c_str(), GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
		if(hFile==INVALID_HANDLE_VALUE) {
            bhjerr(" Error: got invalid hFile for %s", name.c_str());
			throw std::exception("cannot get the length of a file");
        }
		GetFileSizeEx(hFile, (PLARGE_INTEGER)&length);

		char *buffer=new char[(size_t)length];
		DWORD read;
		ReadFile(hFile, buffer, (DWORD)length, &read, NULL);
		result.process_bytes(buffer, (size_t)length);
		delete[] buffer;

		CloseHandle(hFile);

		crc=result.checksum();
	}

	friend bool operator==(FIELD &a, FIELD &b)
	{
		return a.crc==b.crc&&a.length==b.length;
	}
};
typedef list<FIELD> FIELDS;
typedef FIELDS::iterator FIELDSITERATOR;

FIELDS fields;
string htm_dir;

const string BOUNDARYDECL="----=_NextPart_000_0076_01C29953.BE473C30";
const string BOUNDARY="--"+BOUNDARYDECL;
const string BOUNDARY_END="--"+BOUNDARYDECL+"--\r\n";

string LocationFromPath(string file)
{
	for(string::iterator it=file.begin(); it!=file.end(); ++it)
		if(*it=='\\')
			*it='/';
	if(file[0]=='/')
		file.erase(file.begin());

	return "file:///"+file;
}

string ProcessFile(const char *file)
{
	HANDLE hFile=CreateFile(file, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return "";

	LARGE_INTEGER size;
	GetFileSizeEx(hFile, &size);
	char *buffer=new char[size.LowPart];
	int read;
	ReadFile(hFile, buffer, size.LowPart, (LPDWORD)&read, NULL);
	CloseHandle(hFile);

	int len=ToBase64Length(size.LowPart);
	char *b64=new char[len];
	BOOST_TEST(ToBase64((unsigned char *)b64, (const unsigned char *)buffer, size.LowPart)==len);
	delete[] buffer;

	string ret=BOUNDARY+"\r\nContent-Type: application/octet-stream;\r\nContent-Transfer-Encoding: base64\r\nContent-Location: "
		+LocationFromPath(file)+"\r\n\r\n"+b64+"\r\n\r\n";
	delete[] b64;

	return ret;
}

string ProcessText(const char *file)
{
	HANDLE hFile=CreateFile(file, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return "";

	LARGE_INTEGER size;
	GetFileSizeEx(hFile, &size);
	char *buffer=new char[size.LowPart];
	int read;
	ReadFile(hFile, buffer, size.LowPart, (LPDWORD)&read, NULL);
	CloseHandle(hFile);

	string ret=BOUNDARY+"\r\nContent-Type: application/octet-stream;\r\nContent-Transfer-Encoding: text/plain\r\nContent-Location: "
		+LocationFromPath(file)+"\r\n\r\n"+string(buffer,size.LowPart)+"\r\n\r\n";
	delete[] buffer;

	return ret;
}

inline const char *Convert(char ch)
{
    if(ch=='=')
		return "=3D";
	else if (unsigned char (ch) >= 0x80) {
        static char tmp[4] = {0};
        _snprintf(tmp, 4, "=%02X", unsigned char(ch));
        return tmp;
    } else {
		static char tmp[2]={0};
		*tmp=ch;
		return tmp;
	}
}

inline bool ProcessTag(string val, string *out, char **addfrom, const char *addto, const char *newaddfrom)
{
	if(PathIsURL(val.c_str()))
		return true;

    if (val.length() == 0)
        return true;
	if(PathIsRelative(val.c_str()))
		val=htm_dir+"\\"+val;
	if(PathFileExists(val.c_str()))
	{
		FIELD f(val);
		f.CalcAll();

		// now just push everything until now unto tmp and add the new stuff at the end
		for(char *pk=*addfrom; pk<addto; ++pk)
			(*out)+=Convert(*pk);

		string filepath=LocationFromPath(val);
		*addfrom=(char *)newaddfrom;
#pragma warning(disable : 4996)
		for(FIELDSITERATOR it=fields.begin(); it!=fields.end(); ++it)
			if(stricmp(it->name.c_str(), val.c_str())==0)
			{
				// already in the queue
				*out+="'"+filepath+"'";
				return true;
			}
			else if(*it==f)
			{
				// same file, different name; no need to add to queue, reuse the old file
				*out+="'"+LocationFromPath(it->name)+"'";
				return true;
			}

		// new file
		*out+="'"+filepath+"'";
		fields.push_back(f);
	}

	return true;
}

string ProcessHTML(const char *file)
{
	HANDLE hFile=CreateFile(file, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return "";

	LARGE_INTEGER size;
	GetFileSizeEx(hFile, &size);
	char *buffer=new char[size.LowPart];
	int read;
	ReadFile(hFile, buffer, size.LowPart, (LPDWORD)&read, NULL);
	CloseHandle(hFile);

	static regex 
		re(											// main regex -- searches whole tags
			"\\s*<"

			"\\s*(/?\\!?\\w+)\\s*"					// 1 - tag name

			"((?:"									// 2 - all attributes
				"[-0-9a-zA-Z]+"						//   - attribute name

				"(?:\\s*=\\s*"
				"(?:"								//   - value name [opt]
					"(?:'(?:\\\\.|[^'])+')|"
					"(?:\"(?:\\\\.|[^\"])+\")|"
					"(?:[^ \\t>]+)"
				"))?"
			"\\s*)*)"

			"\\s*/?\\s*"
			
			">\\s*"),

		sre(										// subregex -- searches each attribute in a given tag (#2 above)
			"([-0-9a-zA-Z]+)"						// 1 - attribute name

			"(?:\\s*=\\s*"
			"("										// 2 - value name [opt]
				"(?:'(?:\\\\.|[^'])+')|"
				"(?:\"(?:\\\\.|[^\"])+\")|"
				"(?:[^ \\t>]+)"
			"))?\\s*");

	cmatch m, sm;
	const char *p=buffer;
	char *addfrom=buffer;
	string tmp;tmp.reserve(3*size.LowPart);

	while(regex_search(p, (const char *)buffer+size.LowPart, m, re))
	{
		enum eAction
		{
			eA,
			eLink,
			eImg,
			eFrame,
			eBody,

			eDontCare,
			eNumberOfActions=eDontCare
		} type=eDontCare;
		static struct
		{
			const char *tag, *attr;
		} actions[eNumberOfActions]=
		{
			"a",		"href",
			"link",		"href",
			"img",		"src",
			"frame",	"src",
			"body",		"background",
		};

		// figure out the tag...
		for(int i=0; i<eNumberOfActions; ++i)
			if(stricmp(m.str(1).c_str(), actions[i].tag)==0)
			{
				type=(eAction)i;
				break;
			}

		// start processing the thing
		if(type!=eDontCare)
		{
			regex::iterator it=m[2].first, itend=m[2].second;

			while(regex_search(it, itend, sm, sre))
			{
				if(!sm[2].matched)
					break;

				string attr=sm.str(1), val=sm.str(2);

				// remove quotes from value
				if(val[0]=='"'||val[0]=='\'')
				{
					BOOST_ASSERT(val[val.size()-1]=='"'||val[val.size()-1]=='\'');

					val.erase(val.begin());
					val.resize(val.size()-1);
				}

				// now loop through the actions and see what's to be done
				for(int i=0; i<eNumberOfActions; ++i)
                {
                    if(stricmp(attr.c_str(), actions[(int)type].attr)==0 && ProcessTag(val, &tmp, &addfrom, &*sm[2].first, &*sm[2].second))
                        goto __next_tag; 
                    else 
                        break;
                }
				// continue loop
				it=sm[0].second;
			}
		}

__next_tag:
		// continue loop
		p=m[0].second;
	}

	// add the rest
	for(char *p=addfrom; p<buffer+size.LowPart; ++p)
		tmp+=Convert(*p);

	tmp=BOUNDARY+"\r\nContent-Type: text/html; \r\nContent-Transfer-Encoding: quoted-printable\r\nContent-Location: "
		+LocationFromPath(file)+"\r\n\r\n"+tmp+"\r\n\r\n";
	
	delete[] buffer;

	return tmp;
}

string Process(const char *file)
{
	static regex
		re_html("^.*\\.[sp]?html?$"),
		re_text("^.*\\.(?:c|cpp|h|hpp|cxx|hxx|txt|inl|ipp|css)$");

	if(regex_match(file, re_html))
		return ProcessHTML(file);
	else if(regex_match(file, re_text))
		return ProcessText(file);
	else
		return ProcessFile(file);
}

wstring to_wstring(const string& str)
{
	WCHAR tmp;
	int n = MultiByteToWideChar(GetACP(), 
								0,
								str.c_str(),
								str.size(), 
								&tmp, 
								0);

	if (n == 0) {
		return L"Error: MultiByteToWideChar";
	}

	WCHAR* buf = (WCHAR*)malloc((n+1) * sizeof(WCHAR));
	if (!buf) {
		return L"";
	}
	MultiByteToWideChar(GetACP(), 
						0,
						str.c_str(), 
						str.size(),
						buf,
						n+1);
	buf[n] = 0;
	wstring wstr = buf;
	free(buf);
	return wstr;
}

string to_string(const wstring& wstr)
{
	if (wstr.empty()) {
		return "";
	}

	int n = WideCharToMultiByte(GetACP(), 0, wstr.c_str(), wstr.size(), NULL, 0, NULL, NULL);
	if (n == 0) {
		return "Error: WideCharToMultiByte";
	}
	char *buf = (char*)malloc((n+1) * sizeof(char));
	if (!buf) {
		return "Error: malloc";
	}
	WideCharToMultiByte(GetACP(), 0, wstr.c_str(), wstr.size(), buf, n+1, NULL, NULL);
	buf[n] = 0;
	string str = buf;
	free(buf);
	return str;
}

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{

    int argc;
    LPWSTR pWCmdLine=GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(pWCmdLine, &argc);

    if (argc != 2) {
        bhjerr(" \nUsage: %s html-file", to_string(argv[0]).c_str());
    }

    string thePath=to_string(argv[1]);
    string file_out;
    if (thePath.find_last_of("\\/") == string::npos) {
        htm_dir = ".";
        file_out = thePath + ".mht";
    } else {
        htm_dir = thePath.substr(0, thePath.find_last_of("\\/"));
        file_out = thePath.substr(thePath.find_last_of("\\/") + 1) + ".mht";
    }

    fields.clear();
    string out="MIME-version: 1.0\r\nContent-Type: multipart/related;\r\n\tboundary=\""+BOUNDARYDECL+"\";\r\n\ttype=\"text/html\"\r\nX-MimeOLE: Produced By Microsoft MimeOLE V6.00.2800.1106\r\n\r\nThis is a multi-part message in MIME format.\r\n\r\n";
    FIELD field(thePath);
    field.CalcAll();
    fields.push_back(field);
    for(FIELDSITERATOR it=fields.begin(); it!=fields.end(); ++it)
        out+=Process(it->name.c_str());

    HANDLE hFile=CreateFile(file_out.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    DWORD written;
    WriteFile(hFile, out.c_str(), (DWORD)out.size(), &written, NULL);
    WriteFile(hFile, BOUNDARY_END.c_str(), BOUNDARY_END.size(), &written, NULL);
    CloseHandle(hFile);

	exit(0);
}
