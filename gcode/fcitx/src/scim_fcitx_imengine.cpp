/** @file scim_fcitx_imengine.cpp
 * implementation of class FcitxInstance.
 */

/*
 * Smart Common Input Method 
 * and Fcitx ported on to it 
 *
 * Copyright (c) 2002-2005 James Su <suzhe@tsinghua.org.cn> /author of scim
 * Yuking <yuking_net@sohu.com> /author of fcitx
 * Bao Haojun <baohaojun@yahoo.com> /ported fcitx onto scim
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
 *
 * $Id: scim_fcitx_imengine.cpp,v 1.6 2005/05/20 14:41:12 baohaojun Exp $
 *
 */

#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH

#include <scim.h>
#include "scim_fcitx_imengine.h"
#include "main.h"
#include "ime.h"
#include <string.h>
#include <ctype.h>
#include "ime-socket.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#define scim_module_init fcitx_LTX_scim_module_init
#define scim_module_exit fcitx_LTX_scim_module_exit
#define scim_imengine_module_init fcitx_LTX_scim_imengine_module_init
#define scim_imengine_module_create_factory fcitx_LTX_scim_imengine_module_create_factory

#define SCIM_CONFIG_IMENGINE_FCITX_LANGUAGES "/IMEngine/Fcitx/Languages"

#define SCIM_PROP_STATUS                       		"/IMEngine/Fcitx/Status"
#define SCIM_PROP_LETTER                                "/IMEngine/Fcitx/Letter"
#define SCIM_PROP_PUNCT                                 "/IMEngine/Fcitx/Punct"
#define SCIM_PROP_GBK 					"/IMEngine/Fcitx/Gbk"
#define SCIM_PROP_LEGEND				"/IMEngine/Fcitx/Legend"
#define SCIM_PROP_LOCK					"/IMEngine/Fcitx/Lock"

#define SCIM_FULL_LETTER_ICON                             (SCIM_ICONDIR "/full-letter.png")
#define SCIM_HALF_LETTER_ICON                             (SCIM_ICONDIR "/half-letter.png")
#define SCIM_FULL_PUNCT_ICON                              (SCIM_ICONDIR "/full-punct.png")
#define SCIM_HALF_PUNCT_ICON                              (SCIM_ICONDIR "/half-punct.png")
#define SCIM_STATUS_ICON				  (SCIM_ICONDIR "/%s%s.png")
#define SCIM_GBK_ICON				  	  (SCIM_ICONDIR "/%sgbk.png")
#define SCIM_LEGEND_ICON				  (SCIM_ICONDIR "/%slegend.png")
#define SCIM_LOCK_ICON					  (SCIM_ICONDIR "/%slock.png")

#define SCIM_FCITX_ICON_FILE                 (SCIM_ICONDIR "/fcitx.png")



using namespace scim;

static Pointer <FcitxFactory> _scim_fcitx_factory;
static ConfigPointer _scim_config;

static const char * _DEFAULT_LANGUAGES = (
    "zh_CN,zh_TW,zh_HK,zh_SG");

extern "C" {
    void scim_module_init (void)
    {
    }

    void scim_module_exit (void)
    {
        _scim_fcitx_factory.reset ();
        _scim_config.reset ();
    }

    unsigned int scim_imengine_module_init (const ConfigPointer &config)
    {
        _scim_config = config;
        return 1;
    }

    IMEngineFactoryPointer scim_imengine_module_create_factory (unsigned int factory)
    {
        String languages;

        if (factory != 0) return NULL;

        if (!_scim_config.null ()) {
            languages = _scim_config->read (
				String (SCIM_CONFIG_IMENGINE_FCITX_LANGUAGES),
				String ("default"));
        } else {
            languages = String ("default");
        }

        if (_scim_fcitx_factory.null ()) {
            _scim_fcitx_factory =
                new FcitxFactory (utf8_mbstowcs (String (("FCIM"))), languages);
        }
        return _scim_fcitx_factory;
    }
}

#ifdef SF_DEBUG
void FCIM_DEUBG()
{

}
#endif

// implementation of Fcitx
FcitxFactory::FcitxFactory ()
{
    m_name = utf8_mbstowcs (("FCIM"));
    set_languages (String ((_DEFAULT_LANGUAGES)));
}

IConvert FcitxInstance::m_gbiconv("GB18030");
FcitxFactory::FcitxFactory (const WideString& name,
							const String& languages)
{
    // do not allow too long name
    if (name.length () <= 8)
        m_name = name;
    else
        m_name.assign (name.begin (), name.begin () + 8);

    if (languages == String ("default"))
        set_languages (String ((_DEFAULT_LANGUAGES)));
    else
        set_languages (languages);
}

FcitxFactory::~FcitxFactory ()
{
}

WideString
FcitxFactory::get_name () const
{
    return m_name;
}

WideString
FcitxFactory::get_authors () const
{
    return utf8_mbstowcs (String (
							  ("(C) 2002-2004 James Su <suzhe@tsinghua.org.cn>")));
}

WideString
FcitxFactory::get_credits () const
{
    return WideString ();
}

WideString
FcitxFactory::get_help () const
{
    return utf8_mbstowcs (String ((
									  "Hot Keys:\n\n"
									  "  Control+u:\n"
									  "    switch between Multibyte encoding and Unicode.\n\n"
									  "  Control+comma:\n"
									  "    switch between full/half width punctuation mode.\n\n"
									  "  Shift+space:\n"
									  "    switch between full/half width letter mode.\n\n"
									  "  Esc:\n"
									  "    reset the input method.\n")));
}

String
FcitxFactory::get_uuid () const
{
    return String ("39f707ce-b3e0-4e3a-8dd8-a1afb886a9c9");
}

String
FcitxFactory::get_icon_file () const
{
    return String (SCIM_FCITX_ICON_FILE);
}

String
FcitxFactory::get_language () const
{
    return scim_validate_language ("other");
}

IMEngineInstancePointer
FcitxFactory::create_instance (const String& encoding, int id)
{
    return new FcitxInstance (this, encoding, id);
}

int
FcitxFactory::get_maxlen (const String &encoding)
{
    std::vector <String> locales;

    scim_split_string_list (locales, get_locales ());

    for (unsigned int i=0; i<locales.size (); ++i)
        if (scim_get_locale_encoding (locales [i]) == encoding)
            return scim_get_locale_maxlen (locales [i]);
    return 1;
}

// implementation of FcitxInstance
FcitxInstance::FcitxInstance (FcitxFactory *factory,
							  const String& encoding,
							  int id)
    : IMEngineInstanceBase (factory, encoding, id),
	  m_forward (false),
	  m_focused (false),
	  m_iconv (encoding)
{
    imeState = IS_CHN;
    Fcim_main(1, NULL);
}

FcitxInstance::~FcitxInstance ()
{

}

void FcitxInstance::DisplayInputWindow()
{
    String sup, sdown;
    WideString aux, table;
    WideString tmp;
    AttributeList attributes;
    std::vector<WideString> labels;
    int attr_start=0;
    int iCurTmp=iCursorPos;
    attributes.clear();

    for (int i=0; i<uMessageUp; i++) {
		FCIM_DEUBG();
	
		if (!bShowCursor) {
			m_gbiconv.convert(tmp, messageUp[i].strMsg);
			aux.append(tmp);
			attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
			attr_start+=tmp.length();
			continue;
		}

		if (iCurTmp>strlen(messageUp[i].strMsg)) {
			m_gbiconv.convert(tmp, messageUp[i].strMsg);
			aux.append(tmp);
			attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
			attr_start+=tmp.length();
		}
		else if (iCurTmp<strlen(messageUp[i].strMsg)){
			m_gbiconv.convert(tmp, messageUp[i].strMsg, iCurTmp+1);
			aux.append(tmp);
			attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
			attr_start+=tmp.length();
	  
			attributes.push_back(Attribute(attr_start-1, 1, SCIM_ATTR_BACKGROUND, cursorColor));
	  
			if(iCurTmp+1<strlen(messageUp[i].strMsg)) {
				m_gbiconv.convert(tmp, messageUp[i].strMsg+iCurTmp+1);
				aux.append(tmp);
				attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
				attr_start+=tmp.length();
			}
		
		} else {
			m_gbiconv.convert(tmp, messageUp[i].strMsg);  
			aux.append(tmp);
			attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
			attr_start+=tmp.length();
			if(i==uMessageUp-1) {
				aux.append(utf8_mbstowcs(String(" ")));
				attributes.push_back(Attribute(attr_start, 1, SCIM_ATTR_FOREGROUND, messageColor[messageUp[i].type]));
				attributes.push_back(Attribute(attr_start, 1, SCIM_ATTR_BACKGROUND, cursorColor));
			}
	      
		}
		

		iCurTmp-=strlen(messageUp[i].strMsg);	

    }

	
    if (uMessageUp) {
		update_aux_string(aux, attributes);
		show_aux_string();
    } else {
		hide_aux_string();
    }

    attributes.clear();
    attr_start=0;
    for (int i=0; i<uMessageDown; i++) {
		sdown.append(messageDown[i].strMsg);
		m_gbiconv.convert(tmp, messageDown[i].strMsg);
		attributes.push_back(Attribute(attr_start, tmp.length(), SCIM_ATTR_FOREGROUND, messageColor[messageDown[i].type]));
		attr_start+=tmp.length();
    }

    if (!uMessageDown) {
		hide_lookup_table();
		return;
    }

    m_gbiconv.convert(table, sdown);

    m_lookup_table.clear();
    if (bShowPrev) {
		m_lookup_table.append_candidate(utf8_mbstowcs("i want some space ok? some space ok? hehe hehe "));
		labels.push_back(WideString());
    }
    m_lookup_table.append_candidate(table, attributes);
    labels.push_back(WideString());
    if (bShowNext) {
		m_lookup_table.append_candidate(utf8_mbstowcs("i want some space ok? some space ok? hehe hehe "));
		labels.push_back(WideString());
    }

    m_lookup_table.set_page_size(1);
    if (bShowPrev) {
		m_lookup_table.page_down();
    }

    m_lookup_table.set_candidate_labels (labels);	
    update_lookup_table(m_lookup_table);
    show_lookup_table();
		
}

void FcitxInstance::ResetInputWindow()
{
    uMessageDown=uMessageUp=0;
    hide_aux_string();
    hide_lookup_table();
}

void FcitxInstance::ChangeIMState()
{
    if(imeState==IS_CHN) 
		imeState=IS_ENG;
    else
		imeState=IS_CHN;
    
    ResetInput();
    ResetInputWindow();
	
}

void FcitxInstance::send_string(char* str)
{
    String s=str;
    WideString com;
    m_gbiconv.convert(com, s);
  
    commit_string(com);
}


static String get_complex_key_name(const KeyEvent& key)
{
	String name;
	KeyEvent stripped_key(key.code);

	if (key.is_control_down()) {
		name += "C ";
	} 

	if (key.is_shift_down() && !isgraph(stripped_key.get_ascii_code())) {
		name += "S ";
	}

	if (key.is_alt_down()) {
		name += "A ";
	}


	if (isgraph(stripped_key.get_ascii_code())) {
		name.push_back(stripped_key.get_ascii_code());
	} else {
		String stripped_str = stripped_key.get_key_string();

		for (int i = 0; i < stripped_str.size(); i++) {
			name.push_back(tolower(stripped_str[i]));
		}
	}
	return name;
}

bool string_begin_with(const string& src, const string& dst)
{
	if (src.size() < dst.size()) {
		return false;
	}

	if (!strncmp(src.c_str(), dst.c_str(), dst.size())) {
		return true;
	}
	return false;
}

class ime_client
{
public:
	string compstr;
	string candsstr;
	string cand_idx;
	string hintstr;
	string activestr;
	string commitstr;
	string beepstr;
};

ime_client get_client_reply()
{
	ime_client client;
	for (;;) {
		string str = ime_recv_line();
		if (str.empty() || str == "end:") {
			break;
		}

		if (string_begin_with(str, "commit: ")) {
			client.commitstr = str.substr(strlen("commit: "));
		} else if (string_begin_with(str, "hint: ")) {
			client.hintstr = str.substr(strlen("hint: "));
		} else if (string_begin_with(str, "comp: ")) {
			client.compstr = str.substr(strlen("comp: "));
		} else if (string_begin_with(str, "cands: ")) {
			client.candsstr = str.substr(strlen("cands: "));
		} else if (string_begin_with(str, "cand_index: ")) {
			client.cand_idx = str.substr(strlen("cand_index: "));
		} else if (string_begin_with(str, "active: ")) {
			client.activestr = str.substr(strlen("active: "));
		} else if (string_begin_with(str, "beep: ")) {
			client.beepstr = str.substr(strlen("beep: "));
		} else {
			client.compstr = str;
			//beep();
		}
	}
	return client;
}


void 
FcitxInstance::translate_key(const string& key_desc)
{

	ime_write_line(string("keyed ") + key_desc);
	ime_client client = get_client_reply();

	if (!client.compstr.empty()) {
		update_preedit_string(utf8_mbstowcs(client.compstr), AttributeList());
		BHJDEBUG("client comp reply: %s", client.compstr.c_str());
		show_preedit_string();
	} else {
		hide_preedit_string();
	}


	// if (g_comp_str != client.compstr
	// 	|| g_cands_str != client.candsstr
	// 	|| g_cand_idx_str != client.cand_idx
	// 	|| g_hint_str != client.hintstr) {
	// 	g_comp_str = client.compstr;
	// 	g_cands_str = client.candsstr;
	// 	g_cand_idx_str = client.cand_idx;
	// 	g_hint_str = client.hintstr;
	// 	ic.add_show_comp_msg();
	// }

	// if (!client.commitstr.empty()) {
	// 	ic.send_text(client.commitstr);
	// }

	// g_hint_str = client.hintstr;
	// if (!client.beepstr.empty()) {
	// 	beep();
	// }

	// return ic.return_ime_msgs();
}

static bool want(const string& key_desc)
{
	BHJDEBUG(" want %s?", key_desc.c_str());
	ime_write_line(string("want ") + key_desc + "?");
	string ret = ime_recv_line();
	ime_recv_line(); //read the "end:" off 
	if (ret == "yes") {
		return true;
	} else if (ret == "no") {
		return false;
	} else {
		BHJDEBUG(" want: got wrong answer %s", ret.c_str());
	}
}

bool
FcitxInstance::process_key_event (const KeyEvent& key)
{

	static bool ime_server_inited = false;
	if (!ime_server_inited) {
		connect_ime_server();
		ime_server_inited = true;
	}
    if (!m_focused) return false;

    if (key.is_key_release()) {
		return false;
	}

	String key_name;
	int ascii_code = key.get_ascii_code();

	key_name = get_complex_key_name(key);

	if (key_name.empty()) {
		return false;
	}
	BHJDEBUG(" key event %s", key_name.c_str());
	if (want(key_name)) {
		translate_key(key_name);
		return true;
	}
	return false;
}

void
FcitxInstance::select_candidate (unsigned int item)
{
    WideString label = m_lookup_table.get_candidate_label (item);
    KeyEvent key ((int) label [0], 0);
    process_key_event (key);
}

void
FcitxInstance::update_lookup_table_page_size (unsigned int page_size)
{
    if (page_size > 0)
        m_lookup_table.set_page_size (page_size);
}

void
FcitxInstance::lookup_table_page_up ()
{

}

void
FcitxInstance::lookup_table_page_down ()
{

}

void
FcitxInstance::move_preedit_caret (unsigned int /*pos*/)
{
}

void
FcitxInstance::reset ()
{
    m_preedit_string = WideString ();

    m_iconv.set_encoding (get_encoding ());
    m_lookup_table.clear ();

    hide_lookup_table ();
    hide_preedit_string ();
}

void
FcitxInstance::focus_in ()
{
    m_focused = true;
	//ime_write_line("keyed C \\");
    DisplayInputWindow();
}

void
FcitxInstance::focus_out ()
{
	//ime_write_line("keyed C \\");
    m_focused = false;
}

void
FcitxInstance::trigger_property (const String &property)
{
}

/*
  vi:ts=4:nowrap:ai:expandtab
*/
