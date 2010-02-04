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
m_factory (factory),
m_status_property (SCIM_PROP_STATUS, ""),
m_letter_property (SCIM_PROP_LETTER, "Full/Half Letter"),
m_punct_property (SCIM_PROP_PUNCT, "Full/Half Punct"),
m_gbk_property (SCIM_PROP_GBK, "GBK"),
m_legend_property (SCIM_PROP_LEGEND, "Legend"),
m_lock_property (SCIM_PROP_LOCK, "Lock"),
m_unicode (true),
m_forward (false),
m_focused (false),
m_max_preedit_len (4),
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
    refresh_status_property();
	
}

void FcitxInstance::send_string(char* str)
{
    String s=str;
    WideString com;
    m_gbiconv.convert(com, s);
  
    commit_string(com);
}


bool
FcitxInstance::process_key_event (const KeyEvent& key)
{
    if (!m_focused) return false;

#ifdef SF_DEBUG
    if (key.is_key_release()) {
	update_aux_string (utf8_mbstowcs(key.get_key_string()));
	show_aux_string ();
	return false;
    }
#endif
	
    return ProcessKey(*this, key);
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

    if (m_unicode) m_max_preedit_len = 4;
    else if (m_factory)
        m_max_preedit_len = m_factory->get_maxlen (get_encoding ()) * 2;

    m_iconv.set_encoding (get_encoding ());
    m_lookup_table.clear ();

    hide_lookup_table ();
    hide_preedit_string ();
}

void
FcitxInstance::focus_in ()
{
    m_focused = true;

    initialize_properties ();

    DisplayInputWindow();
}

void
FcitxInstance::focus_out ()
{
    m_focused = false;
}

void
FcitxInstance::trigger_property (const String &property)
{
    if (property == SCIM_PROP_STATUS) {
	if (imeState==IS_CHN) {
	    SwitchIM(-1);
	    refresh_status_property();
	}
    }
    else if (property == SCIM_PROP_LOCK) {
	bLocked=!bLocked;
	refresh_lock_property();
    } else if (property == SCIM_PROP_LEGEND) {
	ChangeLegend(*this);
    } else if (property == SCIM_PROP_GBK) {
	ChangeGBK(*this);
    } else if (property == SCIM_PROP_PUNCT) {
	ChangePunc(*this);
    } else if (property == SCIM_PROP_LETTER) {
	ChangeCorner(*this);
    }

}

void
FcitxInstance::initialize_properties ()
{
    PropertyList proplist;

    proplist.push_back (m_status_property);
    proplist.push_back (m_letter_property);
    proplist.push_back (m_punct_property);
    proplist.push_back (m_gbk_property);
    proplist.push_back (m_legend_property);
    proplist.push_back (m_lock_property);

    register_properties (proplist);
    refresh_status_property ();
    refresh_letter_property ();
    refresh_punct_property ();
    refresh_gbk_property ();
    refresh_legend_property ();
    refresh_lock_property ();
}

void
FcitxInstance::refresh_status_property ()
{
    if (!m_focused) return;

    int i = strlen(SCIM_STATUS_ICON) + strlen(im[iIMIndex].strName) + 5;
    char *buff = (char*)malloc(sizeof(char)*i);
    sprintf(buff, SCIM_STATUS_ICON, imeState==IS_CHN?"":"no", im[iIMIndex].strName);

	    
    m_status_property.set_icon(buff);

    update_property (m_status_property);
    free(buff);
}

void 
FcitxInstance::refresh_letter_property ()
{
    if (!m_focused) return;

    if (bCorner)
        m_letter_property.set_icon(SCIM_FULL_LETTER_ICON);
    else
	m_letter_property.set_icon(SCIM_HALF_LETTER_ICON);
    update_property (m_letter_property);
}
void 
FcitxInstance::refresh_punct_property ()
{
    if (!m_focused) return;

    if (bChnPunc)
        m_punct_property.set_icon(SCIM_FULL_PUNCT_ICON);
    else
	m_punct_property.set_icon(SCIM_HALF_PUNCT_ICON);
    update_property (m_punct_property);
}

void
FcitxInstance::refresh_gbk_property ()
{
    if (!m_focused) return;
    int i = strlen(SCIM_GBK_ICON) + 5;
    char *buff = (char*)malloc(sizeof(char)*i);
    sprintf(buff, SCIM_GBK_ICON, bUseGBK?"":"no");
    
    m_gbk_property.set_icon(buff);
    update_property (m_gbk_property);
    free(buff);
}

void
FcitxInstance::refresh_legend_property ()
{
    if (!m_focused) return;
    int i = strlen(SCIM_LEGEND_ICON) + 5;
    char *buff = (char*)malloc(sizeof(char)*i);
    sprintf(buff, SCIM_LEGEND_ICON, bUseLegend?"":"no");
    
    m_legend_property.set_icon(buff);
    update_property (m_legend_property);
    free(buff);
}

void
FcitxInstance::refresh_lock_property ()
{
    if (!m_focused) return;
    int i = strlen(SCIM_LOCK_ICON) + 5;
    char *buff = (char*)malloc(sizeof(char)*i);
    sprintf(buff, SCIM_LOCK_ICON, bLocked?"":"no");
    
    m_lock_property.set_icon(buff);
    update_property (m_lock_property);
    free(buff);
}


/*
  vi:ts=4:nowrap:ai:expandtab
*/
