/** @file scim_fcitx_imengine.h
 * definition of Fcitx related classes.
 */

/* 
 * Smart Common Input Method
 * 
 * Copyright (c) 2002-2005 James Su <suzhe@tsinghua.org.cn>
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
 * $Id: scim_fcitx_imengine.h,v 1.2 2005/05/11 13:07:54 baohaojun Exp $
 */

#if !defined (__SCIM_FCITX_IMENGINE_H)
#define __SCIM_FCITX_IMENGINE_H

#define Bool int
#define Status int
#define True 1
#define False 0
typedef unsigned char BYTE;
typedef char INT8;
typedef short INT16;
using namespace scim;

#define DEFAULT_IMNAME "fcitx"
#define STRBUFLEN 64

typedef enum _IME_STATE {
    IS_CLOSED = 0,
    IS_ENG,
    IS_CHN
} IME_STATE;

#ifdef SF_DEBUG
void FCIM_DEUBG();
#else 
#define FCIM_DEUBG()
#endif

class FcitxFactory : public IMEngineFactoryBase
{
    WideString m_name;

    friend class FcitxInstance;

public:
    FcitxFactory ();
    FcitxFactory (const WideString & name, const String & languages);
    virtual ~FcitxFactory ();

    virtual WideString  get_name () const;
    virtual WideString  get_authors () const;
    virtual WideString  get_credits () const;
    virtual WideString  get_help () const;
    virtual String      get_uuid () const;
    virtual String      get_icon_file () const;
    virtual String      get_language () const;

    virtual IMEngineInstancePointer create_instance (const String& encoding, int id = -1);

private:
    int get_maxlen (const String &encoding);
};

class FcitxInstance : public IMEngineInstanceBase
{
public:
    static IConvert   m_gbiconv;
public:
    Pointer <FcitxFactory> m_factory;

    CommonLookupTable m_lookup_table;
    WideString        m_preedit_string;

    bool              m_unicode;
    bool              m_forward;
    bool              m_focused;
    IME_STATE imeState;

    unsigned int      m_max_preedit_len;

    IConvert m_iconv;
    Property              m_status_property;
    Property              m_letter_property;
    Property              m_punct_property;
    Property              m_gbk_property;
    Property              m_legend_property;
    Property              m_lock_property;


public:
    void send_string(char* str);
    void DisplayInputWindow();
    void ResetInputWindow();
    void ChangeIMState();
    FcitxInstance (FcitxFactory *factory,
                     const String& encoding,
                     int id = -1);
    virtual ~FcitxInstance ();

    virtual bool process_key_event (const KeyEvent& key);
    virtual void move_preedit_caret (unsigned int pos);
    virtual void select_candidate (unsigned int item);
    virtual void update_lookup_table_page_size (unsigned int page_size);
    virtual void lookup_table_page_up ();
    virtual void lookup_table_page_down ();
    virtual void reset ();
    virtual void focus_in ();
    virtual void focus_out ();
    virtual void trigger_property (const String &property);
    void refresh_status_property ();
    void refresh_letter_property ();
    void refresh_punct_property ();
    void refresh_gbk_property ();
    void refresh_legend_property ();
    void refresh_lock_property ();


private:

    void initialize_properties ();

};

#endif
/*
vi:ts=4:nowrap:ai:expandtab
*/
