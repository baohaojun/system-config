#ifndef _xim_h
#define _xim_h

#include <stdio.h>

using namespace scim;
#include "scim_fcitx_imengine.h"
class FcitxInstance;

Bool IsKey (const KeyEvent &key, KeyEvent * keymatch);
void            SendHZtoClient (FcitxInstance &fInst, char *strHZ);




#endif
