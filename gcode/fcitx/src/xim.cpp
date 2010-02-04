#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH

#include "scim.h"
#include "scim_fcitx_imengine.h"
using namespace scim;

#include <iconv.h>

#include "xim.h"

#include "tools.h"
#include "ime.h"

//#include "InputWindow.h"
#include "table.h"

//long            filter_mask = KeyPressMask | KeyReleaseMask;


Bool IsKey (const KeyEvent &key, KeyEvent * keymatch)
{			
  for (int i=0; !keymatch[i].empty(); i++) {
    if (key == keymatch[i])
      return true;
  }
  return key.empty(); //if key is empty:-)
}

void SendHZtoClient (FcitxInstance &fInst, char *strHZ)
{
	fInst.send_string(strHZ);
}

void EnterChineseMode (Bool bState)
{
    if (!bState) {
	ResetInput ();
//	ResetInputWindow ();

	if (im[iIMIndex].ResetIM)
	    im[iIMIndex].ResetIM ();
    }

//    DisplayMainWindow ();
}
