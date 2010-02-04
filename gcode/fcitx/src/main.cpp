/* Created by Anjuta version 1.1.97 */
/*	This file will not be overwritten */

/*Program closes with a mouse click or KeyPressMaskpress */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH

#include <scim.h>
#include "scim_fcitx_imengine.h"
using namespace scim;

#include "version.h"

#include "main.h"

#include "tools.h"




#include "ime.h"
#include "table.h"
#include "punc.h"
#include "py.h"


int Fcim_main (int argc, char *argv[])
{
    LoadConfig (True);

    LoadProfile ();

    if (!LoadPuncDict ());
//	printf ("无法打开中文标点文件，将无法输入中文标点！\n");


    SetIM ();



    return 0;
}
