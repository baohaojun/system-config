#ifndef _TOOLS_H
#define _TOOLS_H

#include <stdio.h>
#include "ime.h"

void            LoadConfig (Bool bMode);
void            SaveConfig (void);
void            LoadProfile (void);
void            SaveProfile (void);
void            SetHotKey (char *strKey, KeyEvent * hotkey);
int             CalculateRecordNumber (FILE * fpDict);
void            SetSwitchKey (char *str);
Bool            CheckHZCharset (char *strHZ);
/*
#if defined(DARWIN)
int             ReverseInt (unsigned int pc_int);
#endif
*/
#endif
#ifndef _TOOLS_H
#define _TOOLS_H

#include <stdio.h>
#include "ime.h"

#define MAX_HZ_SAVED	10

void            LoadConfig (Bool bMode);
void            SaveConfig (void);
void            LoadProfile (void);
void            SaveProfile (void);
void            SetHotKey (char *strKey, KeyEvent * hotkey);
int             CalculateRecordNumber (FILE * fpDict);
void            SetSwitchKey (char *str);

#endif
