#ifndef _PYFA_H
#define _PYFA_H

#include "main.h"

typedef struct MH_PY {
    char           *strMap;
    int             bMode;
} MHPY;

typedef struct {
    char            strPY[7];
    int            *pMH;
} PYTABLE;

extern MHPY            MHPY_C[];
extern MHPY            MHPY_S[];
extern PYTABLE         PYTable[];

int             GetMHIndex_C (char map);
int             GetMHIndex_S (char map);

#endif
