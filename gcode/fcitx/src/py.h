#ifndef _PY_H
#define _PY_H

#include	"main.h"
#include	"ime.h"

#define PY_BASE_FILE	"pybase.mb"
#define PY_PHRASE_FILE	"pyphrase.mb"
#define PY_USERPHRASE_FILE "pyusrphrase.mb"
#define PY_INDEX_FILE	"pyindex.dat"
#define PY_FREQ_FILE	"pyfreq.mb"
#define PY_SYMBOL_FILE	"pySym.mb"

#define MAX_WORDS_USER_INPUT	32
#define MAX_PY_PHRASE_LENGTH	10
#define MAX_PY_LENGTH		6

#define AUTOSAVE_PHRASE_COUNT 	5
#define AUTOSAVE_ORDER_COUNT  	10
#define AUTOSAVE_FREQ_COUNT  	1

typedef enum FIND_MODE {
    FIND_PHRASE,
    FIND_BASE,
    FIND_FREQ,
    FIND_SYM
} FINDMODE;

typedef enum {
    PY_CAND_AUTO,
    PY_CAND_SYMBOL,
    PY_CAND_BASE,
    PY_CAND_SYMPHRASE,
    PY_CAND_USERPHRASE,
    PY_CAND_FREQ,
    PY_CAND_LEGEND
} PY_CAND_WORD_TYPE;

typedef struct _HZ {
    char            strHZ[MAX_PY_PHRASE_LENGTH * 2 + 1];
    int             iPYFA;
    uint            iHit;
    uint            iIndex;
    struct _HZ     *next;
    uint            flag:1;
} HZ;

typedef struct PYFREQ {
    HZ             *HZList;
    char            strPY[MAX_PY_PHRASE_LENGTH * MAX_PY_LENGTH + 1];
    uint            iCount;
    Bool            bIsSym;	//该变量用于实现特殊符号表
    struct PYFREQ  *next;
} PyFreq;

typedef struct PYPHRASE {
    char           *strPhrase;
    char           *strMap;
    struct PYPHRASE *next;
    uint            iIndex;
    uint            iHit;
    uint            flag:1;
} PyPhrase;

typedef struct PYBASE {
    char            strHZ[3];
    struct PYPHRASE *phrase;
    int             iPhrase;
    struct PYPHRASE *userPhrase;
    int             iUserPhrase;
    uint            iIndex;
    uint            iHit;
    uint            flag:1;
} PyBase;

typedef struct _PYFA {
    char            strMap[3];
    struct PYBASE  *pyBase;
    int             iBase;
} PYFA;

typedef struct PYINDEX {
    int             iPYFA;
    int             iBase;
    int             iPhrase;
    struct PYINDEXCANDWORD *next;
    struct PYINDEXCANDWORD *prev;
} PYIndex;

typedef struct PYFREQCANDWORD {
    HZ             *hz;
    char           *strPY;
} PYFreqCandWord;

typedef struct PYPHRASECANDWORD {
    int             iPYFA;
    int             iBase;
    struct PYPHRASE *phrase;
} PYPhraseCandWord;

typedef struct PYBASECANDWORD {
    int             iPYFA;
    int             iBase;
} PYBaseCandWord;

typedef struct PYLEGENDCANDWORD {
    PyPhrase       *phrase;
    int             iLength;
} PYLegendCandWord;

typedef union {
    PYFreqCandWord  sym;
    PYFreqCandWord  freq;
    PYBaseCandWord  base;
    PYPhraseCandWord phrase;
} PCand;

typedef struct PYCANDWORD {
    PCand           cand;
    uint            iWhich:3;	//0->Auto 1->系统单字 2->系统词组 3->用户词组 4->常用字
} PYCandWord;

typedef struct PYCANDINDEX {
    int             iPYFA;
    int             iBase;
    int             iPhrase;
} PYCandIndex;

typedef struct {
    char            strPY[(MAX_PY_LENGTH + 1) * MAX_PY_PHRASE_LENGTH + 1];
    char            strHZ[MAX_PY_PHRASE_LENGTH * 2 + 1];
    char            strMap[MAX_PY_PHRASE_LENGTH * 2 + 1];
} PY_SELECTED;

extern int             iPYFACount;
extern PYFA           *PYFAList;
extern uint            iCounter;
extern Bool            bPYBaseDictLoaded;
extern Bool            bPYOtherDictLoaded;

extern Bool            bSingleHZMode;	//这种情况下，只进行单字输入，不进行词组的匹配、生成及保存，用于码表输入法中的拼音输入

extern PyFreq         *pyFreq;
extern PyFreq	      *pCurFreq;
extern uint            iPYFreqCount;

extern char            strFindString[];
extern struct _ParsePYStruct   findMap;
extern int             iPYInsertPoint;

extern char            strPYLegendSource[];
extern char            strPYLegendMap[];
extern PyBase         *pyBaseForLengend;
extern PYLegendCandWord PYLegendCandWords[];

extern PY_SELECTED     pySelected[];
extern uint            iPYSelected;

extern PYCandWord      PYCandWords[];
extern char            strPYAuto[];
extern char            strPYAutoMap[];
extern Bool            bPYCreateAuto;
extern Bool            bPYSaveAutoAsPhrase;

extern ADJUSTORDER     baseOrder;
extern ADJUSTORDER     phraseOrder;
extern ADJUSTORDER     freqOrder;

extern INT8            iNewPYPhraseCount;
extern INT8            iOrderCount;
extern INT8            iNewFreqCount;

extern INT8            iYCDZ;
extern char            cPYYCDZ[];	//以词定字

extern KeyEvent         hkPYAddFreq[];
extern Bool            bIsPYAddFreq;
extern KeyEvent         hkPYDelFreq[];
extern Bool            bIsPYDelFreq;
extern KeyEvent         hkPYDelUserPhr[];
extern Bool            bIsPYDelUserPhr;

extern Bool            bFullPY;	//该变量指示只要全拼输入(不使用简拼)

void            PYInit (void);
Bool            LoadPYBaseDict (void);
Bool            LoadPYOtherDict (void);
void            ResetPYStatus ();
int             GetBaseIndex (int iPYFA, char *strBase);
INPUT_RETURN_VALUE DoPYInput (const KeyEvent&);
void            UpdateCodeInputPY (void);
void            UpdateFindString (void);
void            CalculateCursorPosition (void);

void            PYResetFlags (void);
void            PYCreateAuto (void);
INPUT_RETURN_VALUE PYGetCandWords (SEARCH_MODE mode);
void		PYCreateCandString(void);
char           *PYGetCandWord (int iIndex);
void            PYGetSymCandWords (SEARCH_MODE mode);
Bool            PYAddSymCandWord (HZ * hz, SEARCH_MODE mode);
void            PYGetBaseCandWords (SEARCH_MODE mode);
Bool            PYAddBaseCandWord (PYCandIndex pos, SEARCH_MODE mode);
void            PYGetFreqCandWords (SEARCH_MODE mode);
Bool            PYAddFreqCandWord (HZ * hz, char *strPY, SEARCH_MODE mode);
void            PYGetPhraseCandWords (SEARCH_MODE mode);
Bool            PYAddPhraseCandWord (PYCandIndex pos, PyPhrase * phrase, SEARCH_MODE mode, Bool b);
void            PYGetCandWordsForward (void);
void            PYGetCandWordsBackward (void);
Bool            PYCheckNextCandPage (void);
void            PYSetCandWordFlag (int iIndex, Bool flag);
void            PYSetCandWordsFlag (Bool flag);
Bool            PYAddUserPhrase (char *phrase, char *map);
void            PYDelUserPhrase (int iPYFA, int iBase, PyPhrase * phrase);
int             GetBaseMapIndex (char *strMap);
void            SavePYUserPhrase (void);
void            SavePYFreq (void);
void            SavePYIndex (void);

void            PYAddFreq (int iIndex);
void            PYDelFreq (int iIndex);
Bool            PYIsInFreq (char *strHZ);

INPUT_RETURN_VALUE PYGetLegendCandWords (SEARCH_MODE iMode);
Bool            PYAddLengendCandWord (PyPhrase * phrase, SEARCH_MODE mode);
char           *PYGetLegendCandWord (int iIndex);
void            PYSetLegendCandWordsFlag (Bool flag);
void		PYGetPYByHZ(char *strHZ, char *strPY);

//void            PP ();
#endif
