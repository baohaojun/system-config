#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH
#include <ctype.h>

#include <scim.h>
#include "scim_fcitx_imengine.h"
using namespace scim;

#include "xim.h"
#include "ime.h"
#include "punc.h"
#include "table.h"
#include "py.h"
#include "tools.h"
#include "sp.h"
#include "qw.h"


int             iCursorPos;
IM             *im = NULL;
INT8            iIMCount = 0;

int             iMaxCandWord = 8;
int             iCandPageCount;
int             iCurrentCandPage;
int             iCandWordCount;

int             iLegendCandWordCount;
int             iLegendCandPageCount;
int             iCurrentLegendCandPage;

int             iCodeInputCount;

// *************************************************************
char            strCodeInput[MAX_USER_INPUT + 1];
char            strStringGet[MAX_USER_INPUT + 1];	//保存输入法返回的需要送到客户程序中的字串
unsigned int    messageColor[MESSAGE_TYPE_COUNT] = {
    SCIM_RGB_COLOR(255, 0, 0),
    SCIM_RGB_COLOR(0, 0, 255),
    SCIM_RGB_COLOR(200, 0, 0),
    SCIM_RGB_COLOR(0, 150, 100),
    SCIM_RGB_COLOR(0, 0, 255),
    SCIM_RGB_COLOR(100, 100, 255),
    SCIM_RGB_COLOR(0, 0, 0)
};
unsigned int    cursorColor = SCIM_RGB_COLOR(92, 210, 131);
// *************************************************************

ENTER_TO_DO     enterToDo = K_ENTER_SEND;

Bool            bCorner = False;	//全半角切换
Bool            bChnPunc = True;	//中英文标点切换
Bool            bUseGBK = False;	//是否支持GBK
Bool            bIsDoInputOnly = False;	//表明是否只由输入法来处理键盘
Bool            bLastIsNumber = False;	//上一次输入是不是阿拉伯数字
Bool            bInCap = False;	//是不是处于大写后的英文状态
Bool            bAutoHideInputWindow = True;	//是否自动隐藏输入条
Bool            bEngPuncAfterNumber = True;	//数字后面输出半角符号(只对'.'/','有效)
Bool            bPhraseTips = False;
INT8            lastIsSingleHZ = 0;

Bool            bEngAfterSemicolon = True;
Bool            bEngAfterCap = True;
Bool            bDisablePagingInLegend = True;

KeyEvent             i2ndSelectKey = KeyEvent("Control+Control_L");	//第二个候选词选择键，为扫描码
KeyEvent             i2ndSelectKeyPress = KeyEvent("Control_L");	//第二个候选词选择键，为扫描码
KeyEvent             i3rdSelectKey = KeyEvent("Control+Control_R");	//第三个候选词选择键，为扫描码
KeyEvent             i3rdSelectKeyPress = KeyEvent("Control_R");	//第三个候选词选择键，为扫描码


KEY_RELEASED    keyReleased = KR_OTHER;
Bool		bDoubleSwitchKey = False;
KeyEvent         switchKey = KeyEvent("Shift+Shift_L");
KeyEvent         switchKeyPress = KeyEvent("Shift_L");

//热键定义

KeyEvent         hkGBK[HOT_KEY_COUNT] = { KeyEvent("Alt+m"), KeyEvent()  };
KeyEvent         hkLegend[HOT_KEY_COUNT] = { KeyEvent("Alt+l"), KeyEvent() };
KeyEvent         hkCorner[HOT_KEY_COUNT] = { KeyEvent("Shift+space"), KeyEvent() };	//全半角切换
KeyEvent         hkPunc[HOT_KEY_COUNT] = { KeyEvent("Alt+space"), KeyEvent() };	//中文标点
KeyEvent         hkNextPage[HOT_KEY_COUNT] = { KeyEvent("period"),KeyEvent() };
KeyEvent         hkPrevPage[HOT_KEY_COUNT] = { KeyEvent("comma"), KeyEvent() };


Bool            bUseLegend = False;
Bool            bIsInLegend = False;

INT8            iIMIndex = 0;
Bool            bUsePinyin = True;
Bool            bUseSP = False;
Bool            bUseQW = False;
Bool            bUseTable = True;


MESSAGE         messageUp[32];
uint            uMessageUp = 0;
MESSAGE         messageDown[32];
uint            uMessageDown = 0;

Bool            bShowPrev = False;
Bool            bShowNext = False;
Bool		bShowCursor = False;
Bool		bLocked = True;

//++++++++++++++++++++++++++++++++++++++++
/*
INT8		iKeyPressed = 0;
Bool		bRelease = True;
*/


void ResetInput (void)
{
    iCandPageCount = 0;
    iCurrentCandPage = 0;
    iCandWordCount = 0;
    iLegendCandWordCount = 0;
    iCurrentLegendCandPage = 0;
    iLegendCandPageCount = 0;
    iCursorPos = 0;

    strCodeInput[0] = '\0';
    iCodeInputCount = 0;

    bIsDoInputOnly = False;

    bShowPrev = False;
    bShowNext = False;

    bIsInLegend = False;
    bInCap = False;

    if (IsIM (NAME_OF_PINYIN))
	bSingleHZMode = False;
    else
	bShowCursor = False;

    if (im[iIMIndex].ResetIM)
	im[iIMIndex].ResetIM ();
}


//FILE           *fd;
bool ProcessKey (FcitxInstance& fInst, const KeyEvent& key)
{

    int 	    iLen;
    
    int             count; //在原先的fcitx中，count表示keyboard buffer中有多少字符。
    int             iKey=key.get_ascii_code();
    char           *pstr;
    KeyEvent key2=KeyEvent(key.code, key.mask&~SCIM_KEY_ReleaseMask, key.layout);
    int             keyCount;
    INPUT_RETURN_VALUE retVal;


    if (key2.empty())
	return false;

    retVal = IRV_TO_PROCESS;

    if (key.is_key_release()) {
	    
	if ((!bIsDoInputOnly)) {
	    if (!bLocked && key2==KeyEvent("Shift+Alt+Shift_L")) {
		
		if (fInst.imeState == IS_CHN) {
		    SwitchIM (-1);
		    fInst.refresh_status_property();
		}
	    }
	    else if (key2 == switchKey && keyReleased == KR_CTRL && !bDoubleSwitchKey) {
	        fInst.ChangeIMState();
		retVal = IRV_DO_NOTHING;
	    }
	    else if (key2 == i2ndSelectKey && keyReleased == KR_2ND_SELECTKEY) {
		if (!bIsInLegend) {
		    pstr = im[iIMIndex].GetCandWord (1);
		    if (pstr) {
			strcpy (strStringGet, pstr);
			if (bIsInLegend)
			    retVal = IRV_GET_LEGEND;
			else
			    retVal = IRV_GET_CANDWORDS;
		    }
		    else if (iCandWordCount)
			retVal = IRV_DISPLAY_CANDWORDS;
		    else
			retVal = IRV_TO_PROCESS;
		}
		else {
			strcpy(strStringGet, " ");
			uMessageDown = 0;
			retVal = IRV_GET_CANDWORDS;
		}
	    }
	    else if (key2 == i3rdSelectKey && keyReleased == KR_3RD_SELECTKEY) {
		if (!bIsInLegend) {
		    pstr = im[iIMIndex].GetCandWord (2);
		    if (pstr) {
			strcpy (strStringGet, pstr);
			if (bIsInLegend)
			    retVal = IRV_GET_LEGEND;
			else
			    retVal = IRV_GET_CANDWORDS;
		    }
		    else if (iCandWordCount)
			retVal = IRV_DISPLAY_CANDWORDS;
		}
		else {
			strcpy(strStringGet, "　");
			uMessageDown = 0;
			retVal = IRV_GET_CANDWORDS;
		}
	    }
	}
    }

    if (retVal == IRV_TO_PROCESS) {
	if (key.is_key_press()) {
	    
	    if (!(key2 == switchKeyPress) )
		keyReleased = KR_OTHER;
		    
	    if (key2 == switchKeyPress) {
		keyReleased = KR_CTRL;
		//retVal = (bDoubleSwitchKey)? IRV_CLEAN:IRV_DO_NOTHING;
		retVal = IRV_DO_NOTHING;
	    }
	}
	if (retVal == IRV_TO_PROCESS && fInst.imeState == IS_CHN) {
	    if (key.is_key_press()) {
	    
		if (!key2.mask) {
		    if (key2 == i2ndSelectKeyPress)
			keyReleased = KR_2ND_SELECTKEY;
		    else if (key2 == i3rdSelectKeyPress)
			keyReleased = KR_3RD_SELECTKEY;
		}

		if (key2 == KeyEvent() ) {
		    if (bLocked)
			retVal = IRV_TO_PROCESS;
		}
		else {
		    if (!bInCap && !bCorner) {
			retVal = im[iIMIndex].DoInput (key);
			if (!IsIM (NAME_OF_PINYIN) && !IsIM (NAME_OF_SHUANGPIN))
			    iCursorPos = iCodeInputCount;
		    }

		    if (!bIsDoInputOnly && retVal == IRV_TO_PROCESS) {
			if (bCorner && (iKey >= 32 && iKey <= 126)&& !key2.mask) {
			    //有人报 空格 的全角不对，正确的是0xa1 0xa1
			    //但查资料却说全角符号总是以0xa3开始。
			    //由于0xa3 0xa0可能会显示乱码，因此采用0xa1 0xa1的方式
			    if (iKey == ' ')
				sprintf (strStringGet, "%c%c", 0xa1, 0xa1);
			    else
				sprintf (strStringGet, "%c%c", 0xa3, 0xa0 + iKey - 32);
			    retVal = IRV_GET_CANDWORDS;
			}
			else if ((iKey >= 'A' && iKey <= 'Z') &&
				 bEngAfterCap && 				 
				 !(key2.mask&~SCIM_KEY_ShiftMask&~SCIM_KEY_CapsLockMask)) {
			    bInCap = True;
			    FCIM_DEUBG();
			    if (!bIsInLegend && iCandWordCount) {
				pstr = im[iIMIndex].GetCandWord (0);
				iCandWordCount = 0;
				if (pstr) {
				    strcpy (strStringGet, pstr);
				    SendHZtoClient (fInst, strStringGet);
				    iCodeInputCount = 0;
				}
			    }
			}
			else if (!key2.mask && iKey == ';' && bEngAfterSemicolon && !iCodeInputCount)
			    bInCap = True;
			else if (IsHotKey (key2, hkCorner))
			    retVal = ChangeCorner (fInst);
			else if (IsHotKey (key2, hkPunc))
			    retVal = ChangePunc (fInst);
			else if (IsHotKey (key2, hkGBK))
			    retVal = ChangeGBK (fInst);
			else if (IsHotKey (key2, hkPrevPage))
			    retVal = im[iIMIndex].GetCandWords (SM_PREV);
			else if (IsHotKey (key2, hkNextPage))
			    retVal = im[iIMIndex].GetCandWords (SM_NEXT);
			else if (IsHotKey (key2, hkLegend))
			    retVal = ChangeLegend (fInst);

			if (retVal == IRV_TO_PROCESS) {
			    if (bInCap) {
				if (iKey == ' ' && !key2.mask && iCodeInputCount==0) {

					strcpy (strStringGet, "；");
					retVal = IRV_ENG;
					bInCap = False;

				}
				else if (isprint (iKey) && iKey < 128 && !(key2.mask&~SCIM_KEY_CapsLockMask&~SCIM_KEY_ShiftMask)) {
				    if (iCodeInputCount == MAX_USER_INPUT)
					retVal = IRV_DO_NOTHING;
				    else {
					if (!bEngAfterSemicolon || !(bEngAfterSemicolon && (iCodeInputCount == 0 && iKey == ';'))) {
					    strCodeInput[iCodeInputCount++] = iKey;
					    strCodeInput[iCodeInputCount] = '\0';
					    bShowCursor = True;
					    iCursorPos = iCodeInputCount;
					}
					retVal = IRV_DISPLAY_MESSAGE;
				    }
				}
				else if (key2 == KeyEvent(SCIM_KEY_BackSpace) && iCodeInputCount) {
				    iCodeInputCount--;
				    iCursorPos--;
				    strCodeInput[iCodeInputCount] = '\0';
				    retVal = IRV_DISPLAY_MESSAGE;
				    if (!iCodeInputCount)
					retVal = IRV_CLEAN;
				}
				uMessageUp = 1;
				uMessageDown = 1;
				if (bEngAfterSemicolon && !iCodeInputCount) {
				    iCursorPos=100;
				    strcpy (messageUp[0].strMsg, "进入英文输入状态");
				    strcpy (messageDown[0].strMsg, "空格输入；Enter输入;");
				}
				else {
				    strcpy (messageUp[0].strMsg, strCodeInput);
				    strcpy (messageDown[0].strMsg, "按 Enter输入英文");
				}
				messageUp[0].type = MSG_INPUT;
				messageDown[0].type = MSG_TIPS;
			    }
			    else if ((bLastIsNumber && bEngPuncAfterNumber)
				     && (iKey == '.' || iKey == ',')
				     && !iCandWordCount) {
				retVal = IRV_TO_PROCESS;
				bLastIsNumber = False;
			    }
			    else {
				if (bChnPunc) {
				    int             iPunc;

				    pstr = NULL;
				    iPunc = IsPunc (iKey);
				    if (iPunc != -1) {
					strStringGet[0] = '\0';
					if (!bIsInLegend)
					    pstr = im[iIMIndex].GetCandWord (0);
					if (pstr)
					    strcpy (strStringGet, pstr);
					strcat (strStringGet, chnPunc[iPunc].strChnPunc[chnPunc[iPunc].iWhich]);

					uMessageUp = 1;
					messageUp[0].strMsg[0] = iKey;
					messageUp[0].strMsg[1] = '\0';
					messageUp[0].type = MSG_INPUT;

					uMessageDown = 1;
					strcpy (messageDown[0].strMsg, chnPunc[iPunc].strChnPunc[chnPunc[iPunc].iWhich]);
					messageDown[0].type = MSG_OTHER;

					chnPunc[iPunc].iWhich++;
					if (chnPunc[iPunc].iWhich >= chnPunc[iPunc].iCount)
					    chnPunc[iPunc].iWhich = 0;

					retVal = IRV_PUNC;
				    }
				    else if (isprint (iKey) && iKey < 128 && !key2.mask) {
					if (iKey >= '0' && iKey <= '9')
					    bLastIsNumber = True;
					else {
					    bLastIsNumber = False;
					    if (iKey == ' ')
						retVal = IRV_DONOT_PROCESS_CLEAN;	//为了与mozilla兼容
					    else {
						strStringGet[0] = '\0';
						if (!bIsInLegend)
						    pstr = im[iIMIndex].GetCandWord (0);
						if (pstr)
						    strcpy (strStringGet, pstr);
						iLen = strlen (strStringGet);
						uMessageDown = uMessageUp = 0;
						strStringGet[iLen] = iKey;
						strStringGet[iLen + 1] = '\0';
						retVal = IRV_ENG;
					    }
					}
				    }
				}
			    }
			}
		    }

		    if (retVal == IRV_TO_PROCESS) {
			if (key2==KeyEvent(SCIM_KEY_Escape)) {
			    if (iCodeInputCount || bInCap || bIsInLegend)
				retVal = IRV_CLEAN;
			    else
				retVal = IRV_DONOT_PROCESS;
			}
			else if (key2==KeyEvent("Control+5")) {
			    SetIM ();
			    fInst.refresh_status_property();
			    LoadConfig (False);


			    retVal = IRV_DO_NOTHING;
			}
			else if (key2.code==SCIM_KEY_Return && (key2.mask&~(SCIM_KEY_ShiftMask|SCIM_KEY_CapsLockMask))==0) {
			    if (bInCap) {
				if (bEngAfterSemicolon && !iCodeInputCount)
				    strcpy (strStringGet, ";");
				else
				    strcpy (strStringGet, strCodeInput);
				retVal = IRV_ENG;
				uMessageUp = uMessageDown = 0;
				bInCap = False;
			    }
			    else if (!iCodeInputCount)
				retVal = IRV_DONOT_PROCESS;
			    else {
				switch (enterToDo) {
				case K_ENTER_NOTHING:
				    retVal = IRV_DO_NOTHING;
				    break;
				case K_ENTER_CLEAN:
				    retVal = IRV_CLEAN;
				    break;
				case K_ENTER_SEND:
				    uMessageDown = 1;
				    strcpy (messageDown[0].strMsg, strCodeInput);
				    strcpy (strStringGet, strCodeInput);
				    retVal = IRV_ENG;
				    break;
				}
			    }
			}
			else if (isprint (iKey) && iKey < 128 && !key2.mask)
			    retVal = IRV_DONOT_PROCESS_CLEAN;
			else
			    retVal = IRV_DONOT_PROCESS;
		    }
		}
	    }
	    else if (key.is_key_press())
		retVal = IRV_DONOT_PROCESS;
	}
    }

    switch (retVal) {
    case IRV_DO_NOTHING:
	return true;
	
    case IRV_TO_PROCESS:
    case IRV_DONOT_PROCESS:
	return false;
	
    case IRV_DONOT_PROCESS_CLEAN:
	ResetInput();
	fInst.ResetInputWindow();
	return false;
    case IRV_CLEAN:
	ResetInput ();
	fInst.ResetInputWindow ();
	return true;

    case IRV_DISPLAY_CANDWORDS:
	bShowNext = bShowPrev = False;
	if (bIsInLegend) {
	    if (iCurrentLegendCandPage > 0)
		bShowPrev = True;
	    if (iCurrentLegendCandPage < iLegendCandPageCount)
		bShowNext = True;
	}
	else {
	    if (iCurrentCandPage > 0)
		bShowPrev = True;
	    if (iCurrentCandPage < iCandPageCount)
		bShowNext = True;
	}

	fInst.DisplayInputWindow ();
	return true;
	
    case IRV_DISPLAY_LAST:
	bShowNext = bShowPrev = False;
	uMessageUp = 1;
	messageUp[0].strMsg[0] = strCodeInput[0];
	messageUp[0].strMsg[1] = '\0';
	messageUp[0].type = MSG_INPUT;
	uMessageDown = 1;
	strcpy (messageDown[0].strMsg, strStringGet);
	messageDown[0].type = MSG_TIPS;
	fInst.DisplayInputWindow ();
	return true;
	
    case IRV_DISPLAY_MESSAGE:
	bShowNext = False;
	bShowPrev = False;
	fInst.DisplayInputWindow ();
	return true;
	
    case IRV_GET_LEGEND:
	SendHZtoClient (fInst, strStringGet);
	if (iLegendCandWordCount) {
	    bShowNext = bShowPrev = False;
	    if (iCurrentLegendCandPage > 0)
		bShowPrev = True;
	    if (iCurrentLegendCandPage < iLegendCandPageCount)
		bShowNext = True;
	    bLastIsNumber = False;
	    iCodeInputCount = 0;
	    fInst.DisplayInputWindow ();
	}
	else {
	    ResetInput ();
	    uMessageDown=uMessageUp=0;
	    		
		fInst.DisplayInputWindow ();
	}

	return true;
    case IRV_GET_CANDWORDS:
	if (bPhraseTips && im[iIMIndex].PhraseTips)
	    DoPhraseTips ();
    case IRV_ENG:
    case IRV_PUNC:
	ResetInput ();
	if (!(uMessageDown && retVal == IRV_GET_CANDWORDS)
	    && bAutoHideInputWindow && (retVal == IRV_PUNC || (!bPhraseTips || (bPhraseTips && !lastIsSingleHZ))))
	    fInst.ResetInputWindow();
	    else
	    fInst.DisplayInputWindow ();

	    
    case IRV_GET_CANDWORDS_NEXT:
	if (retVal == IRV_GET_CANDWORDS_NEXT || lastIsSingleHZ == -1)
	    fInst.DisplayInputWindow ();
	SendHZtoClient (fInst, strStringGet);
	bLastIsNumber = False;
	lastIsSingleHZ = 0;

	return true;
	
    default:
	;
    }
    return true;

}

Bool IsHotKey (const KeyEvent& iKey, KeyEvent * hotkey)
{   
    if (iKey.empty()) return False;
    if (iKey == hotkey[0] || iKey == hotkey[1])
        return True;
    return False;

}

INPUT_RETURN_VALUE ChangeCorner (FcitxInstance& fInst)
{
    ResetInput ();
    fInst.ResetInputWindow ();
    bCorner = !bCorner;
    fInst.refresh_letter_property();

    SaveProfile ();

    return IRV_DO_NOTHING;
}

INPUT_RETURN_VALUE ChangePunc (FcitxInstance& fInst)
{
    bChnPunc = !bChnPunc;
    fInst.refresh_punct_property();
    SaveProfile ();

    return IRV_DO_NOTHING;
}

INPUT_RETURN_VALUE ChangeGBK (FcitxInstance& fInst)
{
    bUseGBK = !bUseGBK;
    fInst.refresh_gbk_property();
    ResetInput ();
    fInst.ResetInputWindow ();


    SaveProfile ();

    return IRV_CLEAN;
}

INPUT_RETURN_VALUE ChangeLegend (FcitxInstance& fInst)
{
    bUseLegend = !bUseLegend;
    ResetInput ();
    fInst.ResetInputWindow ();
    fInst.refresh_legend_property();

    //DisplayMainWindow ();
    //XUnmapWindow (dpy, inputWindow);

    SaveProfile ();

    return IRV_CLEAN;
}

void SwitchIM (INT8 index)
{
    INT8            iLastIM;

    iLastIM = (iIMIndex >= iIMCount) ? (iIMCount - 1) : iIMIndex;

    if (index == (INT8) - 1) {
	if (iIMIndex == (iIMCount - 1))
	    iIMIndex = 0;
	else
	    iIMIndex++;
    }
    else {
	if (index >= iIMCount)
	    iIMIndex = iIMCount - 1;
    }


    if (iIMCount == 1)
	return;

    if (im[iLastIM].Destroy != NULL)
	im[iLastIM].Destroy ();

    ResetInput ();


    SaveProfile ();

    if (im[iIMIndex].Init)
	im[iIMIndex].Init ();
}

void DoPhraseTips (void)
{
    if (!bPhraseTips)
	return;

    if (im[iIMIndex].PhraseTips ())
	lastIsSingleHZ = (unsigned char)-1;
    else
	lastIsSingleHZ = 0;
}

/*
#define _DEBUG
*/
void RegisterNewIM (char *strName, 
		    void (*ResetIM) (void),
		    INPUT_RETURN_VALUE (*DoInput) (const KeyEvent&),
		    INPUT_RETURN_VALUE (*GetCandWords) (SEARCH_MODE),
		    char *(*GetCandWord) (int),
		    char *(*GetLegendCandWord) (int),
		    Bool (*PhraseTips) (void),
		    void (*Init) (void),
		    void (*Destroy) (void))
{
#ifdef _DEBUG
    printf ("REGISTER %s\n", strName);
#endif
    strcpy (im[iIMCount].strName, strName);
    im[iIMCount].ResetIM = ResetIM;
    im[iIMCount].DoInput = DoInput;
    im[iIMCount].GetCandWords = GetCandWords;
    im[iIMCount].GetCandWord = GetCandWord;
    im[iIMCount].GetLegendCandWord = GetLegendCandWord;
    im[iIMCount].PhraseTips = PhraseTips;
    im[iIMCount].Init = Init;
    im[iIMCount].Destroy = Destroy;

    iIMCount++;
}

Bool IsIM (char *strName)
{
    if (strstr (im[iIMIndex].strName, strName))
	return True;

    return False;
}

void SaveIM (void)
{
    if (iTableChanged || iTableOrderChanged)
	SaveTableDict ();
    if (iNewPYPhraseCount)
	SavePYUserPhrase ();
    if (iOrderCount)
	SavePYIndex ();
    if (iNewFreqCount)
	SavePYFreq ();
}

void SetIM (void)
{
    INT8            i;

    if (im)
	free (im);

    if (bUseTable)
	LoadTableInfo ();

    iIMCount = iTableCount;
    if (bUsePinyin)
	iIMCount++;
    if (bUseSP)
	iIMCount++;
    if (bUseQW)
	iIMCount++;

    im = (IM *) malloc (sizeof (IM) * iIMCount);
    iIMCount = 0;

    /* 加入输入法 */
    if (bUsePinyin || (!bUseSP && (!bUseTable || !iTableCount)))	//至少应该有一种输入法
	RegisterNewIM (NAME_OF_PINYIN, ResetPYStatus, DoPYInput, PYGetCandWords, PYGetCandWord, PYGetLegendCandWord, NULL, PYInit, NULL);
    if (bUseSP)
	RegisterNewIM (NAME_OF_SHUANGPIN, ResetPYStatus, DoPYInput, PYGetCandWords, PYGetCandWord, PYGetLegendCandWord, NULL, SPInit, NULL);
    if (bUseQW)
	RegisterNewIM (NAME_OF_QUWEI, NULL, DoQWInput, QWGetCandWords, QWGetCandWord, NULL, NULL, NULL, NULL);
    if (bUseTable) {
	for (i = 0; i < iTableCount; i++) {
	    RegisterNewIM (table[i].strName, TableResetStatus, DoTableInput, TableGetCandWords, TableGetCandWord, TableGetLegendCandWord, TablePhraseTips, TableInit, FreeTableIM);
	    table[i].iIMIndex = iIMCount - 1;
	}
    }

    SwitchIM (iIMIndex);
}

