#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH

#include <scim.h>
#include "scim_fcitx_imengine.h"
using namespace scim;

#include "tools.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <limits.h>
#include <string.h>

#include "version.h"
#include "PYFA.h"
#include "py.h"
#include "sp.h"
#include "ime.h"
#include "table.h"

/*
 * 读取用户的设置文件
 */
void LoadConfig (Bool bMode)
{
    FILE           *fp;
    char            str[PATH_MAX], *pstr;
    char            strPath[PATH_MAX];
    int             i;
    int             r, g, b;	//代表红绿蓝

    strcpy (strPath, (char *) getenv ("HOME"));
    strcat (strPath, "/.fcim/config");

    fp = fopen (strPath, "rt");

    if (!fp) {
	SaveConfig ();
	LoadConfig (True);	//读入默认值
	return;
    }

    for (;;) {
	if (!fgets (str, PATH_MAX, fp))
	    break;

	i = strlen (str) - 1;
	while (str[i] == ' ' || str[i] == '\n')
	    str[i--] = '\0';

	pstr = str;
	if (*pstr == ' ')
	    pstr++;
	if (pstr[0] == '#')
	    continue;

	if (strstr (pstr, "候选词个数=")) {
	    pstr += 11;
	    iMaxCandWord = atoi (pstr);
	    if (iMaxCandWord > 10)
		iMaxCandWord = MAX_CAND_WORD;
	}
	else if (strstr (pstr, "数字后跟半角符号=")) {
	    pstr += 17;
	    bEngPuncAfterNumber = atoi (pstr);
	}
	else if (strstr (pstr, "是否自动隐藏输入条=")) {
	    pstr += 19;
	    bAutoHideInputWindow = atoi (pstr);
	}
	else if (strstr (pstr, "cursor color=") && bMode) {
	    pstr +=13;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    cursorColor=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗提示色=") && bMode) {
	    pstr += 13;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[0]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗用户输入色=") && bMode) {
	    pstr += 17;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[1]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗序号色=") && bMode) {
	    pstr += 13;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[2]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗第一个候选字色=") && bMode) {
	    pstr += 21;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[3]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗用户词组色=") && bMode) {
	    pstr += 17;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[4]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗提示编码色=") && bMode) {
	    pstr += 17;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[5]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "输入窗其它文本色=") && bMode) {
	    pstr += 17;
	    sscanf (pstr, "%d %d %d\n", &r, &g, &b);
	    messageColor[6]=SCIM_RGB_COLOR(r, g, b);
	}
	else if (strstr (pstr, "中英文快速切换键=")) {
	    pstr += 17;
	    SetSwitchKey (pstr);
	}
	else if (strstr (pstr, "双击中英文切换=")) {
	    pstr += 15;
	    bDoubleSwitchKey = atoi (pstr);
	}
	else if (strstr (pstr, "GBK支持=")) {
	    pstr += 8;
	    SetHotKey (pstr, hkGBK);
	}
	else if (strstr (pstr, "分号输入英文=")) {
	    pstr += 13;
	    bEngAfterSemicolon = atoi (pstr);
	}
	else if (strstr (pstr, "大写字母输入英文=")) {
	    pstr += 17;
	    bEngAfterCap = atoi (pstr);
	}
	else if (strstr (pstr, "联想方式禁止翻页=")) {
	    pstr += 17;
	    bDisablePagingInLegend = atoi (pstr);
	}
	else if (strstr (pstr, "联想支持=")) {
	    pstr += 9;
	    SetHotKey (pstr, hkLegend);
	}
	else if (strstr (pstr, "Enter键行为=")) {
	    pstr += 12;
	    enterToDo = (ENTER_TO_DO) atoi (pstr);
	}
	else if (strstr (pstr, "全半角=")) {
	    pstr += 7;
	    SetHotKey (pstr, hkCorner);
	}
	else if (strstr (pstr, "中文标点=")) {
	    pstr += 9;
	    SetHotKey (pstr, hkPunc);
	}
	else if (strstr (pstr, "上一页=")) {
	    pstr += 7;
	    SetHotKey (pstr, hkPrevPage);
	}
	else if (strstr (pstr, "下一页=")) {
	    pstr += 7;
	    SetHotKey (pstr, hkNextPage);
	}
	else if (strstr (pstr, "第二三候选词选择键=")) {
	    pstr += 19;
	    if (!strcasecmp (pstr, "SHIFT")) {
		i2ndSelectKey = KeyEvent("Shift+Shift_L");
		i2ndSelectKeyPress = KeyEvent("Shift_L");
		i3rdSelectKey = KeyEvent("Shift+Shift_R");
		i3rdSelectKeyPress = KeyEvent("Shift_R");
	    }
	    else if (!strcasecmp (pstr, "CTRL")) {
		i2ndSelectKey = KeyEvent("Control+Control_L");
		i2ndSelectKeyPress = KeyEvent("Control_L");
		i3rdSelectKey = KeyEvent("Control+Control_R");
		i3rdSelectKeyPress = KeyEvent("Control_R");
	    }
	}

	else if (strstr (pstr, "使用拼音=")) {
	    pstr += 9;
	    bUsePinyin = atoi (pstr);
	}
	else if (strstr (pstr, "使用双拼=")) {
	    pstr += 9;
	    bUseSP = atoi (pstr);
	}
	else if (strstr (pstr, "使用区位=")) {
	    pstr += 9;
	    bUseQW = atoi (pstr);
	}
	else if (strstr (pstr, "使用码表=")) {
	    pstr += 9;
	    bUseTable = atoi (pstr);
	}
	else if (strstr (pstr, "提示词库中的词组=")) {
	    pstr += 17;
	    bPhraseTips = atoi (pstr);
	}

	else if (strstr (str, "使用全拼=")) {
	    pstr += 9;
	    bFullPY = atoi (pstr);
	}
	else if (strstr (str, "拼音自动组词=")) {
	    pstr += 13;
	    bPYCreateAuto = atoi (pstr);
	}
	else if (strstr (str, "保存自动组词=")) {
	    pstr += 13;
	    bPYSaveAutoAsPhrase = atoi (pstr);
	}
	else if (strstr (str, "增加拼音常用字=")) {
	    pstr += 15;
	    SetHotKey (pstr, hkPYAddFreq);
	}
	else if (strstr (str, "删除拼音常用字=")) {
	    pstr += 15;
	    SetHotKey (pstr, hkPYDelFreq);
	}
	else if (strstr (str, "删除拼音用户词组=")) {
	    pstr += 17;
	    SetHotKey (pstr, hkPYDelUserPhr);
	}
	else if (strstr (str, "拼音以词定字键=")) {
	    pstr += 15;
	    cPYYCDZ[0] = pstr[0];
	    cPYYCDZ[1] = pstr[1];
	}
	else if (strstr (str, "拼音单字重码调整方式=")) {
	    pstr += 21;
	    baseOrder = (ADJUSTORDER) atoi (pstr);
	}
	else if (strstr (str, "拼音词组重码调整方式=")) {
	    pstr += 21;
	    phraseOrder = (ADJUSTORDER) atoi (pstr);
	}
	else if (strstr (str, "拼音常用词重码调整方式=")) {
	    pstr += 23;
	    freqOrder = (ADJUSTORDER) atoi (pstr);
	}
	else if (strstr (str, "是否模糊an和ang=")) {
	    pstr += 16;
	    MHPY_C[0].bMode = atoi (pstr);
	    MHPY_S[5].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊en和eng=")) {
	    pstr += 16;
	    MHPY_C[1].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊ian和iang=")) {
	    pstr += 18;
	    MHPY_C[2].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊in和ing=")) {
	    pstr += 16;
	    MHPY_C[3].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊ou和u=")) {
	    pstr += 14;
	    MHPY_C[4].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊uan和uang=")) {
	    pstr += 18;
	    MHPY_C[5].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊c和ch=")) {
	    pstr += 14;
	    MHPY_S[0].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊f和h=")) {
	    pstr += 13;
	    MHPY_S[1].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊l和n=")) {
	    pstr += 13;
	    MHPY_S[2].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊s和sh=")) {
	    pstr += 14;
	    MHPY_S[3].bMode = atoi (pstr);
	}
	else if (strstr (str, "是否模糊z和zh=")) {
	    pstr += 14;
	    MHPY_S[4].bMode = atoi (pstr);
	}
    }

    fclose (fp);

}

void SaveConfig (void)
{
    FILE           *fp;
    char            strPath[PATH_MAX];

    strcpy (strPath, (char *) getenv ("HOME"));
    strcat (strPath, "/.fcim/");

    if (access (strPath, 0))
	mkdir (strPath, S_IRWXU);

    strcat (strPath, "config");
    fp = fopen (strPath, "wt");
    if (!fp) {
	fprintf (stderr, "\n无法创建文件 config！\n");
	return;
    }

    fprintf (fp, "[程序]\n");

    fprintf (fp, "\n[输出]\n");
    fprintf (fp, "数字后跟半角符号=%d\n", bEngPuncAfterNumber);
    fprintf (fp, "Enter键行为=%d\n", enterToDo);
    fprintf (fp, "分号输入英文=%d\n", bEngAfterSemicolon);
    fprintf (fp, "大写字母输入英文=%d\n", bEngAfterCap);
    fprintf (fp, "联想方式禁止翻页=%d\n", bDisablePagingInLegend);

    fprintf (fp, "\n[界面]\n");
    fprintf (fp, "候选词个数=%d\n", iMaxCandWord);
    fprintf (fp, "是否自动隐藏输入条=%d\n", bAutoHideInputWindow);
    
    fprintf (fp, "cursor color=%d %d %d\n",
	     SCIM_RGB_COLOR_RED(cursorColor), SCIM_RGB_COLOR_GREEN(cursorColor), SCIM_RGB_COLOR_BLUE(cursorColor));
    fprintf (fp, "输入窗提示色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[0]), SCIM_RGB_COLOR_GREEN(messageColor[0]), SCIM_RGB_COLOR_BLUE(messageColor[0]));
    fprintf (fp, "输入窗用户输入色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[1]), SCIM_RGB_COLOR_GREEN(messageColor[1]), SCIM_RGB_COLOR_BLUE(messageColor[1]));
    fprintf (fp, "输入窗序号色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[2]), SCIM_RGB_COLOR_GREEN(messageColor[2]), SCIM_RGB_COLOR_BLUE(messageColor[2]));
    fprintf (fp, "输入窗第一个候选字色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[3]), SCIM_RGB_COLOR_GREEN(messageColor[3]), SCIM_RGB_COLOR_BLUE(messageColor[3]));
    fprintf (fp, "#该颜色值只用于拼音中的用户自造词\n");
    fprintf (fp, "输入窗用户词组色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[4]), SCIM_RGB_COLOR_GREEN(messageColor[4]), SCIM_RGB_COLOR_BLUE(messageColor[4]));
    fprintf (fp, "输入窗提示编码色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[5]), SCIM_RGB_COLOR_GREEN(messageColor[5]), SCIM_RGB_COLOR_BLUE(messageColor[5]));
    fprintf (fp, "#五笔、拼音的单字/系统词组均使用该颜色\n");
    fprintf (fp, "输入窗其它文本色=%d %d %d\n", 
	     SCIM_RGB_COLOR_RED(messageColor[6]), SCIM_RGB_COLOR_GREEN(messageColor[6]), SCIM_RGB_COLOR_BLUE(messageColor[6]));


    fprintf (fp, "\n#除了“中英文快速切换键”外，其它的热键均可设置为两个，中间用空格分隔\n");
    fprintf (fp, "[热键]\n");
    fprintf (fp, "#中英文快速切换键 可以设置为L_CTRL R_CTRL L_SHIFT R_SHIFT\n");
    fprintf (fp, "中英文快速切换键=Shift_L\n");
    fprintf (fp, "双击中英文切换=%d\n", bDoubleSwitchKey);
    fprintf (fp, "GBK支持=Alt+m\n");
    fprintf (fp, "联想支持=Alt+l\n");
    fprintf (fp, "全半角=Shift+space\n");
    fprintf (fp, "中文标点=Alt+space\n");
    fprintf (fp, "上一页=comma\n");
    fprintf (fp, "上一页=minus\n");
    fprintf (fp, "下一页=period\n");
    fprintf (fp, "下一页=equal\n");
    fprintf (fp, "#第二三候选词选择键 SHIFT/CTRL\n");
    fprintf (fp, "第二三候选词选择键=CTRL\n");

    fprintf (fp, "\n[输入法]\n");
    fprintf (fp, "使用拼音=%d\n", bUsePinyin);
    fprintf (fp, "使用双拼=%d\n", bUseSP);
    fprintf (fp, "使用区位=%d\n", bUseQW);
    fprintf (fp, "使用码表=%d\n", bUseTable);
    fprintf (fp, "提示词库中的词组=%d\n", bPhraseTips);

    fprintf (fp, "\n[拼音]\n");
    fprintf (fp, "使用全拼=%d\n", bFullPY);
    fprintf (fp, "拼音自动组词=%d\n", bPYCreateAuto);
    fprintf (fp, "保存自动组词=%d\n", bPYSaveAutoAsPhrase);
    fprintf (fp, "增加拼音常用字=Control+8\n");
    fprintf (fp, "删除拼音常用字=Control+7\n");
    fprintf (fp, "删除拼音用户词组=Control+Delete\n");
    fprintf (fp, "#拼音以词定字键，等号后面紧接键，不要有空格\n");
    fprintf (fp, "拼音以词定字键=%c%c\n", cPYYCDZ[0], cPYYCDZ[1]);
    fprintf (fp, "#重码调整方式说明：0-->不调整  1-->快速调整  2-->按频率调整\n");
    fprintf (fp, "拼音单字重码调整方式=%d\n", baseOrder);
    fprintf (fp, "拼音词组重码调整方式=%d\n", phraseOrder);
    fprintf (fp, "拼音常用词重码调整方式=%d\n", freqOrder);
    fprintf (fp, "是否模糊an和ang=%d\n", MHPY_C[0].bMode);
    fprintf (fp, "是否模糊en和eng=%d\n", MHPY_C[1].bMode);
    fprintf (fp, "是否模糊ian和iang=%d\n", MHPY_C[2].bMode);
    fprintf (fp, "是否模糊in和ing=%d\n", MHPY_C[3].bMode);
    fprintf (fp, "是否模糊ou和u=%d\n", MHPY_C[4].bMode);
    fprintf (fp, "是否模糊uan和uang=%d\n", MHPY_C[5].bMode);
    fprintf (fp, "是否模糊c和ch=%d\n", MHPY_S[0].bMode);
    fprintf (fp, "是否模糊f和h=%d\n", MHPY_S[1].bMode);
    fprintf (fp, "是否模糊l和n=%d\n", MHPY_S[2].bMode);
    fprintf (fp, "是否模糊s和sh=%d\n", MHPY_S[3].bMode);
    fprintf (fp, "是否模糊z和zh=%d\n", MHPY_S[4].bMode);

    fclose (fp);
}

void LoadProfile (void)
{
    FILE           *fp;
    char            str[PATH_MAX], *pstr;
    char            strPath[PATH_MAX];
    int             i;
    Bool            bRetVal;


    bRetVal = False;
    strcpy (strPath, (char *) getenv ("HOME"));
    strcat (strPath, "/.fcim/profile");

    fp = fopen (strPath, "rt");

    if (fp) {
	for (;;) {
	    if (!fgets (str, PATH_MAX, fp))
		break;

	    i = strlen (str) - 1;
	    while (str[i] == ' ' || str[i] == '\n')
		str[i--] = '\0';

	    pstr = str;

	    if (strstr (str, "版本=")) {
		pstr += 5;

		if (!strcasecmp (FCITX_VERSION, pstr))
		    bRetVal = True;
	    }
	    else if (strstr (str, "是否全角=")) {
		pstr += 9;
		bCorner = atoi (pstr);
	    }
	    else if (strstr (str, "是否中文标点=")) {
		pstr += 13;
		bChnPunc = atoi (pstr);
	    }
	    else if (strstr (str, "是否GBK=")) {
		pstr += 8;
		bUseGBK = atoi (pstr);
	    }
	    else if (strstr (str, "是否联想=")) {
		pstr += 9;
		bUseLegend = atoi (pstr);
	    }
	    else if (strstr (str, "当前输入法=")) {
		pstr += 11;
		iIMIndex = atoi (pstr);
	    }
	    else if (strstr (str, "禁止用键盘切换=")) {
		pstr += 15;
		bLocked = atoi (pstr);
	    }
	}

	fclose (fp);
    }

    if (!bRetVal) {
	SaveConfig ();
	SaveProfile ();
    }
}

void SaveProfile (void)
{
    FILE           *fp;
    char            strPath[PATH_MAX];

    strcpy (strPath, (char *) getenv ("HOME"));
    strcat (strPath, "/.fcim/");

    if (access (strPath, 0))
	mkdir (strPath, S_IRWXU);

    strcat (strPath, "profile");
    fp = fopen (strPath, "wt");

    if (!fp) {
	fprintf (stderr, "\n无法创建文件 profile!\n");
	return;
    }

    fprintf (fp, "版本=%s\n", FCITX_VERSION);
    fprintf (fp, "是否全角=%d\n", bCorner);
    fprintf (fp, "是否中文标点=%d\n", bChnPunc);
    fprintf (fp, "是否GBK=%d\n", bUseGBK);
    fprintf (fp, "是否联想=%d\n", bUseLegend);
    fprintf (fp, "当前输入法=%d\n", iIMIndex);
    fprintf (fp, "禁止用键盘切换=%d\n", bLocked);

    

    fclose (fp);
}

void SetHotKey (char *strKeys, KeyEvent * hotkey)
{
  if (hotkey[1].empty())
    hotkey[1] = KeyEvent(strKeys);
  else {
    hotkey[0]=hotkey[1];
    hotkey[1]=KeyEvent(strKeys);
  }
}


/*
 * 计算文件中有多少行
 * 注意:文件中的空行也做为一行处理
 */
int CalculateRecordNumber (FILE * fpDict)
{
    char            strText[101];
    int             nNumber = 0;

    for (;;) {
	if (!fgets (strText, 100, fpDict))
	    break;

	nNumber++;
    }
    rewind (fpDict);

    return nNumber;
}

void SetSwitchKey (char *str)
{
    switchKeyPress = KeyEvent(str);
    char *buff = (char*)malloc(sizeof(char)*(strlen(str)+10));
    if(strstr(str, "Control"))
	sprintf(buff, "Control+%s", str);
    else
	sprintf(buff, "Shift+%s", str);
    switchKey = KeyEvent(buff);
    free(buff);
}


Bool CheckHZCharset (char *strHZ)
{
    if (!bUseGBK) {
	//GB2312的汉字编码规则为：第一个字节的值在0xA1到0xFE之间(实际为0xF7)，第二个字节的值在0xA1到0xFE之间
	//由于查到的资料说法不一，懒得核实，就这样吧
	int             i;

	for (i = 0; i < strlen (strHZ); i++) {
	    if ((unsigned char) strHZ[i] < (unsigned char) 0xA1 || (unsigned char) strHZ[i] > (unsigned char) 0xF7 || (unsigned char) strHZ[i + 1] < (unsigned char) 0xA1 || (unsigned char) strHZ[i + 1] > (unsigned char) 0xFE)
		return False;
	    i++;
	}
    }

    return True;
}
