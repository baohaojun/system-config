/*
 * wv1-glue.c : A "C" wrapper for using wv1 (library to parse
 * Microsoft Word documents).
 *
 * Copyright (C) 2004 Novell, Inc.
 *
 * Author: Veerapuram Varadhan <vvaradhan@novell.com>
 * [Basic framework of this file is taken from wvRTF.c of wv-1.0]
 *
 */

/*
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <wv.h>
#include <string.h>

/* Number of structural-break'ed text-chunks to hold
 * in the text/hot pools, before sending them for
 * indexing.  Increasing the number will give good
 * performance w.r.t for indexing, however, may take
 * large-chunk-of-memory to hold that much data and
 * depends on the length of each structurally-broken
 * lines.
 */
#define BUFFERED_STRUCT_BREAK 12


/* Callback to Handle "text" (or words) extracted out of 
 * M$ Word documents 
 *
 * text: Holds the extracted text/words.
 *
 * hotText: Identifies the attributes of the text.
 *          (bold, italic, underline, superscript, subscript)
 */

typedef void (* wvTextHandlerCallback) (U8* text, int len, 
					U8* hotText, int hotLen, 
					U8 needStructBrk);

typedef struct _UserData {
  /* formatting variables */

  int cFontSize;
  int cCol;

  /* boolean formats */
  int bIsBold:1;
  int bIsItalic:1;
  int bIsStrike:1;
  int bIsUl:1;
  int bIsSup:1;
  int bIsSub:1;
  int bIsSplStyle:1;
  int bIgnore:1;

  /* beagle specifc formats */
  U8 bIsHot;

  /* beagle specifc formats - for partially formatted
   *  texts.
   */
  U8 bWasHot:1;

  /*
    specifies end of para, used to send data to managed code
  */
  int bParaEnd:1;

  /* buffer to hold text */
  GString* txtWord;
  
  /* buffer to hold hot-pool-text */
  GString* txtHotPool;
  
  /* buffer to hold normal-pool-text */
  GString* txtPool;
  
  /* hold number of "structural breaks" encountered
   * since last-update-to-filter.
   */
  short structBrkCount;

  wvTextHandlerCallback WordHandler;
  
} UserData;


/*
 * append_char: fills the txtWord buffer with the character 'ch'
 * converted to UTF8 encoding.  Calls the "WordHandler" for every
 * word/line/end of a paragraph or for every 1023 characters,
 * whichever comes first.
 *
 * ud : carries the UserData filled-in appropriately to hold the 
 *      character (text) attributes.
 * 
 * ch : unicode character
 *
 */

void
append_char (UserData * ud, U16 ch)
{
  gchar tmpBuf[64];
  int len = 0;
  U8 bNeedStructBrk = 0;

  if (ud->bIgnore)
    return;

  switch (ch) {
  case 0x00: /* End of Document */
    bNeedStructBrk = 1;
    break;

  case 0x0B: /* hard line break */
  case 0x0D: /* paragraph end */
  case 0x0C:
  case '\n': /* new-line */
    bNeedStructBrk = 1;
    ch = 0x0A;
    break;

  case 0x20: /* space */
    g_string_append_c (ud->txtWord, ch);
    break;

  default: 
    len =  g_unichar_to_utf8 (ch, tmpBuf);
    /*  FIXME: This is not good, pretty hacky code
     *  to get rid of unwanted characters, especially
     *  some graphic symbols used in a document. 
     *  Ex: a tick mark, a smiley blah blah blah...
     *  in a much sane way without blocking 
     *  printable-non-iso characters ;)
     */
    /*
      int i;
      for (i = 0; i < len; i++)
      if (tmpBuf[i] > 0)
      g_string_append_c (ud->txtWord, tmpBuf[i]);
    */
    g_string_append_len (ud->txtWord, tmpBuf, len);
    break;
  }

  if (ch == 0x00 || ch == 0x20 || ch == 0x0A) {
    if (ud->bWasHot)
      g_string_append_len (ud->txtHotPool, ud->txtWord->str, ud->txtWord->len);

    /*    
    printf ("TxtWord: %s, len: %d\n", ud->txtWord->str, ud->txtWord->len);
    printf ("TxtPool: %s, len: %d\n", ud->txtPool->str, ud->txtPool->len);       
    printf ("HotTxtPool: %s, len: %d\n", ud->txtHotPool->str, ud->txtHotPool->len);
    */

    g_string_append_len (ud->txtPool, ud->txtWord->str, ud->txtWord->len);
    if (bNeedStructBrk) {
      g_string_append_c (ud->txtPool, '\n');
      g_string_append_c (ud->txtHotPool, ' ');
      ud->structBrkCount++;
    }

    if (ud->structBrkCount >= BUFFERED_STRUCT_BREAK ||
	ud->bParaEnd) {
      (*(ud->WordHandler))(ud->txtPool->str, ud->txtPool->len, 
			   ud->txtHotPool->str, ud->txtHotPool->len, bNeedStructBrk);
      /* 
	 g_string_erase () can be used here to erase
	 the previous content, however, using this 
	 call will free the "erased-content-memory"
	 and thereby causing memory fragmentation for
	 every time we transfer data from unmanaged
	 to managed code.  Setting "len" to 0 results
	 in the same way g_string_erase () does, but
	 doesn't do memory-[de/re]allocation
      */

      /*
	ch == 0x00 refers to EOD.  Do not reset len to
	zero, we have to free the gstrings.
       */
      if (ch != 0x00) {
	ud->txtPool->len = 0;
	ud->txtHotPool->len = 0;
	ud->structBrkCount = 0;
      }
    }
    if (ch != 0x00)
      ud->txtWord->len = 0;
    ud->bWasHot = 0;
  }  
}

/*
 * fill_UserData: fills the UserData structure from the 
 * CHP structure that represents the Character Property
 * Information like bold, italic, striked, underlined, 
 * superscript, subscript, fontsize, color, fontface etc.
 *
 */
void
fill_UserData (UserData * ud, CHP * chp, wvParseStruct * ps)
{
  if (!chp || !ud)
    return;

  ud->cCol = 0;
  if (chp->ico)
    ud->cCol = chp->ico - 1;    

  ud->cFontSize = chp->hps;
  ud->bIsBold = (chp->fBold);
  ud->bIsItalic = (chp->fItalic);
  ud->bIsUl = (chp->kul);
  ud->bIsStrike = (chp->fStrike);
  ud->bIsSup = (chp->iss == 1);
  ud->bIsSub = (chp->iss == 2);

  if ((ud->bIsBold 
       || ud->bIsItalic 
       || ud->bIsUl 
       || ud->bIsSup 
       || ud->bIsSub
       || ud->bIsSplStyle) &&
      (!ud->bIgnore))
      ud->bIsHot = 1;
  else
    ud->bIsHot = 0;
}

/* This is a callback that handles the individual 
 * character that are extracted from M$ word file.
 */
static int
charProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid)
{
  /* convert incoming character to unicode */
  if (chartype) {
    eachchar = wvHandleCodePage (eachchar, lid);
  }

  /* take care of any oddities in Microsoft's character "encoding" */
  /* TODO: does the above code page handler take care of these? */
  if (chartype == 1 && eachchar == 146)
    eachchar = 39;		/* apostrophe */

  switch (eachchar)
    {
    case 14:			/* column break */
      break;

    case 19:			/* field begin */
      /* flush current text buffer */
      ps->fieldstate++;
      ps->fieldmiddle = 0;
      return 0;
    case 20:			/* field separator */
      ps->fieldmiddle = 1;
      return 0;
    case 21:			/* field end */
      ps->fieldstate--;
      ps->fieldmiddle = 0;
      return 0;
    case 7:                     /* Cell/Row mark, end of a cell/row*/
      eachchar = 0x20;
      break;
    default:
      break;
    }

  if (eachchar == 0x14)
    return 0;

  /* To handle partially-formatted-texts, Bug#157100,
   * which is applicable to all word-processor-generated
   * documents.
   * 
   * ud->bIsHot is updated for every CHARPROPBEGIN element
   * ud->bWasHot is updated on reading every *word*.
   */
  UserData *ud = (UserData *) ps->userData;
  if (!ud->bWasHot)
    ud->bWasHot = ud->bIsHot;

  append_char (ps->userData, eachchar);
  return 0;
}

/* This is a callback that handles the special 
 * character that are specific to M$ word file.
 */
static int
specCharProc (wvParseStruct * ps, U16 eachchar, CHP * achp)
{
  Blip blip;
  wvStream *fil;
  long pos;
  FSPA *fspa;
  PICF picf;
  FDOA *fdoa;

  switch (eachchar)
    {
    case 19:			/* field begin */
      ps->fieldstate++;
      ps->fieldmiddle = 0;
      return 0;
    case 20:			/* field separator */
      if (achp->fOle2)
	{
/* 	  printf ("Field has an embedded OLE2 object\n"); */
	}
      ps->fieldmiddle = 1;
      return 0;
    case 21:			/* field end */
      ps->fieldstate--;
      ps->fieldmiddle = 0;
      return 0;
    case 7:                     /* Cell/Row mark, end of a cell/row */
      append_char (ps->userData, 0x20);
      break;
    default:
      break;
    }

  if (ps->fieldstate)
    {
      if (eachchar == 0x13 || eachchar == 0x14)
	return 0;
    }

  return 0;
}

/* This is a callback that handles the individual 
 * elements that are marked by libwv1.
 */

static int
eleProc (wvParseStruct * ps, wvTag tag, void *props, int dirty)
{
  /* some word structures */
  PAP *apap;
  CHP *achp;
  SEP *asep;
  int iRes;

  UserData *ud = (UserData *) ps->userData;

  switch (tag)
    {
    case PARABEGIN:
      apap = (PAP *)props;
      switch (ps->stsh.std[apap->istd].sti) {
      case 29:    /* Footnote Text   */
      case 30:    /* Annotation text */
      case 31:    /* Header          */
      case 32:    /* Footer          */
      case 33:    /* Index Heading   */
      case 34:    /* Caption         */
      case 43:    /* Endnote Text    */
      case 62:    /* Title           */
      case 74:    /* Sub title       */
	ud->bIsSplStyle = 1;
	break;
      default:
	ud->bIsSplStyle = 0;
	break;
      }
      ud->bParaEnd = 0;
      break;

    case SECTIONEND:
      append_char (ud, '\n');
      break;

    case PARAEND:		/* pretty much nothing */
      ud->bIsSplStyle = 0;
      ud->bParaEnd = 1;
      append_char (ud, '\n');
      break;

    case CHARPROPBEGIN:
      achp = (CHP *) props;
      /*      switch (ps->stsh.std[achp->istd].sti) {
      case 38:
      case 39:
      case 40:
      case 41:
      case 42:
	ud->bIgnore = 1;
	break;
      default:
	ud->bIgnore = 0;
	break;
      }
      */
      fill_UserData (ud, achp, ps);
      break;


      /* Do not call fill_UserData, as it resets the 
       * *Hot* flag in the ud structure.
       */
    case CHARPROPEND:
      achp = (CHP *) props;
      /*fill_UserData (ud, achp, ps);*/
      break;

    default:
      break;
    }

  return 0;
}

/* This is a callback that handles the document 
 * level tags that are marked by libwv1.
 */

static int
docProc (wvParseStruct * ps, wvTag tag)
{
  UserData *ud = (UserData *) ps->userData;

  switch (tag)
    {
    case DOCEND:
      /* flush the text/hot pools at the EOD */
      ud->structBrkCount = BUFFERED_STRUCT_BREAK;
      append_char (ps->userData, 0x00);
      
      break;

    default:
      break;
    }

  return 0;
}

/*
 * wv1_glue_init_doc_parsing: Initiates the document parsing 
 * procedure.  Sets up all the required handlers and the parser.
 * 
 * fname: Name of the file to parse. (essentially a M$ word file)
 *
 * wvTextHandlerCallback: The callback routine that will be called 
 * on extraction of each word.
 *
 * Return: 0 -> success
 *        -1 -> failure.
 */

int
wv1_glue_init_doc_parsing (char* fname, wvTextHandlerCallback callback)
{
  FILE *input;
  int ret = 0;

  wvParseStruct ps;
  char *dir = NULL;

  UserData ud;

  input = fopen (fname, "rb");
  if (!input)
      return -1;
  fclose (input);

  ret = wvInitParser (&ps, fname);
  if (ret & 0x8000)
    ret = -2;
  else if (ret) 
    ret = -3;

  if (ret) {
    wvOLEFree (&ps);
    return ret;
  }

  ps.filename = fname;
  ps.dir = dir;

  /* set to 0 */
  memset (&ud, 0, sizeof (UserData));
  ud.WordHandler = callback;
  ud.txtWord = g_string_sized_new (32);
  ud.txtHotPool = g_string_sized_new (1024);
  ud.txtPool = g_string_sized_new (1024);
  ps.userData = &ud;

  wvSetElementHandler (&ps, eleProc);
  wvSetDocumentHandler (&ps, docProc);
  wvSetCharHandler (&ps, charProc);
  wvSetSpecialCharHandler (&ps, specCharProc);

  wvText (&ps);
  
  /* free userdata memory */
  g_string_free (ud.txtWord, TRUE);

  /* free text pool memory */
  g_string_free (ud.txtPool, TRUE);

  /* free hot text pool memory */
  g_string_free (ud.txtHotPool, TRUE);

  /* free associated memory */
  wvOLEFree (&ps);

  ud.txtPool = NULL;
  ud.txtWord = NULL;
  ud.txtHotPool = NULL;

  return 0;
}

/*
 * wv1_init (): Initialize the wv1 library
 * NOTE: Do not call this more than once for an application.
 */

int
wv1_init ()
{
  return (wvInit());
}

