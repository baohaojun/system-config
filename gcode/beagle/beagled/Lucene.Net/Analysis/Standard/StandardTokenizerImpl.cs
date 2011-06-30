using System;
using Token = Lucene.Net.Analysis.Token;
namespace Lucene.Net.Analysis.Standard
{
	class StandardTokenizerImpl 
	{

		/** This character denotes the end of file */
		public static readonly int YYEOF = -1;

		/** initial size of the lookahead buffer */
		private static readonly int ZZ_BUFFERSIZE = 16384;

		/** lexical states */
		public static readonly int YYINITIAL = 0;

		/**
		 * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
		 * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
		 *                  at the beginning of a line
		 * l is of the form l = 2*k, k a non negative integer
		 */
		private static readonly int [] ZZ_LEXSTATE = 
			{ 
				0, 0
			};

		/** 
		 * Translates characters to character classes
		 */
		private const String ZZ_CMAP_PACKED = 
			"\x0009\x0000\x0001\x0000\x0001\x0004\x0001\x0000\x0001\x0000\x0001\x0003\x0012\x0000\x0001\x0000" + 
			"\x000f\x0000\x000a\x0001\x0007\x0000\x001a\x0001\x0006\x0000\x001a\x0001\x002f\x0000\x0001\x0001" + 
			"\x000a\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0005\x0000\x0017\x0001\x0001\x0000\x001f\x0001" + 
			"\x0001\x0000\x013f\x0001\x0019\x0000\x0072\x0001\x0004\x0000\x000c\x0001\x000e\x0000\x0005\x0001" + 
			"\x0009\x0000\x0001\x0001\x008b\x0000\x0001\x0001\x000b\x0000\x0001\x0001\x0001\x0000\x0003\x0001" + 
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0014\x0001\x0001\x0000\x002c\x0001\x0001\x0000\x0026\x0001" + 
			"\x0001\x0000\x0005\x0001\x0004\x0000\x0082\x0001\x0008\x0000\x0045\x0001\x0001\x0000\x0026\x0001" + 
			"\x0002\x0000\x0002\x0001\x0006\x0000\x0010\x0001\x0021\x0000\x0026\x0001\x0002\x0000\x0001\x0001" + 
			"\x0007\x0000\x0027\x0001\x0048\x0000\x001b\x0001\x0005\x0000\x0003\x0001\x002e\x0000\x001a\x0001" + 
			"\x0005\x0000\x000b\x0001\x0015\x0000\x000a\x0001\x0004\x0000\x0002\x0001\x0001\x0000\x0063\x0001" + 
			"\x0001\x0000\x0001\x0001\x000f\x0000\x0002\x0001\x0007\x0000\x0002\x0001\x000a\x0001\x0003\x0001" + 
			"\x0002\x0000\x0001\x0001\x0010\x0000\x0001\x0001\x0001\x0000\x001e\x0001\x001d\x0000\x0003\x0001" + 
			"\x0030\x0000\x0026\x0001\x000b\x0000\x0001\x0001\x0152\x0000\x0036\x0001\x0003\x0000\x0001\x0001" + 
			"\x0012\x0000\x0001\x0001\x0007\x0000\x000a\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001" + 
			"\x0002\x0000\x0002\x0001\x0002\x0000\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0001\x0001" + 
			"\x0003\x0000\x0004\x0001\x0003\x0000\x0001\x0001\x001e\x0000\x0002\x0001\x0001\x0000\x0003\x0001" + 
			"\x0004\x0000\x000a\x0001\x0002\x0001\x0013\x0000\x0006\x0001\x0004\x0000\x0002\x0001\x0002\x0000" + 
			"\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0002\x0001\x0001\x0000" + 
			"\x0002\x0001\x001f\x0000\x0004\x0001\x0001\x0000\x0001\x0001\x0007\x0000\x000a\x0001\x0002\x0000" + 
			"\x0003\x0001\x0010\x0000\x0009\x0001\x0001\x0000\x0003\x0001\x0001\x0000\x0016\x0001\x0001\x0000" + 
			"\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0005\x0001\x0003\x0000\x0001\x0001\x0012\x0000" + 
			"\x0001\x0001\x000f\x0000\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001\x0002\x0000" + 
			"\x0002\x0001\x0002\x0000\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000" + 
			"\x0005\x0001\x0003\x0000\x0001\x0001\x001e\x0000\x0002\x0001\x0001\x0000\x0003\x0001\x0004\x0000" + 
			"\x000a\x0001\x0001\x0000\x0001\x0001\x0011\x0000\x0001\x0001\x0001\x0000\x0006\x0001\x0003\x0000" + 
			"\x0003\x0001\x0001\x0000\x0004\x0001\x0003\x0000\x0002\x0001\x0001\x0000\x0001\x0001\x0001\x0000" + 
			"\x0002\x0001\x0003\x0000\x0002\x0001\x0003\x0000\x0003\x0001\x0003\x0000\x0008\x0001\x0001\x0000" + 
			"\x0003\x0001\x002d\x0000\x0009\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000" + 
			"\x0017\x0001\x0001\x0000\x000a\x0001\x0001\x0000\x0005\x0001\x0026\x0000\x0002\x0001\x0004\x0000" + 
			"\x000a\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000\x0017\x0001\x0001\x0000" + 
			"\x000a\x0001\x0001\x0000\x0005\x0001\x0003\x0000\x0001\x0001\x0020\x0000\x0001\x0001\x0001\x0000" + 
			"\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000" + 
			"\x0017\x0001\x0001\x0000\x0010\x0001\x0026\x0000\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000" + 
			"\x0012\x0001\x0003\x0000\x0018\x0001\x0001\x0000\x0009\x0001\x0001\x0000\x0001\x0001\x0002\x0000" + 
			"\x0007\x0001\x0039\x0000\x0001\x0001\x0030\x0001\x0001\x0001\x0002\x0001\x000c\x0001\x0007\x0001" + 
			"\x0009\x0001\x000a\x0001\x0027\x0000\x0002\x0001\x0001\x0000\x0001\x0001\x0002\x0000\x0002\x0001" + 
			"\x0001\x0000\x0001\x0001\x0002\x0000\x0001\x0001\x0006\x0000\x0004\x0001\x0001\x0000\x0007\x0001" + 
			"\x0001\x0000\x0003\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0001\x0001\x0002\x0000\x0002\x0001" + 
			"\x0001\x0000\x0004\x0001\x0001\x0000\x0002\x0001\x0009\x0000\x0001\x0001\x0002\x0000\x0005\x0001" + 
			"\x0001\x0000\x0001\x0001\x0009\x0000\x000a\x0001\x0002\x0000\x0002\x0001\x0022\x0000\x0001\x0001" + 
			"\x001f\x0000\x000a\x0001\x0016\x0000\x0008\x0001\x0001\x0000\x0022\x0001\x001d\x0000\x0004\x0001" + 
			"\x0074\x0000\x0022\x0001\x0001\x0000\x0005\x0001\x0001\x0000\x0002\x0001\x0015\x0000\x000a\x0001" + 
			"\x0006\x0000\x0006\x0001\x004a\x0000\x0026\x0001\x000a\x0000\x0029\x0001\x0007\x0000\x005a\x0001" + 
			"\x0005\x0000\x0044\x0001\x0005\x0000\x0052\x0001\x0006\x0000\x0007\x0001\x0001\x0000\x003f\x0001" + 
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001\x0001\x0000\x0001\x0001" + 
			"\x0001\x0000\x0004\x0001\x0002\x0000\x0027\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001" + 
			"\x0002\x0000\x001f\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001" + 
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001\x0001\x0000\x0007\x0001" + 
			"\x0001\x0000\x0017\x0001\x0001\x0000\x001f\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001" + 
			"\x0002\x0000\x0007\x0001\x0001\x0000\x0027\x0001\x0001\x0000\x0013\x0001\x000e\x0000\x0009\x0001" + 
			"\x002e\x0000\x0055\x0001\x000c\x0000\x026c\x0001\x0002\x0000\x0008\x0001\x000a\x0000\x001a\x0001" + 
			"\x0005\x0000\x004b\x0001\x0015\x0000\x000d\x0001\x0001\x0000\x0004\x0001\x000e\x0000\x0012\x0001" + 
			"\x000e\x0000\x0012\x0001\x000e\x0000\x000d\x0001\x0001\x0000\x0003\x0001\x000f\x0000\x0034\x0001" + 
			"\x0023\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0003\x0000\x000a\x0001\x0026\x0000\x000a\x0001" + 
			"\x0006\x0000\x0058\x0001\x0008\x0000\x0029\x0001\x0057\x0000\x001d\x0001\x0029\x0000\x000a\x0001" + 
			"\x001e\x0001\x0002\x0000\x0005\x0001\x038b\x0000\x006c\x0001\x0094\x0000\x009c\x0001\x0004\x0000" + 
			"\x005a\x0001\x0006\x0000\x0016\x0001\x0002\x0000\x0006\x0001\x0002\x0000\x0026\x0001\x0002\x0000" + 
			"\x0006\x0001\x0002\x0000\x0008\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0001\x0001\x0001\x0000" + 
			"\x0001\x0001\x0001\x0000\x001f\x0001\x0002\x0000\x0035\x0001\x0001\x0000\x0007\x0001\x0001\x0000" + 
			"\x0001\x0001\x0003\x0000\x0003\x0001\x0001\x0000\x0007\x0001\x0003\x0000\x0004\x0001\x0002\x0000" + 
			"\x0006\x0001\x0004\x0000\x000d\x0001\x0005\x0000\x0003\x0001\x0001\x0000\x0007\x0001\x0074\x0000" + 
			"\x0001\x0001\x000d\x0000\x0001\x0001\x0082\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0002\x0000" + 
			"\x000a\x0001\x0001\x0000\x0001\x0001\x0003\x0000\x0005\x0001\x0006\x0000\x0001\x0001\x0001\x0000" + 
			"\x0001\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0001\x0000\x0003\x0001\x0001\x0000" + 
			"\x0007\x0001\x0003\x0000\x0003\x0001\x0005\x0000\x0005\x0001\x0ebb\x0000\x0002\x0001\x002a\x0000" + 
			"\x0005\x0001\x0005\x0000\x0002\x0001\x0003\x0000\x0001\x0002\x0056\x0002\x0006\x0002\x0003\x0002" + 
			"\x0001\x0002\x005a\x0002\x0001\x0002\x0004\x0002\x0005\x0002\x0028\x0002\x0003\x0002\x0001\x0000" + 
			"\x005e\x0001\x0011\x0000\x0018\x0001\x0038\x0000\x0010\x0002\x0100\x0000\x0080\x0002\x0080\x0000" + 
			"\x19b6\x0002\x000a\x0002\x0040\x0000\x51a6\x0002\x005a\x0002\x048d\x0001\x0773\x0000\x2ba4\x0001" + 
			"\x215c\x0000\x012e\x0002\x0002\x0002\x003b\x0002\x0095\x0002\x0007\x0001\x000c\x0000\x0005\x0001" + 
			"\x0005\x0000\x0001\x0001\x0001\x0000\x000a\x0001\x0001\x0000\x000d\x0001\x0001\x0000\x0005\x0001" + 
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x006c\x0001" + 
			"\x0021\x0000\x016b\x0001\x0012\x0000\x0040\x0001\x0002\x0000\x0036\x0001\x0028\x0000\x000c\x0001" + 
			"\x0074\x0000\x0005\x0001\x0001\x0000\x0087\x0001\x0013\x0000\x000a\x0001\x0007\x0000\x001a\x0001" + 
			"\x0006\x0000\x001a\x0001\x000a\x0000\x0001\x0002\x003a\x0002\x001f\x0001\x0003\x0000\x0006\x0001" + 
			"\x0002\x0000\x0006\x0001\x0002\x0000\x0006\x0001\x0002\x0000\x0003\x0001\x0023\x0000";

		/** 
		 * Translates characters to character classes
		 */
		private static readonly char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

		/** 
		 * Translates DFA states to action switch labels.
		 */
		private static readonly int [] ZZ_ACTION = zzUnpackAction();

		private const String ZZ_ACTION_PACKED_0 =
			"\x0001\x0000\x0001\x0001\x0001\x0002\x0001\x0003\x0001\x0001";

		private static int [] zzUnpackAction() 
		{
			int [] result = new int[5];
			int offset = 0;
			offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackAction(String packed, int offset, int [] result) 
		{
			int i = 0;       /* index in packed string  */
			int j = offset;  /* index in unpacked array */
			int l = packed.Length;
			while (i < l) 
				{
					int count = packed.charAt(i++);
					int value = packed.charAt(i++);
					do result[j++] = value; while (--count > 0);
				}
			return j;
		}


		/** 
		 * Translates a state to a row index in the transition table
		 */
		private static readonly int [] ZZ_ROWMAP = zzUnpackRowMap();

		private const String ZZ_ROWMAP_PACKED_0 =
			"\x0000\x0000\x0000\x0005\x0000\x000a\x0000\x0005\x0000\x000f";

		private static int [] zzUnpackRowMap() 
		{
			int [] result = new int[5];
			int offset = 0;
			offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackRowMap(String packed, int offset, int [] result) 
		{
			int i = 0;  /* index in packed string  */
			int j = offset;  /* index in unpacked array */
			int l = packed.Length;
			while (i < l) 
				{
					int high = packed.charAt(i++) << 16;
					result[j++] = high | packed.charAt(i++);
				}
			return j;
		}

		/** 
		 * The transition table of the DFA
		 */
		private static readonly int [] ZZ_TRANS = zzUnpackTrans();

		private const String ZZ_TRANS_PACKED_0 =
			"\x0001\x0002\x0001\x0003\x0001\x0004\x0001\x0005\x0001\x0002\x0006\x0000\x0001\x0003\x0007\x0000" + 
			"\x0001\x0002";

		private static int [] zzUnpackTrans() 
		{
			int [] result = new int[20];
			int offset = 0;
			offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackTrans(String packed, int offset, int [] result) 
		{
			int i = 0;       /* index in packed string  */
			int j = offset;  /* index in unpacked array */
			int l = packed.Length;
			while (i < l) 
				{
					int count = packed.charAt(i++);
					int value = packed.charAt(i++);
					value--;
					do result[j++] = value; while (--count > 0);
				}
			return j;
		}


		/* error codes */
		private static readonly int ZZ_UNKNOWN_ERROR = 0;
		private static readonly int ZZ_NO_MATCH = 1;
		private static readonly int ZZ_PUSHBACK_2BIG = 2;

		/* error messages for the codes above */
		private static readonly String [] ZZ_ERROR_MSG = 
			{
				"Unkown internal scanner error",
				"Error: could not match input",
				"Error: pushback value was too large"
			};

		/**
		 * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
		 */
		private static readonly int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

		private const String ZZ_ATTRIBUTE_PACKED_0 =
			"\x0001\x0000\x0001\x0009\x0001\x0001\x0001\x0009\x0001\x0001";

		private static int [] zzUnpackAttribute() 
		{
			int [] result = new int[5];
			int offset = 0;
			offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackAttribute(String packed, int offset, int [] result) 
		{
			int i = 0;       /* index in packed string  */
			int j = offset;  /* index in unpacked array */
			int l = packed.Length;
			while (i < l) 
				{
					int count = packed.charAt(i++);
					int value = packed.charAt(i++);
					do result[j++] = value; while (--count > 0);
				}
			return j;
		}

		/** the input device */
		private java.io.Reader zzReader;

		/** the current state of the DFA */
		private int zzState;

		/** the current lexical state */
		private int zzLexicalState = YYINITIAL;

		/** this buffer contains the current text to be matched and is
		    the source of the yytext() string */
		private char [] zzBuffer = new char[ZZ_BUFFERSIZE];

		/** the textposition at the last accepting state */
		private int zzMarkedPos;

		/** the current text position in the buffer */
		private int zzCurrentPos;

		/** startRead marks the beginning of the yytext() string in the buffer */
		private int zzStartRead;

		/** endRead marks the last character in the buffer, that has been read
		    from input */
		private int zzEndRead;

		/** number of newlines encountered up to the start of the matched text */
		private int yyline;

		/** the number of characters up to the start of the matched text */
		private int yychar_field;

		/**
		 * the number of characters from the last newline up to the start of the 
		 * matched text
		 */
		private int yycolumn;

		/** 
		 * zzAtBOL == true <=> the scanner is currently at the beginning of a line
		 */
		private boolean zzAtBOL = true;

		/** zzAtEOF == true <=> the scanner is at the EOF */
		private boolean zzAtEOF;

		/** denotes if the user-EOF-code has already been executed */
		private boolean zzEOFDone;

		/* user code: */

		public static readonly int ALPHANUM          = StandardTokenizer.ALPHANUM;
		public static readonly int APOSTROPHE        = StandardTokenizer.APOSTROPHE;
		public static readonly int ACRONYM           = StandardTokenizer.ACRONYM;
		public static readonly int COMPANY           = StandardTokenizer.COMPANY;
		public static readonly int EMAIL             = StandardTokenizer.EMAIL;
		public static readonly int HOST              = StandardTokenizer.HOST;
		public static readonly int NUM               = StandardTokenizer.NUM;
		public static readonly int CJ                = StandardTokenizer.CJ;
		/**
		 * @deprecated this solves a bug where HOSTs that end with '.' are identified
		 *             as ACRONYMs. It is deprecated and will be removed in the next
		 *             release.
		 */
		public static readonly int ACRONYM_DEP       = StandardTokenizer.ACRONYM_DEP;

		public static readonly String [] TOKEN_TYPES = StandardTokenizer.TOKEN_TYPES;

		public readonly int yychar()
	
		{
			return yychar_field;
		}

		/**
		 * Fills Lucene token with the current token text.
		 */
		readonly void getText(Token t) 
		{
			t.setTermBuffer(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
		}

		/**
		 * Fills TermAttribute with the current token text.
		 */
		readonly void getText(TermAttribute t) 
		{
			t.setTermBuffer(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
		}



		/**
		 * Creates a new scanner
		 * There is also a java.io.InputStream version of this constructor.
		 *
		 * @param   in  the java.io.Reader to read input from.
		 */
		StandardTokenizerImpl(java.io.Reader in) 
		{
			this.zzReader = in;
		}

		/**
		 * Creates a new scanner.
		 * There is also java.io.Reader version of this constructor.
		 *
		 * @param   in  the java.io.Inputstream to read input from.
		 */
		StandardTokenizerImpl(java.io.InputStream in) 
		{
			this(new java.io.InputStreamReader(in));
		}

		/** 
		 * Unpacks the compressed character translation table.
		 *
		 * @param packed   the packed character translation table
		 * @return         the unpacked character translation table
		 */
		private static char [] zzUnpackCMap(String packed) 
		{
			char [] map = new char[0x10000];
			int i = 0;  /* index in packed string  */
			int j = 0;  /* index in unpacked array */
			while (i < 1214) 
				{
					int  count = packed.charAt(i++);
					char value = packed.charAt(i++);
					do map[j++] = value; while (--count > 0);
				}
			return map;
		}


		/**
		 * Refills the input buffer.
		 *
		 * @return      <code>false</code>, iff there was new input.
		 * 
		 * @exception   java.io.IOException  if any I/O-Error occurs
		 */
		private boolean zzRefill() throws java.io.IOException 
		{

			/* first: make room (if you can) */
			if (zzStartRead > 0) 
				{
					System.arraycopy(zzBuffer, zzStartRead,
							 zzBuffer, 0,
							 zzEndRead-zzStartRead);

					/* translate stored positions */
					zzEndRead-= zzStartRead;
					zzCurrentPos-= zzStartRead;
					zzMarkedPos-= zzStartRead;
					zzStartRead = 0;
				}

			/* is the buffer big enough? */
			if (zzCurrentPos >= zzBuffer.length) 
				{
					/* if not: blow it up */
					char [] newBuffer = new char[zzCurrentPos*2];
					System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
					zzBuffer = newBuffer;
				}

			/* readonlyly: fill the buffer with new input */
			int numRead = zzReader.read(zzBuffer, zzEndRead,
						    zzBuffer.length-zzEndRead);

			if (numRead > 0) 
				{
					zzEndRead+= numRead;
					return false;
				}
			// unlikely but not impossible: read 0 characters, but not at end of stream    
			if (numRead == 0) 
				{
					int c = zzReader.read();
					if (c == -1) 
						{
							return true;
						} else 
						{
							zzBuffer[zzEndRead++] = (char) c;
							return false;
						}     
				}

			// numRead < 0
			return true;
		}

    
		/**
		 * Closes the input stream.
		 */
		public readonly void yyclose() throws java.io.IOException 
		{
			zzAtEOF = true;            /* indicate end of file */
			zzEndRead = zzStartRead;  /* invalidate buffer    */

			if (zzReader != null)
				zzReader.close();
		}


		/**
		 * Resets the scanner to read from a new input stream.
		 * Does not close the old reader.
		 *
		 * All internal variables are reset, the old input stream 
		 * <b>cannot</b> be reused (internal buffer is discarded and lost).
		 * Lexical state is set to <tt>ZZ_INITIAL</tt>.
		 *
		 * @param reader   the new input stream 
		 */
		public readonly void yyreset(java.io.Reader reader) 
		{
			zzReader = reader;
			zzAtBOL  = true;
			zzAtEOF  = false;
			zzEOFDone = false;
			zzEndRead = zzStartRead = 0;
			zzCurrentPos = zzMarkedPos = 0;
			yyline = yychar_field = yycolumn = 0;
			zzLexicalState = YYINITIAL;
		}


		/**
		 * Returns the current lexical state.
		 */
		public readonly int yystate() 
		{
			return zzLexicalState;
		}


		/**
		 * Enters a new lexical state
		 *
		 * @param newState the new lexical state
		 */
		public readonly void yybegin(int newState) 
		{
			zzLexicalState = newState;
		}


		/**
		 * Returns the text matched by the current regular expression.
		 */
		public readonly String yytext() 
		{
			return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
		}


		/**
		 * Returns the character at position <tt>pos</tt> from the 
		 * matched text. 
		 * 
		 * It is equivalent to yytext().charAt(pos), but faster
		 *
		 * @param pos the position of the character to fetch. 
		 *            A value from 0 to yylength()-1.
		 *
		 * @return the character at position pos
		 */
		public readonly char yycharat(int pos) 
		{
			return zzBuffer[zzStartRead+pos];
		}


		/**
		 * Returns the length of the matched text region.
		 */
		public readonly int yylength() 
		{
			return zzMarkedPos-zzStartRead;
		}


		/**
		 * Reports an error that occured while scanning.
		 *
		 * In a wellformed scanner (no or only correct usage of 
		 * yypushback(int) and a match-all fallback rule) this method 
		 * will only be called with things that "Can't Possibly Happen".
		 * If this method is called, something is seriously wrong
		 * (e.g. a JFlex bug producing a faulty scanner etc.).
		 *
		 * Usual syntax/scanner level error handling should be done
		 * in error fallback rules.
		 *
		 * @param   errorCode  the code of the errormessage to display
		 */
		private void zzScanError(int errorCode) 
		{
			String message;
			try 
				{
					message = ZZ_ERROR_MSG[errorCode];
				}
			catch (ArrayIndexOutOfBoundsException e) 
				{
					message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
				}

			throw new Error(message);
		} 


		/**
		 * Pushes the specified amount of characters back into the input stream.
		 *
		 * They will be read again by then next call of the scanning method
		 *
		 * @param number  the number of characters to be read again.
		 *                This number must not be greater than yylength()!
		 */
		public void yypushback(int number)  
		{
			if ( number > yylength() )
				zzScanError(ZZ_PUSHBACK_2BIG);

			zzMarkedPos -= number;
		}


		/**
		 * Resumes scanning until the next regular expression is matched,
		 * the end of input is encountered or an I/O-Error occurs.
		 *
		 * @return      the next token
		 * @exception   java.io.IOException  if any I/O-Error occurs
		 */
		public int getNextToken() throws java.io.IOException 
		{
			int zzInput;
			int zzAction;

			// cached fields:
			int zzCurrentPosL;
			int zzMarkedPosL;
			int zzEndReadL = zzEndRead;
			char [] zzBufferL = zzBuffer;
			char [] zzCMapL = ZZ_CMAP;

			int [] zzTransL = ZZ_TRANS;
			int [] zzRowMapL = ZZ_ROWMAP;
			int [] zzAttrL = ZZ_ATTRIBUTE;

			while (true) 
				{
					zzMarkedPosL = zzMarkedPos;

					yychar_field+= zzMarkedPosL-zzStartRead;

					zzAction = -1;

					zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
					zzState = ZZ_LEXSTATE[zzLexicalState];


					zzForAction: 
					{
						while (true) 
							{
    
								if (zzCurrentPosL < zzEndReadL)
									zzInput = zzBufferL[zzCurrentPosL++];
								else if (zzAtEOF) 
									{
										zzInput = YYEOF;
										break zzForAction;
									}
								else 
									{
										// store back cached positions
										zzCurrentPos  = zzCurrentPosL;
										zzMarkedPos   = zzMarkedPosL;
										boolean eof = zzRefill();
										// get translated positions and possibly new buffer
										zzCurrentPosL  = zzCurrentPos;
										zzMarkedPosL   = zzMarkedPos;
										zzBufferL      = zzBuffer;
										zzEndReadL     = zzEndRead;
										if (eof) 
											{
												zzInput = YYEOF;
												break zzForAction;
											}
										else 
											{
												zzInput = zzBufferL[zzCurrentPosL++];
											}
									}
								int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
								if (zzNext == -1) break zzForAction;
								zzState = zzNext;

								int zzAttributes = zzAttrL[zzState];
								if ( (zzAttributes & 1) == 1 ) 
									{
										zzAction = zzState;
										zzMarkedPosL = zzCurrentPosL;
										if ( (zzAttributes & 8) == 8 ) break zzForAction;
									}

							}
					}

					// store back cached position
					zzMarkedPos = zzMarkedPosL;

					switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) 
						{
						case 3: 
				
							{ return CJ;
							}
						case 4: break;
						case 1: 
				
							{ /* ignore */
							}
						case 5: break;
						case 2: 
				
							{ return ALPHANUM;
							}
						case 6: break;
						default: 
							if (zzInput == YYEOF && zzStartRead == zzCurrentPos) 
								{
									zzAtEOF = true;
									return YYEOF;
								} 
							else 
								{
									zzScanError(ZZ_NO_MATCH);
								}
						}
				}
		}


	}
}
