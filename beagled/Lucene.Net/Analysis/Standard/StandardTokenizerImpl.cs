using System;
using Token = Lucene.Net.Analysis.Token;
namespace Lucene.Net.Analysis.Standard
{
	public class StandardTokenizerImpl 
	{
		public int GetNextToken()
		{
			int zzInput;
			int zzAction;

 
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

					yychar_Renamed_Field += zzMarkedPosL - zzStartRead;

					zzAction = - 1;

					zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
 
					zzState = zzLexicalState;


					{
						while (true) 
							{
 
								if (zzCurrentPosL < zzEndReadL)
									zzInput = zzBufferL[zzCurrentPosL++];
								else if (zzAtEOF) 
									{
										zzInput = YYEOF;
 
										goto zzForAction_brk;
									}
								else 
									{
 
										zzCurrentPos = zzCurrentPosL;
										zzMarkedPos = zzMarkedPosL;
										bool eof = zzRefill();
 
										zzCurrentPosL = zzCurrentPos;
										zzMarkedPosL = zzMarkedPos;
										zzBufferL = zzBuffer;
										zzEndReadL = zzEndRead;
										if (eof) 
											{
												zzInput = YYEOF;

												goto zzForAction_brk;
											}
										else 
											{
												zzInput = zzBufferL[zzCurrentPosL++];
											}
									}
								int zzNext = zzTransL[zzRowMapL[zzState] + zzCMapL[zzInput]];
								if (zzNext == - 1)
									{
							
										goto zzForAction_brk;
									}
								zzState = zzNext;

								int zzAttributes = zzAttrL[zzState];
								if ((zzAttributes & 1) == 1)
									{
										zzAction = zzState;
										zzMarkedPosL = zzCurrentPosL;
										if ((zzAttributes & 8) == 8)
											{
								
												goto zzForAction_brk;
											}
									}
							}
					}


zzForAction_brk: ;
 


					zzMarkedPos = zzMarkedPosL;

					switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) 
						{
						case 1: break;

						case 3: break;

						case 2: 
							{
								return ALPHANUM; 
							}

						case 4: break;

						default: 
							if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
								zzAtEOF = true;
								return YYEOF;
							} 
							else {
								zzScanError(ZZ_NO_MATCH);
							}
							break;
						}
				}
		} 

		public const int YYEOF = -1;

 

		private const int ZZ_BUFFERSIZE = 16384;

 

		public const int YYINITIAL = 0;

 

		private const System.String ZZ_CMAP_PACKED = 
			"\x0009\x0000\x0001\x0000\x0001\x0003\x0001\x0000\x0001\x0000\x0001\x0002\x0012\x0000\x0001\x0000"+
			"\x000f\x0000\x000a\x0001\x0007\x0000\x001a\x0001\x0006\x0000\x001a\x0001\x002f\x0000\x0001\x0001"+
			"\x000a\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0005\x0000\x0017\x0001\x0001\x0000\x001f\x0001"+
			"\x0001\x0000\x013f\x0001\x0019\x0000\x0072\x0001\x0004\x0000\x000c\x0001\x000e\x0000\x0005\x0001"+
			"\x0009\x0000\x0001\x0001\x008b\x0000\x0001\x0001\x000b\x0000\x0001\x0001\x0001\x0000\x0003\x0001"+
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0014\x0001\x0001\x0000\x002c\x0001\x0001\x0000\x0026\x0001"+
			"\x0001\x0000\x0005\x0001\x0004\x0000\x0082\x0001\x0008\x0000\x0045\x0001\x0001\x0000\x0026\x0001"+
			"\x0002\x0000\x0002\x0001\x0006\x0000\x0010\x0001\x0021\x0000\x0026\x0001\x0002\x0000\x0001\x0001"+
			"\x0007\x0000\x0027\x0001\x0048\x0000\x001b\x0001\x0005\x0000\x0003\x0001\x002e\x0000\x001a\x0001"+
			"\x0005\x0000\x000b\x0001\x0015\x0000\x000a\x0001\x0004\x0000\x0002\x0001\x0001\x0000\x0063\x0001"+
			"\x0001\x0000\x0001\x0001\x000f\x0000\x0002\x0001\x0007\x0000\x0002\x0001\x000a\x0001\x0003\x0001"+
			"\x0002\x0000\x0001\x0001\x0010\x0000\x0001\x0001\x0001\x0000\x001e\x0001\x001d\x0000\x0003\x0001"+
			"\x0030\x0000\x0026\x0001\x000b\x0000\x0001\x0001\x0152\x0000\x0036\x0001\x0003\x0000\x0001\x0001"+
			"\x0012\x0000\x0001\x0001\x0007\x0000\x000a\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001"+
			"\x0002\x0000\x0002\x0001\x0002\x0000\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0001\x0001"+
			"\x0003\x0000\x0004\x0001\x0003\x0000\x0001\x0001\x001e\x0000\x0002\x0001\x0001\x0000\x0003\x0001"+
			"\x0004\x0000\x000a\x0001\x0002\x0001\x0013\x0000\x0006\x0001\x0004\x0000\x0002\x0001\x0002\x0000"+
			"\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0002\x0001\x0001\x0000"+
			"\x0002\x0001\x001f\x0000\x0004\x0001\x0001\x0000\x0001\x0001\x0007\x0000\x000a\x0001\x0002\x0000"+
			"\x0003\x0001\x0010\x0000\x0009\x0001\x0001\x0000\x0003\x0001\x0001\x0000\x0016\x0001\x0001\x0000"+
			"\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0005\x0001\x0003\x0000\x0001\x0001\x0012\x0000"+
			"\x0001\x0001\x000f\x0000\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001\x0002\x0000"+
			"\x0002\x0001\x0002\x0000\x0016\x0001\x0001\x0000\x0007\x0001\x0001\x0000\x0002\x0001\x0001\x0000"+
			"\x0005\x0001\x0003\x0000\x0001\x0001\x001e\x0000\x0002\x0001\x0001\x0000\x0003\x0001\x0004\x0000"+
			"\x000a\x0001\x0001\x0000\x0001\x0001\x0011\x0000\x0001\x0001\x0001\x0000\x0006\x0001\x0003\x0000"+
			"\x0003\x0001\x0001\x0000\x0004\x0001\x0003\x0000\x0002\x0001\x0001\x0000\x0001\x0001\x0001\x0000"+
			"\x0002\x0001\x0003\x0000\x0002\x0001\x0003\x0000\x0003\x0001\x0003\x0000\x0008\x0001\x0001\x0000"+
			"\x0003\x0001\x002d\x0000\x0009\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000"+
			"\x0017\x0001\x0001\x0000\x000a\x0001\x0001\x0000\x0005\x0001\x0026\x0000\x0002\x0001\x0004\x0000"+
			"\x000a\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000\x0017\x0001\x0001\x0000"+
			"\x000a\x0001\x0001\x0000\x0005\x0001\x0003\x0000\x0001\x0001\x0020\x0000\x0001\x0001\x0001\x0000"+
			"\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000\x0008\x0001\x0001\x0000\x0003\x0001\x0001\x0000"+
			"\x0017\x0001\x0001\x0000\x0010\x0001\x0026\x0000\x0002\x0001\x0004\x0000\x000a\x0001\x0015\x0000"+
			"\x0012\x0001\x0003\x0000\x0018\x0001\x0001\x0000\x0009\x0001\x0001\x0000\x0001\x0001\x0002\x0000"+
			"\x0007\x0001\x0039\x0000\x0001\x0001\x0030\x0001\x0001\x0001\x0002\x0001\x000c\x0001\x0007\x0001"+
			"\x0009\x0001\x000a\x0001\x0027\x0000\x0002\x0001\x0001\x0000\x0001\x0001\x0002\x0000\x0002\x0001"+
			"\x0001\x0000\x0001\x0001\x0002\x0000\x0001\x0001\x0006\x0000\x0004\x0001\x0001\x0000\x0007\x0001"+
			"\x0001\x0000\x0003\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0001\x0001\x0002\x0000\x0002\x0001"+
			"\x0001\x0000\x0004\x0001\x0001\x0000\x0002\x0001\x0009\x0000\x0001\x0001\x0002\x0000\x0005\x0001"+
			"\x0001\x0000\x0001\x0001\x0009\x0000\x000a\x0001\x0002\x0000\x0002\x0001\x0022\x0000\x0001\x0001"+
			"\x001f\x0000\x000a\x0001\x0016\x0000\x0008\x0001\x0001\x0000\x0022\x0001\x001d\x0000\x0004\x0001"+
			"\x0074\x0000\x0022\x0001\x0001\x0000\x0005\x0001\x0001\x0000\x0002\x0001\x0015\x0000\x000a\x0001"+
			"\x0006\x0000\x0006\x0001\x004a\x0000\x0026\x0001\x000a\x0000\x0029\x0001\x0007\x0000\x005a\x0001"+
			"\x0005\x0000\x0044\x0001\x0005\x0000\x0052\x0001\x0006\x0000\x0007\x0001\x0001\x0000\x003f\x0001"+
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001\x0001\x0000\x0001\x0001"+
			"\x0001\x0000\x0004\x0001\x0002\x0000\x0027\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001"+
			"\x0002\x0000\x001f\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001"+
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0002\x0000\x0007\x0001\x0001\x0000\x0007\x0001"+
			"\x0001\x0000\x0017\x0001\x0001\x0000\x001f\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001"+
			"\x0002\x0000\x0007\x0001\x0001\x0000\x0027\x0001\x0001\x0000\x0013\x0001\x000e\x0000\x0009\x0001"+
			"\x002e\x0000\x0055\x0001\x000c\x0000\x026c\x0001\x0002\x0000\x0008\x0001\x000a\x0000\x001a\x0001"+
			"\x0005\x0000\x004b\x0001\x0015\x0000\x000d\x0001\x0001\x0000\x0004\x0001\x000e\x0000\x0012\x0001"+
			"\x000e\x0000\x0012\x0001\x000e\x0000\x000d\x0001\x0001\x0000\x0003\x0001\x000f\x0000\x0034\x0001"+
			"\x0023\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0003\x0000\x000a\x0001\x0026\x0000\x000a\x0001"+
			"\x0006\x0000\x0058\x0001\x0008\x0000\x0029\x0001\x0057\x0000\x001d\x0001\x0029\x0000\x000a\x0001"+
			"\x001e\x0001\x0002\x0000\x0005\x0001\x038b\x0000\x006c\x0001\x0094\x0000\x009c\x0001\x0004\x0000"+
			"\x005a\x0001\x0006\x0000\x0016\x0001\x0002\x0000\x0006\x0001\x0002\x0000\x0026\x0001\x0002\x0000"+
			"\x0006\x0001\x0002\x0000\x0008\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0001\x0001\x0001\x0000"+
			"\x0001\x0001\x0001\x0000\x001f\x0001\x0002\x0000\x0035\x0001\x0001\x0000\x0007\x0001\x0001\x0000"+
			"\x0001\x0001\x0003\x0000\x0003\x0001\x0001\x0000\x0007\x0001\x0003\x0000\x0004\x0001\x0002\x0000"+
			"\x0006\x0001\x0004\x0000\x000d\x0001\x0005\x0000\x0003\x0001\x0001\x0000\x0007\x0001\x0074\x0000"+
			"\x0001\x0001\x000d\x0000\x0001\x0001\x0082\x0000\x0001\x0001\x0004\x0000\x0001\x0001\x0002\x0000"+
			"\x000a\x0001\x0001\x0000\x0001\x0001\x0003\x0000\x0005\x0001\x0006\x0000\x0001\x0001\x0001\x0000"+
			"\x0001\x0001\x0001\x0000\x0001\x0001\x0001\x0000\x0004\x0001\x0001\x0000\x0003\x0001\x0001\x0000"+
			"\x0007\x0001\x0003\x0000\x0003\x0001\x0005\x0000\x0005\x0001\x0ebb\x0000\x0002\x0001\x002a\x0000"+
			"\x0005\x0001\x0005\x0000\x0002\x0001\x0003\x0000\x0001\x0000\x0056\x0000\x0006\x0000\x0003\x0000"+
			"\x0001\x0000\x005a\x0000\x0001\x0000\x0004\x0000\x0005\x0000\x0028\x0000\x0003\x0000\x0001\x0000"+
			"\x005e\x0001\x0011\x0000\x0018\x0001\x0038\x0000\x0010\x0000\x0100\x0000\x0080\x0000\x0080\x0000"+
			"\x19b6\x0000\x000a\x0000\x0040\x0000\x51a6\x0000\x005a\x0000\x048d\x0001\x0773\x0000\x2ba4\x0001"+
			"\x215c\x0000\x012e\x0000\x0002\x0000\x003b\x0000\x0095\x0000\x0007\x0001\x000c\x0000\x0005\x0001"+
			"\x0005\x0000\x0001\x0001\x0001\x0000\x000a\x0001\x0001\x0000\x000d\x0001\x0001\x0000\x0005\x0001"+
			"\x0001\x0000\x0001\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x0002\x0001\x0001\x0000\x006c\x0001"+
			"\x0021\x0000\x016b\x0001\x0012\x0000\x0040\x0001\x0002\x0000\x0036\x0001\x0028\x0000\x000c\x0001"+
			"\x0074\x0000\x0005\x0001\x0001\x0000\x0087\x0001\x0013\x0000\x000a\x0001\x0007\x0000\x001a\x0001"+
			"\x0006\x0000\x001a\x0001\x000a\x0000\x0001\x0000\x003a\x0000\x001f\x0001\x0003\x0000\x0006\x0001"+
			"\x0002\x0000\x0006\x0001\x0002\x0000\x0006\x0001\x0002\x0000\x0003\x0001\x0023\x0000";
 

		private static readonly char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

 

		private static readonly int [] ZZ_ACTION = zzUnpackAction();

		private const System.String ZZ_ACTION_PACKED_0 =
			"\x0001\x0000\x0001\x0001\x0001\x0002\x0001\x0001";

		private static int [] zzUnpackAction() {
			int [] result = new int[4];
			int offset = 0;
			offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackAction(System.String packed, int offset, int [] result) 
		{
			int i = 0; 
			int j = offset; 
			int l = packed.Length;
			while (i < l) 
				{
					int count = packed[i++];
					int value_Renamed = packed[i++];
					do 
						result[j++] = value_Renamed; 
					while (--count > 0);
				}
			return j;
		}


 

		private static readonly int[] ZZ_ROWMAP = zzUnpackRowMap();

		private const System.String ZZ_ROWMAP_PACKED_0 =
			"\x0000\x0000\x0000\x0004\x0000\x0008\x0000\x000c";

		private static int[] zzUnpackRowMap() 
		{
			int[] result = new int[4];
			int offset = 0;
			offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackRowMap(System.String packed, int offset, int [] result) 
		{
			int i = 0; 

			int j = offset; 

			int l = packed.Length;
			while (i < l) 
				{
					int high = packed[i++] << 16;
					result[j++] = high | packed[i++];
				}
			return j;
		}

 

		private static readonly int [] ZZ_TRANS = zzUnpackTrans();

		private const System.String ZZ_TRANS_PACKED_0 =
			"\x0001\x0002\x0001\x0003\x0001\x0004\x0001\x0002\x0005\x0000\x0001\x0003\x0005\x0000\x0001\x0002";


		private static int [] zzUnpackTrans() 
		{
			int [] result = new int[16];
			int offset = 0;
			offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackTrans(System.String packed, int offset, int [] result) 
		{
			int i = 0; 

			int j = offset; 

			int l = packed.Length;
			while (i < l) 
				{
					int count = packed[i++];
					int value_Renamed = packed[i++];
					value_Renamed--;
					do
						result[j++] = value_Renamed; 
					while (--count > 0);
				}
			return j;
		}


 

		private const int ZZ_UNKNOWN_ERROR = 0;
		private const int ZZ_NO_MATCH = 1;
		private const int ZZ_PUSHBACK_2BIG = 2;

 

		private static readonly System.String[] ZZ_ERROR_MSG = new System.String[] {
			"Unkown internal scanner error",
			"Error: could not match input",
			"Error: pushback value was too large"
		};

 

		private static readonly int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

		private const System.String ZZ_ATTRIBUTE_PACKED_0 =
			"\x0001\x0000\x0001\x0009\x0002\x0001";


		private static int [] zzUnpackAttribute() 
		{
			int [] result = new int[4];
			int offset = 0;
			offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
			return result;
		}

		private static int zzUnpackAttribute(System.String packed, int offset, int [] result) 
		{
			int i = 0; 

			int j = offset; 

			int l = packed.Length;
			while (i < l) 
				{
					int count = packed[i++];
					int value_Renamed = packed[i++];
					do
						result[j++] = value_Renamed; 
					while (--count > 0);
				}
			return j;
		}

 

		private System.IO.TextReader zzReader;
 

		private int zzState;

 

		private int zzLexicalState = YYINITIAL;

 

		private char[] zzBuffer = new char[ZZ_BUFFERSIZE];

 

		private int zzMarkedPos;
		private int zzPushbackPos;
 

		private int zzCurrentPos;

 

		private int zzStartRead;

 

		private int zzEndRead;

 

		private int yyline;

 

		private int yychar_Renamed_Field;

 

		private int yycolumn;

 

		private bool zzAtBOL = true;

 

		private bool zzAtEOF;

 

		private bool zzEOFDone;

 
		
		public static readonly int ALPHANUM = StandardTokenizer.ALPHANUM;
		
		public static readonly int APOSTROPHE = StandardTokenizer.APOSTROPHE;
		
		public static readonly int ACRONYM = StandardTokenizer.ACRONYM;
		
		public static readonly int COMPANY = StandardTokenizer.COMPANY;
		
		public static readonly int EMAIL = StandardTokenizer.EMAIL;
		
		public static readonly int HOST = StandardTokenizer.HOST;
		
		public static readonly int NUM = StandardTokenizer.NUM;
		
		public static readonly int CJ = StandardTokenizer.CJ;
		
		public static readonly int ACRONYM_DEP = StandardTokenizer.ACRONYM_DEP;

		public static readonly System.String[] TOKEN_TYPES = new System.String[]{"<ALPHANUM>", "<APOSTROPHE>", "<ACRONYM>", "<COMPANY>", "<EMAIL>", "<HOST>", "<NUM>", "<CJ>", "<ACRONYM_DEP>"};

		public int yychar()
		{
			return yychar_Renamed_Field;
		}

		internal StandardTokenizerImpl(System.IO.TextReader in_Renamed)
		{
			this.zzReader = in_Renamed;
		}
		
		
		
		
		
		
		
		internal StandardTokenizerImpl(System.IO.Stream in_Renamed):this(new System.IO.StreamReader(in_Renamed, System.Text.Encoding.Default))
		{
		}

		internal static StandardTokenizerImpl GetStandardTokenizerImpl(System.IO.TextReader reader)
		{
			if (impl==null)
				{
					impl = new StandardTokenizerImpl(reader);
				}
			else
				{
					impl.yyreset(reader);
				}

			return impl;
		}

		private static StandardTokenizerImpl impl = null;
		
		
		
		
		
 		private static char [] zzUnpackCMap(System.String packed) 
		{
			char [] map = new char[0x10000];
			int i = 0; 

			int j = 0; 

			while (i < 1214) 
				{
					int count = packed[i++];
					char value_Renamed = packed[i++];
					do
						map[j++] = value_Renamed; 
					while (--count > 0);
				}
			return map;
		}


 

		private bool zzRefill() 
		{

			if (zzStartRead > 0) 
				{
					Array.Copy(zzBuffer, zzStartRead, zzBuffer, 0, zzEndRead - zzStartRead);
				
				

					zzEndRead -= zzStartRead;
					zzCurrentPos -= zzStartRead;
					zzMarkedPos -= zzStartRead;
					zzPushbackPos -= zzStartRead;
					zzStartRead = 0;
				}
			
			

			if (zzCurrentPos >= zzBuffer.Length)
				{
 

					char[] newBuffer = new char[zzCurrentPos*2];
					Array.Copy(zzBuffer, 0, newBuffer, 0, zzBuffer.Length);
					zzBuffer = newBuffer;
				}


 			int lengthToRead = zzBuffer.Length - zzEndRead;
			int numRead = zzReader.Read(zzBuffer, zzEndRead, lengthToRead);

			
			if (numRead == 0)
				{
					return true;
				}
			else
				{
				
					zzAtEOF = (numRead < lengthToRead);

					zzEndRead += numRead;
					return false;
				}

 		}

 
 		public void  yyclose()
		{
			zzAtEOF = true; 

			zzEndRead = zzStartRead; 

			
			if (zzReader != null)
				zzReader.Close();
		}

		public void  yyreset(System.IO.TextReader reader)
		{
			zzReader = reader;
			zzAtBOL = true;
			zzAtEOF = false;
			zzEndRead = zzStartRead = 0;
			zzCurrentPos = zzMarkedPos = zzPushbackPos = 0;
			yyline = yychar_Renamed_Field = yycolumn = 0;
			zzLexicalState = YYINITIAL;
		}


 

		public int yystate() {
			return zzLexicalState;
		}


 

		public void yybegin(int newState) {
			zzLexicalState = newState;
		}


 
		public System.String yytext()
		{
			return new System.String(zzBuffer, zzStartRead, zzMarkedPos - zzStartRead);
		}


 

		public char yycharat(int pos) {
			return zzBuffer[zzStartRead+pos];
		}


 

		public int yylength() {
			return zzMarkedPos-zzStartRead;
		}


 
		private void  zzScanError(int errorCode)
		{
			System.String message;
			try
				{
					message = ZZ_ERROR_MSG[errorCode];
				}
			catch (System.IndexOutOfRangeException e)
				{
					message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
				}
			
			throw new System.ApplicationException(message);
		} 

		public virtual void  yypushback(int number)
		{
			if (number > yylength())
				zzScanError(ZZ_PUSHBACK_2BIG);
			
			zzMarkedPos -= number;
		}

	}
}
