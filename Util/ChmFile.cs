//
// CHMFile.cs: Basic chmlib Wrapper, CHM file format reader.
//
// 
// Copyright (C) 2005,2006  Miguel Fernando Cabrera <mfcabrera@gmail.com>
//   
// Based on Razvan Cojocaru's X-CHM::CHMFile. 
// Uses Jed Wing's CHMLib.
// For more information about CHM file format 
// check out Pabs' CHM spec at http://bonedaddy.net/pabs3/hhm
// 
 
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Runtime.InteropServices;
using System.Collections;
using System.IO;
using System.Text;


#if true
namespace Beagle.Util {
#endif




/*just to make it 'right'*/
	enum ChmResolve {
		Sucess,
		Failure
	}
	
	
	enum ChmEnumerate {
		Normal = 1,
		Meta = 1 << 1,
		Special = 1 << 2,
		Files = 1 << 3,
		Dirs = 1 << 4,
		All = (1 << 5) -1
	
	}


	public delegate void ChmHtmlParseFunc(TextReader stream);

	public class ChmFile : IDisposable {

	
		private string title = "";
		//private ArrayList tocStrings;
		private ArrayList htmlFiles;
		private string topicsFile;
		private bool hasTopics = false;
		private IntPtr chmfile = IntPtr.Zero;
				
		private bool loaded = false;
		private const int  bufSize = 4096;
		private string defaultFile = "";
		
		public string Title {
			get { return title; }
			
		}
		
		public bool HasTopics {
			get { return hasTopics; }
			
		}	
		
		
		
		
		
		/* ChmLib Glue */
		
		[ StructLayout (LayoutKind.Sequential) ]
		private  class  chmUnitInfo {

			public UInt64             start;
			public UInt64             length;
			public int                space;
			public int                flags;
			[MarshalAs (UnmanagedType.ByValTStr, SizeConst=512)]
			public string path;
			/*[MarshalAs (UnmanagedType.ByValArray, SizeConst=257)]
			  public char[] path;*/
			
		}
		
		
		private  delegate int  ChmEnumerator (IntPtr chmFile,
						      chmUnitInfo info,
						      IntPtr context);
		
	
		[DllImport ("libchm.so.1")]
		private static extern IntPtr  chm_open(string filename);
		
		[DllImport ("libchm.so.1")]
		private static extern ChmResolve chm_resolve_object(IntPtr raw,
								    string  objPath,
								    [Out] chmUnitInfo ui);
		
		[DllImport ("libchm.so.1")]
		private static extern UInt64 chm_retrieve_object(IntPtr raw,
								 [In, Out] chmUnitInfo ui,
								 IntPtr buf,
								 UInt64 addr,
								 UInt64 len);
		
		
		

		[DllImport ("libchm.so.1")]
		private static extern int chm_enumerate(IntPtr raw,
							ChmEnumerate what,
							ChmEnumerator e,
							IntPtr context);
		

		
		
		private int GetHtmlFiles(IntPtr chmFile,
					 chmUnitInfo info,
					 IntPtr context) 
		{
			if(info.path.EndsWith(".html"))
				htmlFiles.Add(info.path.Trim());
			
			return 1;
			
		}

		
		
		
		private string  ChmGetString(IntPtr ptr,
					     int offset,
					     int len)
		{
			
			int i = 0;
			string str;
			
			char[]  cadena = new char[len];
		
			for(i =0; i < len ; i++){
				
				cadena[i] = (char) Marshal.ReadByte(ptr,offset + i);
				if(cadena[i] == '\0') 
					break;
			
			}
			
			str = new string (cadena,0,i);
			
			return str;
		
		
		}
		
		private string ChmFileToString(chmUnitInfo ui) 
		{
			
			
			const ulong tmpBufSize = 1025;
			
			StringBuilder strb = new StringBuilder();
			
		
			ulong size = tmpBufSize -1;
			ulong cur = 0;
			
			
			IntPtr raw = Marshal.AllocCoTaskMem ((int)tmpBufSize);
			
			do {
				size = chm_retrieve_object(chmfile,ui,raw,cur,tmpBufSize-1);
				// If I dont create a copy of the string when i free 'raw' the builder data dissapear
				// the last  chunk readed dissapear (mono bug or mi endless stupidity)
				// I'll have to check it out 
				strb.Append(Marshal.PtrToStringAuto(raw,(int)size));
				cur += size;
			}
			while(size == tmpBufSize-1);
			
			Marshal.FreeCoTaskMem (raw);
			return strb.ToString();
			
		}


		private void CleanUp() 
		{
			Marshal.FreeCoTaskMem (chmfile);
			this.loaded = false;
		}
		
		public void Dispose()
		{
			CleanUp();
			GC.SuppressFinalize (this);
						
		}
		
		~ChmFile()
		{
			CleanUp();
			
		}
		
		public ChmFile() 
		{
			
			//this.tocStrings = new ArrayList();
			this.htmlFiles = new ArrayList();
			
			
		}
		
		private static bool dll_not_found = false;

		public bool Load(string path)
		{
			if (dll_not_found)
				return false;

			//chmUnitInfo ui = new chmUnitInfo() ;

			try {
				this.chmfile = chm_open(path);
			} catch (DllNotFoundException) {
				Log.Error ("libchm.so.1 not found. Disabling chm file filtering.");
				dll_not_found = true;
			}
		
			if(this.chmfile == IntPtr.Zero) {
				throw new System.Exception ("Invalid file Type, not a CHM file");	
			}
			this.loaded = true;
			
			bool info = GetArchiveInfo();
			
			/*if(info && HasTopics)
			  BuildTopicsList();*/
		
			
			chm_enumerate(this.chmfile,
				      ChmEnumerate.All,
				      new ChmEnumerator(GetHtmlFiles),
				      IntPtr.Zero);
			
			
			/*		
				foreach(string str in htmlFiles)
				Console.WriteLine(str); */
			
			return info;
			
		}

		/**
		   
		From the #SYSTEM File we are interested in the title (for now).
		
		*/

		private bool SystemInfo() 
		{
			ChmResolve res; 
			ulong  size;
			bool gottitle = false;
			
			
			
			if(!loaded)
				return false;
			
			
			IntPtr buf = Marshal.AllocCoTaskMem (bufSize);
			
			chmUnitInfo ui = new chmUnitInfo() ;
		
			
			res = chm_resolve_object (this.chmfile,"/#SYSTEM", ui);
			
			
			if(res == ChmResolve.Failure)
				return false;
			
			size  =  chm_retrieve_object (this.chmfile, ui,buf, 4, (ulong)bufSize);
			
			int index = 0;
			ushort value = 0;
			long tol = (long)size - 2;
			
			while(index < tol) {
				
				
				value =  (ushort)Marshal.ReadInt16 (buf, index);
				
				if(value == 3) {
					
					
					index += 2;
					ushort len = (ushort)Marshal.ReadInt16 (buf, (int)index);
					
					if(this.title == "") 
						this.title = ChmGetString (buf,index+2, (int)len);
					gottitle = true;
					break;
					
					
				}
				else 
					index += 2;
				
				
				value = (ushort) Marshal.ReadInt16(buf,(int)index);
				
				index += (int)value + 2;
				
			}
			
			Marshal.FreeCoTaskMem (buf);
			return gottitle;
			
		}

		/*
		  TODO:
		  We should trow something like a FileNotFoundException
		*/
		public TextReader GetFile(string path) 
		{

			chmUnitInfo ui = new chmUnitInfo();
			
			if(chm_resolve_object (chmfile,path,ui)  == ChmResolve.Failure) {
				//Console.WriteLine("Fails to Open: {0}",path);
				return new StringReader("");
				

			}
			
			return (new StringReader(ChmFileToString(ui)));
						
		}
		
		
		public TextReader  GetTopicsFile()
		{
			
			if(HasTopics) {
				
				return GetFile(topicsFile);
				
			}
			
			/*Oh Lina, why don't you love me? :P*/			
			return new StringReader("");
			
			
		}
		
		public TextReader GetDefaultFile()
		{
			
			
							
			return GetFile(defaultFile);
				
			
		}
	
		
		
		
		private bool WindowsInfo() 
		{
			
			int  entries;
			int entrySize;
			IntPtr windowsData;
			long size = 0;
			uint block;
			
			const int headerLen = 0x8;
			
		
			IntPtr buf = Marshal.AllocCoTaskMem (bufSize);
			chmUnitInfo ui = new chmUnitInfo ();
			
			
			if(chm_resolve_object (chmfile,"/#WINDOWS",ui) == ChmResolve.Failure)
				return false;
			
			if(chm_retrieve_object (chmfile,ui,buf,0,headerLen) == 0)
				return false;
			
			entries = Marshal.ReadInt32 (buf);
			entrySize = Marshal.ReadInt32 (buf,0x4);
			
			//Console.WriteLine ("entries -> {0}\nsize = {1}",entries,entrySize);
			
			windowsData  = Marshal.AllocCoTaskMem(entries * entrySize);
			
		
			size = (long)chm_retrieve_object (chmfile,
							  ui,
							  windowsData,
							  headerLen,
							  (ulong)(entries * entrySize));
			if(size == 0)
				return false;
			
			size = 0;
			
			
			if(chm_resolve_object (chmfile,"/#STRINGS",ui) == ChmResolve.Failure)
				return false;
			
			
		/*
		  From Pabs' CHM Spec: 
		  "(STRINGS)This file is a list of ANSI/UTF-8 NT strings.
		  The first is just a NIL character so that offsets to this file can specify 
		  zero & get a valid string.
		  The strings are sliced up into blocks that are 4096 bytes in length."
		*/
			
			
			
			for(int i = 0; i < entries; i++) {
				
				int offset = i * entrySize;
				
				
				uint offTitle  = (uint)Marshal.ReadInt32(windowsData,
									 offset + 0x14);
				
				uint offTocFile = (uint)Marshal.ReadInt32(windowsData,
									  offset + 0x60);
				uint offDefaultFile = (uint)Marshal.ReadInt32(windowsData,
									      offset + 0x68);
				
								
				//Console.WriteLine("offTocFile = {0}",offTocFile);
				
				
				block = offTitle / 4096;
				
				
				if(size == 0) 
					size = (long)chm_retrieve_object(chmfile, 
									 ui, 
									 buf, 
									 block * 4096, 
									 (ulong)bufSize);
				
				
				if(size > 0 && offTitle > 0) 
					this.title = ChmGetString(buf,(int)offTitle,4096);
				
				
				if(block != offTocFile / 4096) {
					block = offTocFile / 4096;
					size = (long)chm_retrieve_object(chmfile, 
									 ui, 
									 buf, 
									 block * 4096, 
									 (ulong)bufSize);
					
				}
				
				
				if(size > 0 && offTocFile > 0){ 				
					topicsFile = "/" + ChmGetString(buf,
									(int)offTocFile % 4096 ,
									4096);
					hasTopics = true;
				}
				
				
				if(block != offDefaultFile / 4096) {
					block = offDefaultFile / 4096;
					size = (long)chm_retrieve_object(chmfile, 
									 ui, 
									 buf, 
									 block * 4096, 
									 (ulong)bufSize);
					
				}
				
				
				if(size > 0 && offDefaultFile > 0) 				
					defaultFile = ("/" + ChmGetString(buf,
									  (int)offDefaultFile % 4096 ,
									  4096) );
												
				

			}
		
			Marshal.FreeCoTaskMem (buf);
			Marshal.FreeCoTaskMem (windowsData);
			return true;
			
		}
		
		
		
		private  bool GetArchiveInfo() 
		{
			/*
			  We only get the chm title (if any) from the #SYSTEM file
			  and the Toc strings for now. 		  
			  I'm sure Razvan will feel some kind of Deja Vu.
			*/
			bool rest = false;
			bool resw = false;
			
			
			if(this.loaded) {
			
				resw = WindowsInfo();
				rest = SystemInfo(); 
				
			}
			return (rest || resw);
			
		}
		
		public void ParseContents(ChmHtmlParseFunc Parse)
		{
			
						
			if(this.loaded)
				foreach(string fileName in htmlFiles) {
					chmUnitInfo ui = new chmUnitInfo();
					
					chm_resolve_object(this.chmfile,
							   fileName,
							   ui) ;
					
					
					//Console.WriteLine("Parsing....{0}",ui.path);
					///Logger.Log.Debug("CHMFile: Parsing {0}....",ui.path);
					Parse( new StringReader(ChmFileToString(ui).Trim()) );
					
					
				}
			
			
			
		}
		
		
		
	}
	
	
	

	
#if true
}
#endif
