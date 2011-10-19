//
// ICalParser.cs
//
// Copyright (C) 2006 Novell, Inc.
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
using System.IO;
using System.Text;
using System.Collections;

namespace Beagle.Util
{
	public class ICalParser : IEnumerator
	{
		public static string KabcMimeType = "text/x-kabc-directory";
		public static string KnotesMimeType = "text/x-knotes-calendar";
 		public static string KOrganizerEventMimeType = "text/x-korganizer-event";
 		public static string KOrganizerTodoMimeType = "text/x-korganizer-todo";

		private ParserState state = ParserState.ParsingProperty;
		private StringBuilder buffer = new StringBuilder ();
		
		private Token current;
		private TextReader reader;
		
		public ICalParser (TextReader reader)
		{
			this.reader = new UnfoldingTextReader (reader);
		}
		
		public object Current
		{
			get { return current; }
		}
		
		public bool MoveNext ()
		{
			string data;
			char stop;
			
			switch (state) {
			case ParserState.ParsingProperty:
				data = Read (new char [] { ':', ';' }, out stop);
				
				if (data == null)
					return false;
				
				switch (stop) {
				case ':':
					state = ParserState.ParsingPropertyValue;
					break;
				case ';':
					state = ParserState.ParsingPropertyParameter;
					break;
				}
				
				current = new Token (TokenType.Property, data.Trim ());
				break;
			case ParserState.ParsingPropertyValue:
				data = Read (new char[] { '\n'}, out stop);
				
				if (data == null)
					return false;
				
				switch (stop) {
				case '\n':
					state = ParserState.ParsingProperty;
					break;
				case ',':
					state = ParserState.ParsingPropertyValue;
					break;
				}
				
				current = new Token (TokenType.PropertyValue, data.Trim ());
				break;
			case ParserState.ParsingPropertyParameter:              
				data = Read (new char[] { '\n', '=', ';', ','}, out stop);
				
				if (data == null)
					return false;
				
				switch (stop) {
				case '=':
					state = ParserState.ParsingPropertyParameterValue;
					break;
				case '\n':
					state = ParserState.ParsingProperty;
					break;
				case ';':
				case ',':
					break;
				}
				
				current = new Token (TokenType.PropertyParameter, data.Trim ());
				break;
			case ParserState.ParsingPropertyParameterValue:
				data = Read (new char[] { ',', ':', ';' }, out stop);
				
				switch (stop) {
				case ';':
					state = ParserState.ParsingPropertyParameter;
					break;
				case ':':
					state = ParserState.ParsingPropertyValue;
					break;
				case ',':
					break;
				}
				
				current = new Token (TokenType.PropertyParameterValue, data.Trim ());
				break;
			}
			
			return true;
		}
		
		public void Reset()
		{
			throw new NotImplementedException ();
		}
		
		public void Dispose()
		{
			
		}
		
		private string Read (char[] stops, out char stop)
		{
			stop = (char) 0;
			
			lock (buffer) {
				int i;
				buffer.Length = 0;
				
				while ((i = reader.Read ()) != -1) {
					if (Array.IndexOf (stops, (char) i) != -1) {
						stop = (char) i;
						break;
					}
					
					buffer.Append((char)i);
				}
				
				return (buffer.Length == 0) ? null : buffer.ToString ();
			}
		}
		
		public class Token
		{
			private TokenType type;
			private string value;
			
			public TokenType Type
			{
				get { return type; }
			}
			
			public string Value
			{
				get { return value; }
			}
			
			public Token (TokenType type, string value)
			{
				this.type = type;
				this.value = value;
			}
			
			public override string ToString ()
			{
				return String.Format ("{0} ({1})", type, value);
			}
		}
		
		public enum TokenType
		{
			Property,
			PropertyValue,
			PropertyParameter,
			PropertyParameterValue,
		}
		
		private enum ParserState
		{
			ParsingProperty,
			ParsingPropertyValue,
			ParsingPropertyParameter,
			ParsingPropertyParameterValue
		}
		
		private class UnfoldingTextReader : TextReader
		{
			private TextReader reader;
			
			public UnfoldingTextReader(TextReader reader)
			{
				this.reader = reader;
			}
			
			public override int Read ()
			{
				int i = reader.Read ();
				
				if (i == '\r')
					i = reader.Read ();
				
				if ((char) i == '\n' && (char) reader.Peek() == ' ') {
					do {
						i = reader.Read ();
						
						if ((char) i == '\r')
							i = reader.Read ();
					} while ((char) i == ' ' || (char) i == '\n');
				}
				return i;
			}
		}
	}
	
#if false
	class Driver
	{
		static void Main (string [] args)
		{
			ICalParser parser = new ICalParser (File.OpenText ("test.ical"));
			
			while (parser.MoveNext()) {
				ICalParser.Token token = (ICalParser.Token) parser.Current;
				
				if (token.Type != ICalParser.TokenType.Property)
					Console.Write ("\t");
				
				if (token.Type == ICalParser.TokenType.PropertyParameterValue)
					Console.Write ("\t");
				
				Console.WriteLine (token);
			}
		}
	}
#endif
}
