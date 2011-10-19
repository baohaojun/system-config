//
// Password.cs
//
// Copyright (C) 2006 Kyle Ambroff
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
using System.Text;
using System.Security.Cryptography;

namespace Beagle.Util {

        public class Password
        {
                public static string Encode (string password)
                {
                        Byte [] password_bytes = null;
                        Byte [] encoded_bytes = null;
                                                
                        MD5 md5 = new MD5CryptoServiceProvider ();
                        password_bytes = ASCIIEncoding.Default.GetBytes (password);
                        encoded_bytes = md5.ComputeHash (password_bytes);
                        
                        return System.BitConverter.ToString (encoded_bytes);
                }
        }
}
