/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
using Lucene.Net.Analysis;

namespace Lucene.Net.Analysis.Standard
{
	
    /// <summary>Normalizes tokens extracted with {@link StandardTokenizer}. </summary>
	
    public sealed class StandardFilter : TokenFilter
    {
		
		
        /// <summary>Construct filtering <i>in</i>. </summary>
        public StandardFilter(TokenStream in_Renamed) : base(in_Renamed)
        {
        }
		
        /// <summary>Returns the next token in the stream, or null at EOS.
        /// <p>Removes <tt>'s</tt> from the end of words.
        /// <p>Removes dots from acronyms.
        /// </summary>
        public override Lucene.Net.Analysis.Token Next()
        {
            Lucene.Net.Analysis.Token t = input.Next();
			
            if (t == null)
                return null;
			
	    return t;
        }
    }
}
