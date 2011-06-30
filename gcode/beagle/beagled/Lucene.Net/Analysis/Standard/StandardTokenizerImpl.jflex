package org.apache.lucene.analysis.standard;

/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*

NOTE: if you change StandardTokenizerImpl.jflex and need to regenerate
      the tokenizer, remember to use JRE 1.4 to run jflex (before
      Lucene 3.0).  This grammar now uses constructs (eg :digit:,
      :letter:) whose meaning can vary according to the JRE used to
      run jflex.  See
      https://issues.apache.org/jira/browse/LUCENE-1126 for details.

*/

import org.apache.lucene.analysis.Token;
import org.apache.lucene.analysis.tokenattributes.TermAttribute;

%%

%class StandardTokenizerImpl
%unicode
%integer
%function getNextToken
%pack
%char

%{

public static final int ALPHANUM          = StandardTokenizer.ALPHANUM;
public static final int APOSTROPHE        = StandardTokenizer.APOSTROPHE;
public static final int ACRONYM           = StandardTokenizer.ACRONYM;
public static final int COMPANY           = StandardTokenizer.COMPANY;
public static final int EMAIL             = StandardTokenizer.EMAIL;
public static final int HOST              = StandardTokenizer.HOST;
public static final int NUM               = StandardTokenizer.NUM;
public static final int CJ                = StandardTokenizer.CJ;
/**
 * @deprecated this solves a bug where HOSTs that end with '.' are identified
 *             as ACRONYMs. It is deprecated and will be removed in the next
 *             release.
 */
public static final int ACRONYM_DEP       = StandardTokenizer.ACRONYM_DEP;

public static final String [] TOKEN_TYPES = StandardTokenizer.TOKEN_TYPES;

public final int yychar()
{
    return yychar;
}

/**
 * Fills Lucene token with the current token text.
 */
final void getText(Token t) {
  t.setTermBuffer(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
}

/**
 * Fills TermAttribute with the current token text.
 */
final void getText(TermAttribute t) {
  t.setTermBuffer(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
}

%}

THAI       = [\u0E00-\u0E59]

// basic word: a sequence of digits & letters (includes Thai to enable ThaiAnalyzer to function)
ALPHANUM   = ({LETTER}|{THAI}|[:digit:])+

// From the JFlex manual: "the expression that matches everything of <a> not matched by <b> is !(!<a>|<b>)"
LETTER     = !(![:letter:]|{CJ})

// Chinese and Japanese (but NOT Korean, which is included in [:letter:])
CJ         = [\u3100-\u312f\u3040-\u309F\u30A0-\u30FF\u31F0-\u31FF\u3300-\u337f\u3400-\u4dbf\u4e00-\u9fff\uf900-\ufaff\uff65-\uff9f]

WHITESPACE = \r\n | [ \r\n\t\f]

%%

{ALPHANUM}                                                     { return ALPHANUM; }
{CJ}                                                           { return CJ; }

/** Ignore the rest */
. | {WHITESPACE}                                               { /* ignore */ }
