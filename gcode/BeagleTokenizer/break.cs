using System;
using System.Text;
using Lucene.Net.Analysis.Standard;
using System.IO;
using Lucene.Net.Analysis;

public class StandardTokenizer {
  public const int ALPHANUM          = 0;
  public const int APOSTROPHE        = 1;
  public const int ACRONYM           = 2;
  public const int COMPANY           = 3;
  public const int EMAIL             = 4;
  public const int HOST              = 5;
  public const int NUM               = 6;
  public const int CJ                = 7;
  public const int ACRONYM_DEP       = 8; /* deprecated */

  
  public static void Main(string[] args) {
    StringBuilder sb = new StringBuilder();
    
    foreach (string arg in args) {
      sb.Append(arg + " ");
    }

    StringReader reader = new StringReader(sb.ToString());

    StandardTokenizerImpl scanner = StandardTokenizerImpl.GetStandardTokenizerImpl(reader);
    Lucene.Net.Analysis.Token token;
    while ((token = Next(scanner)) != null) {
      Console.WriteLine("{0}", token.TermText());
    }

  }

  public static Token Next(StandardTokenizerImpl scanner)
  {
    int tokenType = scanner.GetNextToken();

    if (tokenType == StandardTokenizerImpl.YYEOF) {
      return null;
    }

    int startPosition = scanner.yychar();

    string tokenImage = scanner.yytext();
    return new Token(tokenImage, startPosition, startPosition
                     + tokenImage.Length,
                     StandardTokenizerImpl.TOKEN_TYPES[tokenType]);
  }

}
