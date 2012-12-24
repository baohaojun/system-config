/*
 [GPL v2 licence]
 Copyright (c) 2012 Bao Haojun
 All rights reserved.

*/

grammar OrgMode;

options {backtrack=true; memoize=true; output=template; rewrite=true;}

tokens 
        {
            BEGIN_EXAMPLE;
            END_EXAMPLE;
            BEGIN_SRC;
            END_SRC;
        }

@members {
    public boolean noSpace() {
        if (input.LT(1) == null) {
            return true;
        }
        boolean result = input.LT(1).getCharPositionInLine() == input.LT(-1).getCharPositionInLine() + input.LT(-1).getText().length();
        System.out.println("noSpace? " + input.LT(1) + " " + input.LT(-1) + ": " + result);
        return result;
    }
}

@lexer::members {
  protected boolean bol = true;
  boolean in_example = false;
  protected boolean assertIsKeyword = true;
  
  void debug(String s) {
      System.out.println(s + " at " + getLine() + ":" + getCharPositionInLine());
  }

  void debug(int do_it, String s) {
      if (do_it != 0) {
          System.out.println(s + " at " + getLine() + ":" + getCharPositionInLine());
      }
  }
  boolean looking_backat_white_space() {
      return true;
  }

  boolean inExample(String w) {
      System.out.println("checking for inExample in " + w);
      return in_example && getCharPositionInLine() == 0;
  }
  boolean looking_at_white_space() {
      return true;
  }

  public String getErrorMessage(RecognitionException e,
                                String[] tokenNames)
  {
      List stack = getRuleInvocationStack(e, this.getClass().getName());
      String msg = null;
      if ( e instanceof NoViableAltException ) {
          NoViableAltException nvae = (NoViableAltException)e;
          msg = " no viable alt; token="+e.token+
              " (decision="+nvae.decisionNumber+
              " state "+nvae.stateNumber+")"+
              " decision=<<"+nvae.grammarDecisionDescription+">>";
      }
      else {
          msg = super.getErrorMessage(e, tokenNames);
      }
      return stack+" "+msg;
  }
}



orgFile : block* ;

block :
        header
    |   exampleParagraph
    |   normalParagraph
    |   NL
    ;
header : HEADER_STAR notNL+ NL {System.out.println("got a header " + $text + " end of header");};

exampleParagraph : BEG_EXAMPLE .* END_EXAMPLE {System.out.println("matched exampleParagraph: " + $text);} ;

normalParagraph : (lineText NL)+ ;

lineText : textItem+;

textItem : normalText;


notNL : ~NL ;

normalText : notNL {System.out.println("matched normal text: " + $text);} ;

BEG_BLOCK : '#+begin_' WORDF ;
END_BLOCK : '#+end_' WORDF ;

fragment
WORDF : ('a'..'z' | 'A'..'Z' | '_')+ ;

WS :   ( ' ' | '\t' | '\r' | '\f' )+ {$channel = HIDDEN;} ;

fragment
WSF : ' ' | '\t' | '\f' | '\r';


/* start code-generator
# BOLD_INLINE : '*' ~('*' | AWSF) .* (~AWSF '*' AWSF {debug("matched * :" + $text);} | '\n' ) ;
   cat << EOF | perl -npe "s#(.*?)\s*: '(.)'#
                             \$1 : '\$2' ~('\$2' | AWSF) .* 
(
    ~AWSF '\$2' (
                 WSF { emit(new CommonToken(\$1, \\\$text)); debug(\"matched code \$1 :\" + \\\$text); }
                |   NL  { emit(new CommonToken(\$1, \\\$text)); emit(new CommonToken(NL)); }
                )

    | NL { emit(new CommonToken(NWS, \\\$text)); emit(new CommonToken(NL)); }
) ;#"
BOLD_INLINE : '*'
UNDERLINED_INLINE : '_'
CODE_INLINE : '='
VERBATIM_INLINE : '~'
STRIKE_INLINE : '+'
ITALIC_INLINE : '/'   
EOF
   end code-generator */
// start generated code

BOLD_INLINE : '*' ~('*' | AWSF) .* 
        (
            ~AWSF '*' (
                WSF { emit(new CommonToken(BOLD_INLINE, $text)); debug("matched code BOLD_INLINE :" + $text); }
            |   NL  { emit(new CommonToken(BOLD_INLINE, $text)); emit(new CommonToken(NL)); }
            )

        | NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
        ) ;

UNDERLINED_INLINE : '_' ~('_' | AWSF) .* 
        (
            ~AWSF '_' (
                WSF { emit(new CommonToken(UNDERLINED_INLINE, $text)); debug("matched code UNDERLINED_INLINE :" + $text); }
            |   NL  { emit(new CommonToken(UNDERLINED_INLINE, $text)); emit(new CommonToken(NL)); }
            )

        | NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
        ) ;

CODE_INLINE : '=' ~('=' | AWSF) .* 
        (
            ~AWSF '=' (
                WSF { emit(new CommonToken(CODE_INLINE, $text)); debug("matched code CODE_INLINE :" + $text); }
            |   NL  { emit(new CommonToken(CODE_INLINE, $text)); emit(new CommonToken(NL)); }
            )

        | NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
        ) ;

VERBATIM_INLINE : '~' ~('~' | AWSF) .* 
        (
            ~AWSF '~' (
                WSF { emit(new CommonToken(VERBATIM_INLINE, $text)); debug("matched code VERBATIM_INLINE :" + $text); }
            |   NL  { emit(new CommonToken(VERBATIM_INLINE, $text)); emit(new CommonToken(NL)); }
            )

        | NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
        ) ;

STRIKE_INLINE : '+' ~('+' | AWSF) .* 
    (
        ~AWSF '+' (
            WSF { emit(new CommonToken(STRIKE_INLINE, $text)); debug("matched code STRIKE_INLINE :" + $text); }
    |   NL  { emit(new CommonToken(STRIKE_INLINE, $text)); emit(new CommonToken(NL)); }
    )

| NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
    ) ;

ITALIC_INLINE : '/' ~('/' | AWSF) .* 
    (
        ~AWSF '/' (
            WSF { emit(new CommonToken(ITALIC_INLINE, $text)); debug("matched code ITALIC_INLINE :" + $text); }
    |   NL  { emit(new CommonToken(ITALIC_INLINE, $text)); emit(new CommonToken(NL)); }
    )

| NL { emit(new CommonToken(NWS, $text)); emit(new CommonToken(NL)); }
    ) ;   

// end generated code

LINK_URL : '[[' ~('['|']')* ']]' ;
LINK_URL_DESC : '[[' ~('['|']')* '][' ~('['|']')* ']]' ;
fragment
AWSF : WSF | NL ;

fragment
NL : '\n' ;
LE : '\n' ;

HEADER_STAR : s='*' '*'* WSF ;
OL_START : ('0' .. '9')+ '.' WSF ;
UL_START : ('-' | '+' | '*') WSF ;
COL_START : ':' WSF ;
SHARP_SETTING : '#+' WORDF ':' WSF ;

WORD_NWS : WORDF (~ AWSF)* ;
NWS : (~ AWSF)+ {debug("mateched nws " + $text);} ;
