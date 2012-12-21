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
            STAR = '*';
            PLUS = '+';
            MINUS = '-';
            EQ = '=';
        }

@members {
    public boolean notNewlineP() {
        if (input.LT(1) == null) {
            return true;
        }

        int type = input.LT(1).getType();
        System.out.println("NL? " + input.LT(1));
        return ! (type == NL || type == DOUBLE_NL);
    }
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
  protected boolean assertIsKeyword = true;

  boolean looking_backat_white_space() {
      return true;
  }

  boolean looking_at_white_space() {
      return true;
  }
}



orgFile : block* ;

block :
        header
    |   exampleParagraph
    |   codeParagraph
    |   normalParagraph
    |   NL
    |   DOUBLE_NL

    ;
header : {input.LT(1).getCharPositionInLine() == 0}? STAR+ notNL+ (NL|DOUBLE_NL) {System.out.println("got a header " + $text);};

exampleParagraph : BEGIN_EXAMPLE normalParagraph END_EXAMPLE ;

codeParagraph : BEGIN_SRC WORD normalParagraph END_SRC ;


normalParagraph : (lineText NL)+ ;

lineText : textItem+;

textItem : normalText;


notNL : ~(NL|DOUBLE_NL) ;

normalText : notNL {System.out.println("matched normal text: " + $text);} ;

WORD : ('a'..'z'|'A'..'Z')+ ;

BEGIN_EXAMPLE : '#+begin_example' ;

END_EXAMPLE : '#+end_example' ;

BEGIN_SRC : '#+begin_src' ;
END_SRC : '#+end_src' ;

DOUBLE_NL : '\n' ( (' ' | '\f' | '\r' | '\t')* '\n' )+ ;
NL : '\n' ;

WS :   ( ' ' | '\t' | '\r' )+ {$channel = HIDDEN;} ;


/* start code-generator
   cat << EOF | perl -npe "s#: '(.)'#: {looking_backat_white_space()}? '\$1' ~('\$1' | ' ' | '\\\\t' | '\\\\r' | '\\\\n') .* ~(' ' | '\\\\t' | '\\\\r' | '\\\\n') '\$1'+ {looking_at_white_space()}? {System.out.println(\"matched inline: \" + \\\$text);};#"
BOLD_INLINE : '*'
UNDERLINED_INLINE : '_'
CODE_INLINE : '='
VERBATIM_INLINE : '~'
STRIKE_INLINE : '+'
ITALIC_INLINE : '/'   
EOF
   end code-generator */
// start generated code
BOLD_INLINE : {looking_backat_white_space()}? '*' ~('*' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '*'+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};
UNDERLINED_INLINE : {looking_backat_white_space()}? '_' ~('_' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '_'+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};
CODE_INLINE : {looking_backat_white_space()}? '=' ~('=' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '='+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};
VERBATIM_INLINE : {looking_backat_white_space()}? '~' ~('~' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '~'+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};
STRIKE_INLINE : {looking_backat_white_space()}? '+' ~('+' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '+'+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};
ITALIC_INLINE : {looking_backat_white_space()}? '/' ~('/' | ' ' | '\t' | '\r' | '\n') .* ~(' ' | '\t' | '\r' | '\n') '/'+ {looking_at_white_space()}? {System.out.println("matched inline: " + $text);};

// end generated code

LINK_URL : '[[' ~('['|']')* ']]' {System.out.println("matched link url: " + $text);} ;
LINK_URL_DESC : '[[' ~('['|']')* '][' ~('['|']')* ']]' {System.out.println("matched link desc: " + $text);} ;
