lexer grammar BeagleTokens;

fragment
Letter: 'a'..'z'|'A'..'Z';

fragment
Digit: '0'..'9';

fragment
Alphanum : (Letter|Digit)+ ;

fragment
Alpha : (Letter)+ ;

fragment
Has_Digit 
    : (Letter)* Digit (Letter|Digit)*
    ;

fragment
Sep 
    : '_'|'-'|'/'|'.'|','
    ;

ACRONYM : Alphanum ('.' Alphanum)+ ;

ALPHA : Alpha;

ALPHANUM : Alphanum ;



COMPANY : Alpha ('&'|'@') Alpha ;

NUM 
    : (Alpha Sep Has_Digit
        | Has_Digit Sep Alphanum
        | Alpha (Sep Has_Digit Sep Alphanum)+
        | Has_Digit (Sep Alphanum Sep Has_Digit)+
        | Alpha Sep Has_Digit (Sep Alphanum Sep Has_Digit)+
        | Has_Digit Sep Alphanum (Sep Has_Digit Sep Alphanum)+) 
    ;

EMAIL : Alphanum (('.'|'-'|'_') Alphanum)* '@' Alphanum (('.'|'-') Alphanum)+ ;

HOST : Alphanum (('.') Alphanum)+ '.'? ;


APOSTROPHE : Alphanum ('\'' Alphanum)+ ;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n')+
    ;

OTHER : . ;
