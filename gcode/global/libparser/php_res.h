/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: gperf --language=ANSI-C --struct-type --slot-name=name --hash-fn-name=php_hash --lookup-fn-name=php_lookup  */
/* Computed positions: -k'1-2,4-6,9' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif


#include "strmake.h"
#define START_VARIABLE	1001
#define START_WORD	2001
#define START_SHARP	3001
#define START_YACC	4001
#define IS_RESERVED_WORD(a)	((a) >= START_WORD)
#define IS_RESERVED_VARIABLE(a)	((a) >= START_VARIABLE && (a) < START_WORD)
#define IS_RESERVED_SHARP(a)	((a) >= START_SHARP && (a) < START_YACC)
#define IS_RESERVED_YACC(a)	((a) >= START_YACC)

#define PHP___FILE__	2001
#define PHP___LINE__	2002
#define PHP___FUNCTION__	2003
#define PHP___CLASS__	2004
#define PHP_AND	2005
#define PHP_ARRAY	2006
#define PHP_AS	2007
#define PHP_BREAK	2008
#define PHP_CASE	2009
#define PHP_CFUNCTION	2010
#define PHP_CLASS	2011
#define PHP_CONST	2012
#define PHP_CONTINUE	2013
#define PHP_DECLARE	2014
#define PHP_DEFAULT	2015
#define PHP_DEFINE	2016
#define PHP_DO	2017
#define PHP_DIE	2018
#define PHP_EACH	2019
#define PHP_ECHO	2020
#define PHP_ELSE	2021
#define PHP_ELSEIF	2022
#define PHP_EMPTY	2023
#define PHP_ENDDECLARE	2024
#define PHP_ENDFOR	2025
#define PHP_ENDFOREACH	2026
#define PHP_ENDIF	2027
#define PHP_ENDWHILE	2028
#define PHP_ENDSWITCH	2029
#define PHP_EVAL	2030
#define PHP_EXIT	2031
#define PHP_FAILURE	2032
#define PHP_FALSE	2033
#define PHP_FOR	2034
#define PHP_FOREACH	2035
#define PHP_FUNCTION	2036
#define PHP_GLOBAL	2037
#define PHP_HEADER	2038
#define PHP_LIST	2039
#define PHP_IF	2040
#define PHP_INCLUDE	2041
#define PHP_INCLUDE_ONCE	2042
#define PHP_IS_ARRAY	2043
#define PHP_IS_SET	2044
#define PHP_NEW	2045
#define PHP_OLD_FUNCTION	2046
#define PHP_OR	2047
#define PHP_PRINT	2048
#define PHP_PRINTF	2049
#define PHP_RETURN	2050
#define PHP_REQUIRE	2051
#define PHP_REQUIRE_ONCE	2052
#define PHP_SETCOOKIE	2053
#define PHP_SUCCESS	2054
#define PHP_STATIC	2055
#define PHP_SWITCH	2056
#define PHP_TRUE	2057
#define PHP_VAR	2058
#define PHP_WHILE	2059
#define PHP_UNSET	2060
#define PHP_XOR	2061
#define PHP_GLOBALS	1001
#define PHP_HTTP_COOKIE_VARS	1002
#define PHP_HTTP_ENV_VARS	1003
#define PHP_HTTP_GET_VARS	1004
#define PHP_HTTP_POST_FILES	1005
#define PHP_HTTP_POST_VARS	1006
#define PHP_HTTP_SERVER_VARS	1007
#define PHP_HTTP_SESSION_VARS	1008
#define PHP__COOKIE	1009
#define PHP__ENV	1010
#define PHP__FILES	1011
#define PHP__GET	1012
#define PHP__POST	1013
#define PHP__REQUEST	1014
#define PHP__SERVER	1015
#define PHP__SESSION	1016
struct keyword { char *name; int token; };

#define TOTAL_KEYWORDS 206
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 17
#define MIN_HASH_VALUE 2
#define MAX_HASH_VALUE 602
/* maximum key range = 601, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
php_hash (register const char *str, register unsigned int len)
{
  static unsigned short asso_values[] =
    {
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603,  70,  30,  40,  90,  50,
       65,  15,  30, 115, 603,   5,  40,   0,  30,  40,
       70,   0, 165,  50,  10,  55,  65,  35, 150,   0,
      603, 603, 603, 603, 603,  55, 603,  30,  35,   0,
       15,   5,   0,   5,  35,   0, 603,  40,  30,  95,
        0,   0,  65, 603,  10,   0,  15,  25, 110, 140,
      115,  10, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603, 603, 603, 603, 603,
      603, 603, 603, 603, 603, 603
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[8]];
      /*FALLTHROUGH*/
      case 8:
      case 7:
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      /*FALLTHROUGH*/
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      /*FALLTHROUGH*/
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      /*FALLTHROUGH*/
      case 3:
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval;
}

#ifdef __GNUC__
__inline
#ifdef __GNUC_STDC_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
struct keyword *
php_lookup (register const char *str, register unsigned int len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""},
      {"if", PHP_IF},
      {"for", PHP_FOR},
      {""}, {""}, {""}, {""},
      {"new", PHP_NEW},
      {"echo", PHP_ECHO},
      {"endif", PHP_ENDIF},
      {""},
      {"or", PHP_OR},
      {""}, {""}, {""}, {""},
      {"do", PHP_DO},
      {"die", PHP_DIE},
      {"setcookie", PHP_SETCOOKIE},
      {"const", PHP_CONST},
      {"endfor", PHP_ENDFOR},
      {""},
      {"continue", PHP_CONTINUE},
      {"cfunction", PHP_CFUNCTION},
      {"endforeach", PHP_ENDFOREACH},
      {"is_set", PHP_IS_SET},
      {""}, {""},
      {"True", PHP_TRUE},
      {""},
      {"define", PHP_DEFINE},
      {"as", PHP_AS},
      {"and", PHP_AND},
      {"true", PHP_TRUE},
      {"class", PHP_CLASS},
      {"static", PHP_STATIC},
      {"success", PHP_SUCCESS},
      {"New", PHP_NEW},
      {"case", PHP_CASE},
      {"false", PHP_FALSE},
      {""},
      {"foreach", PHP_FOREACH},
      {""},
      {"else", PHP_ELSE},
      {"enddeclare", PHP_ENDDECLARE},
      {"elseif", PHP_ELSEIF},
      {""},
      {"function", PHP_FUNCTION},
      {"list", PHP_LIST},
      {"unset", PHP_UNSET},
      {""},
      {"Or", PHP_OR},
      {""},
      {"Echo", PHP_ECHO},
      {"Endif", PHP_ENDIF},
      {"return", PHP_RETURN},
      {"require", PHP_REQUIRE},
      {"is_array", PHP_IS_ARRAY},
      {"List", PHP_LIST},
      {"Const", PHP_CONST},
      {""},
      {"require_once", PHP_REQUIRE_ONCE},
      {"Continue", PHP_CONTINUE},
      {"Cfunction", PHP_CFUNCTION},
      {"EMPTY", PHP_EMPTY},
      {"Endfor", PHP_ENDFOR},
      {""},
      {"For", PHP_FOR},
      {"Setcookie", PHP_SETCOOKIE},
      {"Endforeach", PHP_ENDFOREACH},
      {"Header", PHP_HEADER},
      {"As", PHP_AS},
      {"And", PHP_AND},
      {"each", PHP_EACH},
      {"Class", PHP_CLASS},
      {"header", PHP_HEADER},
      {"include", PHP_INCLUDE},
      {""},
      {"Case", PHP_CASE},
      {"Unset", PHP_UNSET},
      {""},
      {"include_once", PHP_INCLUDE_ONCE},
      {"NEW", PHP_NEW},
      {"_GET", PHP__GET},
      {"array", PHP_ARRAY},
      {"Static", PHP_STATIC},
      {"Success", PHP_SUCCESS},
      {""},
      {"Else", PHP_ELSE},
      {"Enddeclare", PHP_ENDDECLARE},
      {"Elseif", PHP_ELSEIF},
      {"Do", PHP_DO},
      {"Die", PHP_DIE},
      {""},
      {"print", PHP_PRINT},
      {"printf", PHP_PRINTF},
      {"declare", PHP_DECLARE},
      {"Var", PHP_VAR},
      {""},
      {"Print", PHP_PRINT},
      {"Printf", PHP_PRINTF},
      {"failure", PHP_FAILURE},
      {"AND", PHP_AND},
      {""},
      {"False", PHP_FALSE},
      {"Define", PHP_DEFINE},
      {"Foreach", PHP_FOREACH},
      {"FOR", PHP_FOR},
      {"SetCookie", PHP_SETCOOKIE},
      {"While", PHP_WHILE},
      {""},
      {"default", PHP_DEFAULT},
      {"Function", PHP_FUNCTION},
      {""},
      {"Break", PHP_BREAK},
      {""},
      {"If", PHP_IF},
      {"xor", PHP_XOR},
      {"Each", PHP_EACH},
      {"break", PHP_BREAK},
      {""},
      {"AS", PHP_AS},
      {""}, {""},
      {"Array", PHP_ARRAY},
      {""}, {""},
      {"EndWhile", PHP_ENDWHILE},
      {"CFunction", PHP_CFUNCTION},
      {"empty", PHP_EMPTY},
      {"EndFor", PHP_ENDFOR},
      {"DO", PHP_DO},
      {""},
      {"ECHO", PHP_ECHO},
      {"EndForeach", PHP_ENDFOREACH},
      {"global", PHP_GLOBAL},
      {"old_function", PHP_OLD_FUNCTION},
      {"VAR", PHP_VAR},
      {"exit", PHP_EXIT},
      {""},
      {"Is_set", PHP_IS_SET},
      {""},
      {"var", PHP_VAR},
      {"ELSE", PHP_ELSE},
      {"CONST", PHP_CONST},
      {"Global", PHP_GLOBAL},
      {""}, {""},
      {"eval", PHP_EVAL},
      {"UNSET", PHP_UNSET},
      {""},
      {"ForEach", PHP_FOREACH},
      {"Xor", PHP_XOR},
      {"EACH", PHP_EACH},
      {""}, {""}, {""}, {""},
      {"EVAL", PHP_EVAL},
      {"WHILE", PHP_WHILE},
      {""}, {""}, {""},
      {"CASE", PHP_CASE},
      {"EndDeclare", PHP_ENDDECLARE},
      {""},
      {"Failure", PHP_FAILURE},
      {""},
      {"LIST", PHP_LIST},
      {"EndIf", PHP_ENDIF},
      {"SWITCH", PHP_SWITCH},
      {"Declare", PHP_DECLARE},
      {"Is_array", PHP_IS_ARRAY},
      {"_ENV", PHP__ENV},
      {"Empty", PHP_EMPTY},
      {""},
      {"Old_function", PHP_OLD_FUNCTION},
      {""}, {""}, {""}, {""},
      {"IF", PHP_IF},
      {""},
      {"Exit", PHP_EXIT},
      {"CLASS", PHP_CLASS},
      {""},
      {"Default", PHP_DEFAULT},
      {"endwhile", PHP_ENDWHILE},
      {"endswitch", PHP_ENDSWITCH},
      {"_POST", PHP__POST},
      {"Is_Set", PHP_IS_SET},
      {"Include", PHP_INCLUDE},
      {"XOR", PHP_XOR},
      {"Eval", PHP_EVAL},
      {""},
      {"switch", PHP_SWITCH},
      {"Include_once", PHP_INCLUDE_ONCE},
      {""}, {""}, {""},
      {"GLOBAL", PHP_GLOBAL},
      {"GLOBALS", PHP_GLOBALS},
      {""}, {""}, {""},
      {"ElseIf", PHP_ELSEIF},
      {"OR", PHP_OR},
      {"DIE", PHP_DIE},
      {""}, {""},
      {"Return", PHP_RETURN},
      {"Require", PHP_REQUIRE},
      {"Is_Array", PHP_IS_ARRAY},
      {"EXIT", PHP_EXIT},
      {"while", PHP_WHILE},
      {""},
      {"Require_once", PHP_REQUIRE_ONCE},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"CFUNCTION", PHP_CFUNCTION},
      {""},
      {"HTTP_COOKIE_VARS", PHP_HTTP_COOKIE_VARS},
      {""}, {""},
      {"TRUE", PHP_TRUE},
      {""},
      {"STATIC", PHP_STATIC},
      {""},
      {"Endwhile", PHP_ENDWHILE},
      {"Endswitch", PHP_ENDSWITCH},
      {""}, {""},
      {"Include_Once", PHP_INCLUDE_ONCE},
      {""}, {""},
      {"FALSE", PHP_FALSE},
      {""},
      {"Old_Function", PHP_OLD_FUNCTION},
      {"CONTINUE", PHP_CONTINUE},
      {""}, {""},
      {"Switch", PHP_SWITCH},
      {""},
      {"HTTP_GET_VARS", PHP_HTTP_GET_VARS},
      {""}, {""}, {""},
      {"SUCCESS", PHP_SUCCESS},
      {""}, {""}, {""}, {""},
      {"Require_Once", PHP_REQUIRE_ONCE},
      {""},
      {"HTTP_POST_VARS", PHP_HTTP_POST_VARS},
      {"HTTP_POST_FILES", PHP_HTTP_POST_FILES},
      {""},
      {"_COOKIE", PHP__COOKIE},
      {""}, {""},
      {"ENDIF", PHP_ENDIF},
      {"_FILES", PHP__FILES},
      {""},
      {"ENDWHILE", PHP_ENDWHILE},
      {""}, {""}, {""},
      {"FOREACH", PHP_FOREACH},
      {""}, {""},
      {"BREAK", PHP_BREAK},
      {""},
      {"OLD_FUNCTION", PHP_OLD_FUNCTION},
      {""},
      {"SETCOOKIE", PHP_SETCOOKIE},
      {"PRINT", PHP_PRINT},
      {"IS_SET", PHP_IS_SET},
      {"HTTP_SESSION_VARS", PHP_HTTP_SESSION_VARS},
      {"HTTP_ENV_VARS", PHP_HTTP_ENV_VARS},
      {"EndSwitch", PHP_ENDSWITCH},
      {""}, {""},
      {"__FUNCTION__", PHP___FUNCTION__},
      {""}, {""}, {""}, {""}, {""},
      {"FUNCTION", PHP_FUNCTION},
      {""}, {""},
      {"HTTP_SERVER_VARS", PHP_HTTP_SERVER_VARS},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
      {"ARRAY", PHP_ARRAY},
      {""},
      {"DEFAULT", PHP_DEFAULT},
      {"__LINE__", PHP___LINE__},
      {""}, {""}, {""}, {""}, {""},
      {"ENDSWITCH", PHP_ENDSWITCH},
      {""}, {""}, {""},
      {"__FILE__", PHP___FILE__},
      {""}, {""},
      {"ELSEIF", PHP_ELSEIF},
      {""},
      {"_SESSION", PHP__SESSION},
      {""}, {""}, {""}, {""},
      {"_REQUEST", PHP__REQUEST},
      {"__CLASS__", PHP___CLASS__},
      {""}, {""},
      {"INCLUDE", PHP_INCLUDE},
      {""}, {""}, {""},
      {"DEFINE", PHP_DEFINE},
      {""}, {""}, {""}, {""},
      {"PRINTF", PHP_PRINTF},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"ENDFOR", PHP_ENDFOR},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"INCLUDE_ONCE", PHP_INCLUDE_ONCE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"HEADER", PHP_HEADER},
      {"_SERVER", PHP__SERVER},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"ENDFOREACH", PHP_ENDFOREACH},
      {""},
      {"FAILURE", PHP_FAILURE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
      {"DECLARE", PHP_DECLARE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
      {"ENDDECLARE", PHP_ENDDECLARE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"RETURN", PHP_RETURN},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
      {"REQUIRE", PHP_REQUIRE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"IS_ARRAY", PHP_IS_ARRAY},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
      {"REQUIRE_ONCE", PHP_REQUIRE_ONCE}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = php_hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}

int
php_reserved_word(const char *str, int len)
{
	struct keyword *keyword;

	keyword = php_lookup(str, len);
	return (keyword && IS_RESERVED_WORD(keyword->token)) ? keyword->token : 0;
}
int
php_reserved_variable(const char *str, int len)
{
	struct keyword *keyword;

	keyword = php_lookup(str, len);
	return (keyword && IS_RESERVED_VARIABLE(keyword->token)) ? keyword->token : 0;
}
