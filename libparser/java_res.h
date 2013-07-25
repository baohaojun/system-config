/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: gperf --language=ANSI-C --struct-type --slot-name=name --hash-fn-name=java_hash --lookup-fn-name=java_lookup  */
/* Computed positions: -k'1-2' */

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

#define JAVA_ABSTRACT	2001
#define JAVA_BOOLEAN	2002
#define JAVA_BREAK	2003
#define JAVA_BYTE	2004
#define JAVA_CASE	2005
#define JAVA_CATCH	2006
#define JAVA_CHAR	2007
#define JAVA_CLASS	2008
#define JAVA_CONST	2009
#define JAVA_CONTINUE	2010
#define JAVA_DEFAULT	2011
#define JAVA_DO	2012
#define JAVA_DOUBLE	2013
#define JAVA_ELSE	2014
#define JAVA_ENUM	2015
#define JAVA_EXTENDS	2016
#define JAVA_FALSE	2017
#define JAVA_FINAL	2018
#define JAVA_FINALLY	2019
#define JAVA_FLOAT	2020
#define JAVA_FOR	2021
#define JAVA_GOTO	2022
#define JAVA_IF	2023
#define JAVA_IMPLEMENTS	2024
#define JAVA_IMPORT	2025
#define JAVA_INSTANCEOF	2026
#define JAVA_INT	2027
#define JAVA_INTERFACE	2028
#define JAVA_LONG	2029
#define JAVA_NATIVE	2030
#define JAVA_NEW	2031
#define JAVA_NULL	2032
#define JAVA_PACKAGE	2033
#define JAVA_PRIVATE	2034
#define JAVA_PROTECTED	2035
#define JAVA_PUBLIC	2036
#define JAVA_RETURN	2037
#define JAVA_SHORT	2038
#define JAVA_STATIC	2039
#define JAVA_STRICTFP	2040
#define JAVA_SUPER	2041
#define JAVA_SWITCH	2042
#define JAVA_SYNCHRONIZED	2043
#define JAVA_THIS	2044
#define JAVA_THROW	2045
#define JAVA_THROWS	2046
#define JAVA_UNION	2047
#define JAVA_TRANSIENT	2048
#define JAVA_TRUE	2049
#define JAVA_TRY	2050
#define JAVA_VOID	2051
#define JAVA_VOLATILE	2052
#define JAVA_WHILE	2053
#define JAVA_WIDEFP	2054
struct keyword { char *name; int token; };

#define TOTAL_KEYWORDS 54
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 82
/* maximum key range = 79, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
java_hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 25, 15, 30,
      50, 25, 20, 35,  0, 10, 83, 83, 40, 50,
      10,  5, 10, 83,  5,  5,  0,  5, 55, 40,
      15, 35, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83
    };
  return len + asso_values[(unsigned char)str[1]] + asso_values[(unsigned char)str[0]];
}

#ifdef __GNUC__
__inline
#ifdef __GNUC_STDC_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
struct keyword *
java_lookup (register const char *str, register unsigned int len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""}, {""}, {""},
      {"this", JAVA_THIS},
      {"throw", JAVA_THROW},
      {"throws", JAVA_THROWS},
      {""},
      {"try", JAVA_TRY},
      {"true", JAVA_TRUE},
      {"short", JAVA_SHORT},
      {"static", JAVA_STATIC},
      {""},
      {"strictfp", JAVA_STRICTFP},
      {"transient", JAVA_TRANSIENT},
      {"super", JAVA_SUPER},
      {""}, {""}, {""},
      {"null", JAVA_NULL},
      {"union", JAVA_UNION},
      {"public", JAVA_PUBLIC},
      {"private", JAVA_PRIVATE},
      {"int", JAVA_INT},
      {"protected", JAVA_PROTECTED},
      {"break", JAVA_BREAK},
      {""},
      {"boolean", JAVA_BOOLEAN},
      {"for", JAVA_FOR},
      {"interface", JAVA_INTERFACE},
      {"instanceof", JAVA_INSTANCEOF},
      {""},
      {"if", JAVA_IF},
      {""},
      {"char", JAVA_CHAR},
      {"final", JAVA_FINAL},
      {"return", JAVA_RETURN},
      {"finally", JAVA_FINALLY},
      {"new", JAVA_NEW},
      {"enum", JAVA_ENUM},
      {"const", JAVA_CONST},
      {"native", JAVA_NATIVE},
      {"package", JAVA_PACKAGE},
      {"continue", JAVA_CONTINUE},
      {"goto", JAVA_GOTO},
      {"while", JAVA_WHILE},
      {""},
      {"extends", JAVA_EXTENDS},
      {"abstract", JAVA_ABSTRACT},
      {"long", JAVA_LONG},
      {"false", JAVA_FALSE},
      {"switch", JAVA_SWITCH},
      {"synchronized", JAVA_SYNCHRONIZED},
      {""},
      {"byte", JAVA_BYTE},
      {""},
      {"widefp", JAVA_WIDEFP},
      {"do", JAVA_DO},
      {""},
      {"case", JAVA_CASE},
      {"catch", JAVA_CATCH},
      {"double", JAVA_DOUBLE},
      {""}, {""},
      {"void", JAVA_VOID},
      {"float", JAVA_FLOAT},
      {"import", JAVA_IMPORT},
      {""},
      {"volatile", JAVA_VOLATILE},
      {"else", JAVA_ELSE},
      {"implements", JAVA_IMPLEMENTS},
      {""}, {""}, {""}, {""},
      {"class", JAVA_CLASS},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"default", JAVA_DEFAULT}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = java_hash (str, len);

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
java_reserved_word(const char *str, int len)
{
	struct keyword *keyword;

	keyword = java_lookup(str, len);
	return (keyword && IS_RESERVED_WORD(keyword->token)) ? keyword->token : 0;
}
