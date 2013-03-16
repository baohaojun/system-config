/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: gperf --language=ANSI-C --struct-type --slot-name=name --hash-fn-name=c_hash --lookup-fn-name=c_lookup  */
/* Computed positions: -k'1-2,4,6-9,12,14' */

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

#define C___P	2001
#define C___ATTRIBUTE__	2002
#define C___EXTENSION__	2003
#define C___THREAD	2004
#define C_ASM	2005
#define C_CONST	2006
#define C_INLINE	2007
#define C_RESTRICT	2008
#define C_SIGNED	2009
#define C_VOLATILE	2010
#define C__BOOL	2011
#define C__COMPLEX	2012
#define C__IMAGINARY	2013
#define C_AUTO	2014
#define C_BREAK	2015
#define C_CASE	2016
#define C_CHAR	2017
#define C_CONTINUE	2018
#define C_DEFAULT	2019
#define C_DO	2020
#define C_DOUBLE	2021
#define C_ELSE	2022
#define C_ENUM	2023
#define C_EXTERN	2024
#define C_FLOAT	2025
#define C_FOR	2026
#define C_GOTO	2027
#define C_IF	2028
#define C_INT	2029
#define C_LONG	2030
#define C_REGISTER	2031
#define C_RETURN	2032
#define C_SHORT	2033
#define C_SIZEOF	2034
#define C_STATIC	2035
#define C_STRUCT	2036
#define C_SWITCH	2037
#define C_TYPEDEF	2038
#define C_UNION	2039
#define C_UNSIGNED	2040
#define C_VOID	2041
#define C_WHILE	2042
#define SHARP_SHARP	3001
#define SHARP_ASSERT	3002
#define SHARP_DEFINE	3003
#define SHARP_ELIF	3004
#define SHARP_ELSE	3005
#define SHARP_ENDIF	3006
#define SHARP_ERROR	3007
#define SHARP_IDENT	3008
#define SHARP_IF	3009
#define SHARP_IFDEF	3010
#define SHARP_IFNDEF	3011
#define SHARP_IMPORT	3012
#define SHARP_INCLUDE	3013
#define SHARP_INCLUDE_NEXT	3014
#define SHARP_LINE	3015
#define SHARP_PRAGMA	3016
#define SHARP_SCCS	3017
#define SHARP_UNASSERT	3018
#define SHARP_UNDEF	3019
#define SHARP_WARNING	3020
#define YACC_SEP	4001
#define YACC_DEBUG	4002
#define YACC_DEFAULT_PREC	4003
#define YACC_DEFINE	4004
#define YACC_DEFINES	4005
#define YACC_DESTRUCTOR	4006
#define YACC_DPREC	4007
#define YACC_ERROR_VERBOSE	4008
#define YACC_EXPECT	4009
#define YACC_EXPECT_RR	4010
#define YACC_FILE_PREFIX	4011
#define YACC_GLR_PARSER	4012
#define YACC_INITIAL_ACTION	4013
#define YACC_LEFT	4014
#define YACC_LEX_PARAM	4015
#define YACC_LOCATIONS	4016
#define YACC_MERGE	4017
#define YACC_NAME_PREFIX	4018
#define YACC_NO_DEFAULT_PREC	4019
#define YACC_NO_LINES	4020
#define YACC_NONASSOC	4021
#define YACC_NONDETERMINISTIC_PARSER	4022
#define YACC_NTERM	4023
#define YACC_OUTPUT	4024
#define YACC_PARSE_PARAM	4025
#define YACC_PREC	4026
#define YACC_PRINTER	4027
#define YACC_PURE_PARSER	4028
#define YACC_REQUIRE	4029
#define YACC_RIGHT	4030
#define YACC_SKELETON	4031
#define YACC_START	4032
#define YACC_TOKEN	4033
#define YACC_TOKEN_TABLE	4034
#define YACC_TYPE	4035
#define YACC_UNION	4036
#define YACC_VERBOSE	4037
#define YACC_YACC	4038
#define YACC_BEGIN	4039
#define YACC_END	4040
struct keyword { char *name; int token; };

#define TOTAL_KEYWORDS 131
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 24
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 352
/* maximum key range = 350, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
c_hash (register const char *str, register unsigned int len)
{
  static unsigned short asso_values[] =
    {
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353,  10, 353,  25, 353, 353,
      353, 353, 353, 353, 353,  10,  55, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353,   5,   0, 353, 353,
      353, 353, 353,   0, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353,   0,  30,   0,  80,  30,
       10,   0,   5,  50,  65,   5, 353,   5, 105, 115,
        0,  30,  75,   0,  50,  30,   5,  25,   0,   0,
       15,  65, 353,  50, 353,  35, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353, 353, 353, 353,
      353, 353, 353, 353, 353, 353, 353
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[13]+1];
      /*FALLTHROUGH*/
      case 13:
      case 12:
        hval += asso_values[(unsigned char)str[11]+1];
      /*FALLTHROUGH*/
      case 11:
      case 10:
      case 9:
        hval += asso_values[(unsigned char)str[8]];
      /*FALLTHROUGH*/
      case 8:
        hval += asso_values[(unsigned char)str[7]];
      /*FALLTHROUGH*/
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      /*FALLTHROUGH*/
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      /*FALLTHROUGH*/
      case 5:
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
c_lookup (register const char *str, register unsigned int len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""}, {""},
      {"__P", C___P},
      {""}, {""}, {""}, {""},
      {"int", C_INT},
      {""}, {""}, {""},
      {"if", C_IF},
      {"__inline", C_INLINE},
      {""},
      {"__inline__", C_INLINE},
      {"inline", C_INLINE},
      {""},
      {"#if", SHARP_IF},
      {""},
      {"#elif", SHARP_ELIF},
      {"extern", C_EXTERN},
      {"##", SHARP_SHARP},
      {"__signed", C_SIGNED},
      {""},
      {"__signed__", C_SIGNED},
      {"#ident", SHARP_IDENT},
      {"#ifndef", SHARP_IFNDEF},
      {""}, {""}, {""},
      {"#endif", SHARP_ENDIF},
      {"#define", SHARP_DEFINE},
      {"asm", C_ASM},
      {"case", C_CASE},
      {"__asm", C_ASM},
      {"#ifdef", SHARP_IFDEF},
      {"__asm__", C_ASM},
      {"for", C_FOR},
      {""},
      {"_Bool", C__BOOL},
      {"%token", YACC_TOKEN},
      {"do", C_DO},
      {""},
      {"void", C_VOID},
      {"#else", SHARP_ELSE},
      {"sizeof", C_SIZEOF},
      {"%define", YACC_DEFINE},
      {"unsigned", C_UNSIGNED},
      {""}, {""},
      {"signed", C_SIGNED},
      {"%%", YACC_SEP},
      {""}, {""}, {""},
      {"#undef", SHARP_UNDEF},
      {"%token_table", YACC_TOKEN_TABLE},
      {""},
      {"auto", C_AUTO},
      {"union", C_UNION},
      {"%union", YACC_UNION},
      {"%}", YACC_END},
      {""}, {""},
      {"_Imaginary", C__IMAGINARY},
      {"%start", YACC_START},
      {"%token-table", YACC_TOKEN_TABLE},
      {""},
      {"%no_lines", YACC_NO_LINES},
      {""},
      {"struct", C_STRUCT},
      {"__const", C_CONST},
      {""},
      {"__const__", C_CONST},
      {"#sccs", SHARP_SCCS},
      {"static", C_STATIC},
      {"%{", YACC_BEGIN},
      {"%defines", YACC_DEFINES},
      {"%no-lines", YACC_NO_LINES},
      {""},
      {"return", C_RETURN},
      {"typedef", C_TYPEDEF},
      {"__thread", C___THREAD},
      {""},
      {"%term", YACC_TOKEN},
      {""}, {""},
      {"#include", SHARP_INCLUDE},
      {""}, {""}, {""}, {""},
      {"__extension__", C___EXTENSION__},
      {""},
      {"const", C_CONST},
      {""},
      {"%output", YACC_OUTPUT},
      {"continue", C_CONTINUE},
      {"%skeleton", YACC_SKELETON},
      {"__restrict", C_RESTRICT},
      {""},
      {"#assert", SHARP_ASSERT},
      {"restrict", C_RESTRICT},
      {""},
      {"%prec", YACC_PREC},
      {"switch", C_SWITCH},
      {""}, {""},
      {"else", C_ELSE},
      {"%type", YACC_TYPE},
      {""}, {""}, {""},
      {"goto", C_GOTO},
      {"float", C_FLOAT},
      {"#error", SHARP_ERROR},
      {""},
      {"register", C_REGISTER},
      {"enum", C_ENUM},
      {"#line", SHARP_LINE},
      {"%dprec", YACC_DPREC},
      {""},
      {"#warning", SHARP_WARNING},
      {""},
      {"%yacc", YACC_YACC},
      {"double", C_DOUBLE},
      {"default", C_DEFAULT},
      {""},
      {"#unassert", SHARP_UNASSERT},
      {""},
      {"%no_default_prec", YACC_NO_DEFAULT_PREC},
      {"__restrict__", C_RESTRICT},
      {""}, {""},
      {"break", C_BREAK},
      {"%right", YACC_RIGHT},
      {""},
      {"%require", YACC_REQUIRE},
      {"%nondeterministic-parser", YACC_NONDETERMINISTIC_PARSER},
      {"%left", YACC_LEFT},
      {"%no-default_prec", YACC_NO_DEFAULT_PREC},
      {"%expect", YACC_EXPECT},
      {"%verbose", YACC_VERBOSE},
      {""}, {""},
      {"%nterm", YACC_NTERM},
      {""},
      {"volatile", C_VOLATILE},
      {"char", C_CHAR},
      {"short", C_SHORT},
      {""},
      {"#import", SHARP_IMPORT},
      {""},
      {"%nonassoc", YACC_NONASSOC},
      {"__volatile", C_VOLATILE},
      {"%no_default-prec", YACC_NO_DEFAULT_PREC},
      {""},
      {"#include_next", SHARP_INCLUDE_NEXT},
      {"%fixed_output_files", YACC_YACC},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"%no-default-prec", YACC_NO_DEFAULT_PREC},
      {""},
      {"%printer", YACC_PRINTER},
      {"%fixed-output_files", YACC_YACC},
      {""},
      {"%debug", YACC_DEBUG},
      {""}, {""}, {""},
      {"while", C_WHILE},
      {"__attribute", C___ATTRIBUTE__},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%fixed_output-files", YACC_YACC},
      {""},
      {"%destructor", YACC_DESTRUCTOR},
      {"__volatile__", C_VOLATILE},
      {""},
      {"long", C_LONG},
      {""}, {""}, {""},
      {"%default_prec", YACC_DEFAULT_PREC},
      {"%fixed-output-files", YACC_YACC},
      {"%expect_rr", YACC_EXPECT_RR},
      {"%merge", YACC_MERGE},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"%default-prec", YACC_DEFAULT_PREC},
      {""},
      {"%expect-rr", YACC_EXPECT_RR},
      {""},
      {"#pragma", SHARP_PRAGMA},
      {"__attribute__", C___ATTRIBUTE__},
      {""},
      {"%locations", YACC_LOCATIONS},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%error_verbose", YACC_ERROR_VERBOSE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%binary", YACC_NONASSOC},
      {""},
      {"%error-verbose", YACC_ERROR_VERBOSE},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
      {"_Complex", C__COMPLEX},
      {""}, {""}, {""},
      {"%parse-param", YACC_PARSE_PARAM},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
      {"%initial-action", YACC_INITIAL_ACTION},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%lex-param", YACC_LEX_PARAM},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
      {"%glr-parser", YACC_GLR_PARSER},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%pure_parser", YACC_PURE_PARSER},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"%pure-parser", YACC_PURE_PARSER},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
      {"%name_prefix", YACC_NAME_PREFIX},
      {""}, {""}, {""}, {""},
      {"%file-prefix", YACC_FILE_PREFIX},
      {""}, {""}, {""}, {""},
      {"%name-prefix", YACC_NAME_PREFIX}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = c_hash (str, len);

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
c_reserved_word(const char *str, int len)
{
	struct keyword *keyword;

	keyword = c_lookup(str, len);
	return (keyword && IS_RESERVED_WORD(keyword->token)) ? keyword->token : 0;
}
int
c_reserved_sharp(const char *str, int len)
{
	struct keyword *keyword;

	/* Delete blanks. Ex. ' # define ' => '#define' */
	str = strtrim(str, TRIM_ALL, &len);

	keyword = c_lookup(str, len);
	return (keyword && IS_RESERVED_SHARP(keyword->token)) ? keyword->token : 0;
}
int
c_reserved_yacc(const char *str, int len)
{
	struct keyword *keyword;

	keyword = c_lookup(str, len);
	return (keyword && IS_RESERVED_YACC(keyword->token)) ? keyword->token : 0;
}
