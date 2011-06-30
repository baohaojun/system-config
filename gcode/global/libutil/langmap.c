/*
 * Copyright (c) 2002, 2004, 2005, 2008
 *	Tama Communications Corporation
 *
 * This file is part of GNU GLOBAL.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "die.h"
#include "locatestring.h"
#include "strbuf.h"
#include "langmap.h"
#include "strlist.h"
#include "checkalloc.h"
#ifdef DEBUG
#include <assert.h>
#else
#define assert(e)
#endif

#define LANG_AUTO   (-1)
#define LANG_IGNORE (-2)

typedef int langType;

static int match_suffix_list(const char *, const char *);

static STRBUF *active_map;
static STRBUF *active_pattern_map;

/*
 * construct language map.
 *
 * copy string langmap and convert it to language map like this:
 *
 * langmap (string)	"c:.c.h,java:.java,cpp:.C.H,make:([mM]akefile*).mk.make(*.mak),python:(*.py)"
 *	|
 *	v
 * language map		c\0.c.h\0java\0.java\0cpp\0.C.H\0make\0.mk.make\0
 * pattern map      make\0[mM]akefile*\0*.mak\0\0python\0*.py\0
 *                                             ^ 2 consecutive \0 means start of a different language.
 */

#define EXTENSION_SEPARATOR '.'
#define PATTERN_START '('
#define PATTERN_STOP  ')'
#define IGNORE_SEPARATORS   ", \t\n"

#ifndef DEFAULT_FILE_FORMAT
# define DEFAULT_FILE_FORMAT  2
#endif


typedef struct {
	/* defined by parser */
	char* name;                    /* name of language */
	stringList* currentPatterns;   /* current list of file name patterns */
	stringList* currentExtensions; /* current list of extensions */
} parserDefinition;

static parserDefinition** s_lang_table = NULL;
static unsigned int LanguageCount = 0;

static langType getExtensionLanguage (const char *const extension)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const exts = s_lang_table [i]->currentExtensions;
		if (exts != NULL  &&  stringListExtensionMatched (exts, extension))
			result = i;
	}
	return result;
}

#ifndef PATH_SEPARATOR
# if defined (MSDOS_STYLE_PATH)
#  define PATH_SEPARATOR '\\'
# elif defined (QDOS)
#  define PATH_SEPARATOR '_'
# else
#  define PATH_SEPARATOR '/'
# endif
#endif

extern const char *baseFilename (const char *const filePath)
{
#if defined (MSDOS_STYLE_PATH) || defined (VMS)
	const char *tail = NULL;
	unsigned int i;

	/*  Find whichever of the path delimiters is last.
	 */
	for (i = 0  ;  i < strlen (PathDelimiters)  ;  ++i)
	{
		const char *sep = strrchr (filePath, PathDelimiters [i]);

		if (sep > tail)
			tail = sep;
	}
#else
	const char *tail = strrchr (filePath, PATH_SEPARATOR);
#endif
	if (tail == NULL)
		tail = filePath;
	else
		++tail;  /* step past last delimiter */
#ifdef VAXC
	{
		/* remove version number from filename */
		char *p = strrchr ((char *) tail, ';');
		if (p != NULL)
			*p = '\0';
	}
#endif

	return tail;
}

langType getPatternLanguage (const char *const fileName)
{
	langType result = LANG_IGNORE;
	const char* base = baseFilename (fileName);
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const ptrns = s_lang_table [i]->currentPatterns;
		if (ptrns != NULL  &&  stringListFileMatched (ptrns, base))
			result = i;
	}
	return result;
}

static parserDefinition * languageFromIndex(const langType lang)
{
	assert(lang >= 0 && lang < LanguageCount);
	return s_lang_table[lang];
}

static parserDefinition * languageFromName(const char* lang)
{
	unsigned int i;
	for(i = 0; i < LanguageCount; i++) {
		if (!strcmp(lang, s_lang_table[i]->name)) {
			return s_lang_table[i];
		}
	}

	if (s_lang_table == NULL) {
		s_lang_table = xMalloc (1, parserDefinition*);
	} else {
		s_lang_table = xRealloc(s_lang_table, LanguageCount + 1, parserDefinition*);
	}
	
	LanguageCount ++;
	s_lang_table[i] = xCalloc(1, parserDefinition);
	languageFromIndex(i)->name = check_strdup(lang);
	languageFromIndex(i)->currentPatterns = stringListNew ();
	languageFromIndex(i)->currentExtensions = stringListNew ();
	
	return languageFromIndex(i);
}

static char* skipPastMap (char* p)
{
	while (*p != EXTENSION_SEPARATOR  &&
			*p != PATTERN_START  &&  *p != ','  &&  *p != '\0')
		++p;
	return p;
}


extern const char *getLanguageName (const langType language)
{
	const char* result;
	if (language == LANG_IGNORE)
		result = "unknown";
	else
	{
		assert (0 <= language  &&  language < (int) LanguageCount);
		result = s_lang_table [language]->name;
	}
	return result;
}

extern boolean removeLanguageExtensionMap (const char *const extension)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const exts = s_lang_table [i]->currentExtensions;
		if (exts != NULL  &&  stringListRemoveExtension (exts, extension))
		{
			message (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguageExtensionMap (
		const char* language, const char* extension)
{
	vString* const str = vStringNewInit (extension);
	removeLanguageExtensionMap (extension);
	stringListAdd (languageFromName(language)->currentExtensions, str);
}

extern void addLanguagePatternMap (const char* language, const char* ptrn)
{
	vString* const str = vStringNewInit (ptrn);
	parserDefinition* lang;
	lang = languageFromName(language);
	if (lang->currentPatterns == NULL)
		lang->currentPatterns = stringListNew ();
	stringListAdd (lang->currentPatterns, str);
}

static char* addLanguageMap (const char* language, char* map)
{
	char* p = NULL;
	const char first = *map;
	if (first == EXTENSION_SEPARATOR)  /* extension map */
	{
		++map;
		p = skipPastMap (map);
		if (*p == '\0')
		{
			message (" .%s", map);
			addLanguageExtensionMap (language, map);
			p = map + strlen (map);
		}
		else
		{
			const char separator = *p;
			*p = '\0';
			message (" .%s", map);
			addLanguageExtensionMap (language, map);
			*p = separator;
		}
	}
	else if (first == PATTERN_START)  /* pattern map */
	{
		++map;
		for (p = map  ;  *p != PATTERN_STOP  &&  *p != '\0'  ;  ++p)
		{
			if (*p == '\\'  &&  *(p + 1) == PATTERN_STOP)
				++p;
		}
		if (*p == '\0')
			die ("Unterminated file name pattern for %s language", language);
		else
		{
			*p++ = '\0';
			message (" (%s)", map);
			addLanguagePatternMap (language, map);
		}
	}
	else
		die ("Badly formed language map for %s language",
				language);
	return p;
}

static char* processLanguageMap (const char* map)
{
	char* const separator = strchr (map, ':');
	char* result = NULL;
	if (separator != NULL)
	{
		const char* language;
		char *list = separator + 1;
		*separator = '\0';
		language = map;
		if (1)
		{
			const char *const deflt = "default";
			char* p;

			for (p = list  ;  *p != ','  &&  *p != '\0'  ;  ++p)  /*no-op*/ ;

			while (list != NULL  &&  *list != '\0'  &&  *list != ',')
				list = addLanguageMap (language, list);

			if (list != NULL  &&  *list == ',')
				++list;
			result = list;
		}
	}
	return result;
}

static void processLanguageMapOption (const char *const parameter)
{
	char *const maps = check_strdup (parameter);
	char *map = maps;

	while (map != NULL  &&  *map != '\0')
	{
		char* const next = processLanguageMap (map);
		if (next == NULL)
			die_with_code (-2, "Unknown language \"%s\"", parameter);
		map = next;
	}
	check_free (maps);
}

void
setup_langmap(const char *map)
{
	char *p;
	int onsuffix = 0;		/* not on suffix string */

	char * option = check_strdup(map);
	processLanguageMapOption(option);
	check_free(option);

	active_map = strbuf_open(0);
	strbuf_puts(active_map, map);
	for (p = strbuf_value(active_map); *p; p++) {
		/*
		 * "c:.c.h,java:.java,cpp:.C.H"
		 */
		if ((onsuffix == 0 && *p == ',') || (onsuffix == 1 && *p == ':'))
			die_with_code(2, "syntax error in langmap '%s'.", map);
		if (*p == ':' || *p == ',') {
			onsuffix ^= 1;
			*p = '\0';
		}
	}
	if (onsuffix == 0)
		die_with_code(2, "syntax error in langmap '%s'.", map);
	/* strbuf_close(active_map); */
}

/*
 * decide the language of the suffix.
 */
const char *
decide_lang(const char *suffix)
{
	message("decide_lang %s", suffix);
	const char *lang, *list, *tail;

	/*
	 * Though '*.h' files are shared by C and C++, GLOBAL treats them
	 * as C source files by default. If you set an environment variable
	 * 'GTAGS_FORCECPP' then C++ parser will be invoked.
	 */
	if (!strcmp(suffix, ".h") && getenv("GTAGSFORCECPP") != NULL)
		return "cpp";
	lang = strbuf_value(active_map);
	tail = lang + strbuf_getlen(active_map);

	/* check whether or not list includes suffix. */
	while (lang < tail) {
		list = lang + strlen(lang) + 1;
		if (match_suffix_list(suffix, list))
			return lang;
		lang = list + strlen(list) + 1;
	}

	return NULL;
}

const char *decide_lang_exuberant(const char *suffix, const char* path)
{
	if (suffix && *suffix == '.') {
		suffix ++;
	}
	langType lt = getExtensionLanguage(suffix);
	if (lt == LANG_IGNORE) {
		lt = getPatternLanguage(path);
		if (lt == LANG_IGNORE) {
			return NULL;
		}
	} 
	return languageFromIndex(lt)->name;
}
/*
 * return true if the suffix matches with one in the list.
 */
static int
match_suffix_list(const char *suffix, const char *list)
{
	const char *p;

	while (*list) {
		if ((p = locatestring(list, suffix, MATCH_AT_FIRST
#if defined(_WIN32) || defined(__DJGPP__)
							     |IGNORE_CASE
#endif
			)) != NULL && (*p == '\0' || *p == '.'))
			return 1;
		for (list++; *list && *list != '.'; list++)
			;
	}
	return 0;
}

/*
 * make suffix value from langmap value.
 */
void
make_suffixes(const char *langmap, STRBUF *sb)
{
	const char *p;
	int onsuffix = 0;		/* not on suffix string */
	int first_dot = 1;

	for (p = langmap; *p; p++) {
		/*
		 * "c:.c.h,java:.java,cpp:.C.H"
		 */
		if (*p == '(') {
			while (*++p && *p != ')')
				;
			if (!*p)
				break;
                        continue;
		}
		if ((onsuffix == 0 && *p == ',') || (onsuffix == 1 && *p == ':'))
			die_with_code(2, "syntax error in langmap '%s'.", langmap);
		if (*p == ':')
			onsuffix = 1;
		else if (*p == ',')
			onsuffix = 0;
		else if (onsuffix) {
			if (*p == '.') {
				if (first_dot)
					first_dot = 0;
				else
					strbuf_putc(sb, ',');
			} else 
				strbuf_putc(sb, *p);
		}
	}
	if (onsuffix == 0)
		die_with_code(2, "syntax error in langmap '%s'.", langmap);
}
