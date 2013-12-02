/*
 * Copyright (C) 2013 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.ide.common.res2;

import static com.android.SdkConstants.AMP_ENTITY;
import static com.android.SdkConstants.APOS_ENTITY;
import static com.android.SdkConstants.GT_ENTITY;
import static com.android.SdkConstants.LT_ENTITY;
import static com.android.SdkConstants.QUOT_ENTITY;

import com.android.annotations.Nullable;
import com.android.annotations.VisibleForTesting;

/**
 * Helper class to help with XML values resource file.
 */
public class ValueXmlHelper {

    /**
     * Replaces escapes in an XML resource string with the actual characters,
     * performing unicode substitutions (replacing any {@code \\uNNNN} references in the
     * given string with the corresponding unicode characters), etc.
     *
     * @param s the string to unescape
     * @param escapeEntities XML entities
     * @param trim whether surrounding space and quotes should be trimmed
     * @return the string with the escape characters removed and expanded
     */
    @Nullable
    public static String unescapeResourceString(
            @Nullable String s,
            boolean escapeEntities, boolean trim) {
        if (s == null) {
            return null;
        }

        // Trim space surrounding optional quotes
        int i = 0;
        int n = s.length();
        if (trim) {
            while (i < n) {
                char c = s.charAt(i);
                if (!Character.isWhitespace(c)) {
                    break;
                }
                i++;
            }
            while (n > i) {
                char c = s.charAt(n - 1);
                if (!Character.isWhitespace(c)) {
                    //See if this was a \, and if so, see whether it was escaped
                    if (n < s.length() && isEscaped(s, n)) {
                        n++;
                    }
                    break;
                }
                n--;
            }

            // Trim surrounding quotes. Note that there can be *any* number of these, and
            // the left side and right side do not have to match; e.g. you can have
            //    """"f"" => f
            int quoteStart = i;
            int quoteEnd = n;
            while (i < n) {
                char c = s.charAt(i);
                if (c != '"') {
                    break;
                }
                i++;
            }
            // Searching backwards is slightly more complicated; make sure we don't trim
            // quotes that have been escaped.
            while (n > i) {
                char c = s.charAt(n - 1);
                if (c != '"') {
                    if (n < s.length() && isEscaped(s, n)) {
                        n++;
                    }
                    break;
                }
                n--;
            }
            if (n == i) {
                return ""; //$NON-NLS-1$
            }

            // Only trim leading spaces if we didn't already process a leading quote:
            if (i == quoteStart) {
                while (i < n) {
                    char c = s.charAt(i);
                    if (!Character.isWhitespace(c)) {
                        break;
                    }
                    i++;
                }
            }
            // Only trim trailing spaces if we didn't already process a trailing quote:
            if (n == quoteEnd) {
                while (n > i) {
                    char c = s.charAt(n - 1);
                    if (!Character.isWhitespace(c)) {
                        //See if this was a \, and if so, see whether it was escaped
                        if (n < s.length() && isEscaped(s, n)) {
                            n++;
                        }
                        break;
                    }
                    n--;
                }
            }
            if (n == i) {
                return ""; //$NON-NLS-1$
            }
        }

        // If no surrounding whitespace and no escape characters, no need to do any
        // more work
        if (i == 0 && n == s.length() && s.indexOf('\\') == -1
                && (!escapeEntities || s.indexOf('&') == -1)) {
            return s;
        }

        StringBuilder sb = new StringBuilder(n - i);
        for (; i < n; i++) {
            char c = s.charAt(i);
            if (c == '\\' && i < n - 1) {
                char next = s.charAt(i + 1);
                // Unicode escapes
                if (next == 'u' && i < n - 5) { // case sensitive
                    String hex = s.substring(i + 2, i + 6);
                    try {
                        int unicodeValue = Integer.parseInt(hex, 16);
                        sb.append((char) unicodeValue);
                        i += 5;
                        continue;
                    } catch (NumberFormatException e) {
                        // Invalid escape: Just proceed to literally transcribe it
                        sb.append(c);
                    }
                } else if (next == 'n') {
                    sb.append('\n');
                    i++;
                } else if (next == 't') {
                    sb.append('\t');
                    i++;
                } else {
                    sb.append(next);
                    i++;
                    continue;
                }
            } else {
                if (c == '&' && escapeEntities) {
                    if (s.regionMatches(true, i, LT_ENTITY, 0, LT_ENTITY.length())) {
                        sb.append('<');
                        i += LT_ENTITY.length() - 1;
                        continue;
                    } else if (s.regionMatches(true, i, AMP_ENTITY, 0, AMP_ENTITY.length())) {
                        sb.append('&');
                        i += AMP_ENTITY.length() - 1;
                        continue;
                    } else if (s.regionMatches(true, i, QUOT_ENTITY, 0, QUOT_ENTITY.length())) {
                      sb.append('"');
                      i += QUOT_ENTITY.length() - 1;
                      continue;
                    } else if (s.regionMatches(true, i, APOS_ENTITY, 0, APOS_ENTITY.length())) {
                      sb.append('\'');
                      i += APOS_ENTITY.length() - 1;
                      continue;
                    } else if (s.regionMatches(true, i, GT_ENTITY, 0, GT_ENTITY.length())) {
                      sb.append('>');
                      i += GT_ENTITY.length() - 1;
                      continue;
                    }
                }
                sb.append(c);
            }
        }
        s = sb.toString();

        return s;
    }

    /**
     * Returns true if the character at the given offset in the string is escaped
     * (the previous character is a \, and that character isn't itself an escaped \)
     *
     * @param s the string
     * @param index the index of the character in the string to check
     * @return true if the character is escaped
     */
    @VisibleForTesting
    static boolean isEscaped(String s, int index) {
        if (index == 0 || index == s.length()) {
            return false;
        }
        int prevPos = index - 1;
        char prev = s.charAt(prevPos);
        if (prev != '\\') {
            return false;
        }
        // The character *may* be escaped; not sure if the \ we ran into is
        // an escape character, or an escaped backslash; we have to search backwards
        // to be certain.
        int j = prevPos - 1;
        while (j >= 0) {
            if (s.charAt(j) != '\\') {
                break;
            }
            j--;
        }
        // If we passed an odd number of \'s, the space is escaped
        return (prevPos - j) % 2 == 1;
    }

    /**
     * Escape a string value to be placed in a string resource file such that it complies with
     * the escaping rules described here:
     *   http://developer.android.com/guide/topics/resources/string-resource.html
     * More examples of the escaping rules can be found here:
     *   http://androidcookbook.com/Recipe.seam?recipeId=2219&recipeFrom=ViewTOC
     * This method assumes that the String is not escaped already.
     *
     * Rules:
     * <ul>
     * <li>Double quotes are needed if string starts or ends with at least one space.
     * <li>{@code @, ?} at beginning of string have to be escaped with a backslash.
     * <li>{@code ', ", \} have to be escaped with a backslash.
     * <li>{@code <, >, &} have to be replaced by their predefined xml entity.
     * <li>{@code \n, \t} have to be replaced by a backslash and the appropriate character.
     * </ul>
     * @param s the string to be escaped
     * @return the escaped string as it would appear in the XML text in a values file
     */
    public static String escapeResourceString(String s) {
        int n = s.length();
        if (n == 0) {
            return "";
        }

        StringBuilder sb = new StringBuilder(s.length() * 2);
        boolean hasSpace = s.charAt(0) == ' ' || s.charAt(n - 1) == ' ';

        if (hasSpace) {
            sb.append('"');
        } else if (s.charAt(0) == '@' || s.charAt(0) == '?') {
            sb.append('\\');
        }

        for (int i = 0; i < n; ++i) {
            char c = s.charAt(i);
            switch (c) {
                case '\'':
                    if (!hasSpace) {
                        sb.append('\\');
                    }
                    sb.append(c);
                    break;
                case '"':
                case '\\':
                    sb.append('\\');
                    sb.append(c);
                    break;
                case '<':
                    sb.append(LT_ENTITY);
                    break;
                case '&':
                    sb.append(AMP_ENTITY);
                    break;
                case '\n':
                    sb.append("\\n"); //$NON-NLS-1$
                    break;
                case '\t':
                    sb.append("\\t"); //$NON-NLS-1$
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }

        if (hasSpace) {
            sb.append('"');
        }

        return sb.toString();
    }
}
