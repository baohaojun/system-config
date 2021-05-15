                xr -- Emacs regexp parser and analyser
                ======================================

XR converts Emacs regular expressions to the structured rx form, thus
being an inverse of rx. It can also find mistakes and questionable
constructs inside regexp strings.

It can be useful for:

- Migrating existing code to rx form
- Understanding what a regexp string really means
- Finding errors in regexp strings

It can also parse and find mistakes in skip-sets, the regexp-like
arguments to skip-chars-forward and skip-chars-backward.

The xr package can be used interactively or by other code as a library.


* Example

  (xr-pp "\\`\\(?:[^^]\\|\\^\\(?: \\*\\|\\[\\)\\)")

  outputs

  (seq bos 
       (or (not (any "^"))
           (seq "^"
                (or " *" "["))))


* Installation

  From GNU ELPA (https://elpa.gnu.org/packages/xr.html):

    M-x package-install RET xr RET


* Interface

  Functions parsing regexp strings:
  
   xr       --  convert regexp to rx
   xr-pp    --  convert regexp to rx and pretty-print
   xr-lint  --  find mistakes in regexp
  
  Functions parsing skip sets:
  
   xr-skip-set       --  convert skip-set to rx
   xr-skip-set-pp    --  convert skip-set to rx and pretty-print
   xr-skip-set-lint  --  find mistakes in skip-set
  
  Utility:
  
   xr-pp-rx-to-str  --  pretty-print rx expression to string
  

* What the diagnostics mean

  - Unescaped literal 'X'

    A special character is taken literally because it occurs in a
    position where it does not need to be backslash-escaped. It is
    good style to do so anyway (assuming that it should occur as a
    literal character).

  - Escaped non-special character 'X'
  
    A character is backslash-escaped even though this is not necessary
    and does not turn it into a special sequence. Maybe the backslash
    was in error, or should be doubled if a literal backslash was
    expected.
  
  - Duplicated 'X' inside character alternative
  
    A character occurs twice inside [...]; this is obviously
    pointless. In particular, backslashes are not special inside
    [...]; they have no escaping power, and do not need to be escaped
    in order to include a literal backslash.
  
  - Repetition of repetition
  - Repetition of option
  - Optional repetition
  - Optional option
  
    A repetition construct is applied to an expression that is already
    repeated, such as a*+ or \(x?\)?. These expressions can be written
    with a single repetition and often indicate a different mistake,
    perhaps a missing backslash.

    When a repetition construct is ? or ??, it is termed 'option'
    instead; the principle is the same.

  - Reversed range 'Y-X' matches nothing

    The last character of a range precedes the first and therefore
    includes no characters at all (not even the endpoints). Most such
    ranges are caused by a misplaced hyphen.

  - Character 'B' included in range 'A-C'

    A range includes a character that also occurs individually. This
    is often caused by a misplaced hyphen.

  - Ranges 'A-M' and 'D-Z' overlap

    Two ranges have at least one character in common. This is often
    caused by a misplaced hyphen.

  - Two-character range 'A-B'

    A range only consists of its two endpoints, since they have
    consecutive character codes. This is often caused by a misplaced
    hyphen.

  - Duplicated character class '[:class:]'

    A character class occurs twice in a single character alternative
    or skip set.

  - Duplicated alternative branch

    The same expression occurs in two different branches, like in
    A\|A. This has the effect of only including it once.

  - Branch matches superset/subset of a previous branch

    A branch in an or-expression matches a superset or subset of what
    another branch matches, like in [ab]\|a. This means that one of
    the branches can be eliminated without changing the meaning of the
    regexp.

  - Repetition subsumes/subsumed by preceding repetition

    An repeating expression matches a superset or subset of what the
    previous expression matches, in such a way that one of them is
    unnecessary. For example, [ab]+a* matches the same text as [ab]+,
    so the a* could be removed without changing the meaning of the
    regexp.

  - First/last item in repetition subsumes last/first item (wrapped)

    The first and last items in a repeated sequence, being effectively
    adjacent, match a superset or subset of each other, which makes
    for an unexpected inefficiency. For example, \(?:a*c[ab]+\)* can
    be seen as a*c[ab]+a*c[ab]+... where the [ab]+a* in the middle is
    a slow way of writing [ab]+ which is made worse by the outer
    repetition. The general remedy is to move the subsumed item out of
    the repeated sequence, resulting in a*\(?:c[ab]+\)* in the example
    above.

  - End-of-line anchor followed by non-newline
  - Non-newline followed by line-start anchor

    A pattern that does not match a newline occurs right after an
    end-of-line anchor ($) or before a line-start anchor (^).
    This combination can never match.

  - End-of-text anchor followed by non-empty pattern

    A pattern that only matches a non-empty string occurs right after
    an end-of-text anchor (\'). This combination can never match.

  - Uncounted repetition

    The construct A\{,\} repeats A zero or more times which was
    probably not intended.

  - Implicit zero repetition

    The construct A\{\} only matches the empty string, which was
    probably not intended.

  - Suspect '[' in char alternative

    This warning indicates badly-placed square brackets in a character
    alternative, as in [A[B]C]. A literal ] must come first
    (possibly after a negating ^).

  - Literal '-' not first or last

    It is good style to put a literal hyphen last in character
    alternatives and skip sets, to clearly indicate that it was not
    intended as part of a range.

  - Repetition of zero-width assertion
  - Optional zero-width assertion

    A repetition operator was applied to a zero-width assertion, like
    ^ or \<, which is completely pointless. The error may be a missing
    escaping backslash.

  - Repetition of expression matching an empty string
  - Optional expression matching an empty string

    A repetition operator was applied to a sub-expression that could
    match the empty string; this is not necessarily wrong, but such
    constructs run very slowly on Emacs's regexp engine. Consider
    rewriting them into a form where the repeated expression cannot
    match the empty string.

    Example: \(?:a*b*\)* is equivalent to the much faster \(?:a\|b\)*.

    Another example: \(?:a?b*\)? is better written a?b*. 

    In general, A?, where A matches the empty string, can be
    simplified to just A.

  - Unnecessarily escaped 'X'

    A character is backslash-escaped in a skip set despite not being
    one of the three special characters - (hyphen), \ (backslash) and
    ^ (caret). It could be unnecessary, or a backslash that should
    have been escaped.

  - Single-element range 'X-X'

    A range in a skip set has identical first and last elements. It is
    rather pointless to have it as a range.

  - Stray '\\' at end of string

    A single backslash at the end of a skip set is always ignored;
    double it if you want a literal backslash to be included.

  - Suspect skip set framed in '[...]'

    A skip set appears to be enclosed in [...], as if it were a
    regexp. Skip sets are not regexps and do not use brackets. To
    include the brackets themselves, put them next to each other.

  - Suspect character class framed in '[...]'

    A skip set contains a character class enclosed in double pairs of
    square brackets, as if it were a regexp. Character classes in skip
    sets are written inside a single pair of square brackets, like
    [:digit:].

  - Empty set matches nothing

    The empty string is a skip set that does not match anything, and
    is therefore pointless.

  - Negated empty set matches anything

    The string "^" is a skip set that matches anything, and is therefore
    pointless.


* See also

  The relint package (https://elpa.gnu.org/packages/relint.html) uses xr
  to find regexp mistakes in elisp code.

  The lex package (https://elpa.gnu.org/packages/lex.html), a lexical
  analyser generator, provides the lex-parse-re function which
  translates regexps to rx, but does not attempt to handle all the
  edge cases of Elisp's regexp syntax or pretty-print the result.

  The pcre2el package (https://github.com/joddie/pcre2el), a regexp
  syntax converter and interactive regexp explainer, can also be used
  for translating regexps to rx. However, xr is more accurate for this
  purpose.
