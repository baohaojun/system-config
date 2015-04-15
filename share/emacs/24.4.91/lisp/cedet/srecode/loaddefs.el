;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "srecode/compile" "compile.el" "13a6e818d50afe07e2a46fe6d96bafc0")
;;; Generated autoloads from compile.el

(autoload 'srecode-compile-templates "srecode/compile" "\
Compile a semantic recode template file into a mode-local variable.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "srecode/cpp" "cpp.el" "5a4e2a92af20335e612066940dc17c5f")
;;; Generated autoloads from cpp.el

(autoload 'srecode-semantic-handle-:c "srecode/cpp" "\
Add macros into the dictionary DICT based on the current c file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:cpp "srecode/cpp" "\
Add macros into the dictionary DICT based on the current c file.
Calls `srecode-semantic-handle-:c.
Also adds the following:
 - nothing -

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "srecode/document" "document.el" "5926f0c792a02e521f353276775e9b01")
;;; Generated autoloads from document.el

(autoload 'srecode-document-insert-comment "srecode/document" "\
Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments.

\(fn)" t nil)

(autoload 'srecode-document-insert-function-comment "srecode/document" "\
Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN.

\(fn &optional FCN-IN)" t nil)

(autoload 'srecode-document-insert-variable-one-line-comment "srecode/document" "\
Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN.

\(fn &optional VAR-IN)" t nil)

(autoload 'srecode-document-insert-group-comments "srecode/document" "\
Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil "srecode/el" "el.el" "1ecd2db472627c48613b4d0ba9f03dcc")
;;; Generated autoloads from el.el

(autoload 'srecode-semantic-handle-:el "srecode/el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  PRENAME - The common name prefix of this file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:el-custom "srecode/el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  GROUP - The 'defgroup' name we guess you want for variables.
  FACEGROUP - The `defgroup' name you might want for faces.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "srecode/expandproto" "expandproto.el" "51b16bb758c137ad26dff108407fc924")
;;; Generated autoloads from expandproto.el

(autoload 'srecode-insert-prototype-expansion "srecode/expandproto" "\
Insert get/set methods for the current class.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "srecode/getset" "getset.el" "a13607d709b498d1591c00c79d6e50a0")
;;; Generated autoloads from getset.el

(autoload 'srecode-insert-getset "srecode/getset" "\
Insert get/set methods for the current class.
CLASS-IN is the semantic tag of the class to update.
FIELD-IN is the semantic tag, or string name, of the field to add.
If you do not specify CLASS-IN or FIELD-IN then a class and field
will be derived.

\(fn &optional CLASS-IN FIELD-IN)" t nil)

;;;***

;;;### (autoloads nil "srecode/insert" "insert.el" "a29527588420871e6b13db6ba55d7c3c")
;;; Generated autoloads from insert.el

(autoload 'srecode-insert "srecode/insert" "\
Insert the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add.

\(fn TEMPLATE-NAME &rest DICT-ENTRIES)" t nil)

;;;***

;;;### (autoloads nil "srecode/java" "java.el" "49411a2bdd7d184f996003fe1d424030")
;;; Generated autoloads from java.el

(autoload 'srecode-semantic-handle-:java "srecode/java" "\
Add macros into the dictionary DICT based on the current java file.
Adds the following:
FILENAME_AS_PACKAGE - file/dir converted into a java package name.
FILENAME_AS_CLASS - file converted to a Java class name.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "srecode/map" "map.el" "57b8e045b19a4a3e3e4464ce5b72b8b7")
;;; Generated autoloads from map.el

(autoload 'srecode-get-maps "srecode/map" "\
Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map.

\(fn &optional RESET)" t nil)

;;;***

;;;### (autoloads nil "srecode/mode" "mode.el" "fac7f17eb32957a02beb41a947673cb9")
;;; Generated autoloads from mode.el

(autoload 'srecode-minor-mode "srecode/mode" "\
Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-srecode-minor-mode nil "\
Non-nil if Global-Srecode minor mode is enabled.
See the command `global-srecode-minor-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-srecode-minor-mode'.")

(custom-autoload 'global-srecode-minor-mode "srecode/mode" nil)

(autoload 'global-srecode-minor-mode "srecode/mode" "\
Toggle global use of srecode minor mode.
If ARG is positive or nil, enable, if it is negative, disable.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "srecode/srt" "srt.el" "4207647e6ec9d2b1b5cfc99dcf977cae")
;;; Generated autoloads from srt.el

(autoload 'srecode-semantic-handle-:srt "srecode/srt" "\
Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end
MODE - The mode of this buffer.  If not declared yet, guess.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "srecode/template" "template.el" "906bde54bf3be815c975268510013fb3")
;;; Generated autoloads from template.el

(autoload 'srecode-template-setup-parser "srecode/template" "\
Setup buffer for parse.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "srecode/texi" "texi.el" "dfdd72a7fcb36d696fc49787d8091d18")
;;; Generated autoloads from texi.el

(autoload 'srecode-semantic-handle-:texi "srecode/texi" "\
Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:texitag "srecode/texi" "\
Add macros into the dictionary DICT based on the current :tag file.
Adds the following:
  TAGDOC - Texinfo formatted doc string for :tag.

\(fn DICT)" nil nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
