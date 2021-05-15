;;; nim-vars.el --- nim-mode's variables -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defgroup nim nil
  "A major mode for the Nim programming language."
  :link '(url-link "http://nim-lang.org/")
  :group 'languages)

(defface nim-font-lock-export-face
  '((t :weight bold
       :slant italic
       :inherit font-lock-function-name-face))
  "Font Lock face for export (XXX*)"
  :group 'nim)

(defface nim-font-lock-pragma-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Font Lock face for pragmas."
  :group 'nim)

(defface nim-non-overloadable-face
  '((t :inherit font-lock-builtin-face
       :slant italic))
  "Font Lock face for nonoverloadable builtins."
  :group 'nim)

(defface nim-font-lock-number-face
  '((t :slant italic))
  "Font Lock face for numbers."
  :group 'nim)

(defcustom nim-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `nim-indent-line' call."
  :type '(repeat symbol)
  :group 'nim)

(defcustom nim-indent-offset 2
  "Number of spaces per level of indentation."
  :type 'integer
  :group 'nim)

(defcustom nim-smie-function-indent 4
  "Number of spaces between ‘proc ... =’.
Note that this configuration affects other ‘template’, ‘macro’,
‘iterator’, and ‘converter’ declaration as well."
  :type 'integer
  :group 'nim)

(defcustom nim-smie-indent-stoppers
  '("proc" "func" "template" "macro" "iterator" "converter" "type")
  "Indentation behavior after empty line.
You can specify list of string, which you want to stop indenting.
If it’s nil, it does nothing."
  :type '(choice
          (repeat :tag "" string)
          (const :tag "" nil))
  :group 'nim)

(defcustom nim-smie-indent-dedenters 'all-dedent
  "Indentation behavior after empty line.
If you set ‘all-dedent’, it forces dedent whatever point starts.
Or you can specify list of string, which you want to dedent.
If it’s nil, it does nothing."
  :type '(choice
          (repeat :tag "Dedenter symbols" string)
          (const :tag "Don't dedent" nil)
          (const :tag
                 "Dedent all if previous line is empty line" all-dedent))
  :group 'nim)

(defcustom nim-smie-dedent-after-break '()
  "List of string that dedent after break statement.
This feature is activated if only the break line has
other tokens like ’:’ or ’=’."
  :type '(choise
          (repeat :tag "List of dedenter token" string)
          (const :tag "" nil)))

(defcustom nim-smie-after-indent-hook nil
  "Hook run after indenting."
  :type 'hook
  :group 'nim)

(defcustom nim-mode-init-hook nil
  "This hook is called when ‘nim-mode’ is initialized."
  :type 'hook
  :group 'nim)

(defcustom nim-common-init-hook nil
  "A hook for both nim-mode and nimscript-mode."
  :type 'hook
  :group 'nim)

(defcustom nim-capf-after-exit-function-hook nil
  "A hook that is called with an argument.
The argument is string that has some properties."
  :type 'hook
  :group 'nim)

(defcustom nim-pretty-triple-double-quotes
  ;; What character should be default? („…“, “…”, ‘…’, or etc.?)
  (cons ?„ ?”)
  "Change triple double quotes to another quote form.
This configuration is enabled only in `prettify-symbols-mode`."
  :type 'cons
  :group 'nim)

(defcustom nim-compile-command "nim"
  "Path to the nim executable.
You don't need to set this if the nim executable is inside your PATH."
  :type 'string
  :group 'nim)

(defcustom nim-compile-user-args '()
  "The arguments to pass to `nim-compile-command' to compile a file."
  :type '(repeat string)
  :group 'nim)

(defcustom nimsuggest-path (executable-find "nimsuggest")
  "Path to the nimsuggest binary."
  :type '(choice (const :tag "Path of nimsuggest binary" string)
                 (const :tag "" nil))
  :group 'nim)

(defcustom nimsuggest-options '("--refresh")
  "Command line options for Nimsuggest.
‘--epc’ are automatically passed to nim-mode’s EPC (Emacs RPC) function."
  :type '(choice (repeat :tag "List of options" string)
                 (const :tag "" nil))
  :group 'nim)

(defcustom nimsuggest-local-options '()
  "Options for Nimsuggest.
Please use this variable to set nimsuggest’s options for
specific directory or buffer.  See also ‘dir-locals-file’."
  :type '(choice (repeat :tag "List of options" string)
                 (const :tag "" nil))
  :group 'nim)

(defcustom nimsuggest-dirty-directory
  ;; Even users changed the temp directory name,
  ;; ‘file-name-as-directory’ ensures suffix directory separator.
  (mapconcat 'file-name-as-directory
             `(,temporary-file-directory "emacs-nim-mode") "")
  "Directory name, which nimsuggest uses temporarily.
Note that this directory is removed when you exit from Emacs."
  :type 'directory
  :group 'nim)

(defcustom nimsuggest-accept-process-delay 150
  "Number of delay msec to check nimsuggest epc connection is established."
  :type 'integer
  :group 'nim)

(defcustom nimsuggest-accept-process-timeout-count 100
  "Number of count that Emacs can try to check nimsuggest epc connection.
For example, if you set `nimsuggest-accept-process-delay' to 150 and this value
was 100, the total time of timeout for nimsuggest epc connection would be about
15sec."
  :type 'integer
  :group 'nim)

(defcustom nimsuggest-show-doc-function 'nimsuggest--show-doc-rst
  "The style used to display Nim documentation.
Options available are `nimsuggest--show-doc-rst' and `nimsuggest--show-doc-org'.
Note `nimsuggest--show-doc-org' enables syntax highlighting and section folding,
but is experimental."
  :type 'function
  :group 'nim)


(defvar nimsuggest-eldoc-function 'ignore) ; #208
(defvar nimsuggest-ignore-dir-regex
  (rx (or "\\" "/") (in "nN") "im" (or "\\" "/") "compiler" (or "\\" "/")))
(defvar nim--inside-compiler-dir-p nil)


;; Keymaps

;; Supported basic keybinds:
;; C means Control-key
;; M means Meta-key
;; C-M-a        -- jump to head of proc
;; C-M-e        -- jump to end of proc
;; C-M-h        -- mark region of function

(defvar nim-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Allowed keys: C-c with control-letter, or {,}, <, >, :, ;
    ;; See also: http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
    (define-key map (kbd "C-c C-c") 'nim-compile)
    (define-key map (kbd "C-c <") 'nim-indent-shift-left)
    (define-key map (kbd "C-c >") 'nim-indent-shift-right)
    ;; TODO:
    ;; C-c C-j - imemu
    ;; implement mark-defun
    ;;
    map))

(defvar nimsuggest-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (cond
     ((and (eq nimsuggest-show-doc-function 'nimsuggest--show-doc-org)
           (fboundp 'org-mode))
      (set-keymap-parent map (make-composed-keymap org-mode-map))
      (define-key map (kbd "RET") 'org-open-at-point))
     (t (set-keymap-parent map (make-composed-keymap special-mode-map))))
    (define-key map (kbd ">") 'nimsuggest-doc-next)
    (define-key map (kbd "<") 'nimsuggest-doc-previous)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Nimsuggest doc mode keymap.")

;; xref supported key binds:
;;   esc-map "." xref-find-definitions
;;   esc-map "," xref-pop-marker-stack
;;   esc-map "?" xref-find-references
;;   ctl-x-4-map "." xref-find-definitions-other-window
;;   ctl-x-5-map "." xref-find-definitions-other-frame
;;   TODO: esc-map [?\C-.] xref-find-apropos

;; hs-minor-mode:
;; C-c @ C-M-h  -- hide/fold functions
;; C-c @ C-M-s  -- show functions
;; (You can do same thing by zr and zm keys on evil, a vim emulation plugin)


;; Syntax table
(defvar nim-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)

    ;; Comment
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?\n ">" table)
    ;; Use "." Punctuation syntax class because I got error when I
    ;; used "$" from smie.el
    (modify-syntax-entry ?` "'" table)

    ;; Use _ syntax to single quote
    ;; See also `nim-syntax-propertize-function'.
    (modify-syntax-entry ?\' "_" table)

    ;; Parentheses
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for Nim files.")

(defvar nim-dotty-syntax-table
  (let ((table (copy-syntax-table nim-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Nim files.
It makes underscores and dots word constituent chars.")

(defvar nimscript-mode-syntax-table
  (copy-syntax-table nim-mode-syntax-table)
  "Syntax table for NimScript files.")

(defconst nim-comment
  `((single
     . ((comment-start      . "#")
        (comment-end        . "")
        (comment-start-skip . ,(rx "#" (? "#") (? " ")))
        (comment-use-syntax . t)))
    (multi
     . ((comment-start      . "#[")
        (comment-end        . "]#")
        (comment-start-skip
         . ,(rx (group
                 (syntax comment-start) (? "#") "[")))
        (comment-end-skip
         . ,(rx (group
                 "]#" (? "#"))))
        ;; comment-continue has to include non space character
        ;; otherwise it makes trouble when you do ‘uncomment-region’.
        (comment-continue   . " |")
        (comment-padding    . "  ")
        (comment-multi-line . t)
        (comment-use-syntax . nil)))))


;; Nim keywords

;; Those keywords are used to syntax highlight as well as
;; auto-completion. If you want to change those keywords,
;; please consider about auto-completion; it inserts what
;; you registered. (snark or camel)

(defconst nim-keywords
  '("addr" "and" "as" "asm" "atomic" "bind" "block" "break" "case"
    "cast" "concept" "const" "continue" "converter" "defer" "discard" "distinct"
    "div" "do" "elif" "else" "end" "enum" "except" "export" "finally" "for"
    "func" "from" "generic" "if" "import" "in" "include" "interface" "isnot"
    "iterator" "lambda" "let" "macro" "method" "mixin" "mod" "nil" "not"
    "notin" "object" "of" "or" "out" "proc" "ptr" "raise" "ref" "return"
    "shared" "shl" "shr" "static" "template" "try" "tuple" "type" "unsafeAddr"
    "using" "var" "when" "while" "with" "without" "xor" "yield")
  "Nim keywords.
The above string is taken from URL
`https://nim-lang.org/docs/manual.html#lexical-analysis-identifiers-amp-keywords'
for easy updating.")

(defconst nim-types
  '("int" "int8" "int16" "int32" "int64" "uint" "uint8" "uint16" "uint32"
    "uint64" "float" "float32" "float64" "bool" "char" "string"
    "pointer" "typedesc" "void" "auto" "any" "sink" "lent"
    "untyped" "typed" "range" "array" "openArray" "varargs" "seq" "set" "byte"
    ;; c interop types
    "cchar" "cschar" "cshort" "cint" "clong" "clonglong" "cfloat" "cdouble"
    "cstring" "clongdouble" "cstringArray")
  "Nim types defined in <lib/system.nim>.")

(defconst nim-exceptions
  '("Exception" "SystemError" "IOError" "OSError" "LibraryError"
    "ResourceExhaustedError" "ArithmeticError" "DivByZeroError" "OverflowError"
    "AccessViolationError" "AssertionError" "ValueError" "KeyError"
    "OutOfMemError" "IndexError" "FieldError" "RangeError"
    "StackOverflowError" "ReraiseError" "ObjectAssignmentError"
    "ObjectConversionError" "DeadThreadError" "FloatInexactError"
    "FloatUnderflowError" "FloatingPointError" "FloatInvalidOpError"
    "FloatDivByZeroError" "FloatOverflowError")
  "Nim exceptions defined in <lib/system.nim>.")

(defconst nim-variables
  '("programResult" "globalRaiseHook" "localRaiseHook" "outOfMemHook"
    ;; not nimscript
    "stdin" "stdout" "stderr")
  "Nim variables defined in <lib/system.nim>.")

(defconst nim-constants
  '("isMainModule" "CompileDate" "CompileTime" "NimVersion"
    "NimMajor" "NimMinor" "NimPatch" "NimStackTrace" "cpuEndian" "hostOS"
    "hostCPU" "appType" "Inf" "NegInf" "NaN" "nimvm" "QuitSuccess"
    "QuitFailure" "true" "false" "nil"
    "on" "off" "NoFakeVars")
  "Nim constants defined in <lib/system.nim>.")

(defconst nim-nonoverloadable-builtins
  '("declared" "defined" "definedInScope" "compiles" "low" "high" "sizeOf"
    "is" "of" "shallowCopy" "getAst" "astToStr" "spawn" "procCall")
  "Nim nonoverloadable builtins.")

(defconst nim-builtin-functions
  '("div" "mod" "shr" "shl" "and" "or" "xor"
    "not" "notin" "isnot" "cmp" "succ" "pred" "inc"
    "dec" "newseq" "len" "xlen" "incl" "excl" "card" "ord" "chr" "ze" "ze64"
    "toU8" "toU16" "toU32" "min" "max" "setLen" "newString" "add"
    "compileOption" "del" "delete" "insert" "repr" "toFloat"
    "toBiggestFloat" "toInt" "toBiggestInt" "addQuitProc" "copy"
    "slurp" "staticRead" "gorge" "staticExec" "instantiationInfo"
    "currentSourcePath" "raiseAssert" "failedAssertImpl" "assert" "doAssert"
    "onFailedAssert" "shallow" "eval" "locals"
    "swap" "getRefcount" "countdown" "countup" "min" "max" "abs" "clamp"
    "items" "mitems" "pairs" "mpairs" "isNil"
    "find" "contains" "pop" "fields" "fieldPairs" "each"
    "accumulateresult" "echo" "debugEcho" "newException"
    "getTypeInfo" "quit" "open" "reopen" "close" "endOfFile"
    "readChar" "flushFile" "readAll" "readFile" "write" "writeFile"
    "readLine" "writeLn" "writeLine"
    "getFileSize" "readBytes" "readChars" "readBuffer" "writeBytes"
    "writeChars" "writeBuffer" "setFilePos" "getFilePos" "getFileHandle"
    "lines" "cstringArrayToSeq" "getDiscriminant" "selectBranch"
    ;; hasAlloc
    "safeAdd")
  "Standard library functions fundamental enough to count as builtins.
Magic functions.")

(defconst nim-builtins-without-nimscript
  '(;; hasAlloc && not nimscript && not JS
    "deepCopy"
    ;; not nimscirpt
    "zeroMem" "copyMem" "moveMem" "equalMem"
    ;; not nimscirpt && hasAlloc
    "alloc" "createU" "alloc0" "create" "realloc" "resize" "dealloc"
    "allocShared" "createShareU" "allocShared0" "createShared"
    "reallocShared" "resizeShared" "deallocShared" "freeShared"
    "getOccupiedMem" "getFreeMem" "getTotalMem"
    "GC_disable" "GC_enable" "GC_fullCollect" "GC_setStrategy"
    "GC_enableMarkAndSweep" "GC_disableMarkAndSweep"
    "GC_getStatistics" "GC_ref" "GC_unref"
    ;; not nimscirpt && hasAlloc && hasThreadSupport
    "getOccupiedSharedMem" "getFreeSharedMem" "getTotalSharedMem"
    ;; not nimscirpt && Not JS
    "likely" "unlikely" "rawProc" "rawEnv" "finished"
    ;; not nimscirpt && not hostOS "standalone" && Not JS
    "getCurrentException" "getCurrentExceptionMsg" "onRaise"
    "setCurrentException")
  "Builtin functions copied from system.nim.
But all those functions can not use in NimScript.")

;; Nimscript
(defvar nim-nimble-ini-format-regex (rx line-start "[Package]"))

(defconst nimscript-builtins
  '("listDirs" "listFiles" "paramStr" "paramCount" "switch" "getCommand"
    "setCommand" "cmpic" "getEnv" "existsEnv" "fileExists" "dirExists"
    "existsFile" "existsDir" "toExe" "toDll" "rmDir" "rmFile" "mkDir"
    "mvFile" "cpFile" "exec" "put" "get" "exists" "nimcacheDir" "thisDir"
    "cd" "requires"
    ;; templates
    "--" "withDir" "task"))

(defconst nimscript-variables
  '("packageName" "version" "author" "description" "license"
    "srcDir" "binDir" "backend" "mode" "skipDirs" "skipFiles"
    "skipExt" "installDirs" "installFiles" "installExt" "bin"
    "requiresData"))

(defconst nim-pragmas
  ;; alist of (PRAGMA_NAME . DESCRIPTION)
  ;; the DESCRIPTION can be either a string or list of string.
  ;; Use list of string when you want to describe a pragma, but there
  ;; are more than two ways to use. I.e, pure pragma
  ;; (though I don't implemented displaying list of string yet)
  '(("deprecated" .
     ("deprecate a symbol"
      "[OLD: NEW, ...] -- deprecate symbols"))
    ("noSideEffect" .
     "used to mark a proc/iterator to have no side effects")
    ("procvar" .
     "used to mark a proc that it can be passed to a procedural variable.")
    ("destructor" .
     "used to mark a proc to act as a type destructor. [duplicated]")
    ("compileTime" .
     "used to mark a proc or variable to be used at compile time only")
    ("noReturn" .
     "The ``noreturn`` pragma is used to mark a proc that never returns")
    ("acyclic" .
     "can be used for object types to mark them as acyclic")
    ("final" .
     "can be used for an object type to specify that it cannot be inherited from.")
    ("pure" .
     ("object type can be marked to its type field, which is used for runtime type identification is omitted"
      "enum type can be marked to access of its fields always requires full qualification"))
    ("shallow" .
     "affects the semantics of a type: The compiler is allowed to make a shallow copy.")
    ("asmNoStackFrame" .
     "A proc can be marked with this pragma to tell the compiler it should not generate a stack frame for the proc.")
    ("error" .
     ("used to make the compiler output an error message with the given content."
      "can be used to annotate a symbol (like an iterator or proc)"))
    ("fatal" .
     "In contrast to the error pragma, compilation is guaranteed to be aborted by this pragma.")
    ("warning" .
     "is used to make the compiler output a warning message with the given content. Compilation continues after the warning.")
    ("hint" .
     "is used to make/disable the compiler output a hint message with the given content.")
    ("line" .
     "can be used to affect line information of the annotated statement as seen in stack backtraces")
    ("linearScanEnd" .
     "can be used to tell the compiler how to compile a Nim `case`:idx: statement. Syntactically it has to be used as a statement")
    ("computedGoto".
     "can be used to tell the compiler how to compile a Nim `case`:idx: in a ``while true`` statement.")
    ("unroll" .
     "can be used to tell the compiler that it should unroll a `for`:idx: or `while`:idx: loop for runtime efficiency")
    ("register".
     "declares variable as register, giving the compiler a hint that the variable should be placed in a hardware register for faster access.")
    ("global" .
     "can be applied to a variable within a proc to instruct the compiler to store it in a global location and initialize it once at program startup.")
    ("deadCodeElim" .
     "on -- tells the compiler to activate (or deactivate) dead code elimination for the module the pragma appears in.")
    ("noForward" .
     "on|off -- no forward declaration")
    ("pragma" .
     "can be used to declare user defined pragmas.")
    ("experimental" .
     "enables experimental language features.")
    ("push" .
     "are used to override the settings temporarily with pop")
    ("pop" .
     "are used to override the settings temporarily with push")
    ;; implementation specific pragmas
    ("bitsize" .
     "is for object field members. It declares the field as a bitfield in C/C++.")
    ("volatile" .
     "is for variables only. It declares the variable as `volatile`, whatever that means in C/C++.")
    ("noDecl" .
     "tell Nim that it should not generate a declaration for the symbol in the C code.")
    ("header" .
     "can be applied to almost any symbol and specifies that it should not be declared and instead the generated code should contain an `#include`")
    ("incompleteStruct" .
     "tells the compiler to not use the underlying C ``struct`` in a ``sizeof`` expression")
    ("compile" .
     "STRING -- can be used to compile and link a C/C++ source file with the project")
    ("link" .
     "STRING -- can be used to link an additional file with the project")
    ("passC" .
     ("STRING -- can be used to pass additional parameters to the C compiler like commandline switch `--passC`"
      "you can use `gorge` from the `system module` to embed parameters from an external command at compile time"))
    ("passL" .
     ("STRING -- can be used to pass additional parameters to the linker like commandline switch `--passL`"
      "you can use `gorge` from the `system module` to embed parameters from an external command at compile time"))
    ("emit" .
     "STRING -- can be used to directly affect the output of the compiler's code generator.")
    ("importcpp" . "")
    ("importobjc" . "")
    ("codegenDecl" .
     "STRING -- can be used to directly influence Nim's code generator.")
    ("injectStmt" .
     "can be used to inject a statement before every other statement in the current module.")
    ("intdefine" . "Reads in a build-time define as an integer")
    ("strdefine" . "Reads in a build-time define as a string")
    ;; Compilation option pragmas
    ("checks" .
     "on|off -- Turns the code generation for all runtime checks on or off.")
    ("boundChecks" .
     "on|off -- Turns the code generation for array bound checks on or off.")
    ("overflowChecks" .
     "on|off -- Turns the code generation for over- or underflow checks on or off.")
    ("nilChecks" .
     "on|off -- Turns the code generation for nil pointer checks on or off.")
    ("assertions" .
     "on|off -- Turns the code generation for assertions on or off.")
    ("warnings" .
     "on|off -- Turns the warning messages of the compiler on or off.")
    ("hints" .
     "on|off -- Turns the hint messages of the compiler on or off.")
    ("optimization" .
     "none|speed|size -- optimize the code for the options")
    ("callconv" .
     "cdecl|... -- Specifies the default calling convention for all procedures (and procedure types) that follow.")
    ;; ffi.txt
    ("importc" . "STRING")
    ("exportc" . "STRING")
    ("extern" . "STRING")
    ("bycopy" . "can be applied to an object or tuple type and instructs the compiler to pass the type by value to procs")
    ("byref" . "can be applied to an object or tuple type and instructs the compiler to pass the type by reference (hidden pointer) to procs.")
    ("varargs" . "tells Nim that the proc can take a variable number of parameters after the last specified parameter.")
    ("union" . "can be applied to any ``object`` type. It means all of the object's fields are overlaid in memory.")
    ("packed" . "ensures that the fields of an object are packed back-to-back in memory.")
    ("unchecked" . "can be used to mark a named array as `unchecked` meaning its bounds are not checked.")
    ("dynlib" . "STRING -- dynamic library")
    ;; threads.txt
    ("thread" . "")
    ("threadvar" . "")
    ;; locking.txt
    ("guard" . "")
    ("locks" . "[X, ...]")
    ;; effects.txt
    ("raises" . "[EXCEPTION, ...] -- can be used to explicitly define which exceptions a proc/iterator/method/converter is allowed to raise.")
    ("tags" . "[TYPE, ...]")
    ("effects" . "")
    ;; types.txt
    ("nimcall" .
     "default convention used for a Nim proc. It is the same as `fastcall`, but only for C compilers that support `fastcall`")
    ("closure" .
     "default calling convention for a procedural type that lacks any pragma annotations.")
    ("stdcall" .
     "convention as specified by Microsoft; the generated C procedure is declared with `__stdcall` keyword.")
    ("cdecl" .
     "The cdecl convention means that a procedure shall use the same convention as the C compiler.")
    ("safecall" . "")
    ("inline" . "")
    ("fastcall" . "means different things to different C compilers. One gets whatever the C `__fastcall` means.")
    ("syscall" . "syscall convention is the same as `__syscall` in C. It is used for interrupts.")
    ("noconv" . "generated C code will not have any explicit calling convention and thus use the C compiler's default calling convention.")
    ("inheritable" . "introduce new object roots apart from `system.RootObj`")
    ;; http://forum.nim-lang.org/t/1100
    ;; copied Araq's explanation
    ("gensym" . "generate a fresh temporary variable here for every instantiation to resemble function call semantics")
    ("dirty" . "everything is resolved in the instantiation context")
    ("inject" . "the instantiation scope sees this symbol")
    ("immediate" . "don't resolve types and expand this thing eagerly")
    ;; http://nim-lang.org/docs/manual.html#statements-and-expressions-var-statement
    ("noInit" . "avoid implicit initialization for `var`")
    ("requiresInit" . "avoid implicit initialization for `var` and it does a control flow analysis to prove the variable has been initialized and does not rely on syntactic properties")
    ;; http://nim-lang.org/docs/manual.html#types-pre-defined-floating-point-types
    ("NanChecks" . "on|off")
    ("InfChecks" . "on|off")
    ("floatChecks" "on|off")
    ;;; not sure where to look
    ("noinline" . "")
    ("benign" . "")
    ("profiler" . "")
    ("stackTrace" . "")
    ("sideEffect" . "")
    ("compilerRtl" . "")
    ("merge" . "")
    ("gcsafe" . "")
    ("rtl" . "")
    ;; from 14.0
    ("this" . "self|ID -- automatic self insertion")
    ;; seems related to this: http://nim-lang.org/docs/intern.html#how-the-rtl-is-compiled
    ;; but not sure...
    ("compilerProc" . "")
    ("magic" . "compiler intrinsics")
    )
  "Alist of (pragma name . description).
The description is unofficial; PRs are welcome.")

(defconst nim-environment-variables
  '(; from unittest.nim
    "NIMTEST_OUTPUT_LVL" "NIMTEST_NO_COLOR" "NIMTEST_ABORT_ON_ERROR"))


;; obsolete
(make-obsolete
 'nim-tab-face
 "The nim-tab-face was obsoleted, use `white-space-mode' instead to highlight tabs."
 "Oct/20/2017")

;; Added Oct 17, 2017
(define-obsolete-variable-alias 'nim-nimsuggest-path 'nimsuggest-path "Oct/20/2017")
(define-obsolete-variable-alias 'nim-dirty-directory 'nimsuggest-dirty-directory "Oct/20/2017")
(define-obsolete-variable-alias 'nim-suggest-options 'nimsuggest-options "Oct/23/2017")
(define-obsolete-variable-alias 'nim-suggest-local-options 'nimsuggest-local-options "Oct/23/2017")
(define-obsolete-variable-alias 'nim-suggest-ignore-dir-regex 'nimsuggest-ignore-dir-regex "Oct/23/2017")
(define-obsolete-variable-alias 'nim-inside-compiler-dir-p 'nim--inside-compiler-dir-p "Oct/23/2017")

(provide 'nim-vars)
;;; nim-vars.el ends here
