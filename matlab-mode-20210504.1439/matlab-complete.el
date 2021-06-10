;;; matlab-complete.el --- Simple completion tool for matlab-mode
;;
;; Copyright (C) 2019, 2020 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; This library supports a simple completion technique for MATLAB.
;;
;; It uses the following techniques:
;; * Lists of symbols that can be completed (from ~ 2004ish)
;; * scan buffer for functions and symbols.
;; * look for local files that might be functions.
;;
;; Moved to separate library, as some modern tools do a better job
;; and this can be loaded optionally.

;;; Code:
(require 'matlab)

;;; Customizations ===========================================================
;;
(defcustom matlab-completion-technique 'complete
  "*How the `matlab-complete-symbol' interfaces with the user.
Valid values are:

'increment - which means that new strings are tried with each
             successive call until all methods are exhausted.
             (Similar to `hippie-expand'.)
'complete  - Which means that if there is no single completion, then
             all possibilities are displayed in a completion buffer."
  :group 'matlab
  :type '(radio (const :tag "Incremental completion (hippie-expand)."
		       increment)
		(const :tag "Show completion buffer."
		       complete)))

;;; Lists for matlab keywords =================================================

(defvar matlab-keywords-solo
  '("break" "case" "else" "elseif" "end" "for" "parfor" "function" "if" "tic" "toc"
    "otherwise" "profile" "switch" "while" "try" "catch" "spmd")
  "Keywords that appear on a line by themselves.")
(defvar matlab-keywords-return
  '("acos" "acosh" "acot" "acoth" "acsch" "asech" "asin" "asinh"
    "atan" "atan2" "atanh" "cos" "cosh" "coth" "csc" "csch" "exp"
    "log" "log10" "log2" "sec" "sech" "sin" "sinh" "tanh"
    "abs" "sign" "sqrt" )
  "List of MATLAB keywords that have return arguments.
This list still needs lots of help.")
(defvar matlab-keywords-boolean
  '("all" "any" "exist" "isempty" "isequal" "ishold" "isfinite" "isglobal"
    "isinf" "isletter" "islogical" "isnan" "isprime" "isreal" "isspace"
    "logical" "isa")
  "List of keywords that are typically used as boolean expressions.")

(defvar matlab-core-properties
  '("ButtonDownFcn" "Children" "Clipping" "CreateFcn" "DeleteFcn"
    "BusyAction" "HandleVisibility" "HitTest" "Interruptible"
    "Parent" "Selected" "SelectionHighlight" "Tag" "Type"
    "UIContextMenu" "UserData" "Visible")
  "List of properties belonging to all HG objects.")

(defvar matlab-property-lists
  '(("root" .
     ("CallbackObject" "Language" "CurrentFigure" "Diary" "DiaryFile"
      "Echo" "ErrorMessage" "Format" "FormatSpacing" "PointerLocation"
      "MonitorPositions"
      "PointerWindow" "Profile" "ProfileFile" "ProfileCount"
      "ProfileInterval" "RecursionLimit" "ScreenDepth" "ScreenSize"
      "ShowHiddenHandles" "TerminalHideGraphCommand" "TerminalOneWindow"
      "TerminalDimensions" "TerminalProtocol" "TerminalShowGraphCommand"
      "Units" "AutomaticFileUpdates" ))
    ("axes" .
     ("AmbientLightColor" "Box" "CameraPosition" "CameraPositionMode"
      "CameraTarget" "CameraTargetMode" "CameraUpVector"
      "CameraUpVectorMode" "CameraViewAngle" "CameraViewAngleMode" "CLim"
      "CLimMode" "Color" "CurrentPoint" "ColorOrder" "DataAspectRatio"
      "DataAspectRatioMode" "DrawMode" "FontAngle" "FontName" "FontSize"
      "FontUnits" "FontWeight" "GridLineStyle" "Layer" "LineStyleOrder"
      "LineWidth" "NextPlot" "PlotBoxAspectRatio" "PlotBoxAspectRatioMode"
      "Projection" "Position" "TickLength" "TickDir" "TickDirMode" "Title"
      "Units" "View" "XColor" "XDir" "XGrid" "XLabel" "XAxisLocation" "XLim"
      "XLimMode" "XScale" "XTick" "XTickLabel" "XTickLabelMode" "XTickMode"
      "YColor" "YDir" "YGrid" "YLabel" "YAxisLocation" "YLim" "YLimMode"
      "YScale" "YTick" "YTickLabel" "YTickLabelMode" "YTickMode" "ZColor"
      "ZDir" "ZGrid" "ZLabel" "ZLim" "ZLimMode" "ZScale" "ZTick"
      "ZTickLabel" "ZTickLabelMode" "ZTickMode"))
    ("figure" .
     ("BackingStore" "CloseRequestFcn" "Color" "Colormap"
      "CurrentAxes" "CurrentCharacter" "CurrentObject" "CurrentPoint"
      "Dithermap" "DithermapMode" "FixedColors" "IntegerHandle"
      "InvertHardcopy" "KeyPressFcn" "MenuBar" "MinColormap" "Name"
      "NextPlot" "NumberTitle" "PaperUnits" "PaperOrientation"
      "PaperPosition" "PaperPositionMode" "PaperSize" "PaperType"
      "Pointer" "PointerShapeCData" "PointerShapeHotSpot" "Position"
      "Renderer" "RendererMode" "Resize" "ResizeFcn" "SelectionType"
      "ShareColors" "Units" "WindowButtonDownFcn"
      "WindowButtonMotionFcn" "WindowButtonUpFcn" "WindowStyle"))
    ("image" . ("CData" "CDataMapping" "EraseMode" "XData" "YData"))
    ("light" . ("Position" "Color" "Style"))
    ("line" .
     ("Color" "EraseMode" "LineStyle" "LineWidth" "Marker" "LineSmoothing"
      "MarkerSize" "MarkerEdgeColor" "MarkerFaceColor" "XData" "YData"
      "ZData"))
    ("patch" .
     ("CData" "CDataMapping" "FaceVertexCData" "EdgeColor" "EraseMode"
      "FaceColor" "Faces" "LineStyle" "LineWidth" "Marker" "LineSmoothing"
      "MarkerEdgeColor" "MarkerFaceColor" "MarkerSize" "Vertices"
      "XData" "YData" "ZData" "FaceLighting" "EdgeLighting"
      "BackFaceLighting" "AmbientStrength" "DiffuseStrength"
      "SpecularStrength" "SpecularExponent" "SpecularColorReflectance"
      "VertexNormals" "NormalMode"))
    ("surface" .
     ("CData" "CDataMapping" "EdgeColor" "EraseMode" "FaceColor"
      "LineStyle" "LineWidth" "Marker" "MarkerEdgeColor" "LineSmoothing"
      "MarkerFaceColor" "MarkerSize" "MeshStyle" "XData" "YData"
      "ZData" "FaceLighting" "EdgeLighting" "BackFaceLighting"
      "AmbientStrength" "DiffuseStrength" "SpecularStrength"
      "SpecularExponent" "SpecularColorReflectance" "VertexNormals"
      "NormalMode"))
    ("text\\|title\\|xlabel\\|ylabel\\|zlabel" .
     ("Color" "EraseMode" "Editing" "Extent" "FontAngle" "FontName"
      "FontSize" "FontUnits" "FontWeight" "HorizontalAlignment"
      "BackgroundColor" "EdgeColor" "Margin"
      "Position" "Rotation" "String" "Units" "Interpreter"
      "VerticalAlignment"))
    ("uicontextmenu" . ("Callback"))
    ("uicontrol" .
     ("BackgroundColor" "Callback" "CData" "Enable" "Extent"
      "FontAngle" "FontName" "FontSize" "FontUnits" "FontWeight"
      "ForegroundColor" "HorizontalAlignment" "ListboxTop" "Max" "Min"
      "Position" "String" "Style" "SliderStep" "TooltipString" "Units"
      "Value"))
    ("uimenu" .
     ("Accelerator" "Callback" "Checked" "Enable" "ForegroundColor"
      "Label" "Position" "Separator"))
    ;; Flesh this out more later.
    ("uipushtool\\|uitoggletool\\|uitoolbar" .
     ("Cdata" "Callback" "Separator" "Visible"))
    )
  "List of property lists on a per object type basis.")

(defvar matlab-unknown-type-commands
  "[gs]et\\|findobj\\|waitfor"
  "Expression for commands that have unknown types.")

(defun matlab-all-known-properties ()
  "Return a list of all properties."
  (let ((lst matlab-core-properties)
	(tl matlab-property-lists))
    (while tl
      (setq lst (append lst (cdr (car tl)))
	    tl (cdr tl)))
    (matlab-uniquify-list lst)))

(defvar matlab-all-known-properties (matlab-all-known-properties)
  "List of all the known properties.")

;;;###autoload
(defmacro matlab-property-function ()
  "Regexp of all builtin functions that take property lists."
  '(let ((r matlab-unknown-type-commands)
	 (tl matlab-property-lists))
     (while tl
       (setq r (concat r "\\|" (car (car tl)))
	     tl (cdr tl)))
     r))

;;; Completion Framework ===================================================
;;

(defun matlab-find-recent-variable-list (prefix)
  "Return a list of most recent variables starting with PREFIX as a string.
Reverse searches for the following are done first:
  1) Assignment
  2) if|for|while|switch <var>
  3) global variables
  4) function arguments.
All elements are saved in a list, which is then uniquified.
If NEXT is non-nil, then the next element from the saved list is used.
If the list is empty, then searches continue backwards through the code."
  (matlab-navigation-syntax
    (let* ((bounds (save-excursion
		     (if (re-search-backward "^\\s-*function\\>" nil t)
			 (match-beginning 0) (point-min))))
	   (syms
	    (append
	     (save-excursion
	       (let ((lst nil))
		 (while (and
			 (re-search-backward
			  (concat "^\\s-*\\(" prefix "\\w+\\)\\s-*=")
			  bounds t)
			 (< (length lst) 10))
		   (setq lst (cons (match-string 1) lst)))
		 (nreverse lst)))
	     (save-excursion
	       (let ((lst nil))
		 (while (and (re-search-backward
			      (concat "\\<\\(" matlab-block-beg-pre-no-if
				      "\\)\\s-+(?\\s-*\\(" prefix
				      "\\w+\\)\\>")
			      bounds t)
			     (< (length lst) 10))
		   (setq lst (cons (match-string 2) lst)))
		 (nreverse lst)))
	     (save-excursion
	       (if (re-search-backward "^\\s-*global\\s-+" bounds t)
		   (let ((lst nil) m e)
		     (goto-char (match-end 0))
		     (while (looking-at "\\(\\w+\\)\\([ \t]+\\|$\\)")
		       (setq m (match-string 1)
			     e (match-end 0))
		       (if (equal 0 (string-match prefix m))
			   (setq lst (cons m lst)))
		       (goto-char e))
		     (nreverse lst))))
	     (save-excursion
	       (if (and (re-search-backward "^\\s-*function\\>" bounds t)
			(re-search-forward "\\<\\(\\w+\\)("
					   (matlab-point-at-eol) t))
		   (let ((lst nil) m e)
		     (while (looking-at "\\(\\w+\\)\\s-*[,)]\\s-*")
		       (setq m (match-string 1)
			     e (match-end 0))
		       (if (equal 0 (string-match prefix m))
			   (setq lst (cons m lst)))
		       (goto-char e))
		     (nreverse lst))))))
	   (fl nil))
      (while syms
	(if (car syms) (setq fl (cons (car syms) fl)))
	(setq syms (cdr syms)))
      (matlab-uniquify-list (nreverse fl)))))

(defvar matlab-most-recent-variable-list nil
  "Maintained by `matlab-find-recent-variable'.")

(defun matlab-find-recent-variable (prefix &optional next)
  "Return the most recently used variable starting with PREFIX as a string.
See `matlab-find-recent-variable-list' for details.
In NEXT is non-nil, than continue through the list of elements."
  (if next
      (let ((next (car matlab-most-recent-variable-list)))
	(setq matlab-most-recent-variable-list
	      (cdr matlab-most-recent-variable-list))
	next)
    (let ((syms (matlab-find-recent-variable-list prefix))
	  (first nil))
      (if (eq matlab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq matlab-most-recent-variable-list (cdr syms))
	first))))

(defun matlab-find-user-functions-list (prefix)
  "Return a list of user defined functions that match PREFIX."
  (matlab-navigation-syntax
    (let ((syms
	   (append
	    (save-excursion
	      (goto-char (point-min))
	      (let ((lst nil))
		(while (re-search-forward "^\\s-*function\\>" nil t)
		  (if (re-search-forward
		       (concat "\\(" prefix "\\w+\\)\\s-*\\($\\|(\\)")
		       (matlab-point-at-eol) t)
		      (setq lst (cons (match-string 1) lst))))
		(nreverse lst)))
	    (let ((lst nil)
		  (files (directory-files
			  default-directory nil
			  (concat "^" prefix
				  "[a-zA-Z][a-zA-Z0-9_]+\\.m$"))))
	      (while files
		(setq lst (cons (progn (string-match "\\.m" (car files))
				       (substring (car files) 0
						  (match-beginning 0)))
				lst)
		      files (cdr files)))
	      lst)))
	  (fl nil))
      (while syms
	(if (car syms) (setq fl (cons (car syms) fl)))
	(setq syms (cdr syms)))
      (matlab-uniquify-list (nreverse fl)))))

(defvar matlab-user-function-list nil
  "Maintained by `matlab-find-user-functions'.")

(defun matlab-find-user-functions (prefix &optional next)
  "Return a user function that match PREFIX and return it.
If optional argument NEXT is non-nil, then return the next found
object."
  (if next
      (let ((next (car matlab-user-function-list)))
	(setq matlab-user-function-list (cdr matlab-user-function-list))
	next)
    (let ((syms (matlab-find-user-functions-list prefix))
	  (first nil))
      (if (eq matlab-completion-technique 'complete)
	  syms
	(setq first (car syms))
	(setq matlab-user-function-list (cdr syms))
	first))))

(defvar matlab-generic-list-placeholder nil
  "Maintained by `matlab-generic-list-expand'.
Holds sub-lists of symbols left to be expanded.")

(defun matlab-generic-list-expand (list prefix &optional next)
  "Return an element from LIST that start with PREFIX.
If optional NEXT argument is non nil, then the next element in the
list is used.  nil is returned if there are not matches."
  (if next
      (let ((next (car matlab-generic-list-placeholder)))
	(setq matlab-generic-list-placeholder
	      (cdr matlab-generic-list-placeholder))
	next)
    (let ((re (concat "^" (regexp-quote prefix)))
	  (first nil)
	  (fl nil))
      (while list
	(if (string-match re (car list))
	    (setq fl (cons (car list) fl)))
	(setq list (cdr list)))
      (setq fl (nreverse fl))
      (if (eq matlab-completion-technique 'complete)
	  fl
	(setq first (car fl))
	(setq matlab-generic-list-placeholder (cdr fl))
	first))))

(defun matlab-solo-completions (prefix &optional next)
  "Return PREFIX matching elements for solo symbols.
If NEXT then the next patch from the list is used."
  (matlab-generic-list-expand matlab-keywords-solo prefix next))

(defun matlab-value-completions (prefix &optional next)
  "Return PREFIX matching elements for value symbols.
If NEXT then the next patch from the list is used."
  (matlab-generic-list-expand matlab-keywords-return prefix next))

(defun matlab-boolean-completions (prefix &optional next)
  "Return PREFIX matching elements for boolean symbols.
If NEXT then the next patch from the list is used."
  (matlab-generic-list-expand matlab-keywords-boolean prefix next))

(defun matlab-property-completions (prefix &optional next)
  "Return PREFIX matching elements for property names in strings.
If NEXT then the next property from the list is used."
  (let ((f (matlab-function-called-at-point))
	(lst matlab-property-lists)
	(foundlst nil)
	(expandto nil))
    ;; Look for this function.  If it is a known function then we
    ;; can now use a subset of available properties!
    (while (and lst (not foundlst))
      (if (string= (car (car lst)) f)
	  (setq foundlst (cdr (car lst))))
      (setq lst (cdr lst)))
    (if foundlst
	(setq foundlst (append foundlst matlab-core-properties))
      (setq foundlst matlab-all-known-properties))
    (setq expandto (matlab-generic-list-expand foundlst prefix next))
    ;; This looks to see if we have a singular completion.  If so,
    ;; then return it, and also append the "'" to the end.
    (cond ((and (listp expandto) (= (length expandto) 1))
	   (setq expandto (list (concat (car expandto) "'"))))
	  ((stringp expandto)
	   (setq expandto (concat expandto "'"))))
    expandto))

(defvar matlab-last-prefix nil
  "Maintained by `matlab-complete-symbol'.
The prefix used for the first completion command.")
(defvar matlab-last-semantic nil
  "Maintained by `matlab-complete-symbol'.
The last type of semantic used while completing things.")
(defvar matlab-completion-search-state nil
  "List of searching things we will be doing.")

;;;###autoload
(defun matlab-complete-symbol (&optional arg)
  "Complete a partially typed symbol in a MATLAB mode buffer."
  (interactive "P")
  (if (and (featurep 'matlab-shell) (matlab-shell-active-p) matlab-shell-ask-MATLAB-for-completions)
      ;; Use MATLAB shell if active and asking for completions is enabled.
      (matlab-complete-symbol-with-shell arg)
    ;; Else, do the antique version.
    (matlab-complete-symbol-local arg)
    ))

(defun matlab-complete-symbol-with-shell (&optional arg)
  "Complete a partially typed symbol in a MATLAB mode buffer using `matlab-shell'.
Use `completion-in-region' to support the completion behavior."
  (interactive "P")
  ;; Try to do completion with the shell
  (matlab-navigation-syntax
    (let* ((common-substr-start-pt nil)
	   (common-substr-end-pt nil)
	   (prefix (if (and (not (eq last-command 'matlab-complete-symbol))
			    (member (preceding-char) '(?  ?\t ?\n ?, ?\( ?\[ ?\')))
		       ""
		     (buffer-substring-no-properties
		      (save-excursion (forward-word -1) (setq common-substr-start-pt (point)))
		      (setq common-substr-end-pt (point)))))
	   (completion-info (matlab-shell-completion-list prefix))
           (completions (cdr (assoc 'completions completion-info)))
	   )
      (completion-in-region common-substr-start-pt common-substr-end-pt completions)
      ))
  )
  
(defun matlab-complete-symbol-local (&optional arg)
  "Complete a partially typed symbol in a MATLAB mode buffer.
If the previously entered command was also `matlab-complete-symbol'
then undo the last completion, and find a new one.
  The types of symbols tried are based on the semantics of the current
cursor position.  There are two types of symbols.  For example, if the
cursor is in an if statement, boolean style functions and symbols are
tried first.  If the line is blank, then flow control, or high level
functions are tried first.
  The completion technique is controlled with `matlab-completion-technique'
It defaults to incremental completion described above.  If a
completion list is preferred, then change this to 'complete.  If you
just want a completion list once, then use the universal argument ARG
to change it temporarily."
  (interactive "P")
  (matlab-navigation-syntax
    (let* ((prefix (if (and (not (eq last-command 'matlab-complete-symbol))
			    (member (preceding-char) '(?  ?\t ?\n ?, ?\( ?\[ ?\')))
		       ""
		     (buffer-substring-no-properties
		      (save-excursion (forward-word -1) (point))
		      (point))))
	   (sem (matlab-lattr-semantics prefix))
	   (matlab-completion-technique
	    (if arg (cond ((eq matlab-completion-technique 'complete)
			   'increment)
			  (t 'complete))
	      matlab-completion-technique)))
      (if (not (eq last-command 'matlab-complete-symbol))
	  (setq matlab-last-prefix prefix
		matlab-last-semantic sem
		matlab-completion-search-state
		(cond ((eq sem 'solo)
		       '(matlab-solo-completions
			 matlab-find-user-functions
			 matlab-find-recent-variable))
		      ((eq sem 'boolean)
		       '(matlab-find-recent-variable
			 matlab-boolean-completions
			 matlab-find-user-functions
			 matlab-value-completions))
		      ((eq sem 'value)
		       '(matlab-find-recent-variable
			 matlab-find-user-functions
			 matlab-value-completions
			 matlab-boolean-completions))
		      ((eq sem 'property)
		       '(matlab-property-completions
			 matlab-find-user-functions
			 matlab-find-recent-variable
			 matlab-value-completions))
		      (t '(matlab-find-recent-variable
			   matlab-find-user-functions
			   matlab-value-completions
			   matlab-boolean-completions)))))
      (cond
       ((eq matlab-completion-technique 'increment)
	(let ((r nil) (donext (eq last-command 'matlab-complete-symbol)))
	  (while (and (not r) matlab-completion-search-state)
	    (message "Expand with %S" (car matlab-completion-search-state))
	    (setq r (funcall (car matlab-completion-search-state)
			     matlab-last-prefix donext))
	    (if (not r) (setq matlab-completion-search-state
			      (cdr matlab-completion-search-state)
			      donext nil)))
	  (delete-region (point) (progn (forward-char (- (length prefix)))
					(point)))
	  (if r
	      (insert r)
	    (insert matlab-last-prefix)
	    (message "No completions."))))
       ((eq matlab-completion-technique 'complete)
	(let ((allsyms (apply 'append
			      (mapcar (lambda (f) (funcall f prefix))
				      matlab-completion-search-state))))
	  (cond ((null allsyms)
		 (message "No completions.")
		 (ding))
		((= (length allsyms) 1)
		 (delete-region (point) (progn
					  (forward-char (- (length prefix)))
					  (point)))
		 (insert (car allsyms)))
		((= (length allsyms) 0)
		 (message "No completions."))
		(t
		 (let* ((al (mapcar (lambda (a) (list a)) allsyms))
			(c (try-completion prefix al)))
		   ;; This completion stuff lets us expand as much as is
		   ;; available to us. When the completion is the prefix
		   ;; then we want to display all the strings we've
		   ;; encountered.
		   (if (and (stringp c) (not (string= prefix c)))
		       (progn
			 (delete-region
			  (point)
			  (progn (forward-char (- (length prefix)))
				 (point)))
			 (insert c))
		     ;; `display-completion-list' does all the complex
		     ;; ui work for us.
		     (with-output-to-temp-buffer "*Completions*"
		       (display-completion-list
			(matlab-uniquify-list allsyms)))))))))))))


(provide 'matlab-complete)

;;; matlab-complete.el ends here

;; LocalWords:  el Ludlam zappo ish defcustom CLim XColor XDir XLabel
;; LocalWords:  XAxis XScale YColor YDir YAxis YScale YTick ZColor ZDir ZGrid
;; LocalWords:  ZLabel ZScale ZTick Dithermap defun lst tl setq cdr defmacro
;; LocalWords:  nreverse eol progn foundlst expandto listp stringp sem lattr
;; LocalWords:  donext funcall allsyms mapcar
