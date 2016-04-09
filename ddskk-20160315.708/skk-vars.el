;;; skk-vars.el --- common vars and consts in SKK -*- coding: iso-2022-7bit-ss2 -*-

;; Copyright (C) 1999-2010 SKK Development Team <skk@ring.gr.jp>

;; Author: SKK Development Team <skk@ring.gr.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'wid-edit)

(eval-when-compile
  ;; shut down compiler warnings.
  (defvar charset-list)
  (defvar word-across-newline)
  (defvar emacs-beta-version)
  (defvar mule-version))

;; Functions needed prior to loading skk-macs.el.
(when (eval-when-compile (featurep 'emacs))
  (defsubst find-coding-system (obj)
    "Return OBJ if it is a coding-system."
    (if (coding-system-p obj)
	obj)))

(defun skk-find-window-system ()
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; GNU Emacs 23 or later
    (let ((frames (frame-list))
	  val)
      (while (and (not val) frames)
	;; $BJQ?t(B window-system $B$O(B frame local $BCM$r;}$D!#(B
	;; $BNc$($P(B window system $B$H(B "emacsclient -nw" $B$NJ;MQ;~$J$I(B
	;; $B$$$:$l$+$N(B frame $B$,(B window system $B2<$GF0$$$F$$$k$3$H$r(B
	;; $B3NG'$9$k!#(B
	(setq val (window-system (car frames))
	      frames (cdr frames)))
      val))
   (t
    ;; Emacs 22 and XEmacs
    window-system)))

;;;###autoload
(put 'skk-deflocalvar 'lisp-indent-function 'defun)
(defmacro skk-deflocalvar (symbol initvalue &optional docstring)
  (if docstring
      `(progn
	 (defvar ,symbol ,initvalue ,docstring)
	 (make-variable-buffer-local ',symbol))
    `(progn
       (defvar ,symbol ,initvalue)
       (make-variable-buffer-local ',symbol))))

(defconst skk-ml-address "skk@ring.gr.jp")
(defconst skk-ml-command-address "skk-subscribe@ring.gr.jp")

;;;; Custom group definitions

(defgroup skk nil "$BF|K\8lF~NO%7%9%F%`(B SKK $B$N%+%9%?%^%$%:(B"
  :group 'mule
  :group 'applications)

;;; by function
(defgroup skk-basic nil "SKK $B4pK\$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-dictionary nil "SKK $B<-=q$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-private nil "SKK $B8D?M<-=q$N<h$j07$$(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-input-basic nil "SKK $BF~NOF0:n$N4pK\@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-input-enhanced nil "SKK $BF~NOF0:n$N3HD%@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-henkan nil "SKK $BJQ49F0:n$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kakutei nil "SKK $BJQ498uJd3NDjF0:n$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-okurigana nil "SKK $BAw$j$,$J$N<h$j07$$(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-misc nil "SKK $B$=$NB>$$$m$$$m(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-visual nil "SKK $B$N8+$?L\$r%+%9%?%^%$%:(B"
  :prefix "skk-"
  :group 'skk)

;;; by filename
(defgroup skk-annotation nil "SKK $B%"%N%F!<%7%g%sI=<((B/$BJT=8$N@_Dj(B"
  :prefix "skk-annotation-"
  :group 'skk)

(defgroup skk-auto nil "SKK $B<+F0Aw$j$,$J5!G=$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-cdb nil "SKK CDB $B<-=q8!:w5!G=$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-comp nil "SKK $BJd405!G=$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-server-completion nil "$B<-=q%5!<%PJd405!G=$K4X$9$k@_Dj(B"
  :group 'skk)

(defgroup skk-cursor nil "SKK $B%+!<%=%k@)8f$N@_Dj(B"
  :prefix "skk-cursor-"
  :group 'skk)

(defgroup skk-dcomp nil "SKK $BF0E*Jd40$N@_Dj(B"
  :prefix "skk-dcomp-"
  :group 'skk)

(defgroup skk-gadget nil "SKK $B<B9TJQ495!G=(B (gadget) $B$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-hint nil "SKK $B%R%s%HIU$-JQ495!G=$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-isearch nil "SKK $B%$%s%/%j%a%s%?%k!&%5!<%A$N@_Dj(B"
  :prefix "skk-isearch-"
  :group 'skk)

(defgroup skk-jisx0201 nil "SKK JIS X 0201 ($B$*$b$KH>3Q%+%J(B) $B4XO"$N@_Dj(B"
  :prefix "skk-jisx0201-"
  :group 'skk)

(defgroup skk-jisx0213 nil "SKK JIS X 0213 $B4XO"$N@_Dj(B"
  :group 'skk)

(defgroup skk-jisyo-edit-mode nil "SKK $B$N<-=qJT=85!G=$N@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kakasi nil "SKK $B$+$i(B kakasi $B$r;H$&@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-kcode nil "SKK $BJ8;z%3!<%I$r07$&@_Dj(B"
  :prefix "skk-"
  :group 'skk)

(defgroup skk-look nil "SKK $B$+$i(B look $B%3%^%s%I$rMxMQ$9$k@_Dj(B"
  :prefix "skk-look-"
  :group 'skk)

(defgroup skk-lookup nil "SKK $B$+$i(B Lookup $B%Q%C%1!<%8$rMxMQ$9$k@_Dj(B"
  :prefix "skk-lookup-"
  :group 'skk)

(defgroup skk-num nil "SKK $B$G?t;z$r07$&$?$a$N@_Dj(B"
  :prefix "skk-num-"
  :group 'skk)

(defgroup skk-server nil "$B<-=q%5!<%P$H$NDL?.$K4X$9$k@_Dj(B"
  :prefix "skk-server-"
  :group 'skk)

(defgroup skk-sticky nil "SKK $BJQ490LCV;XDjJ}<0$N@_Dj(B"
  :prefix "skk-sticky-"
  :group 'skk)

(defgroup skk-study nil "SKK $B3X=,5!G=$N@_Dj(B"
  :prefix "skk-study-"
  :group 'skk)

(defgroup skk-tankan nil "SKK $BC14A;zJQ495!G=$N@_Dj(B"
  :prefix "skk-tankan-"
  :group 'skk)

(defgroup skk-tooltip nil "SKK $B%D!<%k%F%#%C%WI=<($N@_Dj(B"
  :prefix "skk-tooltip-"
  :group 'skk)

(defgroup skk-tut nil "SKK $B%A%e!<%H%j%"%k$N@_Dj(B"
  :prefix "skk-tut-"
  :group 'skk)

(defgroup skk-viper nil "SKK/Viper $B4XO"$N@_Dj(B"
  :prefix "skk-viper-"
  :group 'skk)

(defgroup skk-act nil "SKK $B$G3HD%%m!<%^;zF~NO(B ACT $B$r;H$&@_Dj(B"
  :prefix "skk-act-"
  :group 'skk-input-enhanced)

(defgroup skk-azik nil "SKK $B$G3HD%%m!<%^;zF~NO(B AZIK $B$r;H$&@_Dj(B"
  :prefix "skk-azik-"
  :group 'skk-input-enhanced)

(defgroup skk-kanagaki nil "SKK $B$+$JF~NO$N@_Dj(B"
  :prefix "skk-kanagaki-"
  :group 'skk-input-enhanced)

(defgroup skk-nicola nil "SKK $B?F;X%7%U%HF~NO$N@_Dj(B"
  :prefix "skk-nicola-"
  :group 'skk-kanagaki)

;;; skk-vars.el related.
(defcustom skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (or frame-background-mode
      (cond
       ((featurep 'xemacs)
	(if (< (apply '+ (color-rgb-components
			  (face-property 'default 'background)))
	       (/ (apply '+ (color-rgb-components
			     (make-color-specifier "white")))
		  3))
	    'dark
	  'light))
       (t
	(cond
	 ((and window-system (x-display-color-p))
	  (let ((bg-resource (x-get-resource ".backgroundMode"
					     "BackgroundMode"))
		(params (frame-parameters)))
	    (cond
	     (bg-resource
	      (intern (downcase bg-resource)))
	     ((and (eq system-type 'windows-nt)
		   (not (fboundp 'x-color-values)))
	      (if (string-match "light"
				(cdr (assq 'background-color params)))
		  'light
		'dark))
	     ((not (null (cdr (assq 'background-mode params))))
	      ;; Emacs20.x (Meadow)
	      (cdr (assq 'background-mode params)))
	     ((< (apply '+ (x-color-values
			    (cdr (assq 'background-color params))))
		 (/ (apply '+ (x-color-values "white")) 3))
	      'dark)
	     (t
	      'light))))
	 (t
	  nil)))))
  "*SKK $B$NI8=`$N%U%'%$%9?'$r7h$a$k$?$a$NGX7J?'$K4X$9$k>pJs!#(B
$BI8=`$G$O(B `frame-background-mode' $B$r@_Dj$7$F$$$k>l9g$O$=$l$K=>$$!"(B
$B@_Dj$7$F$$$J$$>l9g$OFH<+$NJ}K!$G(B `light' $B$+(B `dark' $B$+$r7h$a$k!#(B
$B$?$@$7!"%?!<%_%J%k$G(B Emacs $B$rMxMQ$7$F$$$k>l9g$OH=Dj$G$-$:!"(B
$B%f!<%6$N0U?^$H9g$o$J$$$+$b$7$l$J$$$N$G!"$3$N%*%W%7%g%s$+(B
`frame-background-mode' $B$r$"$i$+$8$a@_Dj$7$F$*$/$3$H$,K>$^$7$$!#(B
$B$3$N%*%W%7%g%s$O(B ~/.skk $B$K@_Dj$7$F$bH?1G$5$l$J$$!#(B~/.emacs.d/init.el $B$+(B
\\[customize] $B$K$F!"(BSKK $B$,FI$_9~$^$l$kA0$K@_Dj$9$k$3$H$,I,MW!#(B"
  :type '(choice (const dark)
		 (const light)
		 (const :tag "$B<+F0$G7h$a$k(B" nil))
  :group 'skk-basic
  :group 'skk-visual)

;;; skk.el related.
(defcustom skk-user-directory nil
  "*SKK $B$N@_Dj%U%!%$%k$J$I$rCV$/%G%#%l%/%H%jL>!#(B
$B3F<o@_Dj%U%!%$%k$r$R$H$D$N%G%#%l%/%H%j$K$^$H$a$?$$>l9g$K@_Dj$9$k!#(B

  ($BNc(B) (setq skk-user-directory \"~/.ddskk\")
"
  :type '(radio (directory :tag "$B%G%#%l%/%H%jL>(B" "~/.ddskk")
		(const :tag "$B@_Dj$7$J$$(B" nil))
  :group 'skk-basic)

(defcustom skk-init-file (if skk-user-directory
			     (expand-file-name "init" skk-user-directory)
			   (convert-standard-filename "~/.skk"))
  "*SKK $B$N=i4|@_Dj$r5-=R$9$k%U%!%$%kL>!#(BSKK $B$r5/F0$7$?:G=i$N0lEY$@$1FI$_(B
$B9~$^$l$k!#$3$N%U%!%$%k$K5-=R$9$kBe$o$j$K(B ~/.emacs.d/init.el $B$K(B SKK $B$N3F<o=i4|@_Dj$r(B
$B5-=R$9$k$3$H$b2DG=$@$,!"8e<T$N>l9g$O(B \\[skk-restart] $B$G$OH?1G$5$l$J$$!#(B

~/.emacs.d/init.el $B$G(B $BJQ?t(B `skk-byte-compile-init-file' $B$r@_Dj$9$k$3$H$G(B `skk-init-file' $B$r(B
$B<+F0E*$K%P%$%H%3%s%Q%$%k$9$k$3$H$b2DG=!#(B"
  ;;"*Name of the SKK initialization file.
  ;;From skk.el 9.x on all customization may be done in ~/.emacs."
  :type '(file :tag "$B%U%!%$%kL>(B")
  :group 'skk-basic)

(defcustom skk-japanese-message-and-error nil
  "*Non-nil $B$G$"$l$P!"(BSKK $B$N%a%C%;!<%8$H%(%i!<$rF|K\8l$GI=<($9$k!#(B
nil $B$G$"$l$P!"1Q8l$GI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-basic)

(defcustom skk-version-codename-ja nil
  "*Non-nil $B$G$"$l$P!"4X?t(B `skk-version' $B$G$N%3!<%I%M!<%`$rF|K\8l$GI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-basic)

(defcustom skk-jisyo-fix-order nil
  "*Non-nil $B$G$"$l$P!"3NDj$N:]$K8D?M<-=q$NF12;8l$N=g=x$rJQ99$;$:!"(B
$B8D?M<-=q$K?75,DI2C$9$k:]$O4{=P8l$N8e$KDI2C$9$k!#(B"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-kakutei-jisyo nil
  ;; $B%=!<%H$5$l$F$$$kI,MW$,$"$k$+$I$&$+$O@_Dj<!Bh$@$,!"$=$3$^$G@bL@$9$k$N$OLLE](B
  ;; (FILE . CODE) $B$N7A<0$b$$$1$k$O$:(B ($B$=$N$h$&$J@_Dj$N$7$+$?$ONI$/$J$$(B?)
  "*$B!V3NDjJQ49!W$G8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G$"$l$P!";XDj$5$l$?<-=q$r%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B
$B3F8+=P$78l$N:G=i$N%(%s%H%j$G3NDj$5$l$k!#(B
$B3NDj%"%s%I%%;~$K$O(B 2 $BHVL\0J9_$N%(%s%H%j$bMxMQ$G$-$k$,!"(B
$B$3$N;EMM$OJQ99$5$l$k2DG=@-$b$"$j!"$^$?3NDj<-=q$NK\<A$HL54X78$G$"$k!#(B

$B4X?t(B `skk-search-kakutei-jisyo-file' $B$N0z?t$H$7$F;HMQ$5$l$k!#(B
$B3NDjJQ495!G=$rMxMQ$9$k>l9g$K$O!"(B
  (skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
$B$N$h$&$JMWAG$r(B `skk-search-prog-list' $B$N@hF,$KG[CV$9$k$3$H!#(B"
  ;;  "*The first dictionary to be searched.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;The keys must be sorted.
  ;;Only the first entry in each key is checked; if several entries are
  ;;present the second and following entries are ignored.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(radio (file :tag "$B<-=q%U%!%$%kL>(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dictionary)

(defcustom skk-initial-search-jisyo nil
  ;; $B%=!<%H$5$l$F$$$kI,MW$,$"$k$+$I$&$+$O@_Dj<!Bh$@$,!"$=$3$^$G@bL@$9$k$N$OLLE](B
  ;; (FILE . CODE) $B$N7A<0$b$$$1$k$O$:(B
  "*$B8D?M<-=q$N8!:w$NA0$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G$"$l$P!";XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B

`skk-search-prog-list' $B$K$*$$$F!"(B
  (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
$B$N$h$&$JMWAG$,(B
  (skk-search-jisyo-file skk-jisyo 0 t)
$B$h$j@h$KG[CV$5$l$F$$$k;v$K$h$j$=$N0UL#$r@.$7$F$$$k!#(B"
  ;;  "*This dictionary is searched before the user's personal dictionary.
  ;;The keys must be sorted.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(radio (file :tag "$B<-=q%U%!%$%kL>(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dictionary)

(defcustom skk-large-jisyo nil
  ;; (FILE . CODE) $B$N7A<0$b$$$1$k$O$:(B
  "*$B8D?M<-=q$N8!:w$N8e$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G$"$l$P!";XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B"
  :type `(radio (file :tag "$B<-=q%U%!%$%kL>(B"
		      ,(cond
			((featurep 'xemacs)
			 (or (locate-data-file "SKK-JISYO.L")
			     ""))
			((fboundp 'locate-file)
			 (or (locate-file "skk/SKK-JISYO.L"
					  (list
					   (expand-file-name "../../.."
							     data-directory)))
			     (locate-file "skk/SKK-JISYO.L"
					  (list data-directory))
			     ""))))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dictionary)

(defcustom skk-aux-large-jisyo nil
  ;; (FILE . CODE) $B$N7A<0$b$$$1$k$O$:(B
  "*$B<-=q%5!<%P$,;H$($J$$;~$K!"Be$o$j$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G$"$l$P!"<-=q%5!<%P$,(B active $B$G$J$$;~$K!"(B
$B;XDj$5$l$?<-=q$r%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B"
  :type `(radio (file :tag "$B<-=q%U%!%$%kL>(B"
		      ,(cond
			((featurep 'xemacs)
			 (or (locate-data-file "SKK-JISYO.L")
			     ""))
			((fboundp 'locate-file)
			 (or (locate-file "skk/SKK-JISYO.L"
					  (list
					   (expand-file-name "../../.."
							     data-directory)))
			     (locate-file "skk/SKK-JISYO.L"
					  (list data-directory))
			     ""))))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dictionary
  :group 'skk-server)

(defcustom skk-inhibit-ja-dic-search nil
  "*$B!V(BGNU Emacs $BIUB0$N<-=q$rMQ$$$?8!:w!W$N6X;_$r;X<($9$k%*%W%7%g%s!#(B
GNU Emacs $B$K$O(B SKK-JISYO.L $B$r85$KJQ49$5$l$?(B ja-dic.el $B$H$$$&<-=q$,IUB0$9$k!#(B
$B$3$l$rMQ$$$FDL>o$N$+$J4A;zJQ49(B ($BAw$j$"$j!"Aw$j$J$7!"@\F,<-!"@\Hx<-(B) $B$,2DG=(B
$B$G$"$k(B ($B$?$@$7(B SKK-JISYO.L $B$K$h$k1Q?tJQ49!"?tCMJQ49$J$I$O$G$-$J$$(B)$B!#(B
DDSKK 14.2 $B$h$j!V(Bja-dic.el $B8!:w5!G=(B `skk-search-ja-dic'$B!W$,DI2C$5$l$?!#(B
$B$3$N(B `skk-search-ja-dic' $B$O!"(B `skk-large-jisyo'$B!"(B`skk-aux-large-jisyo'$B!"(B
`skk-cdb-large-jisyo' $B5Z$S(B `skk-server-host' $B$NA4$F$,L58z$J>l9g$KM-8z$H$J$k(B
$B$,!"$"$i$f$k>lLL$G6X;_$7$?$$>l9g$O!"$3$NJQ?t$r(B Non-nil $B$K@_Dj$9$k!#(B"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-extra-jisyo-file-list nil
  "*$B%a%$%s<-=q$NB>$K8!:w$9$k<-=q$N%j%9%H$r;XDj$9$k!#(B
$B$$$:$l$N<-=q$b!"8+=P$78l$O%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B

  (setq skk-extra-jisyo-file-list
        (list '(\"/usr/share/skk/SKK-JISYO.JIS3_4\" . euc-jisx0213)
             \"/usr/share/skk/SKK-JISYO.zipcode\"))

SKK $B<-=q$K$O(B SKK OpenLab $B$GG[I[$7$F$$$k$b$N!"Bh;0<T$K$h$k$b$N$J$IB??t$"$k$,!"(B
$B%a%$%s<-=q(B (SKK-JISYO.L $B$d<-=q%5!<%P$J$I(B) $B$NB>$K8!:w$7$?$$<-=q$N%U%!%$%kL>$N(B
$B%j%9%H$r;XDj$9$k!#%U%!%$%kL>$NBe$o$j$K!"%U%!%$%kL>$H%3!<%I7O$N%Z%"$r;XDj$9$k(B
$B$3$H$b$G$-$k!#<-=q$O;XDj$5$l$?=g$K8!:w$5$l$k!#(B"
  :type '(repeat (file :tag "$B<-=q%U%!%$%kL>(B"))
  :group 'skk-dictionary)

(defcustom skk-itaiji-jisyo nil
  "$B0[BN;z<-=q(B `SKK-JISYO.itaiji', `SKK-JISYO.itaiji.JIS3_4' $B$X$N%Q%9$r;XDj$9$k!#(B"
  :type '(radio (file :tag "$B<-=q%U%!%$%kL>(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-tankan-search 'skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    (skk-okuri-search)
    (skk-search-cdb-jisyo skk-cdb-large-jisyo)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-server skk-aux-large-jisyo 10000)
    (skk-search-ja-dic-maybe)
    (skk-search-extra-jisyo-files)
    (skk-search-katakana-maybe)
    (skk-search-sagyo-henkaku-maybe)
    (skk-search-itaiji))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B

$B$3$NJQ?t$NCM$r<jF0$GJQ99$9$k$H!"(BSKK $B$NF0:n$K1F6A$9$k2DG=@-$,$"$k$N$GCm0U$rMW$9$k!#(B

$BJQ49$7$?8uJd$rJV$9(B S $B<0$r%j%9%H$N7A$KI=5-$7$?$b$N!#(B
$B4X?t(B `skk-search' $B$,(B `skk-search-prog-list' $B$N(B car $B$+$i8eJ}8~$X=gHV$K(B S $B<0$r(B
$BI>2A$9$k$3$H$K$h$C$F$+$J4A;zJQ49$r<B9T$9$k!#(B

$BI,MW$K1~$8$F(B
  (skk-okuri-search)
  (skk-look)
  (skk-search-server skk-aux-large-jisyo 10000)
$B$3$l$i$N%W%m%0%i%`!JMWAG!K$,<+F0E*$KDI2C$5$l$k!#(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-1 nil
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-1 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-2 nil
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-2 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-3 nil
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-3 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-4 nil
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-4 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-5 '((skk-search-tankanji))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-5 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-6 '((skk-search-identity))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-6 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-7 '((skk-search-katakana))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-7 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-8 '((skk-search-hankaku-katakana))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-8 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-9 '((skk-search-jisx0208-romaji))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-9 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-search-prog-list-0 '((skk-search-romaji))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
C-0 SPC $B$G;HMQ$5$l$k(B"
  :type '(repeat (sexp :tag "S$B<0(B"))
  :group 'skk-dictionary)

(defcustom skk-count-jisyo-candidates-function
  'skk-count-jisyo-candidates-original
  "*`skk-count-jisyo-candidates' $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-dictionary)

(defcustom skk-public-jisyo-to-be-searched-function
  'skk-public-jisyo-to-be-searched-original
  "*`skk-public-jisyo-has-word-p' $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-dictionary)

(defcustom skk-jisyo (if skk-user-directory
			 (expand-file-name "jisyo" skk-user-directory)
		       (convert-standard-filename "~/.skk-jisyo"))
  "*SKK $B$N8D?M<-=q!#(B"
  :type `(file :tag "$B<-=q%U%!%$%kL>(B")
  :group 'skk-private)

(defcustom skk-backup-jisyo (if skk-user-directory
				(expand-file-name "jisyo.bak"
						  skk-user-directory)
			      (convert-standard-filename "~/.skk-jisyo.BAK"))
  "*SKK $B$N8D?M<-=q$N%P%C%/%"%C%W%U%!%$%k!#(B"
  :type '(file :tag "$B<-=q%U%!%$%kL>(B")
  :group 'skk-private)

(defcustom skk-jisyo-code nil
  ;; $B8=:_$N<BAu$K$Y$C$?$j$J@bL@$ONI$/$J$$$+$b(B
  "*$B<-=q%P%C%U%!$N%3!<%G%#%s%0%7%9%F%`!#(B
$B4pK\E*$K$O(B coding system $BL>$r;XDj$9$k!#(B
$BJ8;zNs(B \"euc\", \"ujis\", \"sjis\", \"jis\" $B$N;XDj$b<u$1IU$1$k(B (`skk-coding-system-alist')$B!#(B
$B%G%U%)%k%H$O(B nil $B$G$"$j!"<-=q%P%C%U%!$N%3!<%G%#%s%0%7%9%F%`$O(B euc-jis-2004 (GNU Emacs 22 $B$^$G$O(B euc-jisx0213) $B$H$J$k(B (`skk-find-coding-system')$B!#(B
$B8D?M<-=q$b$3$N%3!<%G%#%s%0%7%9%F%`$GJ]B8$5$l$k!#(B"
  :type '(radio coding-system
		(radio :tag "$B%3!<%I$NDL>N(B"
		       (const "euc")
		       (const "ujis")
		       (const "sjis")
		       (const "jis"))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-private)

(defcustom skk-share-private-jisyo nil "\
*Non-nil $B$G$"$l$P!"8D?M<-=q$r99?7$9$k:]$K!VJ#?t$N(B SKK $B%W%m%;%9$,FCDj$N8D(B
$B?M<-=q$r6&M-$7$F$$$k!W$r9MN8$7$?>e$G=hM}$r9T$&!#(B
SKK $B5/F08e$K$3$NJQ?t$NCM$rJQ99$7$?>l9g$O(B \\[skk-restart] $B$GH?1G$5$;$k;v!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-private)

(defcustom skk-jisyo-save-count 50
  "*$B?tCM$G$"$l$P!"$=$N2s?t$@$18D?M<-=q$,99?7$5$l$?$H$-$K<+F0E*$K%;!<%V$9$k!#(B
nil $B$G$"$l$P!"8D?M<-=q$N%*!<%H%;!<%V$r9T$o$J$$!#(B
SKK $B5/F08e$G!"JQ?t(B `skk-share-private-jisyo' $B$,(B non-nil $B$J>l9g(B
$B$K(B `skk-jisyo-save-count' $B$NCM$rJQ99$7$?>l9g$O(B
\\[skk-restart] $B$GH?1G$5$;$k;v!#(B"
  :type '(radio (integer :tag "$B@0?t(B" 50)
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-private)

(defcustom skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$r=*N;$9$k$H$-$K(B `skk-record-file' $B$KJ]B8$5$l(B
$B$kE}7W>pJs$N!V8l?t!W$r@53N$K?t$($k!#(B
nil $B$G$"$l$P!"(B1 $B9T$KJ#?t$N8uJd$,$"$C$F$b(B 1 $B8uJd$H$7$F?t$($k!#(B"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-compare-jisyo-size-when-saving t
  "*Non-nil $B$G$"$l$P!"(B`skk-jisyo' $B$N%;!<%V;~$K%U%!%$%k%5%$%:$r%A%'%C%/$9$k!#(B
$BA02s%;!<%V$7$?(B `skk-jisyo' $B$H:#2s%;!<%V$7$h$&$H$9$k<-=q$H$N%5%$%:$rHf3S$7!"(B
$B8e<T$NJ}$,Bg$-$$$H$-$K%f!<%6!<$K%;!<%V$rB3$1$k$+$I$&$+$N3NG'$r5a$a$k!#(B"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-search-excluding-word-pattern-function nil
  "*$B!V!H8D?M<-=q$K<h$j9~$^$J$$J8;zNs$N%Q%?!<%s!I$r8!:w$9$k>r7o!W$r;XDj$9$k!#(B
$B$3$NJQ?t$K$O!"0z?t#18D$N4X?t!JKt$O4X?t$N%j%9%H!K$rBeF~$9$k!#(B
$BBeF~$7$?4X?t$O!"3NDj$7$?J8;zNs$r0z?t$K(B `skk-update-jisyo-p' $BFb$G(B `funcall' $B$5$l$k!#(B
$B$3$NJQ?t$N%G%U%)%k%H$O(B nil $B$G$"$k$?$a!"4X?t(B `skk-update-jisyo-p' $B$O(B t $B$rJV$9!#(B

$B4pK\E*$K!"$3$NJQ?t$O%U%C%/JQ?t$G$"$j!"$=$NCM$r@_Dj$7$?$$>l9g$K$O(B `add-hook'
$B$GDI2C$9$k$+(B `remove-hook' $B$G:o=|$9$k!#(B

SKK $B$G$O!"$+$J4A;zJQ49!&3NDj$r9T$C$?J8;zNs$OA4$F8D?M<-=q$K<h$j9~$^$l$k$,!"(B
$B$3$NJQ?t$G;XDj$5$l$?4X?t$,(B non-nil $B$rJV$9$H!"$=$NJ8;zNs$O8D?M<-=q$K<h$j9~(B
$B$^$l$J$$!#(B

$BNc$($P!"$3$NJQ?t$K2<5-$N$h$&$J(B lambda $B4X?t$r;XDj$9$k$H!"$+$J4A;zJQ49$K$h(B
$B$C$F(B (SKK abbrev mode $B$G$NJQ49$r=|$/(B) $B%+%?%+%J$N$_$+$i@.$kJ8;zNs$rF@$F3N(B
$BDj$7$F$b!"$=$l$r8D?M<-=q$K<h$j9~$^$J$$!#(B

 (add-hook 'skk-search-excluding-word-pattern-function
	   (lambda (kakutei-word)
	       ;; $B$3$N4X?t$,(B non-nil $B$rJV$7$?$H$-$O!"$=$NJ8;zNs$O8D?M(B
	       ;; $B<-=q$K<h$j9~$^$l$J$$!#(B
	       (and
	        ;; $BAw$j$J$7JQ49$G!"(B
	        (not skk-okuri-char)
	        ;; $B3NDj8l$,%+%?%+%J$N$_$+$i9=@.$5$l$F$$$F!"(B
	        (string-match \"^[$B!<%!(B-$B%s(B]+$\" kakutei-word)
	        ;; SKK abbrev mode $B0J30$G$NJQ49$+!"(B
	        (or (not skk-abbrev-mode)
		    ;; $B8+=P$78l$,%+%?%+%J!"$R$i$,$J0J30$N$H$-!#(B
		    ;; ($B8e$G"&%^!<%/$rIU$1$?$H$-$O!"8+=P$78l$,1QJ8;z$G$b!"(B
		    ;; skk-abbrev-mode$B$,(B t $B$K$J$C$F$$$J$$(B)$B!#(B
		    (not (string-match \"^[^$B!<%!(B-$B%s$!(B-$B$s(B]+$\"
                                       skk-henkan-key))))))

$B!V$+$J4A;zJQ49$K$h$C$F%+%?%+%J$r5a$a$?$$$,!"8D?M<-=q$K$O%+%?%+%J$N$_$N8u(B
$BJd$r<h$j9~$_$?$/$J$$!W$J$I!"8D?M<-=q$,I,MW0J>e$KKD$l$k$N$rM^$($kL\E*$K;H(B
$BMQ$G$-$k!#(B

$B$J$*!"8D?M<-=q$K<h$j9~$^$J$$8+=P$78l$K$D$$$F$O!"Jd40$,8z$+$J$$$N$GCm0U$9(B
$B$k$3$H!#(B"
  :type 'hook
  :group 'skk-private)

(defcustom skk-update-jisyo-function 'skk-update-jisyo-original
  "*$B$3$NJQ?t$,;X$94X?t$O!"4X?t(B `skk-update-jisyo' $B$K$F(B funcall $B$G<B9T$5$l$k!#(B"
  :type 'function
  :group 'skk-private)

(defcustom skk-save-jisyo-function 'skk-save-jisyo-original
  "*$B$3$NJQ?t$,;X$94X?t$O!"(B $B4X?t(B `skk-save-jisyo' $B$K$F(B funcall $B$G<B9T$5$l$k!#(B"
  :type 'function
  :group 'skk-private)

(defcustom skk-update-end-function nil
  "*$B8D?M<-=q$N99?7=*N;;~$K%3!<%k$5$l$k4X?t!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, WORD, PURGE $B$N(B 5 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$C$?%P%C%U%!%m!<%+%k$J(B
$B>pJs$r<h$j=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B
`skk-kakutei-initialize' $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!":G8e$N(B
$B3NDj$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B"
  :type '(list symbol)
  :group 'skk-private)

(defcustom skk-learn-combined-word nil
  "*$B@\F,<-!"@\Hx<-$NF~NO$N7k2L$r<+F0E*$K3X=,$9$k$+$I$&$+$r@_Dj$9$k!#(B
Non-nil $B$J$i$P!"@\F,<-$^$?$O@\Hx<-F~NO$N:]!"@\F,<-$^$?$O@\Hx<-$H7k9g$7$?(B
$B8l$r<+F0E*$K3X=,$9$k!#(B"
  :type 'boolean
  :group 'skk-private)

(defcustom skk-save-jisyo-instantly nil
  "*non-nil $B$G$"$l$P!"C18lEPO?!JC18l:o=|!K$NETEY!"8D?M<-=q$rJ]B8$9$k!#(B"
  :type 'boolean
  :group 'skk-private)

(defvar skk-jisyo-updated nil
  "`skk-henkan-in-minibuff' ($BC18lEPO?(B) $B$5$l$l$P(B t $B$H$J$k!#(B
`skk-update-jisyo' $B$G;2>H$7$F$$$k!#(B")

(defcustom skk-rom-kana-base-rule-list
  '(("a" nil ("$B%"(B" . "$B$"(B"))
    ("bb" "b" ("$B%C(B" . "$B$C(B"))
    ("ba" nil ("$B%P(B" . "$B$P(B"))
    ("be" nil ("$B%Y(B" . "$B$Y(B"))
    ("bi" nil ("$B%S(B" . "$B$S(B"))
    ("bo" nil ("$B%\(B" . "$B$\(B"))
    ("bu" nil ("$B%V(B" . "$B$V(B"))
    ("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
    ("bye" nil ("$B%S%'(B" . "$B$S$'(B"))
    ("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
    ("byo" nil ("$B%S%g(B" . "$B$S$g(B"))
    ("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
    ("cc" "c" ("$B%C(B" . "$B$C(B"))
    ("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("che" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("chi" nil ("$B%A(B" . "$B$A(B"))
    ("cho" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("cya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("cyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("cyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("dd" "d" ("$B%C(B" . "$B$C(B"))
    ("da" nil ("$B%@(B" . "$B$@(B"))
    ("de" nil ("$B%G(B" . "$B$G(B"))
    ("dha" nil ("$B%G%c(B" . "$B$G$c(B"))
    ("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
    ("dhi" nil ("$B%G%#(B" . "$B$G$#(B"))
    ("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
    ("dhu" nil ("$B%G%e(B" . "$B$G$e(B"))
    ("di" nil ("$B%B(B" . "$B$B(B"))
    ("do" nil ("$B%I(B" . "$B$I(B"))
    ("du" nil ("$B%E(B" . "$B$E(B"))
    ("dya" nil ("$B%B%c(B" . "$B$B$c(B"))
    ("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
    ("dyi" nil ("$B%B%#(B" . "$B$B$#(B"))
    ("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
    ("dyu" nil ("$B%B%e(B" . "$B$B$e(B"))
    ("e" nil ("$B%((B" . "$B$((B"))
    ("ff" "f" ("$B%C(B" . "$B$C(B"))
    ("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
    ("fe" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fo" nil ("$B%U%)(B" . "$B$U$)(B"))
    ("fu" nil ("$B%U(B" . "$B$U(B"))
    ("fya" nil ("$B%U%c(B" . "$B$U$c(B"))
    ("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fyi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
    ("fyu" nil ("$B%U%e(B" . "$B$U$e(B"))
    ("gg" "g" ("$B%C(B" . "$B$C(B"))
    ("ga" nil ("$B%,(B" . "$B$,(B"))
    ("ge" nil ("$B%2(B" . "$B$2(B"))
    ("gi" nil ("$B%.(B" . "$B$.(B"))
    ("go" nil ("$B%4(B" . "$B$4(B"))
    ("gu" nil ("$B%0(B" . "$B$0(B"))
    ("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
    ("gye" nil ("$B%.%'(B" . "$B$.$'(B"))
    ("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
    ("gyo" nil ("$B%.%g(B" . "$B$.$g(B"))
    ("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
    ;;("h" "" ("$B%*(B" . "$B$*(B"))
    ("ha" nil ("$B%O(B" . "$B$O(B"))
    ("he" nil ("$B%X(B" . "$B$X(B"))
    ("hi" nil ("$B%R(B" . "$B$R(B"))
    ("ho" nil ("$B%[(B" . "$B$[(B"))
    ("hu" nil ("$B%U(B" . "$B$U(B"))
    ("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
    ("hye" nil ("$B%R%'(B" . "$B$R$'(B"))
    ("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
    ("hyo" nil ("$B%R%g(B" . "$B$R$g(B"))
    ("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
    ("i" nil ("$B%$(B" . "$B$$(B"))
    ("jj" "j" ("$B%C(B" . "$B$C(B"))
    ("ja" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("je" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("ji" nil ("$B%8(B" . "$B$8(B"))
    ("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("ju" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("jye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("jyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("kk" "k" ("$B%C(B" . "$B$C(B"))
    ("ka" nil ("$B%+(B" . "$B$+(B"))
    ("ke" nil ("$B%1(B" . "$B$1(B"))
    ("ki" nil ("$B%-(B" . "$B$-(B"))
    ("ko" nil ("$B%3(B" . "$B$3(B"))
    ("ku" nil ("$B%/(B" . "$B$/(B"))
    ("kya" nil ("$B%-%c(B" . "$B$-$c(B"))
    ("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
    ("kyi" nil ("$B%-%#(B" . "$B$-$#(B"))
    ("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
    ("kyu" nil ("$B%-%e(B" . "$B$-$e(B"))
    ("ma" nil ("$B%^(B" . "$B$^(B"))
    ("me" nil ("$B%a(B" . "$B$a(B"))
    ("mi" nil ("$B%_(B" . "$B$_(B"))
    ("mo" nil ("$B%b(B" . "$B$b(B"))
    ("mu" nil ("$B%`(B" . "$B$`(B"))
    ("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
    ("mye" nil ("$B%_%'(B" . "$B$_$'(B"))
    ("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
    ("myo" nil ("$B%_%g(B" . "$B$_$g(B"))
    ("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
    ("n" nil ("$B%s(B" . "$B$s(B"))
    ("n'" nil ("$B%s(B" . "$B$s(B"))
    ("na" nil ("$B%J(B" . "$B$J(B"))
    ("ne" nil ("$B%M(B" . "$B$M(B"))
    ("ni" nil ("$B%K(B" . "$B$K(B"))
    ("nn" nil ("$B%s(B" . "$B$s(B"))
    ("no" nil ("$B%N(B" . "$B$N(B"))
    ("nu" nil ("$B%L(B" . "$B$L(B"))
    ("nya" nil ("$B%K%c(B" . "$B$K$c(B"))
    ("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
    ("nyi" nil ("$B%K%#(B" . "$B$K$#(B"))
    ("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
    ("nyu" nil ("$B%K%e(B" . "$B$K$e(B"))
    ("o" nil ("$B%*(B" . "$B$*(B"))
    ("pp" "p" ("$B%C(B" . "$B$C(B"))
    ("pa" nil ("$B%Q(B" . "$B$Q(B"))
    ("pe" nil ("$B%Z(B" . "$B$Z(B"))
    ("pi" nil ("$B%T(B" . "$B$T(B"))
    ("po" nil ("$B%](B" . "$B$](B"))
    ("pu" nil ("$B%W(B" . "$B$W(B"))
    ("pya" nil ("$B%T%c(B" . "$B$T$c(B"))
    ("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
    ("pyi" nil ("$B%T%#(B" . "$B$T$#(B"))
    ("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
    ("pyu" nil ("$B%T%e(B" . "$B$T$e(B"))
    ("rr" "r" ("$B%C(B" . "$B$C(B"))
    ("ra" nil ("$B%i(B" . "$B$i(B"))
    ("re" nil ("$B%l(B" . "$B$l(B"))
    ("ri" nil ("$B%j(B" . "$B$j(B"))
    ("ro" nil ("$B%m(B" . "$B$m(B"))
    ("ru" nil ("$B%k(B" . "$B$k(B"))
    ("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
    ("rye" nil ("$B%j%'(B" . "$B$j$'(B"))
    ("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
    ("ryo" nil ("$B%j%g(B" . "$B$j$g(B"))
    ("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
    ("ss" "s" ("$B%C(B" . "$B$C(B"))
    ("sa" nil ("$B%5(B" . "$B$5(B"))
    ("se" nil ("$B%;(B" . "$B$;(B"))
    ("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("she" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("shi" nil ("$B%7(B" . "$B$7(B"))
    ("sho" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("si" nil ("$B%7(B" . "$B$7(B"))
    ("so" nil ("$B%=(B" . "$B$=(B"))
    ("su" nil ("$B%9(B" . "$B$9(B"))
    ("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("sye" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
    ("syo" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("tt" "t" ("$B%C(B" . "$B$C(B"))
    ("ta" nil ("$B%?(B" . "$B$?(B"))
    ("te" nil ("$B%F(B" . "$B$F(B"))
    ("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
    ("the" nil ("$B%F%'(B" . "$B$F$'(B"))
    ("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
    ("tho" nil ("$B%F%g(B" . "$B$F$g(B"))
    ("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
    ("ti" nil ("$B%A(B" . "$B$A(B"))
    ("to" nil ("$B%H(B" . "$B$H(B"))
    ("tsu" nil ("$B%D(B" . "$B$D(B"))
    ("tu" nil ("$B%D(B" . "$B$D(B"))
    ("tya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("tyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("tyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("u" nil ("$B%&(B" . "$B$&(B"))
    ("vv" "v" ("$B%C(B" . "$B$C(B"))
    ("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
    ("ve" nil ("$B%t%'(B" . "$B$&!+$'(B"))
    ("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
    ("vo" nil ("$B%t%)(B" . "$B$&!+$)(B"))
    ("vu" nil ("$B%t(B" . "$B$&!+(B"))
    ("ww" "w" ("$B%C(B" . "$B$C(B"))
    ("wa" nil ("$B%o(B" . "$B$o(B"))
    ("we" nil ("$B%&%'(B" . "$B$&$'(B"))
    ("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
    ("wo" nil ("$B%r(B" . "$B$r(B"))
    ("wu" nil ("$B%&(B" . "$B$&(B"))
    ("xx" "x" ("$B%C(B" . "$B$C(B"))
    ("xa" nil ("$B%!(B" . "$B$!(B"))
    ("xe" nil ("$B%'(B" . "$B$'(B"))
    ("xi" nil ("$B%#(B" . "$B$#(B"))
    ("xka" nil ("$B%u(B" . "$B$+(B"))
    ("xke" nil ("$B%v(B" . "$B$1(B"))
    ("xo" nil ("$B%)(B" . "$B$)(B"))
    ("xtsu" nil ("$B%C(B" . "$B$C(B"))
    ("xtu" nil ("$B%C(B" . "$B$C(B"))
    ("xu" nil ("$B%%(B" . "$B$%(B"))
    ("xwa" nil ("$B%n(B" . "$B$n(B"))
    ("xwe" nil ("$B%q(B" . "$B$q(B"))
    ("xwi" nil ("$B%p(B" . "$B$p(B"))
    ("xya" nil ("$B%c(B" . "$B$c(B"))
    ("xyo" nil ("$B%g(B" . "$B$g(B"))
    ("xyu" nil ("$B%e(B" . "$B$e(B"))
    ("yy" "y" ("$B%C(B" . "$B$C(B"))
    ("ya" nil ("$B%d(B" . "$B$d(B"))
    ("ye" nil ("$B%$%'(B" . "$B$$$'(B"))
    ("yo" nil ("$B%h(B" . "$B$h(B"))
    ("yu" nil ("$B%f(B" . "$B$f(B"))
    ("zz" "z" ("$B%C(B" . "$B$C(B"))
    ("z " nil "$B!!(B")
    ("z*" nil "$B"((B")
    ("z," nil "$B!E(B")
    ("z-" nil "$B!A(B")
    ("z." nil "$B!D(B")
    ("z/" nil "$B!&(B")
    ("z0" nil "$B!{(B")
    ("z:" nil "$(O!,(B")
    ("z;" nil "$(O!+(B")
    ("z@" nil "$B!}(B")
    ("z[" nil "$B!X(B")
    ("z]" nil "$B!Y(B")
    ("z{" nil "$B!Z(B")
    ("z}" nil "$B![(B")
    ("z(" nil "$B!J(B")
    ("z)" nil "$B!K(B")
    ("za" nil ("$B%6(B" . "$B$6(B"))
    ("ze" nil ("$B%<(B" . "$B$<(B"))
    ("zh" nil "$B"+(B")
    ("zi" nil ("$B%8(B" . "$B$8(B"))
    ("zj" nil "$B"-(B")
    ("zk" nil "$B",(B")
    ("zl" nil "$B"*(B")
    ("zL" nil "$B"M(B")
    ("zo" nil ("$B%>(B" . "$B$>(B"))
    ("zu" nil ("$B%:(B" . "$B$:(B"))
    ("zya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("zyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("zyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("." nil skk-auto-kutouten)
    ("," nil skk-auto-kutouten)
    ("-" nil skk-auto-kutouten)
    (":" nil "$B!'(B")
    (";" nil "$B!((B")
    ("?" nil "$B!)(B")
    ("[" nil "$B!V(B")
    ("]" nil "$B!W(B")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    (skk-kakutei-key nil skk-kakutei)
    ;; XXX
    ;;("\t" nil skk-insert)
    ;;("," nil skk-previous-candidate)
    ;;("\M-\040" nil skk-start-henkan-with-completion); M-SPC
    ;;("\M-\121" nil skk-backward-and-set-henkan-point); M-Q
    )
  ;; $B%3%s%9%?%s%H$K$7$F$7$^$o$J$$$N$O!"%m!<%^;zF~NO$H$OA4$/JL$N@_Dj$r(B
  ;; $B$9$k?M$b$$$k$+$i$G$9!#(B
  "*$B%-!<F~NO$r$$$+$K=hM}$9$k$+$rI=$9!">uBVA+0\5,B'$N%j%9%H!#(B

$B%j%9%H$N3FMWAG$O!"$=$l$>$l$,0l$D$N5,B'$G$"$j!"2<5-$N7A<0$rK~$?$7$F$$$J$1$l$P(B
$B$J$i$J$$!#(B

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK $B$O(B INPUT-STATE $B$r8!=P$9$k$H!"(BOUTPUT $B$r%P%C%U%!$KA^F~$7!"B3$$$F(B
NEXT-STATE $B$K>uBV$r0\$7$?$&$($G!"F~NOBT$A>uBV$H$J$k!#(B

$BNc$($P!"(B

     (\"a\" nil (\"$B%"(B\" . \"$B$"(B\"))
     (\"ki\" nil (\"$B%-(B\" . \"$B$-(B\"))
     (\"tt\" \"t\" (\"$B%C(B\" . \"$B$C(B\"))
     (\"nn\" nil (\"$B%s(B\" . \"$B$s(B\"))
     (\"n'\" nil (\"$B%s(B\" . \"$B$s(B\"))

$B>e5-$N5,B'$O!"$=$l$>$l!"(B

     a  => $B$"(B
     ki => $B$-(B
     tt => $B$C(Bt
     nn => $B$s(B
     n' => $B$s(B

$B$3$N$h$&$K>uBV$,0\$jJQ$o$k$3$H$r0UL#$9$k!#(B

INPUT-STATE $B5Z$S(B NEXT-STATE $B$O!"DL>o(B US-ASCII $BJ8;z$+$i@.$kJ8;zNs$rMQ$$$k!#(B
$B$?$@$7!"FCJL$J>l9g$K$O(B INPUT-STATE $B$K$=$l0J30$NJ8;zNs$r;XDj$9$k$3$H$,$"$k!#(B

OUTPUT $B$K$O!"0J2<$N(B 3$B$D$N7A<0$r;XDj$G$-$k!#(B

$BJ8;zNs(B -- $B$+$J%b!<%I!"%+%J%b!<%I$H$b!"$3$l$,A^F~$5$l$k!#(B
$BJ8;zNs$HJ8;zNs$N%;%k(B ($B%I%C%H%Z%"(B)
       -- $B$+$J%b!<%I$K$*$$$F$O(B CDR $B$N!"%+%J%b!<%I$K$*$$$F$O(B CAR $B$NJ8;zNs$,!"(B
          $B$=$l$>$lA^F~$5$l$k!#(B
$B4X?tL>%7%s%\%k(B
       -- $B4X?t$r<B9T$9$k!#$b$7$=$N4X?t$NJV$jCM$,J8;zNs$J$i$P!"$=$NJ8;zNs$r(B
          $BA^F~$9$k!#(B

$BF1MM$N5,B'$rI=$9JQ?t$K(B `skk-rom-kana-rule-list' $B$,$"$k!#(BSKK $B$ON>J}$N5,B'$rMx(B
$BMQ$9$k$,!"(B `skk-rom-kana-rule-list' $B$NJ}$,M%@h$5$l$k!#=>$C$F%f!<%6$,FH<+$N5,(B
$BB'$r@_Dj$7$?$$>l9g$K$O!"(B`skk-rom-kana-rule-list' $B$NJ}$r;H$&$N$,$h$$!#(B"
  :type '(repeat
	  (list :tag "$B%k!<%k(B"
		(radio :tag "1 $BF~NO(B"
		       (string :tag "$BJ8;zNs(B")
		       (symbol :tag "$BJQ?tL>(B"))
		(radio :tag "2 $B<!$N>uBV(B"
		       (string :tag "$BJ8;zNs(B")
		       (const :tag "nil ($B6u$N>uBV(B)" nil))
		(radio :tag "3 $B=PNO(B"
		       (function :tag "$B4X?t$G$-$a$k(B")
		       (string :tag "$BJ8;zNs(B")
		       (cons :tag "$BJ8;zNs$NAH(B"
			     (string :tag "3-1 $B%+%?%+%J(B")
			     (string :tag "3-2 $B$R$i$,$J(B")))))
  :group 'skk-input-basic)

(defcustom skk-rom-kana-rule-list
  '(;; $B%f!<%6!<$N9%$_$G@_Dj$,J,$l$=$&$JMWAG$O!"(B
    ;; skk-rom-kana-base-rule-list $B$+$i$3$A$i$X0\$7$^$7$g$&(B...$B!#(B
    ("hh" "h" ("$B%C(B" . "$B$C(B"))
    ;; when you may want to insert $B!V$,$s$^!W(Bby "gamma"...
    ("mm" "m" ("$B%s(B" . "$B$s(B")))
  "*$B>uBVA+0\5,B'$N%j%9%H$G!"%f!<%6$NDI2C@_DjMQ$NJQ?t!#(B

$B$3$NJQ?t$O!"(B`skk-rom-kana-base-rule-list' $B$HF1MM$N=q<0$rK~$?$9I,MW$,$"$k!#(B

SKK $B$O5/F0;~$K$3$N(B 2 $BJQ?t$rJT=8$7$F(B `skk-rule-tree' $B$r:n@.$9$k$,!"(B
`skk-rom-kana-rule-list' $B$N5,B'$O(B `skk-rom-kana-base-rule-list' $B$N5,B'$h$j$b(B
$BM%@h$5$l$k!#(B

$B%j%9%H$N3FMWAG$O!"$=$l$>$l$,0l$D$N5,B'$G$"$j!"2<5-$N7A<0$rK~$?$7$F$$$J$1$l$P(B
$B$J$i$J$$!#(B

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK $B$O(B INPUT-STATE $B$r8!=P$9$k$H!"(BOUTPUT $B$r%P%C%U%!$KA^F~$7!"B3$$$F(B
NEXT-STATE $B$K>uBV$r0\$7$?$&$($G!"F~NOBT$A>uBV$H$J$k!#(B

$B>\$7$/$O!"(B`skk-rom-kana-base-rule-list' $B$N@bL@$r;2>H$N$3$H!#(B

$B%f!<%6$O!"DI2C$7$?$$5,B'$r!"Nc$($P(B

    (setq skk-rom-kana-rule-list
      '(
	(\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))
	(\"@\" nil \"$B!w(B\")
	...
	))

$B>e5-$N$h$&$K(B `~/.emacs.d/init.el' $B$^$?$O(B `skk-init-file' $B$K$F@_Dj$9$k$3$H$,$G$-$k!#(B

$B$3$NJQ?t$O!"I8=`$G$O(B

    (\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))

$B$N@_Dj$,$5$l$F$$$k!#$3$N5,B'$K=>$&$H!"(B

    ohhonn => $B$*$C$[$s(B
    ohhira => $B$*$C$R$i(B

$B$N$h$&$KA^F~$5$l$k!#$b$7$3$l$r(B

    ohhonn  => $B$*$*$[$s(B
    ohhira  => $B$*$*$R$i(B

$B$N$h$&$KJQ99$7$?$1$l$P!"$3$N@_Dj(B

    (\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))

$B$r:o=|$9$k!#(B

$B$^$?!"(B`@' $B$G(B `skk-today' ($BEvF|$NF|IU$NF~NO(B) $B$r5/F0$9$kBe$j$K(B `$B!w(B' $B$rF~(B
$BNO$7$?$$>l9g$O!"(B`skk-rom-kana-rule-list' $B$K(B

    (\"@\" nil \"$B!w(B\")

$B$H$$$&MWAG$r2C$($k!#(B

$B$b$7!"(BSKK $B$r5/F0$7$?8e$G(B `skk-rom-kana-rule-list' $B$rJQ99$7$?>l9g!"$=$N@_(B
$BDj$rH?1G$5$;$k$K$O(B \\[skk-restart] $B$r<B9T$9$kI,MW$,$"$k!#(B"
  :type '(repeat
	  (list :tag "$B%k!<%k(B"
		(radio :tag "1 $BF~NO(B"
		       (string :tag "$BJ8;zNs(B")
		       (symbol :tag "$BJQ?tL>(B"))
		(radio :tag "2 $B<!$N>uBV(B"
		       (string :tag "$BJ8;zNs(B")
		       (const :tag "nil ($B6u$N>uBV(B)" nil))
		(radio :tag "3 $B=PNO(B"
		       (function :tag "$B4X?t$G$-$a$k(B")
		       (string :tag "$BJ8;zNs(B")
		       (cons :tag "$BJ8;zNs$NAH(B"
			     (string :tag "3-1 $B%+%?%+%J(B")
			     (string :tag "3-2 $B$R$i$,$J(B")))))
  :group 'skk-input-basic)

(defcustom skk-kana-input-search-function
  (lambda ()
    (save-match-data
      (and (string-match "^h\\([bcdfghjklmnpqrstvwxz]\\)$" skk-prefix)
	   (member (char-to-string (preceding-char)) '("$B$*(B" "$B%*(B"))
	   (cons '("$B%*(B" . "$B$*(B") (match-string 1 skk-prefix)))))
  "*$B%k!<%k%j%9%H$NCf$K5-$;$J$$JQ49%k!<%k$r=hM}$9$k4X?t!#(B
`skk-rom-kana-base-rule-list' $B$H(B `skk-rom-kana-rule-list' $B$NMWAG$rA4$F8!:w(B
$B$7$?8e$K%3!<%k$5$l$k!#0z?t$O$J$$!#(B

 ($B8=:_$NF~NO$KBP$9$k=PNO(B . \"$BB3$/(B unfixed prefix\")

$B$H$$$&%;%k$rJV$9!#=PNO$N<oN`$K$D$$$F$O(B `skk-rom-kana-base-rule-list' $B$r(B
$B;2>H$N$3$H!#(B

$B%G%U%)%k%H$G$O!"(B\"$B$*(B\" $B$N8e$N(B \"h\" + $B;R2;$NF~NO$r(B \"$B$*$*(B\" + $BB3$/;R2;(B
$B=hM}MQ$N(B unfixed prefix $B$KJQ49$7$F$$$k!#(B"
  :type 'function
  :group 'skk-input-basic)

(defcustom skk-downcase-alist nil
  "*$BJQ49%-!<(B ($BBgJ8;z%m!<%^;z(B) $B$N>.J8;z$X$NJQ495,B'$rI=$o$9O"A[%j%9%H!#(B
$BJQ49%-!<$NF~NO$r3+;O$9$k:]!"(BSKK $B$G$OBgJ8;z$GF~NO$r9T$&$N$G!"(B
`skk-set-henkan-point' $B$NCf$G$3$l$r>.J8;z$KJQ49$9$k!#$3$NO"A[(B
$B%j%9%H$KBgJ8;z(B -> $B>.J8;z$NJQ49%k!<%k$r=q$$$F$*$/$3$H$G!"%-!<F~NO$r%+%9(B
$B%?%^%$%:$9$k$3$H$,$G$-$k!#$3$NO"A[%j%9%H$,6u%j%9%H$N>l9g$O!"C1$K(B
downcase $B$5$l$k!#(B"
  :type '(repeat (cons character character))
  :group 'skk-input-basic)

(defcustom skk-jisx0208-latin-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "*`skk-jisx0208-latin-insert' $B$G;2>H$5$l$kJ8;z%F!<%V%k!#(B
$B%-!<$KBP1~$9$k0LCV$KJ8;zNs$,$"$l$P!"A41Q%b!<%I$G3:Ev$N%-!<$r2!$9$3$H$G!"BP1~$9(B
$B$kJ8;z$,A^F~$5$l$k!#(B
$BNc$($P!"%9%Z!<%9%-!<$KBP1~$7$F!"H>3Q%9%Z!<%9$rA^F~$5$;$k$h$&$KJQ99$7$?$1$l$P!"(B
skk.el $B$N%m!<%I8e(B ($B$b$7$/$O(B `skk-load-hook' $B$rMxMQ$7$F(B)$B!"(B

     (aset skk-jisx0208-latin-vector 32 \" \")

$B$H$9$k$+!"$b$7$/$O!"(B`skk-jisx0208-latin-vector' $B$N(B 32 $BHVL\(B (0 $BHV$+$i?t$($F(B)
 $B$NCM$r(B \" \"$B$H$9$k$h$&$J(B `skk-jisx0208-latin-vector' $B$rD>@\=q$-!"(Bsetq $B$G(B
$BBeF~$9$k!#(B32 $B$O!"(B?  ($BH>3Q%9%Z!<%9$N(B char type) $B$rI>2A$7$?$H$-$NCM!#(B"
  :type 'sexp
  :group 'skk-input-basic)

(defcustom skk-special-midashi-char-list '(?> ?< ??)
  "*$B@\F,<-!"@\Hx<-$NF~NO$r;XDj$9$kJ8;z$N%j%9%H!#(B"
  ;;  "*List of characters for entering prefixes and suffixes."
  :type '(repeat character)
  :group 'skk-input-basic)

(defcustom skk-kuten-touten-alist
  '((jp . ("$B!#(B" . "$B!"(B"))
    (en . ("$B!%(B" . "$B!$(B"))
    (jp-en . ("$B!#(B" . "$B!$(B"))
    (en-jp . ("$B!%(B" . "$B!"(B")))
  "*$B6gE@$HFIE@$NO"A[%j%9%H!#(B
$B3FMWAG$N7A<0$O!"(B

   ($B%7%s%\%k(B . ($B6gE@$rI=$o$9J8;zNs(B . $BFIE@$rI=$o$9J8;zNs(B))

$B$H$$$&(B cons cell$B!#%7%s%\%k$NItJ,$O!"(B`jp' $B$b$7$/$O(B `en' $B!#(B
\\[skk-toggle-kutouten] $B$O!"$3$l$r%H%0%k$G@Z$j49$($k!#(B
$B%G%U%)%k%H$N6gFIE@$N%?%$%W$O!"JQ?t(B `skk-kutouten-type' $B$G;XDj$9$k!#(B"
  :type '(repeat (cons (radio :tag "$BAH$N$J$^$((B"
			      (const jp)
			      (const en)
			      (const jp-en)
			      (const en-jp))
		       (cons :tag "$B6gFIE@$NAH(B"
			     (string :tag "$B6gE@(B" "$B!#(B")
			     (string :tag "$BFIE@(B" "$B!"(B"))))
  :group 'skk-input-basic)

(defcustom skk-kutouten-type 'jp
  "*$BI8=`$N6gFIE@$N%?%$%W!#(B
$B$3$NJQ?t$NCM$K;XDj$G$-$k%7%s%\%k$H6gFIE@$NAH$H$NBP1~$O0J2<$NDL$j!#(B

      `jp': $B!V!#!W!V!"!W(B
      `en': $B!V!%!W!V!$!W(B
   `jp-en': $B!V!#!W!V!$!W(B
   `en-jp': $B!V!%!W!V!"!W(B

$B$3$NJQ?t$K$O%3%s%9!&%;%k$r;XDj$9$k$3$H$b2DG=!#$=$N>l9g$O(B

 ($B6gE@$r<($9J8;zNs(B . $BFIE@$r<($9J8;zNs(B)

$B$N$h$&$K;XDj$9$k!#(B

$B$3$NJQ?t$O(B `skk-use-kana-keyboard' $B$,(B non-nil $B$J$i$PL58z$G$"$k!#(B

$B$3$NJQ?t$O(B `setq' $B$9$k$H%P%C%U%!%m!<%+%k2=$5$l$k$?$a!"%0%m!<%P%k$K(B
$BCM$r@_Dj$7$?$$>l9g$O(B `setq-default' $B$rMQ$$$k$3$H$,?d>)$5$l$k!#(B"
  :type '(radio (const jp)
		(const en)
		(const jp-en)
		(const en-jp)
		(cons :tag "$BG$0U$NAH(B"
		 (string :tag "$B6gE@(B" "$B!#(B")
		 (string :tag "$BFIE@(B" "$B!"(B")))
  :group 'skk-input-basic)
(make-variable-buffer-local 'skk-kutouten-type)
;;;###autoload
(put 'skk-kutouten-type 'safe-local-variable 'symbolp)

(defcustom skk-use-auto-kutouten nil
  "*Non-nil $B$G$"$l$P!"$+$J%b!<%I$K$*$1$kD92;(B($B!<(B)$B!"6gE@(B($B!#(B)$BKt$OFIE@(B($B!"(B)$B$NF0:n$rJQ99$9$k!#(B
ASCII $B?t;z$ND>8e$G$"$l$P!"D92;(B($B!<(B)$B$O(B `-' $B$X!"6gE@(B($B!#(B)$B$O(B `.' $B$X!"FIE@(B($B!"(B)$B$O(B `,' $B$X$HJQ99$7!"(BJISX0208($BA43Q(B)$B?t;z$ND>8e$G$"$l$P!"D92;(B($B!<(B)$B$O(B `$B!](B' $B$X!"6gE@(B($B!#(B)$B$O(B `$B!%(B' $B$X!"FIE@(B($B!"(B)$B$O(B `$B!$(B' $B$X$HJQ99$9$k!#(B"
  :type 'boolean
  :group 'skk-input-basic)

(defcustom skk-auto-insert-paren nil
  "*Non-nil $B$G$"$l$P!"3g8L$HJD3g8L$r$^$H$a$FA^F~$9$k!#(B
$BNc$($P!"(B\"$B!V(B\" $B$rF~NO$7$?$H$-$K(B \"$B!W(B\" $B$r<+F0E*$KA^F~$7!"N>$+$.3g8L$N4V$K(B
$B%+!<%=%k$r0\F0$9$k!#(B
$BA^F~$9$kJ8;zNs$O!"(B`skk-auto-paren-string-alist' $B$G;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-input-basic)

(defcustom skk-auto-paren-string-alist
  '(("$B!V(B" . "$B!W(B") ("$B!X(B" . "$B!Y(B") ("(" . ")") ("$B!J(B" . "$B!K(B")
    ("{" . "}")("$B!P(B" . "$B!Q(B") ("$B!R(B" . "$B!S(B") ("$B!T(B" . "$B!U(B")
    ("[" . "]") ("$B!N(B" . "$B!O(B") ("$B!L(B" . "$B!M(B") ("$B!Z(B" . "$B![(B")
    ("\"" . "\"")("$B!H(B" . "$B!I(B") ("`" . "'")
    ;;("<" . ">") ;; skk-special-midashi-char-list $B$NCf$K$"$kJ8;z!#(B
  )
  "*$B<+F0E*$KBP$K$J$kJ8;zNs$rF~NO$9$k$?$a$NO"A[%j%9%H!#(B
`skk-auto-insert-paren' $B$,(B non-nil $B$N>l9g!"(Bcar $B$NJ8;zNs$,A^F~$5$l$?$H$-(B
$B$K(B cdr $B$NJ8;zNs$r<+F0E*$KA^F~$7!"%+!<%=%k$O$=$N(B 2 $B$D$NJ8;z$N4V$K0\F0$9$k!#(B
`skk-special-midashi-char-list' $B$NMWAG$K$J$C$F$$$kJ8;z$O!"(B
`skk-auto-paren-string-alist' $B$K4^$a$F$b:o=|$5$l$k!#(B "
  :type '(repeat (cons string string))
  :group 'skk-input-basic)

(defcustom skk-use-auto-enclose-pair-of-region nil
  "*Non-nil $B$G$"$l$P!"%j!<%8%g%s$,M-8z$J>uBV$G(B `skk-auto-insert-paren' $B$r<B9T$7$?:]$K$O!"%j!<%8%g%s$r3g8L$HJD3g8L$G0O$`!#(B"
  :type 'boolean
  :group 'skk-input-basic)

(defcustom skk-start-henkan-char ?\040	; SPC
  "*$B4A;zJQ49$r3+;O$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-previous-candidate-keys (list "x" "\C-p")
  "*`skk-previous-candidate' $B$r3dEv$F$k%-!<!#(B
$B$3$NJQ?t$K$O%-!<$rI=$9%*%V%8%'%/%H$N%j%9%H$r;XDj$9$k!#(B
$B%*%V%8%'%/%H$H$7$F$O!"%-!<$rI=$9J8;zNs$^$?$O(B event vector $B$,;XDj$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    '(repeat (key-sequence :tag "$B%-!<(B (C-q key $B$G<hF@2D(B)"))
	  '(repeat sexp))
  :group 'skk-henkan)

(defvar skk-previous-candidate-char nil)

(make-obsolete-variable 'skk-previous-candidate-char
			'skk-previous-candidate-keys
			"DDSKK 14.2")

(defcustom skk-set-henkan-point-key
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z)
  "*$BJQ49$N3+;OCOE@$r7h$a$k%-!<$N%j%9%H!#(B"
  :type '(repeat character)
  :group 'skk-henkan)

(defcustom skk-henkan-show-candidates-keys
  '(?a ?s ?d ?f ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?z ?c ?v ?b ?n ?m ?,)
  "*$B%a%K%e!<7A<0$G8uJd$rA*Br$9$k$H$-$NA*Br%-!<$N%j%9%H!#(B
\"x\", \" \" $B5Z$S(B \"C-g\" $B0J30$N(B 7 $B$NG\?t8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"(B
$B$k!#(B\"x\", \" \" $B5Z$S(B \"C-g\" $B$O8uJdA*Br;~$K$=$l$>$lFCJL$J5!G=$K3d$jEv(B
$B$F$i$l$F$$$k$N$G!"$3$N%j%9%H$NCf$K$O4^$a$J$$$3$H!#(B"
  :type '(repeat character)
  :group 'skk-henkan)

(defface skk-henkan-show-candidates-keys-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*$BA*Br%-!<$N(B face $BB0@-!#(B"
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-henkan-rest-indicator nil
  "*Non-nil $B$G$"$l$P(B \[$B;D$j(B 99++\] $B$NI=<($r1&4s$;G[CV$9$k!#(B"
  :type 'boolean
  :group 'skk-henkan
  :group 'skk-visual)

(defface skk-henkan-rest-indicator-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*\[$B;D$j(B 99++\] $B$N(B face $BB0@-!#(B"
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-auto-start-henkan t
  "*$BC18l$dJ8@a$N6h@Z$j$r<($9J8;z$NBG80$K$h$j<+F0E*$KJQ49$r3+;O$9$k!#(B
`skk-auto-start-henkan-keyword-list' $B$K$h$jC18l$dJ8@a$N6h@Z$j$r<($9J8;z$r(B
$B;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-auto-start-henkan-keyword-list
  '("$B$r(B" "$B!"(B" "$B!#(B" "$B!%(B" "$B!$(B" "$B!)(B" "$B!W(B" "$B!*(B" "$B!((B" "$B!'(B" ")" ";" ":"
    "$B!K(B" "$B!I(B" "$B![(B" "$B!Y(B" "$B!U(B" "$B!S(B" "$B!Q(B" "$B!O(B" "$B!M(B" "}" "]" "?" "."
    "," "!")
  ;; $B$"$^$j%-!<%o!<%I$,B?$/$J$k$H!"DL>o$NJQ49$r:$Fq$K$9$k!)(B
  "*$B<+F0JQ49$r3+;O$9$k%-!<%o!<%I!#(B
`skk-auto-start-henkan' $B$,(B non-nil $B$N$H$-!"$3$N%j%9%H$NMWAG$NJ8;z$rBG80(B
$B$9$k$H!"(BSPC (`skk-start-henkan-char') $B$r2!$7$?$+$N$h$&$KJQ49$r3+;O$7$F(B
$B"'%b!<%I$KF~$k!#(B"
  :type '(repeat string)
  :group 'skk-henkan)

(defcustom skk-force-registration-mode-char ?.
  "*$B6/@)E*$K<-=qEPO?%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B
$B%(%3!<%(%j%"$G8uJd$rI=<($7$F$$$k$H$-$K$3$NJQ?t$GDj5A$7$?%-!<%-%c%i%/%?$r(B
$B%?%$%W$9$k$H!"6/@)E*$K<-=qEPO?%b!<%I$KF~$j$^$9!#(B"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-show-candidates-toggle-display-place-char ?\C-f
  "*$B8uJdI=<(0lMw$N0LCV$r%(%3!<%(%j%"$H%P%C%U%!$H$G@Z$jBX$($k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-backward-and-set-henkan-point-char ?\321 ; M-Q
  "*$B%]%$%s%H$rLa$7$F"&%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-henkan)

(defcustom skk-show-inline nil
  "*Non-nil $B$G$"$l$P!"JQ498uJd$r%$%s%i%$%sI=<($9$k!#(B
`vertical' $B$G$"$l$P!"=DJ}8~$K%$%s%i%$%sI=<($9$k!J(BXEmacs $B$G$OF0:n$7$J$$!K!#(B"
  :type '(radio (const :tag "$BM-8z(B" t)
		(const :tag "$BM-8z(B ($B=DI=<((B)" vertical)
		(const :tag "$BL58z(B" nil))
  :group 'skk-basic
  :group 'skk-henkan)

(defcustom skk-inline-show-face 'underline
  "*$B%$%s%i%$%sI=<($9$kJQ498uJd$rAu>~$9$k%U%'%$%9$r;XDj$9$kJQ?t!#(B
$B8uJdJ8;zNs$N%U%'%$%9B0@-$r$=$N$^$^;H$$$?$$>l9g$O(B nil $B$K@_Dj$9$k!#(B"
  :type '(radio (face :tag "$B%U%'%$%9$r;XDj(B")
		(const :tag "$B8uJdJ8;zNs$N%U%'%$%9B0@-$r$=$N$^$^;HMQ(B" nil))
  :group 'skk-visual)

(defcustom skk-inline-show-background-color
  (if (eq skk-background-mode 'light)
      "beige"
    "gray15")
  "*$B%$%s%i%$%sI=<($9$kJQ498uJd$NGX7J?'$r;XDj$9$kJQ?t!#(B
`skk-inline-show-face' $B$^$?$O(B `skk-treat-candidate-appearance-function' $B$G(B
$BGX7J?'$,;XDj$5$l$F$$$J$$J8;z$KBP$7$F$N$_:nMQ$9$k!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-inline-show-background-color-odd
  (if (eq skk-background-mode 'light)
      "wheat"
    "gray20")
  "*$B%$%s%i%$%sI=<($9$kJQ498uJd$NGX7J?'(B($B4q?t%i%$%s(B)$B$r;XDj$9$kJQ?t!#(B
`skk-inline-show-face' $B$^$?$O(B `skk-treat-candidate-appearance-function' $B$G(B
$BGX7J?'$,;XDj$5$l$F$$$J$$J8;z$KBP$7$F$N$_:nMQ$9$k!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-search-end-function nil
  "*$BC18l8!:w=*N;;~$K%3!<%k$5$l$k4X?t!#(B
$B$3$N4X?t$rMxMQ$7$F8!:w$7$?C18l$NM%@h=g0L$rJQ99$9$k$J$I$N:n6H$,2DG=!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, ENTRY $B$N(B 4 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B2C9)$7$?(B ENTRY $B$rJV$9$3$H!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$C$?%P%C%U%!%m!<%+%k$J(B
$B>pJs$r<h$j=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B"
  :type '(list symbol)
  :group 'skk-henkan)

(defcustom skk-allow-spaces-newlines-and-tabs t
  "*Non-nil $B$G$"$l$P!"8+=P$78l$NCf$N%9%Z!<%9!"%?%V!"2~9T$r<h$j=|$$$FJQ49(B
$B$G$-$k!#Nc$($P!"2<5-$N$h$&$KESCf$K2~9T$,F~$C$F$$$k8+=P$78l$G$bJQ49$,2DG=(B
$B$G$"$k!#(B

     \"$B"&$+(B
  $B$J(B\"
   -> \"$B2>L>(B\"

$B$3$NCM$,(B nil $B$G$"$l$P!":G=i$N%9%Z!<%9$G8+=P$78l$r@Z$j5M$a$F$7$^$$!"0J9_$N%9%Z!<(B
$B%9!"%?%V!"2~9T$OL5;k$5$l$k!#(B
$B$3$NCM$O!"(B`skk-toggle-characters' $B5Z$S(B `skk-backward-and-set-henkan-point' $B$NF0(B
$B:n$K1F6A$9$k!#(B"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-delete-okuri-when-quit nil
  "*Non-nil $B$G$"$l$PJQ49Cf$N(B \\[keyboard-quit] $B$GAw$j2>L>$r>C$7$F"&%b!<%I(B
$B$KF~$k!#(B
  $BNc!K(B \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \\[keyboard-quit] ->$B"&$J(B\"

nil $B$G$"$l$P!"Aw$j2>L>$r4^$a$?8+=P$78l$r$=$N$^$^;D$7$F"&%b!<%I$KF~$k!#(B
  $BNc!K(B \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \\[keyboard-quit] -> $B"&$J$/(B\""
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-henkan-show-candidates-rows 1
  "*$BJQ498uJd72$NI=<(9T?t!#(B"
  :type 'integer
  :group 'skk-henkan)

(defcustom skk-show-candidates-always-pop-to-buffer nil
  "*$B$3$NJQ?t$,(B non-nil $B$G$"$l$P!">o$K(B\"*$B8uJd(B*\"$B%P%C%U%!$r:n@=$7$F!"JQ498uJd0lMw$r@lMQ%&%#%s%I%&$KI=<($9$k!#(B
$B$3$NJQ?t$,(B nil $B$G$"$l$P!"8uJd0lMw$r%(%3!<%(%j%"$KI=<($9$k!#$?$@$7!"8uJd0lMw$NJ8;zNs$ND9$5$,%U%l!<%`$N2#I}$K<}$^$i$J$$>l9g$O!"(B\"*$B8uJd(B*\"$B%P%C%U%!$r:n@=(B(pop-to-buffer)$B$7$F@lMQ$N%&%#%s%I%&$GI=<($9$k!#(B
"
  :type 'boolean
  :group 'skk-henkan)

(defcustom skk-candidate-buffer-background-color nil
  "*\"*$B8uJd(B*$B%P%C%U%!(B\"$B$NGX7J?'!#(B"
  :type '(radio (string :tag "$B?'$NL>A0(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-candidate-buffer-background-color-odd nil
  "*\"*$B8uJd(B*$B%P%C%U%!(B\"$B$NGX7J?'!J4q?t%i%$%s!K!#(B"
  :type '(radio (string :tag "$B?'$NL>A0(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-henkan
  :group 'skk-visual)

(defcustom skk-candidate-buffer-display-fringes t
  "*nil $B$G$"$l$P!"(B\"*$B8uJd(B*$B%P%C%U%!(B\" $B$N(B fringe $B$r>C$9(B."
  :type 'boolean
  :group 'skk-henkan
  :group 'skk-visual)

(defvar skk-candidate-buffer-fringe-width
  (list nil
	(cdr (assoc 'left-fringe (frame-parameters)))
	(cdr (assoc 'right-fringe (frame-parameters))))
  "skk $B5/F0;~$N(B fringe $B$NI}(B")

(defcustom skk-search-katakana nil
  "*$B$+$J$rC1=c$K%+%?%+%JJQ49$7$?8uJd$rI=<($9$k$+$I$&$+$r7h$a$k%*%W%7%g%s!#(B
nil $B$J$i$P4^$a$J$$!#(Bt $B$J$i$PA43Q%+%J8uJd$r4^$a$k!#(B
`jisx0201-kana' $B$J$i$PA43Q$K2C$($FH>3Q%+%J8uJd$b4^$a$k!#(B
$B$3$N5!G=$O0lHLE*$J(B FEP $B$N;H$$>!<j$K6aIU$1$?$$%f!<%6!<!"8D?M<-=q$r0i$F$?$$(B
$B%f!<%6!<8~$1$KDs6!$5$l$k!#(B"
  :type '(radio (const :tag "$B$3$N5!G=$rL58z$K$9$k(B" nil)
		(const :tag "$BA43Q%+%J$N$_(B" t)
		(const :tag "$BH>3Q%+%J$b4^$a$k(B" jisx0201-kana))
  :group 'skk-henkan)

(defcustom skk-search-sagyo-henkaku nil
  "*$B4J0W$J%5JQF0;lJQ495!G=$rM-8z$K$9$k$+$I$&$+7h$a$k%*%W%7%g%s!#(B
nil $B$J$i$P!"Aw$j2>L>$,(B \"$B$5(B\" \"$B$7(B\" \"$B$9(B\" \"$B$;(B\" $B$N$$$:$l$+$N;~$K(B
$BAw$j$J$78uJd$,JQ498uJd$K8=$l$k!#(B
anything $B$K@_Dj$9$k$H!"Aw$j2>L>$,2?$G$"$C$F$bAw$j$J$78uJd$rAw$j$"$jJQ49$K(B
$BMQ$$$k!#$3$N>l9g!"Aw$j2>L>$H$$$&$h$j$b!"G$0U$N4A;z$H$+$J$N@Z$jBX$(0LCV$r(B
$B;XDj$9$k$h$&$JF~NO$K$J$k!#(B
$B$3$N5!G=$OIT@53N$J=PNO$r$9$k2DG=@-$KCm0U$9$kI,MW$,$"$k$,!"8D?M<-=q$r0i$F$?$$(B
$B%f!<%6!<8~$1$KDs6!$5$l$k!#(B"
  :type '(radio (const :tag "$B$3$N5!G=$rL58z$K$9$k(B" nil)
		(const :tag "$B4J0W%5JQF0;lJQ49$r$9$k(B" t)
		(const :tag "$B$3$N5!G=$rG$0U$NAw$j$"$jJQ49$K3HD%$9$k(B" anything))
  :group 'skk-henkan)

(defcustom skk-kakutei-key "\C-j"
  "*$B4A;zJQ49$N3NDjF0:n$r9T$&%-!<!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-kakutei-early t
  "*Non-nil $B$G$"$l$P(B `skk-insert' $B$,8F$P$l$?$H$-$K8=:_$N8uJd$r3NDj$9$k!#(B
$BNc$($P!"(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B3NDj(Bs -> $B3NDj$9(B\"

$B$N$h$&$KJQ498e!"!V$9!W$N(B prefix $B$G$"$k(B \"s\" $B$rF~NO$7$?;~E@$G3NDj$9$k!#(B
nil $B$G$"$l$P!"Nc$($P(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B"'3NDj(Bs -> $B"'3NDj$9$k(B -> $B3NDj$9$k!#(B\"

$B$N$h$&$K(B `skk-kakutei' $B$rD>@\!"4V@\$K%3!<%k$9$k$^$G(B ($B6gFIE@$rF~NO$7$?$j!"(B
$B?7$?$J"&%b!<%I$KF~$C$?$j$9$k$H4V@\E*$K(B `skk-kakutei' $B$r%3!<%k$9$k(B) $B$O!"3NDj(B
$B$7$J$$$N$G!"$=$N4V$O!"JQ498uJd$rA*$SD>$9$3$H$J$I$,2DG=!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-egg-like-newline nil
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G(B RET $B$r%?%$%W$7$F$b3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-delete-implies-kakutei t
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G(B BS $B$r2!$9$H!"A0$N0lJ8;z$r:o=|$73NDj$9$k!#(B
nil $B$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B
$B%7%s%\%k(B `dont-update' $B$G$"$l$P!"8D?M<-=q$r99?7$7$J$$!#(B

$B$J$*!"$3$NJQ?t$NCM$K$+$+$o$i$:!"8uJd0lMw$rI=<($7$F$$$k$H$-$N(B BS $BBG80$O(B
$BA08uJd(B($B72(B)$B$NI=<($K$J$k!#(B"
  :type '(radio	(const t)
		(const dont-update)
		(const nil))
  :group 'skk-basic
  :group 'skk-kakutei)

(defcustom skk-kakutei-when-unique-candidate nil
  "*Non-nil $B$G$"$l$P!"JQ498uJd$,0l$D$7$+$J$$$H$-3NDjJQ49$9$k!#(B

$B$3$NCM$,(B t $B$G$"$l$P$I$NJQ49%b!<%I$G$b3NDjJQ49$9$k!#(B
`okuri-ari', `okuri-nasi', `abbrev' $B$N$$$:$l$+$rMWAG$H$9$k%j%9%H$G(B
$B$"$l$P!"JQ49%b!<%I$,$=$N>r7o$K9gCW$7$?>l9g$N$_3NDjJQ49$9$k!#(B

$B8uJd$,B>$KL5$$;v$r3NG'$9$k$?$a!"(B`skk-search-prog-list' $B$NFbMF<!Bh(B
$B$G%l%9%]%s%9$,0-$/$J$k2DG=@-$,$"$k!#$=$N>l9g(B
`skk-kakutei-search-prog-limit' $B$r@_Dj$9$k$3$H$G8!:wBP>]$r@)8B$9$k(B
$B$3$H$b2DG=!#(B"
  :type '(radio (const :tag "$B>o$KM-8z(B" t)
		(set :tag "$BM-8z$K$9$kJQ49%b!<%I(B"
		     (const :tag "$BAw$jM-$jJQ49(B" okuri-ari)
		     (const :tag "$BAw$jL5$7JQ49(B" okuri-nasi)
		     (const :tag "abbrev $BJQ49(B" abbrev))
		(const :tag "$BL58z(B" nil))
  :group 'skk-kakutei)

(defcustom skk-kakutei-search-prog-limit nil
  "*$BJ#?t<-=q$K$h$k3NDjJQ49$K$*$$$F!"8!:wBP>]$H$9$k<-=q$r@)8B$9$k!#(B

$B$3$l$,?tCM$G$"$l$P!"8!:wBP>]$r(B `skk-search-prog-list' $B$N@hF,$+$i$3(B
$B$N8D?t$^$G$N<-=q$K@)8B$9$k!#(B
$B$=$l0J30$G$"$l$PL5@)8B$KA4$F$N<-=q$rBP>]$H$9$k!#(B

`skk-kakutei-when-unique-candidate' $B$,(B non-nil $B$N$H$-$N$_M-8z!#(B"
  :type '(radio (integer :tag "$BBP>]$H$9$k<-=q$N?t(B")
		(const :tag "$B@)8B$7$J$$(B" nil))
  :group 'skk-kakutei)

(defcustom skk-kakutei-end-function nil
  "*$B3NDj;~$K%3!<%k$5$l$k4X?t!#(B
`skk-kakutei-initialize' $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!"(B
$B:G8e$N3NDj$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B"
  :type '(radio (function :tag "$B4X?t(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-kakutei)

(defcustom skk-henkan-okuri-strictly nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?$H$-$@$18uJd$H$7$F=PNO$9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,(B `skk-jisyo' ($B8D?M<-=q(B) $B$K$"$C$?(B
$B>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"(B\"$BB?$/(B\" $B$N$_$r=PNO$7!"(B\"$BBg$/(B\" $B$r=PNO$7$J$$!#(B

SKK-JISYO.[SML] $B$NAw$j2>L>%(%s%H%j$O>e5-$N7A<0$K$J$C$F$$$J$$$N$G!"(B`skk-jisyo'
 $B$NAw$j$"$j$N<-=q%(%s%H%j$,$3$N7A<0$N$b$N$r$"$^$j4^$s$G$$$J$$>l9g$O!"$3$N(B
$B%*%W%7%g%s$r(B on $B$K$9$k$3$H$G!"$9$0$KC18lEPO?$KF~$C$F$7$^$&$N$GCm0U$9$k$3$H!#(B

`skk-process-okuri-early' $B$NCM$,(B nil $B$J$i$P!">e5-$N7A<0$G(B `skk-jisyo' $B$,(B
$B:n$i$l$k!#(B

$B2<5-$N<0$rI>2A$9$k$3$H$G!"C18lEPO?$KF~$C$?$H$-$@$1(B
$B0l;~E*$K$3$N%*%W%7%g%s$rL58z$K$9$k$3$H$,$G$-$k!#(B

    (add-hook 'minibuffer-setup-hook
              (function
               (lambda ()
                 (if (and (boundp 'skk-henkan-okuri-strictly)
                          skk-henkan-okuri-strictly
                          (not (eq last-command 'skk-purge-from-jisyo)))
                     (progn
                       (setq skk-henkan-okuri-strictly nil)
                       (put 'skk-henkan-okuri-strictly 'temporary-nil t))))))

    (add-hook 'minibuffer-exit-hook
              (function
               (lambda ()
                 (if (and (get 'skk-henkan-okuri-strictly 'temporary-nil)
                          (<= (minibuffer-depth) 1))
                     (progn
                       (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                       (setq skk-henkan-okuri-strictly t))))))

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-okurigana)

(defcustom skk-henkan-strict-okuri-precedence nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?8uJd$rM%@h$7$FI=<($9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,(B `skk-jisyo' ($B8D?M<-=q(B) $B$K$"$C$?(B
$B>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"$^$:(B\"$BB?$/(B\" $B$r=PNO$7!"(B
$B<!$K(B \"$BBg$/(B\" $B$r=PNO$9$k!#(B

\"$BBg$/(B\" $B$J$I$N8uJd$O$&$C$H$&$7$$$,!"$9$0$KC18lEPO?$KF~$C$F$7$^$&$N$b(B
$B7y$J?M$K$*4+$a!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B
$B$^$?(B `skk-henkan-okuri-strictly' $B$,(B non-nil $B$N$H$-$O!"$3$NJQ?t$OL5;k$5$l$k!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-okurigana)

(defcustom skk-process-okuri-early nil
  "*Non-nil $B$G$"$l$PAw$j2>L>$N%m!<%^;z%W%l%U%#%C%/%9F~NO;~E@$GJQ49$r3+;O$9$k!#(B
$BNc$($P!"(B

    \"UgoK -> $B"'F0(Bk\"$B!#(B

$BAw$j2>L>$,J,$i$J$$$^$^JQ49$7$F$$$k$3$H$K$J$k$N$G!"(B`skk-jisyo' $B$,Aw$j2>L>$K(B
$BBP1~$7$?7A$K@.D9$7$J$$!#$D$^$j(B

    \"$B$&$4(Bk /$BF0(B/\"

$B$N$h$&$J7ABV$N$^$^$H$J$k!#$?$@$7!"4{$K(B

    \"$B$&$4(Bk /$BF0(B/[$B$/(B/$BF0(B/]/[$B$+(B/$BF0(B/]/[$B$1(B/$BF0(B/]/[$B$-(B/$BF0(B/]/[$B$3(B/$BF0(B/]/\"

$B$N$h$&$J%(%s%H%j$,(B `skk-jisyo' $B$K$"$l$P!"$=$l$rGK2u$7$J$$!#(B

nil $B$G$"$l$P!"Aw$j2>L>$NF~NO$,40N;$7$?;~E@$GJQ49$,3+;O$9$k!#Nc$($P!"(B

    \"UgoK -> $B"&$&$4(B*k\", \"UgoKu -> $B"'F0$/(B\"

$B$3$N%*%W%7%g%s$r(B on $B$K$7$F(B `skk-mode' $B$r5/F0$9$k$H!"N>N)$G$-$J$$%*%W%7%g%s(B
$B$G$"$k(B `skk-kakutei-early', `skk-auto-okuri-process' $B5Z$S(B
`skk-henkan-okuri-strictly' $B$O(B nil $B$K%;%C%H$5$l$k!#(B"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-check-okurigana-on-touroku nil
  "*Non-nil $B$G$"$l$P!"Aw$j$"$j$NEPO?;~$K!"M>7W$J2>L>$r%A%'%C%/$9$k!#(B

$BNc$($P!"(B

     \"$B$H$S$@(B*$B$9(B $BHt$S=P(B\"

$B$HEPO?$9$k$N$,@5$7$$$K$b$+$+$o$i$:!"%f!<%6$,(B

     \"$B$H$S$@(B*$B$9(B $BHt$S=P$9(B\"

$B$G$&$C$+$j(B [RET] $B$r2!$7$F$7$^$C$?$H$-$K!":G8e$N!V$9!W$,Aw$j2>L>$G$"$k$+$I$&$+(B
$BD4$Y$k!#(B

$B$3$NJQ?t$O0J2<$NCM$r$H$jF@$k!#4{DjCM$O(B nil$B!#(B

ask  -- $B%f!<%6$K3NG'$r5a$a!"Aw$j2>L>$HG'$a$i$l$l$P$3$l$r<h$j=|$$$F$+$iEPO?$9(B
        $B$k!#(B
auto -- $B%f!<%6$K3NG'$r5a$a$:!">!<j$KAw$j2>L>$rH=CG$7$F:o=|$7$F$+$iEPO?$9$k!#(B
nil  -- $B0l@ZAw$j2>L>$N%A%'%C%/$r$;$:!"A4BN$rC18l$H$7$FEPO?$9$k!#$3$l$O(B SKK $BK\(B
        $BMh$NF0:n$G$"$k!#(B"
  :type '(radio (const :tag "$B%f!<%6$K3NG'$9$k(B" ask)
		(const :tag "$B<+F0E*$K=hM}$9$k(B" auto)
		(const :tag "$B%A%'%C%/$7$J$$(B"  nil))
  :group 'skk-basic
  :group 'skk-okurigana
  :group 'skk-private)

(defcustom skk-okuri-char-alist nil
  "*$BAw$j2>L>(B prefix $B$rJQ49$9$k%k!<%k$r5-=R$9$kO"A[%j%9%H!#(B
car $B$K!V<B:]$N%-!<F~NO$K$h$k$+$J(B prefix $BJ8;zNs!W!"(Bcdr $B$K!V(BSKK $B$N<-=q$,M=(B
$BA[$7$F$$$k$+$J(B prefix $BJ8;zNs!W$r;}$D(B cons cell $B$N%j%9%H!#(B

$B$3$N5,B'$,;H$o$l$k$N$O!"(B`skk-process-okuri-early' $B$,(B non-nil $B$N>l9g$N$_$G$"$k!#(B

$BNc$($P!"$+9T$NAw$j2>L>F~NO$K(B \"c\" $B$N(B prefix $B$r;H$&$N$G$"$l$P!"(B

  (setq skk-okuri-char-alist '((\"c\" . \"k\")))

$B$N$h$&$K=q$/!#(B"
  :type '(repeat (cons string string))
  :group 'skk-okurigana)

(defcustom skk-emacs-id-file (if skk-user-directory
				 (expand-file-name "emacs-id"
						   skk-user-directory)
			       (convert-standard-filename "~/.skk-emacs-id"))
  "\
*`skk-jisyo-file' $B$K:G6a%"%/%;%9$7$?(B SKK $B$N(B `skk-emacs-id' $B$rJ]B8$9$k%U%!%$%k!#(B"
  :type 'file
  :group 'skk-misc)

(defcustom skk-keep-record t
  "*Non-nil $B$G$"$l$P!"JQ495Z$S8D?M<-=q$K4X$9$kE}7W$r(B `skk-record-file' $B$K<h$k!#(B
$B?tCM$G$"$l$P!"(B`skk-record-file' $B$r$=$N9T?t$h$jBg$-$/$7$J$$!#(B
nil $B$G$"$l$P!"JQ495Z$S8D?M<-=q$K4X$9$kE}7W$r<h$i$J$$!#(B"
  :type '(radio (integer :tag "$B9T?t$r;XDj(B")
		(const :tag "$B%l%3!<%I%5%$%:@)8B$J$7(B" t)
		(const :tag "$B5-O?$7$J$$(B" nil))
  :group 'skk-misc)

(defcustom skk-record-file (if skk-user-directory
			       (expand-file-name "record" skk-user-directory)
			     (convert-standard-filename "~/.skk-record"))
  "*$BJQ495Z$S8D?M<-=q$K4X$9$kE}7W$r<h$k%U%!%$%k!#(B
$B8D?M<-=q$rJ]B8$7$?F|;~!"C18l$NEPO??t!"3NDj$7$?2s?t!"3NDjN(!"A4BN$N8l?t$N(B
$B>pJs$r<}$a$k!#(B"
  :type 'file
  :group 'skk-misc)

(defcustom skk-byte-compile-init-file nil
  "*Non-nil $B$G$"$l$P!"(B`skk-mode' $B5/F0;~$K(B `skk-init-file' $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
$B@53N$K8@$&$H!"(B

  (1)`skk-init-file' $B$r%P%$%H%3%s%Q%$%k$7$?%U%!%$%k$,$J$$$+!"(B
  (2)`skk-init-file' $B$H$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$rHf3S$7$F!"A0<T$NJ}$,?7$7(B
     $B$$$H$-(B

$B$K(B `skk-init-file' $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
nil $B$G$"$l$P!"(B`skk-init-file' $B$H$=$N%P%$%H%3%s%Q%$%k:Q$_%U%!%$%k$rHf3S$7$F(B
`skk-init-file' $B$NJ}$,?7$7$$$H$-$O!"$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$r>C$9!#(B

$B$3$NJQ?t$O(B ~/.emacs.d/init.el $B$G@_Dj$9$k$3$H!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-read-from-minibuffer-function nil "\
*$B<-=qEPO?%b!<%I$KF~$C$?$H$-$N%W%m%s%W%H$KI=<($9$k=i4|CM$rDs6!$9$k4X?t!#(B
$B$3$N4X?t$OJ8;zNs$rJV$5$J$1$l$P$J$i$J$$!#(B
$B4X?t(B `read-from-minibuffer' $B$N0z?t(B INITIAL-CONTENTS $B$K3:Ev$9$k!#(B

`skk-henkan-key' $B$r$=$N$^$^=i4|CM$H$7$FMxMQ$7$?$$$H$-$O!"(B

  (setq skk-read-from-minibuffer-function
        (lambda () skk-henkan-key))

$B$H;XDj$9$k!#(B"
  :type '(radio (function :tag "$B4X?t(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-misc)

(defface skk-jisyo-registration-badge-face
  '((((class color) (type tty))
     (:inherit default :inverse-video t))
    (((class color) (background light))
     (:inherit default :inverse-video t))
    (((class color) (background dark))
     (:inherit default :inverse-video t))
    (((class grayscale))
     (:inherit default :inverse-video t)))
  "*$B"-<-=qEPO?Cf"-$KE,MQ$9$k%U%'%$%9!#(B"
  :group 'skk-visual)

;;;###autoload
(defcustom skk-preload nil
  "*Non-nil $B$J$i$P!"(BEmacs $B5/F0;~$K(B SKK $B%W%m%0%i%`$H<-=q$NFI$_9~$_$r:Q$^$;$k!#(B
Emacs $B$N5/F0$=$N$b$N$OCY$/$J$k$,!"(BDDSKK $B$N=i2s5/F0$rAa$/$9$k$3$H$,$G$-$k!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-undo-kakutei-word-only nil
  "*Non-nil $B$G$"$l$P(B $B"&%b!<%I$H"'%b!<%I;~$N%"%s%I%%>pJs$r5-O?$7$J$$!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-undo-kakutei-return-previous-point nil
  "*Non-nil $B$G$"$l$P!"3NDj%"%s%I%%=hM}$,40N;$7$?8e$K!"3NDj%"%s%I%%=hM}$N(B
$BD>A0$N0LCV$K%+!<%=%k$rLa$9!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-show-japanese-menu
  (and window-system
       (or (eq window-system 'w32)
	   (boundp 'mac-carbon-version-string) ; Carbon Emacs
	   (featurep 'ns) ; Cocoa Emacs
	   (and (eq window-system 'x)
		(boundp 'gtk-version-string)
		(stringp (symbol-value 'gtk-version-string))
		(string< "2.0" (symbol-value 'gtk-version-string))))
       (equal current-language-environment "Japanese")) "\
*Non-nil $B$G$"$l$P%a%K%e!<%P!<$rF|K\8l$GI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-verbose nil
  "*Non-nil $B$G$"$l$P!"F~NOCf!?JQ49Cf$K%(%3!<%(%j%"$K>iD9$J%a%C%;!<%8$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-verbose-wait 1.5
  "*$B>iD9$J%a%C%;!<%8$rI=<($9$k$^$G$NBT$A;~4V(B ($BIC(B)$B!#(B"
  :type 'number
  :group 'skk-misc)

(defcustom skk-verbose-message-interval 5.0
  "*$B>iD9$J%a%C%;!<%8$,J#?t$"$k>l9g!"#1$D$"$?$jI=<(;~4V(B ($BIC(B)$B!#(B
$B$3$N;~4V$,7P2a$7$?$i<!$N%a%C%;!<%8$K@Z$jBX$($k!#(B"
  :type 'number
  :group 'skk-misc)

(defface skk-verbose-intention-face
  '((((class color) (type tty))
     (:inherit default :bold t))
    (((class color) (background light))
     (:inherit default :bold t))
    (((class color) (background dark))
     (:inherit default :bold t))
    (((class grayscale))
     (:inherit default :bold t)))
  "*$B"'%b!<%I$N>iD9$J%a%C%;!<%8$N(B {$B%"%N%F!<%7%g%s(B} $B$H(B {$B$I$l$r;2>H(B?} $B$KE,MQ$9$k(B
$B%U%'%$%9!#(B"
  :group 'skk-visual)

(defface skk-verbose-kbd-face
  '((((class color) (type tty))
     (:inherit default :foreground "cyan"))
    (((class color) (background light))
     (:inherit default :foreground "Purple"))
    (((class color) (background dark))
     (:inherit default :foreground "Cyan"))
    (((class grayscale))
     (:inherit default :foreground "LightGray")))
  "*$B>iD9$J%a%C%;!<%8$NA`:n%-!<ItJ,$KE,MQ$9$k%U%'%$%9!#(B"
  :group 'skk-visual)

(defcustom skk-henkan-on-message nil
  "*$B"&%b!<%I$GI=<($9$k>iD9$J%a%C%;!<%8$NFbMF!#(B
$BI8=`$G$O<+F0@_Dj$9$k!#(B"
  :type '(radio (string :tag "$BFbMF$r;XDj(B")
		(const :tag "$B<+F0@_Dj(B" nil))
  :group 'skk-misc)

(defcustom skk-j-mode-function-key-usage nil
  "*$B%-!<%\!<%I>e$N(B F1 $B!A(B F10 $B%-!<$N;H$$J}$r;XDj$9$k!#(B
`conversion' $B$J$i$P!"(B`skk-search-prog-list-1' $B!A(B `skk-search-prog-list-0' $B$r(B
$B<B9T$G$-$k!#(B
`kanagaki' $B$J$i$P!"$+$J%-!<%\!<%IF~NOMQ$N@_Dj$K$J$k!#(B
nil $B$J$i$P<+F0@_Dj$O$7$J$$(B ($B<+J,$G9%$-$J@_Dj$,$G$-$k(B)$B!#(B"
  :type '(radio (const :tag "$B@Z$jBX$(JQ495!G=MQ@_Dj(B" conversion)
		(const :tag "$B$+$JF~NOMQ@_Dj(B" kanagaki)
		(const :tag "$B@_Dj$7$J$$(B" nil))
  :group 'skk-misc)

(defcustom skk-mode-hook nil
  "*skk-mode $B$KF~$k$?$S$K8F$P$l$k%U%C%/!#(B
$BB>$K!"(B`skk-auto-fill-mode-hook', `skk-load-hook', `skk-init-file' $B$G$b(B
$B%+%9%?%^%$%:$,2DG=!#(B"
  ;; "*Hook run at SKK startup.  This hook is also run
  ;;in skk-auto-fill-mode after skk-auto-fill-mode-hook.
  ;;skk-auto-fill-mode-hook, skk-load-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-auto-fill-mode-hook nil
  "*`skk-auto-fill-mode' $B$r5/F0$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(B`skk-mode-hook', `skk-load-hook', `skk-init-file' $B$G$b%+%9%?%^%$%:$,(B
$B2DG=!#(B"
  ;;  "*Hook run at startup of skk-auto-fill-mode.
  ;;skk-mode-hook$B!"(Bskk-load-hook, skk-init-file may also be used for
  ;;customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-load-hook nil
  "*skk.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B
$BB>$K!"(B`skk-mode-hook', `skk-auto-fill-mode-hook', `skk-init-file' $B$G$b%+%9%?(B
$B%^%$%:$,2DG=!#(B"
  ;;  "*Hook run when SKK is loaded.
  ;;skk-auto-fill-mode-hook$B!"(Bskk-mode-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-misc)

(defcustom skk-status-indicator 'left
  "*SKK $B$N>uBV$r%b!<%I%i%$%s$N$I$N0LCV$KI=<($9$k$+$r7h$a$k!#(B
left $B$G$"$l$P:8C<$KI=<($9$k!#(B
$B$5$b$J$1$l$P%^%$%J!<%b!<%I$H$7$F$NI=<(K!$r<h$k!#(B"
  :type '(radio (const :tag "$B%b!<%I%i%$%s$N:8C<$KI=<((B" left)
		(const :tag "$B%^%$%J!<%b!<%I$N0l<o$H$7$FI=<((B" minor-mode))
  :group 'skk-visual)

(defcustom skk-latin-mode-string "SKK"
  "*$B%"%9%-!<%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($9$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-hiragana-mode-string "$B$+$J(B"
  "*$B$+$J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($9$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-katakana-mode-string "$B%+%J(B"
  "*$B%+%J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($9$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-jisx0208-latin-mode-string "$BA41Q(B"
  "*$BA41Q%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($9$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-abbrev-mode-string "a$B$"(B"
  "*SKK abbrev $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($9$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-visual)

(defcustom skk-indicator-use-cursor-color (and window-system
					       (fboundp 'x-display-color-p)
					       (x-display-color-p))
  "*Non-nil $B$J$i$P!"%+!<%=%k$HF1$8?'$G%$%s%8%1!<%?$rI=<($9$k(B"
  :type 'boolean
  :group 'skk-visual)

(defcustom skk-show-icon nil
  "*Non-nil $B$G$"$l$P!"%b!<%I%i%$%s$K(B SKK $B$N%"%$%3%s$r>o;~I=<($9$k!#(B
$BI=<($9$k(B SKK $B%"%$%3%s$N2hA|$O(B `skk-icon' $B$G;XDj$9$k!#(B"
  :type 'boolean
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       (set-default symbol value))
	   (when (and (boundp 'skk-mode-invoked)
		      skk-mode-invoked)
	     (cond (value
		    (if (featurep 'xemacs)
			(skk-xemacs-prepare-modeline-properties)
		      (skk-emacs-prepare-modeline-properties))
		    (skk-setup-modeline))
		   (t
		    (setq skk-icon nil))))))
  :group 'skk-visual)

(defcustom skk-echo t
  "*Non-nil $B$G$"$l$P!"2>L>J8;z$N%W%l%U%#%C%/%9$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-visual)

(defcustom skk-use-face (or window-system
			    (fboundp 'selected-frame)
			    ; XEmacs does not have this.
			    (fboundp 'frame-face-alist))
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$N(B face $B$N5!G=$r;HMQ$7$FJQ498uJd$r%O%$%i%$%HI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-visual)

;; should use defface?  however, can I use defface for highlight?
(defcustom skk-henkan-face 'skk-henkan-face-default
  "*$BJQ498uJd$N(B face $BB0@-!#(B`skk-use-face' $B$,(B non-nil $B$N$H$-$N$_M-8z!#(B
Emacs $BI8=`$N%U%'%$%9$N$[$+!"?7$?$K(B face $B$r:n$C$F;XDj$9$k$3$H$b2DG=!#(B
$B?7$?$J(B face $B$r:n$C$F;XDj$9$k$K$O!"(B

      (setq skk-henkan-face (skk-make-face 'DimGray/PeachPuff1))

$B$N$h$&$K(B skk-make-face() $B$rMxMQ$9$k$N$,<j7Z!#(B
foreground $B$H(B background $B$N?';XDj$@$1$G$J$$6E$C$?(B face $B$r:n$k>l9g$O!"(B`skk-make-face' $B$G(B
$B$OBP1~$G$-$J$$$N$G!"(BEmacs $B$N(B hilit19.el $B$N(B `hilit-lookup-face-create' $B$J$I$rMxMQ$9$k!#(B
$B?'$rIU$1$k>l9g$NG[?'$O!"(Bcanna.el $B$N(B `canna:attribute-alist' $B$,NI$$Nc$+$b$7$l$J$$!#(B

$B$3$NJQ?t$h$j$b(B `skk-treat-candidate-appearance-function' $B$N@_Dj$,M%@h$5$l$k!#(B"
  :type 'face
  :group 'skk-visual)

(defface skk-henkan-face-default
  '((((class color) (type tty))
     (:foreground "black" :background "green"))
    (((class color) (background light))
     (:foreground "black" :background "darkseagreen2"))
    (((class color) (background dark))
     (:foreground "white" :background "darkolivegreen"))
    (((class grayscale)) (:underline t)))
  "*$BI8=`$NJQ498uJd$N(B face $BB0@-!#(B"
  :group 'skk-visual)

(when (and skk-use-face
	   (boundp 'frame-background-mode)
	   (not frame-background-mode)
       (fboundp 'face-background)
	   (not (face-background 'skk-henkan-face-default)))
  (set-face-foreground 'skk-henkan-face-default "black")
  (set-face-background 'skk-henkan-face-default "darkseagreen2"))

(defcustom skk-henkan-overlay-priority 600
  "*$BJQ49$7$?8uJd$K=E$M$k(B overlay $B$N(B priority$B!#(B
$BNc$($P!"(BViper $B$G(B R $B%3%^%s%I$K$h$j(B replace $B$r9T$&$H$-$K!"(B
`viper-replace-overlay' $B$H$$$&(B priority 400 $B$N(B overlay $B$r=E$M$i$l$k$,!"(B
`skk-henkan-overlay-priority' $B$N%G%U%)%k%HCM$O$3$N(B overlay $B$h$j(B
priority $B$,9b$$$N$G!"M%@h$7$FI=<($5$l$k!#(B"
  :type 'integer
  :group 'skk-visual)

(defcustom skk-treat-candidate-appearance-function nil
  "*$B8uJd$NI=<($rAu>~$9$k$?$a$N4X?t$r;XDj$9$kJQ?t!#(B
$B%f!<%6$O8uJd$H$J$k$Y$-J8;zNs$KBP$7$F!"$=$NCm<a!J%"%N%F!<%7%g%s!K$b4^$a$F(B
$B$[$\G$0U$N2C9)$r;\$9$3$H$,$G$-$k!#$3$N4X?t$O0J2<$N>r7o$rK~$?$9I,MW$,$"$k!#(B

1. $B0z?t$r#2$D<h$k$3$H!#(B
2. $BBh#10z?t$OJ8;zNs$H$7$F07$&$3$H!#$3$l$O2C9)A0$NJ8;zNs$KAjEv$9$k!#(B
3. $BBh#20z?t$,(B nil $B$N;~$ODL>o$NJQ49;~!"(Bnon-nil $B$N;~$O8uJd0lMwI=<(;~$rI=$9(B
   $B$b$N$H$7$F07$&$3$H!#(B
4. $BJV$jCM$O0J2<$N$$$:$l$+$H$9$k$3$H!#(B
 a. $BJ8;zNs(B
    $B$3$N>l9g!"$3$NJ8;zNs$O8uJd$H%"%N%F!<%7%g%s$rN>J}4^$_$&$k$b$N$H$7$F=h(B
    $BM}$5$l$k!#(B

 b. cons cell ($B8uJd(B . $B%"%N%F!<%7%g%s(B)
    $B$3$N>l9g!"8uJd$O$b$&%"%N%F!<%7%g%s$r4^$^$J$$$b$N$H$7$F=hM}$5$l$k!#(B
    $B%"%N%F!<%7%g%s$K$D$$$F$O@hF,$,(B \";\" $B$+$I$&$+$rD4$Y$?>e$G=hM}$5$l$k!#(B

 c. cons cell ($B8uJd(B . ($B%;%Q%l!<%?(B . $B%"%N%F!<%7%g%s(B))
    $B$3$N>l9g!"8uJd$O$b$&%"%N%F!<%7%g%s$r4^$^$J$$$b$N$H$7$F=hM}$5$l$k!#(B
    $B%;%Q%l!<%?$ODL>o$N(B \";\" $B$NBe$o$j$KMxMQ$5$l$k!#%"%N%F!<%7%g%s$O$b$&(B
    $B%;%Q%l!<%?$r4^$^$J$$$b$N$H$7$F=hM}$5$l$k!#(B

$B$3$N4X?t$O0J2<$N>l9g$K8F$P$l$k!#(B

o $BDL>o$NJQ49F0:n$NETEY(B
  $B$3$N>l9g$O!"8uJd$O%P%C%U%!$K!"%"%N%F!<%7%g%s$O%(%3!<%(%j%"$J$I!J%f!<%6(B
  $B$N@_Dj$K$h$C$F0[$J$k>l=j!K$KI=<($5$l$k!#%;%Q%l!<%?$OI=<($5$l$J$$!#(B

o $B8uJd0lMw$rI=<($9$k$H$-(B ($B8uJd$NJ8;zNs$N8e$m$K%"%N%F!<%7%g%s$,IU2C$5$l$k(B)
  $B$3$N>l9g$O!"8uJd!"%;%Q%l!<%?!"%"%N%F!<%7%g%s$N3FJ8;zNs$,I=<($5$l$k!#(B

 ($B@_DjNc(B)

 (setq skk-treat-candidate-appearance-function
       (lambda (candidate listing-p)
	 (cond
	  ((string-match \";\" candidate)
	   (put-text-property 0 (match-beginning 0)
			      'face (if listing-p 'tooltip 'underline)
			      candidate)
	   (put-text-property (match-beginning 0)
			      (length candidate) 'face 'shadow candidate))
	  (t
	   (put-text-property 0 (length candidate)
			      'face (if listing-p 'tooltip 'underline)
			      candidate)))
	  candidate))

"
  :type '(radio (const :tag "$B@_Dj%5%s%W%k(B1" skk-treat-candidate-sample1)
		(const :tag "$B@_Dj%5%s%W%k(B2" skk-treat-candidate-sample2)
		(const :tag "$B;XDj$7$J$$(B" nil)
		(function :tag "$BG$0U$N4X?t(B"))
  :group 'skk-annotation
  :group 'skk-visual)

;; skk-treat-candidate-appearance-function $B$N$?$a$KMQ0U$9$k4X?t(B
(defun skk-treat-candidate-sample1 (candidate listing-p)
  (cond
   ((string-match ";" candidate)
    (put-text-property 0 (match-beginning 0)
		       'face 'skk-tut-question-face
		       candidate)
    (put-text-property (match-beginning 0) (length candidate)
		       'face 'skk-tut-hint-face
		       candidate))
   (t
    (put-text-property 0 (length candidate)
		       'face 'skk-tut-question-face
		       candidate)))
  candidate)

(defun skk-treat-candidate-sample2 (candidate listing-p)
  (let* ((value (skk-treat-strip-note-from-word candidate))
	 (cand (car value))
	 (note (if listing-p
		   (or (and (eq skk-annotation-lookup-lookup 'always)
			    (skk-lookup-get-content cand t))
		       (and (eq skk-annotation-lookup-DictionaryServices 'always)
			    (skk-annotation-lookup-DictionaryServices cand t))
		       (cdr value))
		 (cdr value)))
	 (sep (if note
		  (propertize (if (skk-annotation-display-p 'list)
				  " $B"b(B "
				" !")
			      'face 'skk-tut-do-it-face)
		nil)))
    (cond (note
	   (put-text-property 0 (length cand)
			      'face 'skk-tut-question-face cand)
	   (put-text-property 0 (length note)
			      'face 'skk-tut-hint-face note)
	   (cons cand (cons sep note)))
	  (t
	   (put-text-property 0 (length cand)
			      'face 'skk-treat-default cand)
	   cand))))

(defface skk-treat-default
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "$BGX7J$J$7$NC1=c$J(B face$B!#(B`default' $B$NBe$o$j$K;H$&!#(B"
  :group 'skk-visual)

;;; -- Internal constants and variables of skk.el
(defconst skk-coding-system-alist
  (cond ((and (featurep 'emacs)
	      (>= emacs-major-version 23))
	 '(("euc" . euc-jis-2004)
	   ("ujis" . euc-jis-2004)
	   ("sjis". japanese-shift-jis-2004)
	   ("jis" . iso-2022-jp-3)))
	((featurep 'jisx0213)
	 '(("euc" . euc-jisx0213)
	   ("ujis" . euc-jisx0213)
	   ("sjis". shift_jisx0213)
	   ("jis" . iso-2022-jp-3-strict)))
	(t
	 '(("euc" . euc-japan)
	   ("ujis" . euc-japan)
	   ("sjis". shift_jis)
	   ("jis" . junet))))
  "coding-system $B$NJ8;zNsI=8=$H!"%7%s%\%kI=8=$NO"A[%j%9%H!#(B")

(defconst skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "t" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "$B$+$JJ8;z$+$i%m!<%^;z$X$NJQ49%k!<%k!#(B
$B2<5-$N3:Ev$9$k$+$JJ8;z$r$=$NJ8;z$N%m!<%^;z%W%l%U%#%C%/%9$G8=$o$7$?$b$N!#(B
    $B$!(B  $B$"(B  $B$#(B  $B$$(B  $B$%(B  $B$&(B  $B$'(B  $B$((B  $B$)(B  $B$*(B  $B$+(B  $B$,(B  $B$-(B  $B$.(B  $B$/(B  $B$0(B
    $B$1(B  $B$2(B  $B$3(B  $B$4(B  $B$5(B  $B$6(B  $B$7(B  $B$8(B  $B$9(B  $B$:(B  $B$;(B  $B$<(B  $B$=(B  $B$>(B  $B$?(B  $B$@(B
    $B$A(B  $B$B(B  $B$C(B  $B$D(B  $B$E(B  $B$F(B  $B$G(B  $B$H(B  $B$I(B  $B$J(B  $B$K(B  $B$L(B  $B$M(B  $B$N(B  $B$O(B  $B$P(B
    $B$Q(B  $B$R(B  $B$S(B  $B$T(B  $B$U(B  $B$V(B  $B$W(B  $B$X(B  $B$Y(B  $B$Z(B  $B$[(B  $B$\(B  $B$](B  $B$^(B  $B$_(B  $B$`(B
    $B$a(B  $B$b(B  $B$c(B  $B$d(B  $B$e(B  $B$f(B  $B$g(B  $B$h(B  $B$i(B  $B$j(B  $B$k(B  $B$l(B  $B$m(B  $B$n(B  $B$o(B  $B$p(B
    $B$q(B  $B$r(B  $B$s(B"
  ;; (length skk-kana-rom-vector)
  ;; --> 83
  ;; (setq kana '("$B$!(B" "$B$"(B" "$B$#(B" "$B$$(B" "$B$%(B" "$B$&(B" "$B$'(B" "$B$((B" "$B$)(B" "$B$*(B"
  ;;		  "$B$+(B" "$B$,(B" "$B$-(B" "$B$.(B" "$B$/(B" "$B$0(B" "$B$1(B" "$B$2(B" "$B$3(B" "$B$4(B"
  ;;	          "$B$5(B" "$B$6(B" "$B$7(B" "$B$8(B" "$B$9(B" "$B$:(B" "$B$;(B" "$B$<(B" "$B$=(B" "$B$>(B"
  ;;	          "$B$?(B" "$B$@(B" "$B$A(B" "$B$B(B" "$B$C(B" "$B$D(B" "$B$E(B" "$B$F(B" "$B$G(B" "$B$H(B" "$B$I(B"
  ;;		  "$B$J(B" "$B$K(B" "$B$L(B" "$B$M(B" "$B$N(B" "$B$O(B" "$B$P(B" "$B$Q(B" "$B$R(B" "$B$S(B" "$B$T(B"
  ;;		  "$B$U(B" "$B$V(B" "$B$W(B" "$B$X(B" "$B$Y(B" "$B$Z(B" "$B$[(B" "$B$\(B" "$B$](B"
  ;;		  "$B$^(B" "$B$_(B" "$B$`(B" "$B$a(B" "$B$b(B" "$B$c(B" "$B$d(B" "$B$e(B" "$B$f(B" "$B$g(B" "$B$h(B"
  ;;	          "$B$i(B" "$B$j(B" "$B$k(B" "$B$l(B" "$B$m(B" "$B$n(B" "$B$o(B" "$B$p(B" "$B$q(B" "$B$r(B" "$B$s(B"))
  ;; (length kana)
  ;; --> 83
  ;; (mapcar (lambda (s) (- (char-octet (string-to-char s) 1) 33))
  ;;	kana)
  ;; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25\
  ;;      26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48\
  ;;      49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71\
  ;;      72 73 74 75 76 77 78 79 80 81 82)
  )

(defconst skk-default-jisx0208-latin-vector
  ;; note that skk-jisx0208-latin-vector is a user variable.
  ;; skk.el $B%m!<%IA0$K(B ~/.emacs.d/init.el $B$J$I$G!"(Bskk-jisx0208-latin-vector $B$NJL$NCM$r%f!<(B
  ;; $B%6!<$,D>@\=q$$$?$j!"(Bskk.el $B%m!<%I8e$K$3$NCM$r(B aset $B$GD>@\$$$8$C$?$j$7$J(B
  ;; $B$1$l$P(B default-value $B$G(B skk-jisx0208-latin-vector $B$K%"%/%;%9$9$k$3$H$G(B
  ;; skk-default-jisx0208-latin-vector $B$NCM$rJ];}$9$k$3$H$b$G$-$h$&$,!"$=$l$O(B
  ;; $BK>$a$J$$(B...$B!#(B
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "`skk-jisx0208-latin-region' $B$G;2>H$9$kJ8;z%F!<%V%k!#(B
\"ascii\" -> \"$B#a#s#c#i#i(B\" $B$N$h$&$JA43QJ8;z$XJQ49$9$k:]$KMxMQ$9$k!#(B")

(defconst skk-kana-cleanup-command-list
  '(skk-undo
    skk-kakutei
    skk-delete-backward-char
    skk-insert
    skk-try-completion
    skk-completion-wrapper
    skk-previous-candidate))

(defconst skk-delete-backward-char-commands
  ;; following two are SKK adviced.
  ;;viper-del-backward-char-in-insert
  ;;vip-del-backward-char-in-insert
  '(backward-delete-char-untabify
    backward-delete-char
    backward-or-forward-delete-char
    delete-backward-char
    picture-backward-clear-column))

(defconst skk-undo-commands
  '(undo advertised-undo))

(defconst skk-quote-char-alist
  '((?\; . "\\073")
    (?/ . "\\057")
    (?\n . "\\n")
    (?\r . "\\r")
    (?\" . "\\\"")
    (?\\  . "\\\\"))
  "$B<-=q%(%s%H%jFb$K4^$a$F$O$J$i$J$$J8;z$rCV$-JQ$($k$?$a$NO"A[%j%9%H!#(B
`;' $B$O!"Cp<a$H4X78$J$$>l9g$@$1CV49$9$k!#(B")

(defvar skk-charset-list nil
  "SKK $B$,07$&J8;z=89g$N%j%9%H!#(BSKK $B=i2s5/F0;~$K(B GNU Emacs 23 $B0J>e$G$"$l$P@_Dj$5$l$k!#(B")

(defvar skk-emacs-id nil
  "$BJ#?t$N(B emacs $B%W%m%;%9$r<1JL$9$kJ8;zNs!#(B
$B$R$H$D$N8D?M<-=q%U%!%$%k$rJ#?t$N(B emacs $B>e$G5/F0$7$F$$$k(B SKK $B$G6&M-$9$k$H(B
$B$-$K;2>H$9$k!#(B")

(defvar skk-jisyo-update-vector nil
  "`skk-share-private-jisyo' $BM-8z;~$K<-=q%P%C%U%!99?7>pJs$rJ];}$9$k(B vector.
$BD9$5$O(B `skk-jisyo-save-count' $B$h$jD9$/$J$k$h$&$K@_Dj$7$F$$$k!#(B
$B<-=q%P%C%U%!99?7$N5-O?$rJ]B8$7!"<-=q%P%C%U%!$r<-=q%U%!%$%k$K%;!<%V$9$k$H$-$K!"(B
$BB>$N(B SKK $B$,<-=q%U%!%$%k$K:G6a%"%/%;%9$7$F$$$k$H$-$K$O!"<-=q%U%!%$%k$r%P%C%U%!(B
$B$KFI$_9~$s$G$+$i!"(B`skk-jisyo-update-vector' $B$rMQ$$$F%P%C%U%!$r99?7D>$7!"$=$N(B
$B7k2L$r%U%!%$%k$K%;!<%V$9$k!#(B")

(defvar skk-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$9%D%j!<$N=i4|>uBV!#(B
$B:G=i$K(B `skk-mode' $B$r5/F0$7$?$H$-$K(B `skk-rom-kana-base-rule-list' $B$H(B
`skk-rom-kana-rule-list' $B$+$iLZ$N7A$K%3%s%Q%$%k$5$l$k!#(B
\\[skk-restart] $B$K$h$C$F$b:F%3%s%Q%$%k$5$l$k!#(B")

(defvar skk-insert-new-word-function nil
  "$B8uJd$rA^F~$7$?$H$-$K(B `funcall' $B$5$l$k4X?t$rJ]B8$9$kJQ?t!#(B")

(defvar skk-mode-invoked nil
  "Non-nil $B$G$"$l$P!"(BEmacs $B$r5/F08e4{$K(B `skk-mode' $B$r5/F0$7$?$3$H$r<($9!#(B")

(defvar skk-kakutei-count 0
  "$BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
`skk-record-file' $B$N(B \"$B3NDj(B:\" $B9`L\$N%+%&%s%?!<!#(B")

(defvar skk-touroku-count 0
  "$B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
`skk-record-file' $B$N(B \"$BEPO?(B:\" $B9`L\$N%+%&%s%?!<!#(B")

(defvar skk-update-jisyo-count 0
  "$B8D?M<-=q$r99?7$7$?2s?t!#(B
$B$3$NJQ?t$N?tCM$,(B `skk-jisyo-save-count' $B0J>e$H$J$C$?$H$-$K8D?M<-=q$,(B
$B%*!<%H%;!<%V$5$l$k!#(B
$B8D?M<-=q$,%;!<%V$5$l$k$H%$%K%7%c%i%$%:$5$l$k!#(B")

(defvar skk-kakutei-history nil
  "$BAw$j$J$7$G3NDj$5$l$?8+=P$78l!&8uJd$NMzNr!#(B

   (\"$B$_$@$7$4(B\" \"$B8+=P$78l(B\" buffer)

   $B$H$$$&7A<0$N%j%9%H!#(B")

(defvar skk-minibuffer-origin-mode nil
  "$BF~NO%b!<%I$rI=$o$9%7%s%\%k!#(B
$BM-8z$JCM$O!"(B`hiragana', `katakana', `abbrev', `latin', `jisx0208-latin'
$B$b$7$/$O(B nil $B$N$$$:$l$+!#(B")

(defvar skk-menu nil)

(defvar skk-icon nil
  "SKK $B%"%$%3%s$N2hA|%U%!%$%k(B skk.xpm $B$N%Q%9!#(B")

(put 'skk-icon 'risky-local-variable t)

(skk-deflocalvar skk-modeline-input-mode nil)
(put 'skk-modeline-input-mode 'risky-local-variable t)

(defvar skk-indicator-alist nil)

(defvar skk-buffer-undo-list nil)

(defvar skk-inline-overlays nil)

(defvar skk-latin-mode-map nil
  "*$B%"%9%-!<%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-j-mode-map nil
  "*$B$+$J%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-jisx0208-latin-mode-map nil
  "*$BA41Q%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-abbrev-mode-map nil
  "*SKK abbrev $B%b!<%I$N%-!<%^%C%W!#(B")

(defvar skk-henkan-in-minibuff-nest-level nil)

(defvar skk-menu-items
  ;; SKK $B%a%K%e!<$NDj5A!#(B
  '("SKK"
    ("Convert Region and Echo"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-gyakubiki-message start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-gyakubiki-katakana-message
	   start end 'all-candidates)))
       skk-use-kakasi])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-hurigana-message start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-hurigana-katakana-message
	   start end 'all-candidates)))
       skk-use-kakasi]))
    ("Convert Region and Replace"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-gyakubiki-region start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-gyakubiki-katakana-region
	   start end 'all-candidates)))
       skk-use-kakasi])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(lambda (start end)
	  (interactive "r")
	  (skk-hurigana-region start end 'all-candidates)))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(lambda (start end) (interactive "r")
	  (skk-hurigana-katakana-region
	   start end 'all-candidates)))
       skk-use-kakasi])
     ["Hiragana to Katakana" skk-katakana-region t]
     ["Katakana to Hiragana" skk-hiragana-region t]
     ["Ascii to Zenkaku" skk-jisx0208-latin-region t]
     ["Zenkaku to Ascii" skk-latin-region t]
     ["Kana and Zenkaku to Romaji" skk-romaji-region skk-use-kakasi])
    ["Count Jisyo Candidates" skk-count-jisyo-candidates t]
    ["Save Jisyo" skk-save-jisyo t]
    ["Undo Kakutei" skk-undo-kakutei t]
    ["Restart SKK" skk-restart t]
    ["Version" (message "%s" (skk-version)) t])
  "Menu used in SKK mode.")

(defvar skk-quit-commands '(keyboard-quit abort-recursive-edit
					  skk-kanagaki-bs
					  skk-kanagaki-esc))

;; ---- buffer local variables

;; <$B%U%i%0N`(B>

;;(skk-deflocalvar skk-current-henkan-data
;;  '(;; global variables

;;    ;; $B%P%C%U%!%m!<%+%kJQ?t$N%G%U%)%k%HCM$r@_Dj$9$k$H!"$3$l$rD>@\=q49$($7$?(B
;;    ;; $B$H$-$KB>$N%P%C%U%!$+$i8+$($kCM$bJQ$o$C$F$7$^$&!#(Bglobal $B$J%U%i%0$O$3$l(B
;;    ;; $B$rMxMQ$7$F%G%U%)%k%HCM$rM?$($F$*$/!#(B

;;    ;; Emacs $B$r5/F08e4{$K(B skk-mode $B$r5/F0$7$?$3$H$r<($9(B
;;    (invoked . nil)

;;    ;; skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0(B
;;    (isearch-message . nil)

;;    ;; $BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (kakutei-count . 0)

;;    ;;$BF~NO%b!<%I$rI=$o$9%7%s%\%k(B
;;    (minibuffer-origin-mode . nil)

;;    ;; $B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (touroku-count . 0)

;;    ;; $B<-=q$r99?7$7$?2s?t(B
;;    (update-jisyo-count . 0)

;;    ;; buffer-local variables.

;;    ;; `skk-search-prog-list' $B$N8=:_$NCM$rJ]B8$9$k%j%9%H(B
;;    ;; (current-search-prog-list . nil)

;;    ;; $B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$3$H$r<($9(B
;;    ;; (exit-show-candidates . nil)

;;    ;; $B"'%b!<%I(B ($BJQ49Cf(B) $B$G$"$k$3$H$r<($9(B
;;    ;; (henkan-active . nil)

;;    ;; `skk-henkan-list' $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N(B
;;    ;; (henkan-count . -1)

;;    ;; $BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (henkan-end-point . nil)

;;    ;; $B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D(B
;;    ;; (henkan-in-minibuff-flag . nil)

;;    ;; $BJQ49$9$Y$-8+=P$78l(B
;;    ;; (henkan-key . nil)

;;    ;; $BJQ497k2L$N8uJd$N%j%9%H(B
;;    ;; (henkan-list . nil)

;;    ;; $B8=:_$NJQ49$NAw$j2>L>ItJ,(B
;;    ;; (henkan-okurigana . nil)

;;    ;; $B"&%b!<%I(B ($BJQ49BP>]$NJ8;zNs7hDj$N$?$a$N%b!<%I(B) $B$G$"$k$3$H$r<($9(B
;;    ;; (henkan-on . nil)

;;    ;; $BJQ493+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (henkan-start-point . nil)

;;    ;; $B3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9(B
;;    ;; (kakutei-flag . nil)

;;    ;; $B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (kana-start-point . nil)

;;    ;; $BF~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9(B
;;    ;; (katakana . nil)

;;    ;; $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-ari-max . nil)

;;    ;; $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-ari-min . nil)

;;    ;; $BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9(B
;;    ;; (okuri-char . nil)

;;    ;; `skk-henkan-list' $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G(B
;;    ;; $B8!:w$7$?:G8e$N8uJd$r;X$9$b$N(B
;;    ;; (okuri-index-max . -1)

;;    ;; `skk-henkan-list' $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G(B
;;    ;; $B8!:w$7$?:G=i$N8uJd$r;X$9$b$N(B
;;    ;; (okuri-index-min . -1)

;;    ;; $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-nasi-min . nil)

;;    ;; $BAw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9(B
;;    ;;(okurigana . nil)

;;    ;; $BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (okurigana-start-point . nil)

;;    ;; $BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9(B
;;    ;; (prefix . "")

;;    ;; $B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(B
;;    ;; `skk-with-point-move' $B$,;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H(B
;;    ;; `skk-after-point-move' $B$,:nF0$9$k(B
;;    ;; (previous-point . nil)

;;    ;; `skk-insert' $B$b$7$/$O(B `skk-jisx0208-latin-insert' $B$GO"B3F~NO$7$?(B
;;    ;; $BJ8;z?t$rI=$o$9%+%&%s%?!<(B
;;    ;; (self-insert-non-undo-count . 1)))

(skk-deflocalvar skk-mode nil "\
Non-nil $B$G$"$l$P!"%+%l%s%H%P%C%U%!$G8=:_(B `skk-mode' $B$r5/F0$7$F$$$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,%"%9%-!<%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-j-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,$+$J!&%+%J%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-katakana nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9!#(B
\"(and (not skk-katakana) skk-j-mode)\" $B$,(B t $B$G$"$l$P!"$+$J%b!<%I$G$"$k$3$H$r(B
$B<($9!#(B")

(skk-deflocalvar skk-jisx0208-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,A41Q%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B SKK abbrev $B%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-okurigana nil
  "Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-henkan-mode nil
  "$BJQ49%b!<%I$r<($9!#(B
`on' $B$G$"$l$P!""&%b!<%I!#(B
`active' $B$G$"$l$P!""'%b!<%I!#(B
`nil' $B$G$"$l$P!"3NDjF~NO%b!<%I!#(B")

(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil $B$J$i3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9!#(B")

(skk-deflocalvar skk-kakutei-henkan-flag nil
  "Non-nil $B$J$i3NDjJQ49$9$k;v$r;X$9!#(B
`skk-search-kakutei-jisyo-file' $B$d!"%f!<%6<+:n$N3NDjJQ49MQ%W%m%0%i%`$O(B
$B$3$NJQ?t$r%;%C%H$9$k!#(B

$B$3$NJQ?t$,(B Non-nil $B$K%;%C%H$5$l$F$b!"JQ49$7$F:G=i$KF@$i$l$?8uJd$G$J$1$l$P(B
$B3NDjJQ49$5$l$J$$$3$H$KCm0U!#(B")

(skk-deflocalvar skk-exit-show-candidates nil
  "$B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$H$-$K(B non-nil $B$H$J$k!#(B
$B$=$NCM$O%j%9%H$G!"(Bcar $B$K(B `skk-henkan-show-candidates' $B4X?t$G(B while $B%k!<%W$r(B
$B2s$C$?2s?t$r<($90l;~JQ?t(B loop $B$NCM$r!"(Bcdr $BIt$K:G8e$K%_%K%P%C%U%!$KI=<($7$?(B
1 $B$DA0$N8uJd72$N:G8e$NMWAG$r;X$9%$%s%G%/%9$,BeF~$5$l$k!#(B
`skk-henkan-show-candidates' $B$H(B `skk-set-exit-show-candidates' $B$GJQ99!"(B
$B;2>H$5$l$k!#(B")

 ;; <$B%-!<%^%C%W4XO"(B>
(skk-deflocalvar skk-current-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$o$9%D%j!<$N8=;~E@$N>uBV!#(B
$B%m!<%^;zF~NO$N=i4|$G$O(B `skk-rule-tree' $B$HF10l$N>uBV$G!"J8;zF~NO$,?J$`$K(B
$B$D$l!"LZ$r$?$I$C$F$f$/>uBV$NA+0\$rI=$9!#(B")

;; <$B<-=q4XO"$NJQ?t(B>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H!#(B
`skk-jisyo' $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B
`skk-jisyo' $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B")

;; <$B$=$NB>(B>
(skk-deflocalvar skk-mode-line nil
  "SKK $B$N%b!<%I$r<($9%b!<%I%i%$%s$NJ8;zNs!#(B
`skk-mode-string', `skk-hiragana-mode-string', `skk-katakana-mode-string',
 `skk-jisx0208-latin-mode-string' $B$N$$$:$l$+$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-previous-point nil
  "`skk-with-point-move' $B4XO"JQ?t!#(B
$B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(B`skk-with-point-move'
$B$,;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H!"(B`skk-after-point-move' $B$,:nF0$9$k!#(B")

(skk-deflocalvar skk-prefix ""
  "$BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9!#(B")

(defface skk-prefix-hiragana-face
  '((((class color) (type tty))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "coral4"))
    (((class color) (background dark))
     (:foreground "pink"))
    (((class grayscale)) (:underline t)))
  "*$B$+$J%b!<%I$N%m!<%^;z%W%l%U%#%C%/%9$N(B face $BB0@-!#(B"
  :group 'skk-visual)

(defface skk-prefix-katakana-face
  '((((class color) (type tty))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "forestgreen"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class grayscale)) (:underline t)))
  "*$B%+%J%b!<%I$N%m!<%^;z%W%l%U%#%C%/%9$N(B face $BB0@-!#(B"
  :group 'skk-visual)

(defface skk-prefix-jisx0201-face
  '((((class color) (type tty))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blueviolet"))
    (((class color) (background dark))
     (:foreground "thistle"))
    (((class grayscale)) (:underline t)))
  "*JISX0201 $B%b!<%I$N%m!<%^;z%W%l%U%#%C%/%9$N(B face $BB0@-!#(B"
  :group 'skk-visual)

(skk-deflocalvar skk-prefix-overlay nil
  "`skk-prefix' $B$rI=<($9$k$?$a$K;HMQ$5$l$k(B overlay$B!#(B
`skk-echo' $B$NCM$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B")

(skk-deflocalvar skk-henkan-start-point nil
  "$BJQ493+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-henkan-end-point nil
  "$BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-kana-start-point nil
  "$B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-okurigana-start-point nil
  "$BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-henkan-key nil
  "$BJQ49$9$Y$-8+=P$78l!#(B
$BNc$($P!"(B\"$B"&$+$J(B\" $B$rJQ49$9$l$P!"(B`skk-henkan-key' $B$K$O(B \"$B$+$J(B\" $B$,BeF~$5$l$k!#(B
\"$B"&$o$i(B*$B$&(B\" $B$N$h$&$JAw$j$"$j$NJQ49$N>l9g$K$O!"(B\"$B$o$i(Bu\" $B$N$h$&$K!"4A;zItJ,$N(B
$BFI$_$,$J(B + $BAw$j2>L>$N:G=i$NJ8;z$N%m!<%^;z$N%W%l%U%#%C%/%9$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-okuri-char nil
  "$BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9!#(B
$BNc$($P!"(B\"$B$*$/(B*$B$j(B\" $B$rJQ49$9$k$H$-$O!"(B`skk-okuri-char' $B$O(B \"r\"$B!#(B
`skk-okuri-char' $B$,(B non-nil $B$G$"$l$P!"Aw$j$"$j$NJQ49$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-henkan-okurigana nil
  "$B8=:_$NJQ49$NAw$j2>L>ItJ,!#(B
$BNc$($P!"(B\"$B"&$&$^$l(B*$B$k(B\" $B$rJQ49$9$l$P!"(B`skk-henkan-okurigana' $B$K$O(B \"$B$k(B\" $B$,BeF~(B
$B$5$l$k!#(B")

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "$B3NDj<-=q$K$h$j:G8e$K3NDj$7$?$H$-$N8+=P$78l!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K(B x $B%-!<$r2!$9$H3NDj$,%"%s%I%%$5$l$F!"3NDjA0$N>uBV$G(B
$B$3$N8+=P$78l$,%+%l%s%H%P%C%U%!$KA^F~$5$l$k!#(B")

(skk-deflocalvar skk-henkan-list nil
  "$BJQ497k2L$N8uJd$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$J(B*$B$/(B\" $B$H$$$&JQ49$9$l$P!"(B`skk-henkan-list' $B$O(B
\(\"$BLD(B\" \"$B5c(B\" \"$BL5(B\" \"$BK4(B\") $B$N$h$&$K$J$k!#(B")

(skk-deflocalvar skk-henkan-count -1
  "`skk-henkan-list' $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N!#(B")

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "$BO"B3F~NO$7$?J8;z?t$rI=$o$9%+%&%s%?!<!#(B
`skk-insert' $B$b$7$/$O(B `skk-jisx0208-latin-insert' $B$G%+%&%s%H$5$l$k!#(B
Emacs $B$N%*%j%8%J%k$NF0:n$G$O!"(B`self-insert-command' $B$K%P%$%s%I$5$l$?%-!<F~NO$O(B
$BO"B3(B 20 $B2s$^$G$,(B 1 $B$D$N%"%s%I%%$NBP>]$H$J$k!#$3$NF0:n$r%(%_%e%l!<%H$9$k$?$a$N(B
$B%+%&%s%?!<!#$3$N%+%&%s%?!<$,!"(B20 $BL$K~$G$"$k$H$-$O!"F~NO$N$?$S$K(B
`cancel-undo-boundary' $B$,%3!<%k$5$l$k!#(B")

(skk-deflocalvar skk-current-search-prog-list nil
  "`skk-search-prog-list' $B$N8=:_$NCM$rJ]B8$9$k%j%9%H!#(B
$B:G=i$NJQ49;~$O(B `skk-search-prog-list' $B$NA4$F$NCM$rJ];}$7!"JQ49$r7+$jJV$9$?$S$K(B
1 $B$D$:$DC;$/$J$C$F$f$/!#(B")

(defvar skk-search-state nil)
(defvar skk-search-ex-state nil)

;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-data nil
  "$B:G8e$K9T$C$?JQ49$K4X$9$k%G!<%?$NO"A[%j%9%H!#%G%U%)%k%H$N%-!<(B
$B$O!"(B`henkan-key', `henkan-okurigana', `okuri-char',
`henkan-list', `henkan-point', `henkan-buffer', `abbrev-mode' $B$N3F(B
$B%7%s%\%k!#(B
 (skk-num $B$r(B require $B$7$F$$$k$H$-$O!"(Bnum-list $B$,DI2C$5$l$k(B)$B!#(B")

(skk-deflocalvar skk-undo-kakutei-flag nil
  "Non-nil $B$J$i$P!"3NDj%"%s%I%%Cf$G$"$k$3$H$r;X$9!#(B")

(skk-deflocalvar skk-undo-kakutei-prev-state nil
  "`skk-undo-kakutei' $B$,8F$P$l$?;~$NF~NO%b!<%I$N>uBV!#(B")

(skk-deflocalvar skk-undo-kakutei-previous-point nil
  "$B3NDj%"%s%I%%D>A0$N%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-undo-kakutei-previous-length nil
  "$B3NDj%"%s%I%%$9$kBP>]$NJQ497k2L$ND9$5!#(B")

(skk-deflocalvar skk-henkan-overlay nil
  "$B8uJd$rI=<($9$k$H$-$K;HMQ$9$k(B Overlay$B!#(B")

(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "$B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D!#(B
`skk-remove-common' $B$G;2>H$5$l$k!#(B")

(skk-deflocalvar skk-okuri-index-min -1
  "`skk-henkan-list' $B$N%$%s%G%/%9$rA^$9%]%$%s%?$N$R$H$D!#(B
$B<+F0Aw$j=hM}$G8!:w$7$?:G=i$N8uJd$r;X$9!#(B")

(skk-deflocalvar skk-okuri-index-max -1
  "`skk-henkan-list' $B$N%$%s%G%/%9$rA^$9%]%$%s%?$N$R$H$D!#(B
$B<+F0Aw$j=hM}$G8!:w$7$?:G8e$N8uJd$r;X$9!#(B")

(skk-deflocalvar skk-last-buffer-undo-list nil
  "$B"&%b!<%I$KF~$kD>A0$N(B `buffer-undo-list' $B$rB`Hr$7$F$*$/JQ?t!#(B")

(skk-deflocalvar skk-after-prefix nil
  "t $B$G$"$l$P!"@\F,<-F~NO8e$N>uBV$K$"$k$3$H$rI=$9!#(B
$B@\F,<-F~NO3+;O;~$K(B t $B$K%;%C%H$5$l!"B3$/8l$N3NDj8e$K(B nil $B$K%;%C%H$5$l$k!#(B")

;; skk-act.el related.
(defcustom skk-use-act nil
  "*Non-nil $B$G$"$l$P3HD%%m!<%^;zF~NO(B ACT $B$rMxMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-act)

(defcustom skk-act-use-normal-y nil
  "*Non-nil $B$G$"$l$P(B \"y\" $B$r;H$C$?Y92;$NF~NO$rM-8z$K$9$k(B."
  :type 'boolean
  :group 'skk-act)

(defcustom skk-act-load-hook nil
  "*skk-act $B$r(B load $B$7$?8e$K<B9T$5$l$k(B hook."
  :type 'hook
  :group 'skk-act)

;; skk-azik.el related.
(defcustom skk-use-azik nil
  "*Non-nil $B$G$"$l$P3HD%%m!<%^;zF~NO(B AZIK $B$rMxMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-azik)

(defcustom skk-azik-keyboard-type 'jp106
  "*AZIK $B$G;H$&$H$-$N%-!<%\!<%I$N%?%$%W$r%7%s%\%k$G;XDj$9$k!#(B
o 'jp106    $BF|K\8l(B 106 $B%-!<%\!<%I(B ($B%G%U%)%k%H(B)
o 'jp-pc98  NEC PC-98 $B%-!<%\!<%I(B
o 'us101    $B1Q8l%-!<%\!<%I(B  $B"((B jp106 $B5Z$S(B jp-pc98 $B0J30$N%7%s%\%k(B

nil $B$,;XDj$5$l$?>l9g$O!"%-!<%\!<%I$N%?%$%W$N0c$$$r5[<}$9$k3dEv$F$r9T$$$^$;$s!#(B"
  :type '(radio (const :tag "$BF|K\8l(B 106 $B%-!<%\!<%I(B" jp106)
		(const :tag "NEC PC-98 $B%-!<%\!<%I(B" jp-pc98)
		(const :tag "$B1Q8l%-!<%\!<%I(B" us101)
		(const :tag "$B%-!<%\!<%I0MB8=hM}$rL58z$K$9$k(B" nil))
  :group 'skk-azik)

(defcustom skk-azik-load-hook nil
  "*skk-azik $B$r(B load $B$7$?8e$K<B9T$5$l$k(B hook"
  :type 'hook
  :group 'skk-azik)

;; skk-annotation.el related.
(defcustom skk-show-annotation nil
  "*Non-nil $B$G$"$l$P!"JQ49;~$K%"%N%F!<%7%g%s$rI=<($9$k!#(B
$B$+$J4A;zJQ49$N:]!"<-=q$N8uJd$K4^$^$l$k(B `;' $B0J9_$NJ8;zNs$r%"%N%F!<%7%g%s$H$7$F(B\
$B%(%3!<%(%j%"!"JL(B Window $B$^$?$O%D!<%k%F%#%C%W$KI=<($9$k!#(B"
  :type '(radio (const :tag "$B>o$KI=<((B" t)
		(const :tag "$B8uJd0lMw$G$OHsI=<((B" (not list))
		(const :tag "$B%_%K%P%C%U%!$G$OHsI=<((B" (not minibuf))
		(const :tag "$B8uJd0lMw$H%_%K%P%C%U%!$G$OHsI=<((B"
		       (not list minibuf))
		(const :tag "$BHsI=<((B" nil))
  :group 'skk-basic
  :group 'skk-annotation)

(defcustom skk-annotation-delay 1.0
  "*$B%"%N%F!<%7%g%s$rI=<($9$k$^$G$NCY1d!#C10L$OIC!#(B"
  :type 'number
  :group 'skk-annotation)

(defcustom skk-annotation-loop-interval 0.1
  "*$B%"%N%F!<%7%g%s$rI=<(Cf$N%W%m%;%9BT$A;~4V(B ($BIC(B)$B!#(B
$B9bB.$J4D6-$G$O>.$5$a$K@_Dj$9$k$H%l%9%]%s%9$,2wE,$K$J$k!#(B
$BDcB.$J4D6-$G$OBg$-$a$K@_Dj$9$k$HF0:n$,2~A1$5$l$&$k!#(B"
  :type 'number
  :group 'skk-annotation)

(defcustom skk-annotation-toggle-display-char ?^
  "*$B8uJd0lMw$rI=<(Cf$K%"%N%F!<%7%g%sI=<($r@Z$jBX$($k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-annotation)

(defcustom skk-annotation-copy-key "\C-w"
  "*$B%"%N%F!<%7%g%s$r%3%T!<$9$k%-!<!#(B
$B$3$N%-!<$r%?%$%W$9$k$H!"8=:_I=<(Cf$N%"%N%F!<%7%g%s$r(B kill ring $B$KJ]B8$9$k!#(B
$BJ]B8$7$?FbMF$r(B Emacs $B0J30$N%"%W%j%1!<%7%g%s$GMxMQ$7$?$$>l9g$O(B
$BJQ?t(B `interprogram-cut-function' $B$r@_Dj$9$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-browse-key "\C-o"
  "*$B%"%N%F!<%7%g%s$r(B URL $B$H8+Pv$7$F%V%i%&%:$9$k%-!<!#(B
$B$3$N%-!<$r%?%$%W$9$k$H!"8=:_I=<(Cf$N%"%N%F!<%7%g%s$r4X?t(B `browse-url' $B$KEO$9!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-function nil
  "*$B%"%N%F!<%7%g%s$rI=<($9$k$+H]$+$rH=Dj$9$k$?$a$K%3!<%k$9$k4X?t$r;XDj$9$k!#(B
$B%"%N%F!<%7%g%s$NBP>]$H$9$kJ8;zNs$r0z?t$K$7$F(B `funcall' $B$5$l!"La$jCM(B
$B$,(B non-nil $B$G$"$l$P%"%N%F!<%7%g%s$rI=<($9$k!#(B
$B8uJd0lMw;~$K$O8F$P$l$J$$!#(B

$B%"%N%F!<%7%g%sI=<($NH=CG$O(B `skk-treat-candidate-appearance-function' $B$G$b(B
$B<B8=$G$-$k!#(B"
  :type 'function
  :group 'skk-annotation)

(defcustom skk-annotation-show-as-message t
  "*Non-nil $B$G$"$l$P!"%"%N%F!<%7%g%s$r%(%3!<%(%j%"$KI=<($9$k!#(B
nil $B$G$"$l$P!"JL$J%&%#%s%I%%$KI=<($9$k!#(B
$B$3$NJQ?t$h$j$b(B `skk-show-tooltip' $B$N@_Dj$,M%@h$5$l$k!#(B"
  :type 'boolean
  :group 'skk-annotation)

(defcustom skk-annotation-mode-hook nil
  "*SKK annotation mode $B$KF~$C$?$H$-$N%U%C%/!#(B"
  :type 'hook
  :group 'skk-annotation)

(defcustom skk-annotation-lookup-DictionaryServices nil
  "*Non-nil $B$G$"$l$P!"(BApple OS X $B$G(B DictionaryServices $B$h$j0UL#$r<hF@$9$k!#(B
$B$3$N>l9g!"(Bpython $B$r(B inferior process $B$H$7$F5/F0$9$k!#(B
$B$3$N@_Dj$O(B `skk-annotation-lookup-dict' $B$h$jM%@h$5$l$k!#(B
Max OS X $B0J30$N4D6-$G$O5!G=$7$J$$!#(B

$B8uJd0lMw$G$b$3$N5!G=$r;H$$$?$$>l9g$O(B `always' $B$K@_Dj$9$k$3$H$G<B8=$G$-$k!#(B
$B$?$@$7!"(B`always' $B$O(B `skk-treat-candidate-appearance-function' $B$r>e=q$-$7$F$7(B
$B$^$&$?$a!">e5i<T8~$1$G$O$J$$!#(B"
  :type '(radio (const :tag "$BDL>o$NJQ49;~$K<-=q$r;2>H$9$k(B" t)
		(const :tag "$B>e5-$K2C$(8uJd0lMw$G$b;2>H$9$k(B" always)
		(const :tag "$BMxMQ$7$J$$(B" nil))
  :group 'skk-annotation)

(defcustom skk-annotation-python-program (or (executable-find "python2.6")
					     (executable-find "python"))
  "*DictionaryServices $B$N$?$a$K5/F0$9$k(B python $B$N%U%!%$%kL>!#(B"
  :type '(radio (file)
		(const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-lookup-lookup nil
  "*Non-nil $B$G$"$l$P(B elisp `lookup' $B$+$iJQ498uJd$N0UL#$r<hF@$9$k!#(B

$B8uJd0lMw$G$b$3$N5!G=$r;H$$$?$$>l9g$O(B `always' $B$K@_Dj$9$k$3$H$G<B8=$G$-$k!#(B
$B$?$@$7!"(B`always' $B$O(B `skk-treat-candidate-appearance-function' $B$r>e=q$-$7(B
$B$F$7$^$&$?$a!">e5i<T8~$1$G$O$J$$!#(B"
  :type '(radio (const :tag "$BDL>o$NJQ49;~$K(B lookup $B$r;2>H$9$k(B" t)
		(const :tag "$B>e5-$K2C$(8uJd0lMw$G$b;2>H$9$k(B" always)
		(const :tag "$BMxMQ$7$J$$(B" nil))
  :group 'skk-annotation
  :group 'skk-lookup)

(defcustom skk-annotation-lookup-dict nil
  "*Non-nil $B$G$"$l$P!"30It%W%m%0%i%`$rFI$s$GJQ498uJd$N0UL#$rI=<($9$k!#(B
$B30It%W%m%0%i%`$O(B `skk-annotation-dict-program' $B$G;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-annotation)

(defcustom skk-annotation-dict-program
  (cond ((eq system-type 'darwin)
	 skk-annotation-python-program)
	(t
	 nil))
  "*$BJQ498uJd$N0UL#$rI=<($9$k$?$a$N30It%W%m%0%i%`$N%U%!%$%kL>!#(B"
  :type '(radio (file)
		(const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-dict-program-arguments
  (cond ((eq system-type 'darwin)
	 '("-c" "import sys, DictionaryServices; word = sys.argv[1].decode(\"utf-8\"); print DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word))).encode(\"utf-8\")"))
	(t
	 nil))
  "*$BJQ498uJd$N0UL#$rI=<($9$k$?$a$N30It%W%m%0%i%`$N0z?t$N%j%9%H!#(B"
  :type '(radio (repeat string)
		(const nil))
  :group 'skk-annotation)

(defcustom skk-annotation-dict-coding-system 'utf-8
  "*$B30It%W%m%0%i%`$+$i%"%N%F!<%7%g%s<hF@$9$k:]$KMQ$$$k%3!<%I7O!#(B"
  :type 'coding-system
  :group 'skk-annotation)

(defcustom skk-annotation-other-sources
 (if (eq system-type 'darwin)
     '(lookup.el $B<-=q(B ja.wiktionary ja.wikipedia
		 en.wiktionary simple.wikipedia en.wikipedia)
   '(lookup.el ja.wiktionary ja.wikipedia
	       en.wiktionary simple.wikipedia en.wikipedia))
  "*$B%"%N%F!<%7%g%s$K;H$&>pJs$N%=!<%9$r;XDj$9$k%*%W%7%g%s!#(B
$BI8=`$G$O(B Wiktionary, Wikipedia ($BF|K\8lHG!"1Q8lHG(B) $B$r;2>H$9$k!#(B
Apple OS X $B$G$OI8=`$N!V<-=q!W$rMxMQ$G$-$k!#(B"
  :type '(radio (repeat :tag "\
$B<!$N%=!<%9$rMxMQ$9$k(B ($B0J2<$K9`L\$H=gHV$r;XDj$7$F$/$@$5$$(B)" symbol)
		(const :tag "Wikimedia $B$J$I$N>pJs$rMxMQ$7$J$$(B" nil))
  :group 'skk-annotation)

(make-obsolete-variable 'skk-annotation-wikipedia-sources
			'skk-annotation-other-sources
			"DDSKK 14.4")

(defcustom skk-annotation-wikipedia-key "\C-i"
  "*$B%"%N%F!<%7%g%s$H$7$F(B Wikipedia $B$NFbMF$rI=<($9$k%-!<!#(B
$B%*%W%7%g%s(B `skk-show-annotation' $B$,(B non-nil $B$N$H$-$@$1M-8z!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-annotation)

(defcustom skk-annotation-wiktionary-preferred-lang-alist
  '(("en" "Translingual" "English" "Japanese")
    ("ja" "$BF|K\8l(B" "$B4A;z(B" "$B1Q8l(B" "$B8EE5F|K\8l(B"))
  "*Wiktionary $B$N5-=R8@8l$H!"C18l=jB08@8l$NM%@h=g$H$NO"A[%j%9%H!#(B"
  :type '(repeat (repeat string))
  :group 'skk-annotation)

(defconst skk-annotation-buffer "*SKK annotation*")

(defvar skk-annotation-first-candidate nil)

(defvar skk-annotation-mode-map nil
  "*SKK annotation $B%b!<%I$N%-!<%^%C%W!#(B")

(defvar skk-annotation-original-window-configuration nil
  "SKK annotation mode $B$KF~$kA0$N(B window configuration$B!#(B
`skk-annotation-save-and-quit' $B$r8F$V$H$3$N(B window configuration
$B$r;H$C$F(B SKK annotation mode $B$KF~$kA0$N(B window $B>uBV$KLa$9!#(B")

(defvar skk-annotation-target-data nil
  "annotation $B$rIU$1$i$l$k8uJd$K4X$9$k%G!<%?!#(B")

(defvar skk-annotation-wikipedia-message nil
  "SKK Wikipedia $BMxMQJ}K!$r<($9%a%C%;!<%8(B ($B<+F0@_Dj(B)$B!#(B")

(defvar skkannot-cached-srcs nil)

(defvar skk-annotation-message nil
  "SKK Annotation $BMxMQJ}K!$r<($9%a%C%;!<%8(B ($B<+F0@_Dj(B)$B!#(B")

(defvar skkannot-remaining-delay 0)

(defvar skkannot-buffer-origin nil)

(defvar skkannot-py-buffer nil)

(defvar skkannot-url-installed-p
  (if (and (featurep 'emacs)
	   (>= emacs-major-version 22))
      t
    'untested))

(defconst skkannot-py-none-regexp "^\\(Traceback\\|AttributeError\\|None\\)")

(defconst skkannot-DictServ-cmd-format-str "word = u\"%s\"; \
print \" %s(word)s in DictionaryServices\" %s {'word': word}; \
print DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word)))")

;; XXX $B$^$@IT40A4(B
(defconst skkannot-en-wiktionary-lang-regexp "\
<h2>.*<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(Aari\\|Abanyom\\|Abaza\\|Abenaki\\|Abkhaz\\|Acehnese\\|Acholi\\|Acholi\
\\|Achumawa\\|Adangme\\|Adele\\|Adnyamathanha\\|Adyghe\\|Adzera\\|Afar\
\\|Afrikaans\\|Aghul\\|Ainu\\|Akan\\|Akawaio\\|Akkadian\\|Aklanon\\|Alabama\
\\|Albanian\\|Aleut\\|Algonquin\\|Alsatian\\|Amaimon\\|Amanab\\|Ambai\
\\|Amharic\\|Amoy\\|Amuzgo\\|Ankave\\|Ansus\\|Apala.ANm\\|\\(Egyptian \\)?Arabic\
\\|Aragonese\\|Aramaic\\|Arapaho\\|Arawak\\|Armenian\\|Aromanian\\|Assamese\
\\|Asturian\\|'Auhelawa\\|Avar\\|Avestan\\|Awabakal\\|Aymara\\|Azeri\
\\|Balinese\\|Balti\\|Bambara\\|Bandjalang\\|Baruga\\|Bashkir\\|Basque\
\\|Baure\
\\|Belarusian\\|Bengali\\|Berbice Creole Dutch\\|Betawi\\|Bhojpuri\\|Biak\
\\|Bikol\\|Bislama\\|Blackfoot\\|BokmNel\\|Bosnian\\|Breton\
\\|Broome Pearling Lugger Pidgin\\|Bube\\|Bulgarian\\|Burmese\
\\|Cantonese\\|Capeverdean Crioulo\\|Catalan\\|Catawba\\|Cebuano\
\\|Central Tarahumara\\|Ch'orti'\\|Chamorro\\|Chechen\\|Cherokee\\|Cheyenne\
\\|Chichewa\\|Chickasaw\\|Chinese Pidgin English\\|Chinese\\|Chinook Jargon\
\\|Chiricahua\\|Choctaw\\|TumbalNa Chol\\|Chukchee\\|Chuvash\
\\|Classical Nahuatl\\|CoatlNan Mixe\\|Comorian\\|Coptic\\|Cornish\\|Corsican\
\\|Cree\\|Creek\\|Crimean Tatar\\|Croatian\\|Czech\
\\|Dacian\\|Dadibi\\|Northern Dagara\\\Dalmatian\\|Danish\\|Dargwa\
\\|Darkinjung\\|Darling\\|Dharuk\\|Dhivehi\\|Dhuwal\\|Dieri\\|Dusner\\|Dutch\
\\|Dyirbal\\|Dzongkha\
\\|Egyptian\\|English\\|Erzya\\|Esan\\|Esperanto\\|Estonian\\|Etruscan\\|Ewe\
\\|Fang\\|Faroese\\|Fijian\\|Filipino\\|Finnish\\|Fon\\|French\\|Frisian\
\\|Friulian\\|Fula\
\\|Ga\\|Gabi-Gabi\\|Gagauz\\|Galician\\|Gallo\\|Gamilaraay\\|Ge'ez\\|Georgian\
\\|\\(Middle High\\)?German\\|Gilbertese\\|Golin\\|Gooniyandi\\|Gothic\
\\|\\(Ancient \\|Mycenaean \\)?Greek\\|Greenlandic\\|GuaranNm\\|MbyNa GuaranNm\
\\|Gujarati\\|Guugu Yimidhirr\
\\|Hausa\\|Hawaiian\\|Hebrew\\|Hindi\\|Hittite\\|Hmong\\|Hopi\\|Hungarian\
\\|Icelandic\\|Ido\\|Igbo\\|Ilocano\\|Indoneian\\|Interlingua\\|Inuktitut\
\\|Irish\\|Italian\
\\|Japanese\\|Javanese\\|Jingpho\\|JNhrriais\
\\|Kabardian\\|Kabyle\\|KadiwNiu\\|Kannada\\|Kanuri\\|Kapingamarangi\\|Karelian\
\\|KaritiNbna\\|Kashmiri\\|Kashubian\\|Kaurna\\|Kazakh\\|Khmer\\|Kickapoo\
\\|Kinyarwanda\\|Kiput\\|Kirundi\\|Kokborok\\|Komi\\|Kongo\\|Korean\\|Kriol\
\\|Krisa\\|!Kung\\|Kurdish\\|Kurnai\\|Kwanyama\\|Kyrgyz\
\\|Ladino\\|Lak\\|Lakota\\|Laotian\\|Latin\\|Latvian\\|Lavukaleve\\|Lenape\
\\|Lezgi\\|Limburgish\\|Lingala\\|Lithuanian\\|Livonian\\|Lojban\
\\|Low Saxon\\|Lower Sorbian\\|Luganda\\|Luxembourgish\
\\|Maay\\|Macedonian\\|Makhuwa\\(-Meetto\\|-Shirima\\)?\\|Malagasy\\|Malay\
\\|Malayalam\\|Maliseet\\|Maltese\\|Manchu\\|Mandarin\\|Mandinka\\|Mangarevan\
\\|Mansi\\|Manx\\|Maori\\|Mapudungun\\|Marathi\\|Marau\\|Maroon\\|Marshallese\
\\|Martuthunira\\|Mati Ke\\|Mbabaram\\|Mende\\|Menominee\\|Meriam\\|Mesquakie\
\\|Mi'kmaq\\|Miami\
\\|Middle \\(Dutch\\|English\\|French\\|Korean\\|Norwegian\\|Scots\\)\
\\|Min Nan\\|Mirandese\\|Miskito\\|\\(Alcozauca \\|YosondNza \\)?Mixtec\
\\|Miyako\\|Mohegan\\|Mohican\\|Moldavian\\|Mongolian\\|Montauk\\|Munduapa\
\\|Munggui\\|Munsee\\|Murrinh-Patha\\|Mutsun\
\\|\\(Isthmus-Mecayapan \\)?Nahuatl\\|Nanticoke\\|Narragansett\\|Nauruan\
\\|Navajo\\|Ndonga\\|Neapolitan\\|Nepali\\|Nhanta\\|Niuean\\|Nootka\\|Norfuk\
\\|Norman\\|Norn\\|Northern Sami\\|Norwegian\\|Novial\\|Nynorsk\\|Nyunga\
\\|O'odham\\|Occitan\\|Ohlone\\|Ojibwe\
\\|Old \\(Church Slavonic\\|English\\|French\\|Frisian\\|High German\\|Irish\
\\|Norse\\|Prussian\\|Saxon\\|Slavonic\\)\\|Oriya\\|Oromo\
\\|Pali\\|Pangasinan\\|Panyjima\\|Papiamentu\\|Papuma\\|Pashto\
\\|Passamaquoddy\\|PaumarNm\\|Pennsylvania German\\|Penobscot\
\\|\\(Old \\)?Perian\\|Phoenician\
\\|PirahNc\\|Pitcairnese\\|Pitjantjatjara\\|Pitta-Pitta\\|Pochutec\\|Polish\
\\|Sayula Popoluca\\|Portuguese\\|Potawatomi\\|Powhatan\
\\|Proto-\\(Germanic\\|Indo-European\\|Uralic\\)\\|ProvenNga\\|Punjabi\
\\|Quechua\\|Quenya\
\\|Rarotongan\\|Reconstructed\\|Rohingya\\|Roman\\(i\\|ian\\|sch\\)\\|Rotokas\
\\|Rotuman\\|Russian\\|Rutul\
\\|Saanich\
\\|\\(Inari \\|Kildin \\|Lule \\|Northern \\|Pite \\|Skolt \\|Sourthern \
\\|Ter \\|Ume \\)?Sami\\|Samoan\\( Plantation Pidgin\\)?\\|Sanskrit\
\\|Sardinian\\|Scots\\|Scottish Gaelic\\|Serbian\\|Serbo-Croatian\\|Seri\
\\|Shabo\\|Shawnee\\|Shelta\\|Shona\\|Shoshoni\\|Shuar\\|Sicilian\\|Sindarin\
\\|Sindhi\\|Sinhalese\\|Slovak\\|Slovene\\|Somali\\|Upper Sorbian\\|Spanish\
\\|Sranan\\|Sumerian\\|Swahili\\|Swazi\\|Swedish\\|Syriac\
\\|Tabassaran\\|TAchelhit\\|Tagalog\\|Tahitian\\|Taimyr Pidgin Russian\\|Tajik\
\\|Tamasheq\\|Tamazight\\|Tamil\\|Tatar\\|Tausug\\|TaNmno\\|Telugu\\|Tetum\
\\|Thai\\|Tibetan\\|Tigrinya\\|Tiwi\\|Tocharian \\(A\\|B\\)\\|Tok Pisin\
\\|Tokelauan\\|Tongan\\|Torres Strait Creole\\|Translingual\\|Tsakhur\
\\|Tshiluba\\|Tswana\\|Tuamotuan\\|Tumbuka\\|Tupi\\|TupinambNa\\|Turkish\
\\|Turkmen\\|Tuvaluan\\|Tuvan\\|Twi\\|Tz'utujil\
\\|Ugaritic\\|Ukrainian\\|Umbundu\\|Unami\\|Unserdeutsch\\|Urdu\\|Uyghur\
\\|Uzbek\
\\|Vandalic\\|Venda\\|Veps\\|Vietnamese\\|VolapN|k\\|Votic\\|VNuro\
\\|Wageman\\|Walloon\\|Wampanoag\\|Wangaaybuwan-Ngiyambaa\\|Warlpiri\\|Welsh\
\\|Wembawemba\\|Western Apache\\|West Frisian\\|Wik-Mungkan\\|Wiradhuri\
\\|Woi\\|Woiwurrung\\|Wolof\\|Worimi\
\\|XavNante\\|Xhosa\\|!XNsNu\
\\|Yapese\\|Yiddish\\|Yidiny\\|Yindjibarndi\\|Yoruba\\|Yucatec\\|Yup'ik\
\\|\\(Yatzachi \\|Zoogocho \\|Isthmus \\)Zapotec\\|Zenga\\|Zhuang\
\\|Zulgo-Gemzek\\|Zulu\\|Zuni\\)\
\\(</a>\\)?\
</span></h2>"
  "en.wiktionary $B$K$*$$$F8@8l$rI=$9%X%C%@$N@55,I=8=(B")

(defconst skkannot-en-wiktionary-part-of-speech-regexp "\
<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(Article\\|Noun\\|Proper Noun\\|Adjective\\|Proper Adjective\
\\|Verb\\( form\\)?\\|Intransitive\\( verb\\)?\\|Transitive\\( verb\\)?\
\\|Adverb\
\\|Conjunction\\|Interjection\\|Numeral\\|Prefix\\|Suffix\\|Particle\
\\|Preposition\\|Contraction\\|Determiner\\|Demonstrative determiner\
\\|Interrogative determiner\\|Pronoun\\|Pronominal possessive adjective\
\\|Demonstrative pronoun\\|Demonstrative adjective\
\\|Quasi-Adjective\\|Proverb\\|Counter\\|Personal pronoun\
\\|Kanji\\|Hanja\\|Hanzi\
\\|Interrogative pronoun\\|Relative pronoun\\|Auxiliary verb\\( form\\)?\
\\|Indefinite article\\|Abbreviation\\|Initialism\\|Acronym\\|Symbol\
\\|\\(Han \\|Hiragana \\|Katakana \\)character\\|Phrase\\|Letter\\)\
\\(</a>\\)?\
</span>"
    "en.wiktionary $B$K$*$$$FIJ;l$rI=$9%X%C%@$N@55,I=8=(B")

(defconst skkannot-ja-wiktionary-lang-regexp "\
<h2>.*<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(.+$B8l(B\\|$B%$%s%?!<%j%s%0%"(B\\|$B%(%9%Z%i%s%H(B\\|$B%5%s%9%/%j%C%H(B\\|$B%H%-%]%J(B\
\\|$B%H%/!&%T%8%s(B\\|$B5-9f(B\\|$B4A;z(B\\)\
\\(</a>\\)?\
</span>"
  "ja.wiktionary $B$K$*$$$F8@8l$rI=$9%X%C%@$N@55,I=8=(B")

(defconst skkannot-ja-wiktionary-part-of-speech-regexp "\
<span class=\"mw-headline\".+>\
\\(<a href=.+>\\)?\
\\(\
\\(\\($B8GM-(B\\|\\($B?M>N(B\\|$B5?Ld(B\\)?$BBe(B\\)?$BL>(B\\|\\($B=u(B\\)?$BF0(B\\|$B7AMFF0(B?\\|\
$B@\B3(B\\|$BA0CV(B\\|$BI{(B\\|$B4'(B\\|$B4X78(B\\|$B4VEj(B\\|$B=u(B\\|$B?t(B\\|$BJ,(B\\|$BN`JL(B\\|$B46F0(B\\)\
$B;l(B.*\
\\|$B4A;z:.$8$jI=5-(B\\|$B0U5A(B\\|$B<ZMQ8l(B\\|$BN,8l(B\\|$B%3%T%e%i(B\\|$B@\F,<-(B\\|$B@\Hx<-(B\
\\|$B?M>N@\<-(B\\|$BJ?2>L>(B\\|$BJR2>L>(B\\|$B0U5A(B\\|$B4A;z(B\\|$BOB8l$N4A;zI=5-(B\\)\
\\(</a>\\)?\
</span>"
    "ja.wiktionary $B$K$*$$$FIJ;l$rI=$9%X%C%@$N@55,I=8=(B")

(skk-deflocalvar skk-annotation-mode nil
  "Non-nil $B$G$"$l$P!"(Bannotation $B%b!<%I$G$"$k$3$H$r<($9!#(B")

;;; skk-auto.el related.
(defcustom skk-auto-okuri-process nil
  "*Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$r<+F0G'<1$7$FJQ49$r9T$&!#(B
$BNc$($P!"(B

    \"Uresii (\"UreSii\" $B$G$O$J$/(B) -> $B4r$7$$(B\"

$B$N$h$&$KJQ49$5$l$k!#C"$7!"(B`skk-jisyo' ($B8D?M<-=q(B) $B$,!"(B

    \"$B$&$l(Bs /$B4r(B/[$B$7(B/$B4r(B/]/\"

$B$N$h$&$J7A<0$K$J$C$F$$$k$3$H$,I,MW$G$"$k(B (SKK-JISYO.[SML] $B$O$3$N7A<0$KBP1~$7(B
$B$F$$$J$$$N$G!"(B`skk-jisyo' $B$K$3$N%(%s%H%j$,$J$1$l$P$J$i$J$$(B)$B!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-okurigana
  :group 'skk-auto)

(defcustom skk-okuri-search-function 'skk-okuri-search-subr-original
  "*`skk-okuri-search' $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-auto)

(defcustom skk-auto-load-hook nil
  "*skk-auto.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-auto)

;; skk-cdb.el related.
(defcustom skk-cdb-large-jisyo nil
  "*$B8D?M<-=q$N8!:w$N8e$K8!:w$9$k(B CDB $B7A<0<-=q%U%!%$%kL>!#(B
Non-nil $B$G$"$l$P!";XDj$5$l$?(B CDB $B7A<0<-=q$r(B Emacs $B$+$iD>@\MxMQ$7!"(B
$B9bB.$J8!:w$r9T$&!#(B"
  :type `(radio (file :tag "$B<-=q%U%!%$%kL>(B"
		      ,(cond
			((featurep 'xemacs)
			 (or (locate-data-file "SKK-JISYO.L.cdb")
			     ""))
			((fboundp 'locate-file)
			 (or (locate-file "skk/SKK-JISYO.L.cdb"
					  (list
					   (expand-file-name "../../.."
							     data-directory)))
			     (locate-file "skk/SKK-JISYO.L.cdb"
					  (list data-directory))
			     ""))))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-cdb
  :group 'skk-dictionary)

(defcustom skk-cdb-coding-system 'euc-jp
  "*$B8D?M<-=q$N8!:w$N8e$K8!:w$9$k(B CDB $B7A<0<-=q$N%3!<%G%#%s%0!&%7%9%F%`!#(B"
  :type 'coding-system
  :group 'skk-cdb
  :group 'skk-dictionary)

;;; skk-comp.el related.
(defcustom skk-try-completion-char ?\011 ; TAB
  "*$B8+=P$78l$NJd40F0:n$r9T$&%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-comp)

(defcustom skk-next-completion-char ?.
  "*$B8+=P$78l$NJd40F0:n$G!"<!$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-comp)

(defcustom skk-previous-completion-char ?,
  "*$B8+=P$78l$NJd40F0:n$G!"A0$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-comp)

(defcustom skk-previous-completion-use-backtab t
  "*$B8+=P$78l$NJd40F0:n!JA08uJd$N=PNO!K$r(B Shift + TAB $B$G$b9T$&!#(B"
  :type 'boolean
  :group 'skk-comp)

(defcustom skk-previous-completion-backtab-key
  (cond ((not (skk-find-window-system))
	 [backtab])
	((featurep 'xemacs)
	 [iso-left-tab])
	((memq system-type '(darwin windows-nt))
	 [S-tab])
	(t
	 ;; X Window System
	 [S-iso-lefttab]))
  "*Shift + TAB $B$KAjEv$9$k%-!<(B (key event)$B!#(B
`skk-previous-completion-use-backtab' $B$,M-8z$J:]$KMQ$$$i$l$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-comp)

(defcustom skk-start-henkan-with-completion-char ?\240 ; M-SPC
  "*$B8+=P$78l$rJd40$7$J$,$i"'%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-comp)

(defcustom skk-comp-load-hook nil
  "*skk-comp.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-comp)

(defcustom skk-kakutei-history-limit 100
  "$BJQ?t(B `skk-kakutei-history' $B$NCM(B ($BO"A[%j%9%H(B) $B$ND9$5$N>e8B!#(B"
  :type 'integer
  :group 'skk-comp)

(defcustom skk-comp-circulate nil
  "*$B8+=P$78l$rJd40$9$k:]$N!"8uJd$NI=<(=g$r@)8f$9$k!#(Bnon-nil $B$G$"$l$P!"(B
$B:G8e$N8+=P$78l$,I=<($5$l$?>uBV$G99$KJd40$r9T$&$HF~NOJ8;zNs$KLa$k!#(B
nil $B$G$"$l$P!":G8e$N8+=P$78l$,I=<($5$l$?>uBV$GDd;_$9$k!#(B"
  :type 'boolean
  :group 'skk-comp)

(defcustom skk-comp-use-prefix nil
  "*$B8+=P$78l$rJd40$9$k:]$K%W%l%U%#%C%/%9(B(`skk-prefix')$B$b;H$&$+!#(B
$BNc$($P!"(B\"$B"&$"(Bk\" $B$H$"$k>uBV$GJd40$r$7$?;~$K!"(Bnon-nil $B$G$"$l$P(B
\"$B$"$5(B\" $B$OBP>]$H$J$i$:!"(B\"$B$"$+(B\", \"$B$"$-(B\" $B$J$I$K9J$i$l$k!#(B
$B$7$+$7<B:]$K$OJd40%W%m%0%i%`$bBP1~$7$F$$$kI,MW$,$"$k!#(B

kakutei-first $B$rA*$s$@;~$O!"(B\"$B$7$s$j(Bn\" $B$rJd40$9$k$H!"(B
\"n\" $B$OA0$b$C$F(B \"$B$s(B\" $B$K3NDj$5$l$F$+$iJd408uJd$r8!:w$9$k$N$G!"(B
\"$B$7$s$j$s(B\" $B<+BN$O8uJd$H$7$FDs<($5$l$J$$;v$KCm0U!#(B"
  :type '(radio (const nil)
		(const t)
		(const kakutei-first))
  :group 'skk-comp)

(defcustom skk-comp-prefix-regexp-alist nil
  "*$B%W%l%U%#%C%/%9$rMxMQ$7$?Jd40;~$K;H$&!"%W%l%U%#%C%/%9$H@55,I=8=$NO"A[%j%9%H!#(B
$B$3$NJQ?t$O(B `skk-rule-tree' $B$rMxMQ$7$F<+F0$GMWAG$,DI2C$5$l$k$,!"(B
$B$=$l$,4|BT$9$k$b$N$G$J$$>l9g$K$OM=$aI,MW$J$b$N$@$1@_Dj$7$F$*$/$3$H!#(B"
  :type '(repeat (cons string regexp))
  :group 'skk-comp)

(defcustom skk-comp-kana-list-filter-function
  (lambda (kana-list prefix)
    ;; "t" $B0J30$G(B "$B$C(B" $B$rJd40$7$J$$(B
    (unless (string= prefix "t")
      (setq kana-list (delete "$B$C(B" kana-list)))
    ;; "m" $B$G(B "$B$s(B" $B$rJd40$7$J$$(B
    (when (string= prefix "m")
      (setq kana-list (delete "$B$s(B" kana-list)))
    ;; "w" $B$G(B "$B$&(B" $B$rJd40$7$J$$(B
    (when (string= prefix "w")
      (setq kana-list (delete "$B$&(B" kana-list)))
    ;; "x" $B$G(B "$B$+(B", "$B$1(B" $B$rJd40$7$J$$(B
    ;; in skk-rom-kana-base-rule-list, "xka"$B"*(B"$B$+(B", "xke"$B"*(B"$B$1(B"
    (when (string= prefix "x")
      (setq kana-list (delete "$B$+(B" kana-list))
      (setq kana-list (delete "$B$1(B" kana-list)))
    ;; $B$$$A$*$&%+%J%b!<%I$r9M$($F(B
    (when (string= prefix "v")
      (add-to-list 'kana-list "$B%t(B"))
    ;; $BJ?2>L>!&JR2>L>$N$_(B ($B5-9fN`$OITMW(B)
    (save-match-data
      (delq nil
	    (mapcar (lambda (kana)
		      (when (string-match "\\(\\cH\\|\\cK\\)" kana)
			kana))
		    kana-list))))
  "*`skk-comp-prefix-regexp-alist' $B$K<+F0$GMWAG$rDI2C$9$k:]$KMxMQ$5$l$k4X?t!#(B
`skk-rule-tree' $B$+$i%W%l%U%#%C%/%9$KBP1~$9$k(B \"$B$+$J(B\" $B$r=8$a$?8e!"(B
$B$3$N4X?t$K$h$C$FD4@0$r9T$&!#(B"
  :type '(radio (function :tag "$B4X?t(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-comp)

(defcustom skk-completion-prog-list
  '((skk-comp-by-history)
    (skk-comp-from-jisyo skk-jisyo)
    (skk-look-completion))
  "*$BJd404X?t!"Jd40BP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-comp-first' $B$,(B t $B$G$"$k;~$K(B
$B?75,Jd408uJd72$N@8@.$r3+;O$7!"#12s$NI>2A$K$D$-#1$D$N8uJd$rJV$9(B S $B<0!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-1 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-1 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-2 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-2 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-3 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-3 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-4 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-4 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-5 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-5 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-6 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-6 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-7 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-7 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-8 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-8 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-9 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-9 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-prog-list-0 nil
  "*$BJd40%W%m%0%i%`$N%j%9%H!#(B
$B%j%9%H$NMWAG$O!"(B`skk-completion-prog-list' $B$HA4$/F1MM!#(B
C-0 TAB $B$G;H$o$l$k!#(B"
  :type '(repeat (sexp))
  :group 'skk-comp)

(defcustom skk-completion-search-char ?~
  "*`skk-completion-search' $B$rMxMQ$9$kJQ49$r;XDj$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-comp)

(defcustom skk-smart-find-file-path load-path
  "*`smart-find' $B$K%U%!%$%kL>$rC5:w$5$;$k%Q%9!#(B
$B$3$NCM$,;XDj$5$l$J$1$l$P!"(B`smart-find-file-path' $B$NCM$,Be$o$j$K;HMQ$5$l$k!#(B"
  :type '(repeat (directory))
  :group 'skk-comp)

(defcustom skk-smart-find-ignored-file-regexp "\
\\(\\.\\(elc\\|o\\(rig\\|ld\\)?\\|diff\\)\\|,v\\|~\\|/\\)$"
  "*`smart-find' $B$KL5;k$5$l$k%U%!%$%kL>$r;XDj$9$k@55,I=8=!#(B"
  :type 'regexp
  :group 'skk-comp)

;; ---- buffer local variables
;; $B6uJ8;zNs$KBP$7$F(B skk-comp-do $B$r8F$V$3$H$b$"$j$&$k$N$G!"(B"" $B$r(B nil $B$G$OBe(B
;; $BMQ$G$-$J$$!#(B
(skk-deflocalvar skk-comp-key ""
  "$BJd40$9$Y$-8+=P$78l!#(B")
;; $B<-=qEPO?;~%_%K%P%C%U%!$GJd40$7$?>l9g!"85$N%P%C%U%!$KLa$C$?$H$-$K(B
;; skk-comp-key $B$NCM$,GK2u$5$l$F$$$J$$J}$,%Y%?!<!#(B

(skk-deflocalvar skk-comp-prefix ""
  "$BJd40;~$N(B `skk-prefix'")

;; buffer local $B$JI,MW$OL5$$$+$b(B?
(skk-deflocalvar skk-current-completion-prog-list nil
  "`skk-completion-prog-list' $B$N8=:_$NCM$rJ]B8$9$k%j%9%H!#(B
$B:G=i$NJd40;~$O(B `skk-completion-prog-list' $B$NA4$F$NCM$rJ];}$7!"(B
car $B$K$"$kJd40%W%m%0%i%`$,(B nil $B$rJV$9$4$H$K(B 1$B$D$:$DC;$/$J$C$F$f$/!#(B")

(skk-deflocalvar skk-comp-first nil
  "$BJd40%W%m%0%i%`$K?7$7$$8uJd72$r@8@.$9$k$h$&DLCN$9$k!#(B")

(skk-deflocalvar skk-comp-stack nil
  "$BJd40$7$?8l$rJ]B8$7$F$*$/%9%?%C%/!#(B")

(skk-deflocalvar skk-comp-depth 0
  "$BJd40$7$?8l$r(B `skk-comp-stack' $B$+$i<h$j=P$90LCV!#(B")

(skk-deflocalvar skk-comp-kakutei-midasi-list nil
  "$B3NDjMzNr$+$iF@$i$l$?8+=P$78l$N%j%9%H!#(B")

(skk-deflocalvar skk-comp-search-done nil
  "$B8+=P$78l$NJd40MQ$N8uJd8!:w$,=*N;$7$?$3$H$r<($9!#(B")

(defvar skk-comp-smart-find-files nil
  "`smart-find' $B$,JV$7$?%U%!%$%kL>%j%9%H$r3JG<$9$k!#(B")

(defvar skk-comp-lisp-symbols nil
  "$BJd40$5$l$?(B lisp symbol $B$N%j%9%H$r3JG<$9$k!#(B")

;;; skk-server-completion.el related.
(defcustom skk-server-completion-search-char ?~
  "*server completion $B$rMxMQ$7$?JQ49$r9T$&%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-server-completion)

(defvar skk-server-completion-words nil
  "server completion $B$K$h$jF@$i$l$?8+=P$78l$N%j%9%H!#(B")

(defvar skk-server-disable-completion nil
  "Non-nil $B$J$i(B server completion $B$N5!G=$rL58z$K$9$k!#(B
server completion $B$,<BAu$5$l$F$*$i$:!"$+$DL5H?1~$J<-=q%5!<%PBP:v!#(B")

;;; skk-cursor.el related.
(defcustom skk-use-color-cursor (and (skk-find-window-system)
				     (fboundp 'x-display-color-p)
				     (x-display-color-p))
  "*Non-nil $B$G$"$l$P!"%+!<%=%k$KF~NO%b!<%I$K1~$8$??'$rIU$1$k!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-cursor)

(defcustom skk-cursor-default-color
  (cond
   ((featurep 'xemacs)
    (frame-property (selected-frame) 'cursor-color))
   (t
    (cdr (assq 'cursor-color (frame-parameters (selected-frame))))))
  "*SKK $B%b!<%I$N%*%U$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-hiragana-color (if (eq skk-background-mode 'light)
					 "coral4"
				       "pink")
  "*$B$+$J%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-katakana-color (if (eq skk-background-mode 'light)
					 "forestgreen"
				       "green")
  "*$B%+%J%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0201-color (if (eq skk-background-mode 'light)
					 "blueviolet"
				       "thistle")
  "*JISX0201 $B%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0208-latin-color "gold"
  "*$BA41Q%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-latin-color (if (eq skk-background-mode 'light)
				      "ivory4"
				    "gray")
  "*$B%"%9%-!<%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

(defcustom skk-cursor-abbrev-color "royalblue"
  "*skk abbrev $B%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-cursor)

;;; skk-dcomp.el related.
(defface skk-dcomp-face
  '((((class color) (type tty))
     (:foreground "DarkKhaki"))
    (((class color) (background light))
     (:foreground "DimGray" :italic t))
    (((class color) (background dark))
     (:foreground "LightGray" :italic t))
    (((class grayscale))
     (:inherit default)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-face
  '((((class color) (type tty))
     (:foreground "blue" :background "yellow"))
    (((class color) (background light))
     (:foreground "dim gray" :background "beige"))
    (((class color) (background  dark))
     (:foreground "gainsboro" :background "gray15"))
    (((class grayscale))
      (:inherit default)))
  "*$BF0E*Jd40$NJ#?tI=<(72$N%U%'%$%9!#(B"
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-trailing-face
  '((((class color) (type tty))
     (:inherit skk-dcomp-multiple-face :foreground "black" :bold t))
    (((class color) (background light))
     (:inherit skk-dcomp-multiple-face :foreground "black" :bold t))
    (((class color) (background  dark))
     (:inherit skk-dcomp-multiple-face :foreground "white" :bold t))
    (((class grayscale))
     (:inherit default)))
  "*$BF0E*Jd40$NJ#?tI=<(72$NJd40ItJ,$N%U%'%$%9!#(B"
  :group 'skk-dcomp)

(defface skk-dcomp-multiple-selected-face
  '((((class color) (type tty))
     (:foreground "white" :background "magenta" :bold t))
    (((class color) (background light))
     (:foreground "yellow" :background "navy" :bold t))
    (((class color) (background  dark))
     (:foreground "dark slate blue" :background "peach puff" :bold t))
    (((class grayscale))
     (:inherit default)))
  "*$BF0E*Jd40$NJ#?tI=<(72$NA*BrBP>]$N%U%'%$%9!#(B"
  :group 'skk-dcomp)

(defcustom skk-dcomp-activate nil
  "*Non-nil $B$G$"$l$P8+=P$78l$NF0E*Jd40$N5!G=$rM-8z$K$9$k!#(B
$B$3$NJQ?t$NCM$,(B `eolp' $B$@$C$?>l9g!"%]%$%s%H$,9TKv$K$"$k;~$@$1Jd40$9$k!#(B"
  :type '(radio (const :tag "always on" t)
		(const :tag "only at the end of a line" eolp)
		(const :tag "off" nil)
		(sexp :tag "$BG$0U$N%k!<%k(B"))
  :group 'skk-dcomp)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-keep-completion-keys nil
  ;;   (delq
  ;;    nil
  ;;    (list
  ;;     (car (rassoc (list nil 'skk-toggle-kana)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-kana)
  ;;                  skk-rom-kana-base-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-base-rule-list))))
  "*$BF0E*Jd40$5$l$?8+=P$78l$r>C$5$J$$%-!<$N%j%9%H!#(B
$BDL>o$O8+=P$78l$NJd408e!"<!$N%-!<F~NO$r$9$k$H!"F0E*(B
$BJd40$5$l$?%-!<F~NO$,>C$($F$7$^$&$,!"$3$N%j%9%H$K;XDj$5$l$?%-!<(B
$BF~NO$,$"$C$?$H$-$OF0E*Jd40$5$l$?8+=P$78l$r>C$5$J$$!#(B"
  :type '(radio (repeat :tag "$B%j%9%H(B"
			(string :tag "$B%-!<(B($BJ8;z(B)"))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-activate nil
  "*Non-nil $B$G$"$l$P!"F0E*Jd40$N8uJd$rJ#?tI=<($9$k!#(B
$B4X?t$G$"$l$P!"$=$NI>2A7k2L$,(B non-nil $B$N;~$@$1F0E*Jd40$N8uJd$rJ#?tI=<($9$k!#(B
$B$3$N5!G=$O(B XEmacs $B$G$OF0:n$7$J$$!#(B"
  :type '(radio (const :tag "always on" t)
		(const :tag "off" nil)
		(sexp :tag "$BG$0U$N%k!<%k(B"))
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-rows 7
  "*$BF0E*Jd40$N8uJd$rJ#?tI=<($9$k>l9g$NI=<(?t!#(B"
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-multiple-keep-point-buffer-list
  (list (concat " *" (file-name-nondirectory skk-jisyo) "*"))
  "*$BJ#?tI=<($N0Y$KJd408uJd$r8!:w$9$k:]$K(B `point' $B$rJ];}$9$k%P%C%U%!$N%j%9%H!#(B

$BF0E*Jd40$G8uJd$rJ#?tI=<($9$k:]$K8!:wBP>]%P%C%U%!(B
$BFb$N(B `point' $B$rF0$+$7$F$7$^$&$HDL>o$NJd40$,@5>o$K5!G=$7$J$/$J$k!#(B
$B$=$N$?$a!"J#?tI=<(MQ$N8!:w$,=*$o$C$?8e$G(B `point' $B$rLa$9$Y$-%P%C%U%!(B
$B$r$3$N%j%9%H$K@_Dj$9$k!#(B

$B6qBNE*$K$O(B `skk-comp-from-jisyo' $B$r;HMQ$7$F8uJd$r8!:w$9$k>l9g!"$=(B
$B$NBP>]%P%C%U%!$O$3$N%j%9%H$K@_Dj$9$kI,MW$,$"$k!#(B"
  :type '(repeat string)
  :group 'skk-dcomp)

(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
(skk-deflocalvar skk-dcomp-multiple-candidates nil)
(skk-deflocalvar skk-dcomp-multiple-key "")
(skk-deflocalvar skk-dcomp-multiple-prefix "")
(skk-deflocalvar skk-dcomp-multiple-search-done nil)
(skk-deflocalvar skk-dcomp-multiple-select-index -1)
(skk-deflocalvar skk-dcomp-multiple-overlays nil)
(defvar skk-dcomp-face 'skk-dcomp-face)

;;; skk-gadget.el related.
(defcustom skk-gengo-alist
  '((heisei "$BJ?@.(B" "H") (showa "$B><OB(B" "S") (taisho "$BBg@5(B" "T")
    (meiji "$BL@<#(B" "M"))
  "*$B859f$rI=5-$7$?J8;zNs$N(B alist$B!#(B
car $B$O859f$r%m!<%^;zI=5-$7$?(B symbol$B!#(B
cdr $B$O859fI=5-$N(B string $B$+$i@.$k%j%9%H!#(B"
  :type '(repeat (list (symbol :tag "roman")
		       (string :tag "$BF|K\8l(B")
		       (string :tag "Initial")))
  :group 'skk-gadget)

(defcustom skk-month-alist
  '(("Jan" "1" "Januar") ("Feb" "2" "Februar") ("Mar" "3" "MNdrz")
    ("Apr" "4" "April") ("May" "5" "Mai")
    ("Jun" "6" "Juni") ("Jul" "7" "Juli") ("Aug" "8" "August")
    ("Sep" "9" "September") ("Oct" "10" "Oktober")
    ("Nov" "11" "November") ("Dec" "12" "Dezember"))
  "*$B7nL>$N1Q8lI=5-$H$=$NB>$NI=5-K!$NO"A[%j%9%H!#(B
$B3F(B cons cell $B$N(B car $B$O(B Emacs $BI8=`4X?t(B `current-time-string' $B$,JV$97A<0!#(B
cdr $B$OBP1~$9$kG$0U$N7A<0!#(B"
  :type '(repeat (list (string :tag "English")
		       (string :tag "$BF|K\<0(B")
		       (string :tag "Deutsch")))
  :group 'skk-gadget)

(defcustom skk-day-of-week-alist
  '(("Sun" "$BF|(B" "So") ("Mon" "$B7n(B" "Mo") ("Tue" "$B2P(B" "Di") ("Wed" "$B?e(B" "Mi")
    ("Thu" "$BLZ(B" "Do") ("Fri" "$B6b(B" "Fr") ("Sat" "$BEZ(B" "Sa"))
  "*$BMKF|$N1Q8lI=5-$H$=$NB>$NI=5-K!$NO"A[%j%9%H!#(B
$B3F(B cons cell $B$N(B car $B$O(B Emacs $BI8=`4X?t(B `current-time-string' $B$,JV$97A<0!#(B
cdr $B$OBP1~$9$kG$0U$N7A<0!#(B"
  :type '(repeat (list (string :tag "English")
		       (string :tag "$BF|K\8l(B")
		       (string :tag "Deutsch")))
  :group 'skk-gadget)

(defcustom skk-default-current-date-function
  (lambda (date-information format gengo and-time)
    (skk-default-current-date date-information nil skk-number-style
			      gengo 0 0 0 and-time))
  "*`skk-current-date' $B$G%3!<%k$5$l$k%G%U%)%k%H$N4X?t!#(B
$B;~4V>pJs$r0z?t$K<h$j2C9)$7$?J8;zNs$r=PNO$9$k!#(B

$B0z?t$O(B DATE-INFORMATION, FORMAT, GENGO, AND-TIME $B$N(B 4 $B$D!#(B
DATE-INFORMATION $B$O(B `current-time-string' $B$,JV$7$?J8;zNs$r(B

  (year month day day-of-week hour minute second)

$B$N7A<0$GJQ49$7$?%j%9%H(B ($B3FMWAG$OJ8;zNs(B)$B!#(B
FORMAT $B$O(B `format' $B$NBh0l0z?t$NMM<0$K$h$k=PNO7ABV$r;XDj$9$kJ8;zNs!#(B
GENGO $B$O859fI=<($9$k$+$I$&$+(B (boolean)$B!#(B
AND-TIME $B$O;~9o$bI=<($9$k$+$I$&$+(B (boolean)$B!#(B"
  :type '(radio (function :tag "$B4X?t(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-gadget)

(defcustom skk-date-ad nil
  "*Non-nil $B$G$"$l$P!"(B`skk-today', `skk-clock' $B$G@>NqI=<($9$k!#(B
nil $B$G$"$l$P!"859fI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-gadget)

(defcustom skk-number-style 1
  "*`skk-today', `skk-clock' $B$GI=<($9$k?t;z$N7A<0$rJQ2=$5$;$k!#(B
  0 , nil : ASCII $B?t;z(B
  1 , t   : $BA43Q?t;z(B
  2       : $B4A?t;z(B($B0L<h(B)
  3       : $B4A?t;z(B"
  :type '(radio (const :tag "ASCII $B?t;z(B" 0)
		(const :tag "$BA43Q?t;z(B" 1)
		(const :tag "$B4A?t;z(B($B0L<h(B)" 2)
		(const :tag "$B4A?t;z(B" 3))
  :group 'skk-gadget)

(defcustom skk-units-alist
  '(("mile" ("km" . 1.6093) ("yard" . 1760))
    ("yard" ("feet" . 3) ("cm" . 91.44))
    ("feet" ("inch" . 12) ("cm" . 30.48))
    ("inch" ("feet" . 0.5) ("cm" . 2.54)))
  "*$BC10L49;;>pJs$NO"A[%j%9%H!#(B
$B3FMWAG$O(B ($B4p=`$H$J$kC10L(B ($BJQ49$9$kC10L(B . $BJQ49;~$NG\N((B)) $B$N7A<0$K$h$k!#(B
`skk-gadget-units-conversion' $B$G;2>H$9$k!#(B"
  :type 'sexp
  :group 'skk-gadget)

(defcustom skk-gadget-load-hook nil
  "*skk-gadget.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-gadget)

;;; skk-isearch.el related.
(defcustom skk-isearch-mode-enable t
  "*Non-nil $B$G$"$l$P!"%$%s%/%j%a%s%?%k!&%5!<%A$G(B skk-isearch $B$rMxMQ$9$k!#(B

$BDL>o$O(B SKK $B%b!<%I$,(B ON $B$N%P%C%U%!$G$N$_(B skk-isearch $B$,M-8z$K$J$k$,!"$3(B
$B$NCM$,(B `always' $B$G$"$l$P(B SKK $B%b!<%I$,(B OFF $B$N%P%C%U%!$G$bM-8z$K$J$k!#(B

$B$3$NCM$,(B nil $B$J$i$P(B skk-isearch $B$OL58z$K$J$k!#(Bmigemo $B$rMxMQ$7$?$$>l9g(B
$B$J$I$K$O(B nil $B$K@_Dj$9$k$+!"$b$7$/$OJ;MQ$9$k$N$G$"$l$P(B
`skk-isearch-start-mode' $B$r(B `latin' $B$K$9$k$N$,NI$$!#(B"
  :type '(radio (const :tag "SKK $B%b!<%I$,(B ON $B$N;~$@$1MxMQ$9$k(B" t)
		(const :tag "$B>o$KMxMQ$9$k(B" always)
		(const :tag "$BMxMQ$7$J$$(B" nil))
  :group 'skk-isearch)

(defcustom skk-isearch-mode-string-alist
  '((hiragana . "[$B$+(B] ") (katakana . "[$B%+(B] ") (jisx0208-latin . "[$B1Q(B] ")
    (latin . "[aa] ") (abbrev . "[a$B$"(B] ") (nil . "[--] "))
  ;;  "*Alist of (MODE-SYMBOL . PROMPT-STRING).
  ;;MODE-SYMBOL is a symbol indicates canonical mode of skk for skk-isearch.
  ;;Valid MODE-SYMBOL is one of `hiragana', `katakana', `jisx0208-latin',
  ;;`latin' or nil.
  ;;PROMPT-STRING is a string used in prompt to indicates current mode of
  ;;skk for skk-isearch. "
  "*$B%$%s%/%j%a%s%?%k!&%5!<%A;~$N%W%m%s%W%HI=<($N$?$a$NO"A[%j%9%H!#(B
$B3FMWAG$O!"(B
  (MODE-SYMBOL . PROMPT-STRING)
$B$H$$$&(B cons cell$B!#(B

MODE-SYMBOL $B$OF~NO%b!<%I$rI=$o$9%7%s%\%k$G!"2<5-$N$$$:$l$+$r;XDj$9$k!#(B
   $B$+$J%b!<%I!'(B `hiragana'
   $B%+%J%b!<%I!'(B `katakana'
   $BA41Q%b!<%I!'(B `jisx0208-latin'
   $B%"%9%-!<%b!<%I!'(B `latin'
   Abbrev $B%b!<%I!'(B `abbrev'
   nil : SKK $B%b!<%I%*%U(B

PROMPT-STRING $B$O!"F~NO%b!<%I$K1~$8$F%W%m%s%W%HI=<($9$kJ8;zNs!#(B"
  :type '(list
	  (cons (const :tag "$B$+$J%b!<%I(B" hiragana)
		(string :tag "$B%W%m%s%W%H(B"))
	  (cons (const :tag "$B%+%J%b!<%I(B" katakana)
		(string :tag "$B%W%m%s%W%H(B"))
	  (cons (const :tag "$BA41Q%b!<%I(B" jisx0208-latin)
		(string :tag "$B%W%m%s%W%H(B"))
	  (cons (const :tag "$B%"%9%-!<%b!<%I(B" latin)
		(string :tag "$B%W%m%s%W%H(B"))
	  (cons (const :tag "Abbrev $B%b!<%I(B" abbrev)
		(string :tag "$B%W%m%s%W%H(B"))
	  (cons (const :tag "SKK$B%b!<%I%*%U(B" nil)
		(string :tag "$B%W%m%s%W%H(B")))
  :group 'skk-isearch)

(defcustom skk-isearch-start-mode nil
  ;;  "*Specifies the search mode when isearch is called.
  ;;This variable is valid only when `skk-isearch-use-previous-mode' is nil.
  ;;If nil, it means that if skk-mode has been called in this buffer, same as
  ;;the mode of the buffer, otherwise perform ascii search.
  ;;If `latin' or `ascii' perfrom ascii search.
  ;;If `hiragana', `hirakana' or `kana' -> hira kana search.
  ;;If `jisx0208-latin' or `eiji', perform zenkaku eiji (i.e. JIS X0208
  ;;alphabet) search."
  "*$B%+%l%s%H%P%C%U%!$G%$%s%/%j%a%s%?%k!&%5!<%A$r9T$&:]$NF~NO%b!<%I!#(B
`skk-isearch-use-previous-mode' $B$,(B nil $B$N>l9g$N$_M-8z!#(B
$B%$%s%/%j%a%s%?%k!&%5!<%A$r9T$&>l9g!">o$K$3$NJQ?t$G;XDj$7$?F~NO%b!<%I$,;HMQ$5$l$k(B
 ($B%f!<%6!<$,L@<(E*$KJQ99$9$k$3$H$O2D(B)$B!#(B
$B2<5-$N$$$:$l$+$N%7%s%\%k$G;XDj$9$k!#(B

   nil:  $B%+%l%s%H%P%C%U%!$G(B SKK $B%b!<%I$,5/F0$5$l$F$$$l$P$=$N%b!<%I!"(B
         $B5/F0$5$l$F$$$J$1$l$P(B $B%"%9%-!<%b!<%I!#(B
   `hiragana' (`hiragana' or `kana'): $B$+$J%b!<%I(B
   `jisx0208-latin' (`eiji') : $BA41Q%b!<%I(B
   `latin' (`ascii'): $B%"%9%-!<%b!<%I(B"
  :type '(radio (const :tag "$B8!:wCf%P%C%U%!$N%b!<%I$r7Q>5(B" nil)
		(const :tag "$B%"%9%-!<%b!<%I(B" latin)
		(const :tag "$B$+$J%b!<%I(B" hiragana)
		(const :tag "$BA41Q%b!<%I(B" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-use-previous-mode nil
  ;; "*Non-nil means use the same search mode as that of the last search."
  "*Non-nil $B$G$"$l$P!"F1$8%P%C%U%!$G$N:G8e$N8!:w;~$N%b!<%I$r;HMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-isearch)

(defcustom skk-isearch-initial-mode-when-skk-mode-disabled 'latin
  ;;  "*Symbol indicates the mode to use as initial mode for skk-isearch when
  ;;skk is turned off in the current buffer."
  "*SKK $B%b!<%I$,%*%U$N%P%C%U%!$G!":G=i$K%$%s%/%j%a%s%?%k!&%5!<%A$r9T$&:]$NF~NO%b!<%I!#(B"
  :type '(radio (const :tag "$B%"%9%-!<%b!<%I(B" latin)
		(const :tag "$B$+$J%b!<%I(B" hiragana)
		(const :tag "$BA41Q%b!<%I(B" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-whitespace-regexp "\\(\\s \\|[ \t\n\r\f]\\)*"
  ;;  "*Regular expression to match a sequence of whitespace chars.
  ;;This applies to regular expression incremental search."
  "$B6uGrJ8;z$NO"B3$H$7$F%^%C%A$5$;$k$Y$-@55,I=8=!#(B
regexp isearch $B$N:]!"$3$N@55,I=8=$K%^%C%A$9$kJ8;z$,8!:wJ8;zNs$N4V$K4^$^$l$F$$$F(B
$B$b%^%C%A$9$k!#(B"
  :type 'regexp
  :group 'skk-isearch)

(defconst skk-isearch-mode-canonical-alist
  '((hiragana . 0) (katakana . 1) (jisx0208-latin . 2) (latin . 3))
  "Alist of (SYMBOL . NUMBER).
The SYMBOL is canonical skk mode, and NUMBER is its numerical representation.")

(defconst skk-isearch-mode-alias-alist
  '((hirakana . hiragana) (kana . hiragana) (eiji . jisx0208-latin)
    (ascii . latin))
  "Alist of (ALIAS . CANONICAL).
The both ALIAS and CANONICAL should be symbol.
ALIAS can be used as an alias of CANONICAL.
CANONICAL should be found in `skk-isearch-mode-canonical-alist'. ")

(defconst skk-isearch-breakable-character-p-function
  (cond ((eval-when-compile (featurep 'emacs))
	 (lambda (char)
	   ;; see emacs/lisp/fill.el how the category `|' is
	   ;; treated.
	   (aref (char-category-set char) ?|)))
	(t
	 (lambda (char)
	   (string-match word-across-newline
			 (char-to-string char)))))
  "Function to test if we can insert a newline around CHAR when filling.")

(defconst skk-isearch-working-buffer " *skk-isearch*"
  "Work buffer for skk isearch.")

(defvar skk-isearch-message nil
  "skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0!#(B
Non-nil $B$G$"$l$P!"(B`skk-isearch-message' $B4X?t$r%3!<%k$9$k!#(B")

(defvar skk-isearch-mode nil
  "Current search mode.
0 means hira kana search.
1 means kana search.
2 means zenkaku eiji (i.e. JIS X0208 alphabet) search.
3 means ascii search.")

(defvar skk-isearch-incomplete-message ""
  "Incomplete isearch message")

(defvar skk-isearch-mode-map nil
  "Keymap for skk isearch mode.
This map should be derived from `isearch-mode-map'.")

(defvar skk-isearch-overriding-local-map
  (if (featurep 'xemacs)
      'overriding-local-map
    'overriding-terminal-local-map)
  "Variable holding overriding local map used in `isearch-mode'.")

(defvar skk-isearch-last-mode-string "")
(defvar skk-isearch-last-mode-regexp "")

;;;###autoload
(defvar skk-isearch-switch nil)
(defvar skk-isearch-state nil)
(defvar skk-isearch-in-editing nil)
(defvar skk-isearch-current-buffer nil)

;;; skk-hint.el related.
(defcustom skk-hint-start-char ?\73 ; ;
  "*$B%R%s%HJQ49$r3+;O$9$k%-!<%-%c%i%/%?(B"
  :type 'character
  :group 'skk-hint)

(skk-deflocalvar skk-hint-henkan-hint nil
  "$B%R%s%HIU$-JQ49;~$N%R%s%HItJ,!#(B
`skk-henkan-key', `skk-henkan-okurigana', `skk-okuri-char' $B$N%j%9%H!#(B")

(skk-deflocalvar skk-hint-start-point nil)
(skk-deflocalvar skk-hint-end-point nil)
(skk-deflocalvar skk-hint-okuri-char nil)
(skk-deflocalvar skk-hint-state nil)
(skk-deflocalvar skk-hint-inhibit-kakutei nil)
(skk-deflocalvar skk-hint-inhibit-dcomp nil)

;;; skk-jisx0201.el related.
(defcustom skk-use-jisx0201-input-method nil "\
*Non-nil $B$J$i(B $BH>3Q%+%J$H(B Japanese Roman $B$NF~NO5!G=$,MxMQ2DG=$K$J$k!#(B"
  :type 'boolean
  :group 'skk-jisx0201)

(defcustom skk-jisx0201-mode-string "(I6@6E(B"
  "*SKK $B$,(B JISX0201 $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-jisx0201)

(defvar skk-jisx0201-base-rule-tree nil)
(defvar skk-jisx0201-roman-rule-tree nil)
(defvar skk-jisx0201-orig-rule-tree nil)
(skk-deflocalvar skk-jisx0201-roman nil)

(skk-deflocalvar skk-jisx0201-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B JISX0201 $B%b!<%I$G$"$k$3$H$r<($9!#(B")

;;; skk-jisx0213.el related.
(defcustom skk-jisx0213-prohibit nil
  "*Non-nil $B$G$"$l$P(B JISX0213 $B$NJ8;zNs$r4^$`8uJd$N=PNO$r$7$J$$!#(B
JISX0213 $B$r07$($J$$$H$-$O$3$NCM$OF0:n$K1F6A$7$J$$!#(B"
  :type 'boolean
  :group 'skk-jisx0213)

;;; skk-jisyo-edit-mode.el related

(defcustom skk-jisyo-edit-user-accepts-editing nil
  "*Non-nil $B$G$"$l$P!"%f!<%6$,8D?M<-=q$NJT=8$r<+8J@UG$$K$F9T$&;]3NG':Q$G$"$k!#(B
nil $B$G$"$l$P!"(B`skk-edit-private-jisyo' $B$N<B9T;~$K3NG'$9$k!#(B"
  :type 'boolean
  :group 'skk-jisyo-edit-mode)

;;; skk-kakasi.el related.
(defcustom skk-use-kakasi (if (executable-find "kakasi") t nil)
  "*Non-nil $B$G$"$l$P(B KAKASI $B$r;H$C$?JQ49$r9T$&!#(B"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-command (executable-find "kakasi")
  "*KAKASI $B%3%^%s%IK\BN!#(B"
  :type 'file
  :group 'skk-kakasi)

(defcustom skk-romaji-*-by-hepburn t
  "*Non-nil $B$G$"$l$P(B KAKASI $B$r;H$C$?%m!<%^;z$X$NJQ49MM<0$K%X%\%s<0$rMQ$$$k!#(B
$BNc$($P!"(B
  \"$B$7(B\" -> \"shi\"

nil $B$G$"$l$P!"71Na<0(B \"($B!VF|K\<0!W$H$b8@$&$h$&$@(B)\" $B$rMQ$$$k!#(B
$BNc$($P!"(B
   \"$B$7(B\" -> \"si\"

$B><OB(B 29 $BG/(B 12 $B7n(B 9 $BF|IUFb3U9p<(Bh0l9f$K$h$l$P!"86B'E*$K71Na<0(B \"($BF|K\<0(B)\" $B$r(B
$BMQ$$$k$+$N$h$&$K5-:\$5$l$F$$$k$,!":#F|0lHLE*$J5-:\J}K!$O!"$`$7$m!"%X%\%s<0$G$"(B
$B$k$h$&$K;W$&!#(B"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-load-hook nil
  "*skk-kakasi.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-kakasi)

(defcustom skk-gyakubiki-jisyo-list nil
  "KAKASI $B$r;H$C$?JQ49$N:]$KDI2C;2>H$9$k5U0z$-%f!<%6<-=q$N%j%9%H!#(B"
  :type '(repeat file)
  :group 'skk-kakasi)

;;; skk-kanagaki.el related.
(defcustom skk-use-kana-keyboard nil "\
*Non-nil $B$J$i2>L>F~NOMQ$N@_Dj$r%m!<%I$9$k!#(B
SKK $B;HMQCf$K$3$NJQ?t$NCM$r@Z$jBX$($k$3$H$G(B  $B%m!<%^;zF~NO(B $B"+"*(B $B2>L>F~NO(B $B$N(B
$B@Z$jBX$($,$G$-$k!#(B"
  :type 'boolean
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       (set-default symbol value))
	   (when (and value
		      (boundp 'skk-mode-invoked)
		      skk-mode-invoked)
	     (require 'skk-kanagaki)
	     (skk-kanagaki-initialize))))
  :group 'skk-kanagaki)

;;; skk-kcode.el related.
(defcustom skk-kcode-method (if (featurep 'emacs)
				'code-or-char-list
			      'code-or-menu)
  "*`skk-input-by-code-or-menu' $B$G;H$o$l$kJ8;zA^F~$N$?$a$N%$%s%?!<%U%'!<%9!#(B
`char-list' $B$G$"$l$P!"J8;z0lMwI=(B (`skk-list-chars') $B$+$iA*Br$9$k!#(B
`code-or-char-list' $B$G$"$l$P!"$^$:(B JIS $B%3!<%I(B/$B6hE@%3!<%IF~NO%W%m%s%W%H$rI=<((B
$B$7!"M-8z$JF~NO$,F@$i$l$J$+$C$?>l9g$K(B `skk-list-chars' $B$r8F$S=P$9!#(B
`code-or-menu' $B$G$"$l$P=>Mh$N$h$&$K!"$^$:(B JIS $B%3!<%I(B/$B6hE@%3!<%IF~NO%W%m%s%W%H(B
$B$rI=<($7!"M-8z$JF~NO$,3NDj$7$J$+$C$?>l9g$K$O8uJdJ8;z0lMw$rI=<($9$k!#(B"
  :type '(radio (const :tag "$B>o$KJ8;z%3!<%II=$+$iA*$V(B" char-list)
		(const :tag "$B%3!<%IF~NO(B $B"*(B $BJ8;z%3!<%II=(B" code-or-char-list)
		(const :tag "$B%3!<%IF~NO(B $B"*(B $BJ8;z8uJd(B ($B5lMh$N%a%K%e!<(B)"
		       code-or-menu)
		(const :tag "$BJ8;z%3!<%II=!?%3!<%IF~NO$OMxMQ$7$J$$(B" this-key))
  :group 'skk-kcode)

(defcustom skk-input-by-code-menu-keys1 '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 1 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
12 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-kcode)

(defcustom skk-input-by-code-menu-keys2
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 2 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
16 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-kcode)

(defcustom skk-kcode-charset
  (cond ((find-coding-system 'euc-jisx0213)
	 'japanese-jisx0213-1)
	(t
	 'japanese-jisx0208))
  "*`skk-input-by-code-or-menu' $B$G;H$o$l$kJ8;z%;%C%H!#(B"
  :type (let ((list (if (find-coding-system 'euc-jisx0213)
			'((const japanese-jisx0213-1)
			  (const japanese-jisx0208))
		      '((const japanese-jisx0208))))
	      (prompt (if (get 'charset 'widget-type)
			  '(charset)
			'(symbol))))
	  (append '(radio) list prompt))
  :group 'skk-jisx0213
  :group 'skk-kcode)

(defcustom skk-kcode-load-hook nil
  "*skk-kcode.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-kcode)

(defconst skk-code-n1-min 161)

(defconst skk-code-n1-max (if (find-coding-system 'euc-jisx0213)
			      254
			    244))

(defconst skk-code-n2-min 161)

(defconst skk-code-n2-max 254)

(defconst skk-code-null 128)

(defconst skk-kcode-charset-list
  (mapcar (lambda (x)
	    (list (symbol-name x)))
	  (if (eval-when-compile (featurep 'emacs))
	      charset-list
	    (charset-list))))

(defvar skk-display-code-method 'code
  "*Non-nil $B$G$"$l$P%]%$%s%H$K$"$kJ8;z$N%3!<%I$rI=<($9$k!#(B
nil $B$G$"$l$P(B `this-command-keys' $B$rA^F~$9$k!#(B")

(defvar skk-input-by-code-or-menu-jump-default skk-code-n1-min)

(defface skk-display-code-prompt-face
  '((((class color) (type tty))
     (:inherit default :foreground "cyan"))
    (((class color) (background light))
     (:inherit default :foreground "cyan"))
    (((class color) (background dark))
     (:inherit default :foreground "cyan"))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' $B$G%(%3!<%(%j%"$KI=<($9$k%a%C%;!<%8Cf$N(B KUTEN:$B!"(BJIS:$B!"(BEUC:$B!"(B
SJIS: $B5Z$S(B UNICODE: $B$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-char-face
  '((((class color) (type tty))
     (:inherit default :foreground "black" :background "yellow"))
    (((class color) (background light))
     (:inherit default :foreground "black" :background "yellow"))
    (((class color) (background dark))
     (:inherit default :foreground "black" :background "yellow"))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' $B$G%(%3!<%(%j%"$KI=<($9$k%a%C%;!<%8Cf$NEv3:J8;z$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-tankan-radical-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' $B$G%(%3!<%(%j%"$KI=<($9$k%a%C%;!<%8Cf$NAm2h?t$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-display-code-tankan-annotation-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-display-code `$' $B$G%(%3!<%(%j%"$KI=<($9$k%a%C%;!<%8Cf$NJ8;zL>$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-kcode
  :group 'skk-visual)

;;; skk-list-chars (in skk-kcode.el) related.
(defvar skk-list-chars-buffer-name "*skk-list-chars*"
  "Docstring.")

(defvar skk-list-chars-original-window-configuration nil
  "skk-list-chars-mode $B$KF~$kA0$N(B window configuration$B!#(B
`skk-list-chars-quit' $B$N<B9T;~!"$3$NJQ?t$r;H$C$F(B skk-list-chars-mode $B$K(B
$BF~$kA0$N(B window $B>uBV$KI|5"$9$k!#(B")

(defvar skk-list-chars-destination-buffer nil
  "skk-list-chars-insert $B$NA^F~@h%P%C%U%!(B")

(defvar skk-list-chars-point nil
  "C-x C-x (skk-list-chars-goto-point) $B$N%8%c%s%W@h(B")

(defvar skk-list-chars-default-charstr nil)

(defvar skk-list-chars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "$" 'skk-list-chars-display-code)
    (define-key map "w" 'skk-list-chars-copy)
    (define-key map "q" 'skk-list-chars-quit)
    (define-key map (kbd "C-x C-x") 'skk-list-chars-goto-point)
    (define-key map "c" 'skk-list-chars-code-input)
;;     (define-key map (kbd "C-a") '$B6h$N@hF,$X(B)
;;     (define-key map (kbd "C-e") '$B6h$NKvHx$X(B)
;;     (define-key map "<" '$B%P%C%U%!@hF,$X(B)
;;     (define-key map ">" '$B%P%C%U%!KvHx$X(B)

    (define-key map (kbd "C-f") 'next-completion)
    (define-key map "f"         'next-completion)
    (define-key map "l"         'next-completion)
    (define-key map [right]     'next-completion)

    (define-key map (kbd "C-b") 'previous-completion)
    (define-key map "b"         'previous-completion)
    (define-key map "h"         'previous-completion)
    (define-key map [left]      'previous-completion)

    (define-key map (kbd "C-n") 'skk-list-chars-next-line)
    (define-key map "n"         'skk-list-chars-next-line)
    (define-key map "j"         'skk-list-chars-next-line)
    (define-key map [down]      'skk-list-chars-next-line)

    (define-key map (kbd "C-p") 'skk-list-chars-previous-line)
    (define-key map "p"         'skk-list-chars-previous-line)
    (define-key map "k"         'skk-list-chars-previous-line)
    (define-key map [up]        'skk-list-chars-previous-line)

    (define-key map (kbd "RET") 'skk-list-chars-insert)
    (define-key map "i"         'skk-list-chars-insert)

    (define-key map "g" 'skk-list-chars-jump)

    (define-key map "o"  'skk-list-chars-other-charset)
    (define-key map "\\" 'skk-list-chars-other-charset)
    map)
  "Keymap used in skk-list-chars mode.")

(defface skk-list-chars-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-list-chars $B%P%C%U%!$K$*$1$k!"L\E*J8;z$r;X$7<($9MQES$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-kcode
  :group 'skk-visual)

(defface skk-list-chars-table-header-face
  '((((class color) (type tty))
     (:inherit default :foreground "red"))
    (((class color) (background light))
     (:inherit default :foreground "Firebrick"))
    (((class color) (background dark))
     (:inherit default :foreground "chocolate1"))
    (((class grayscale))
     (:inherit default :foreground "LightGray")))
  "*skk-list-chars $B%P%C%U%!$K$*$1$k!"0lMw8+=P$7$dOH@~$KE,MQ$9$k(B face $BB0@-!#(B"
  :group 'skk-visual)

;;; skk-look.el related.
(defcustom skk-use-look nil
  "*UNIX look $B%3%^%s%I$rMxMQ$7$?Jd40!&JQ49$r9T$&$+$I$&$+$r;XDj$9$k!#(B
t $B$J$i$P!"Jd40;~$H1Q?t;zJQ49;~$K(B look $B$r;HMQ$9$k!#(B
`completion' $B$J$i$P!"Jd40;~$@$1(B look $B$r;HMQ$9$k!#(B
`conversion' $B$J$i$P!"1Q?t;zJQ49;~$@$1(B look $B$r;HMQ$9$k!#(B
nil $B$J$i$P!"(Blook $B$r;HMQ$7$J$$!#(B

SKK abbrev $B%b!<%I$GJd40$9$k$H!"8D?M<-=q$r8!:w$7?T$7$?8e$G!"(BUNIX look $B%3%^%s(B
$B%I$K$h$k1QC18lJd40$r9T$&!#Nc$($P!"(B

  $B"&(Babstr (TAB)
  ---> $B"&(Babstract

SKK abbrev $B%b!<%I$G!"!V1QJ8;z(B + $B%"%9%?%j%9%/!W$K$FJQ49$9$k$H!"(Blook $B%3%^%s%I(B
$B$K$h$k[#Kf8!:w$r9T$&$3$H$,$G$-$k!#Nc$($P!"(B

 $B"&(Babstra* (SPC)
  ---> $B"'(Babstract

$B$3$N>uBV$G3NDj$9$k$H!"(B`abstra*' $B$r8+=P$78l!"(B`abstract' $B$r8uJd$H$9$k%(%s%H%j(B
$B$,8D?M<-=q$KDI2C$5$l$k!#(B`skk-search-excluding-word-pattern-function' $B$K$h(B
$B$j!"3NDj$7$F$b$3$N$h$&$J%(%s%H%j$rDI2C$7$J$$$h$&$K@_Dj$9$k$3$H$,$G$-$k!#(B"
  :type '(radio (const :tag "$BJd40;~$H1Q?t;zJQ49;~$KM-8z(B" t)
		(const :tag "$BJd40;~$@$1M-8z(B" completion)
		(const :tag "$B1Q?t;zJQ49;~$@$1M-8z(B" conversion)
		(const :tag "$BL58z(B" nil))
  :group 'skk-basic
  :group 'skk-look)

(defcustom skk-look-command (executable-find "look")
  "*UNIX look $B%3%^%s%I$NL>A0!#(B"
  :type `(file :tag "$B%U%!%$%kL>(B" ,(or (executable-find "look") ""))
  :group 'skk-look)

(defcustom skk-look-conversion-arguments
  (concat "-df %s "
	  (cond ((file-exists-p "/usr/share/dict/words")
		 "/usr/share/dict/words")
		((file-exists-p "/usr/share/lib/dict/words")
		 "/usr/share/lib/dict/words")
		((file-exists-p "/usr/dict/words")
		 "/usr/dict/words")
		(t
		 "")))
  "*look $B%3%^%s%I$,1Q?t!VJQ49!W;~$K8F$S=P$5$l$k:]$KEO$90z?t$r;XDj$9$kJQ?t!#(B
$B0lHL$K(B look $B%3%^%s%I$O0J2<$N7A<0$G8F$S=P$5$l$k!#(B

     look [-df] [-t termchar] string [file]

$B$=$l$>$l$N0UL#$K$D$$$F$O(B \\[man] look $B$r;2>H$5$l$?$$!#(B
$B$3$NJQ?t$K$O!">e5-$N$h$&$JA40z?t$N$&$A(B string $B$r(B %s $B$KCV49$7$?$b$N$r;XDj$9$k!#(B

$BCm0U;v9`$H$7$F!"(Blook $B%3%^%s%I$KEO$90z?t(B -d $B$H(B -f $B$K4X$7$F$O!"(B file $B$,(B $BF1$8(B
$B0z?t$G(B sort $B$5$l$F$$$kI,MW$,$"$k!#Nc$($P(B look -df $B$G8!:w$9$k$H$-$O(B sort -df
$B$G!"(B look -d $B$G8!:w$9$k$H$-$O(B sort -d $B$G(B sort $B$5$l$F$$$kI,MW$,$"$k!#$3$N$3$H(B
$B$K4X$7$F$O(B \\[man] sort $B$b;2>H$5$l$?$$!#(B

$B$b$&$R$H$D$NCm0UE@$H$7$F!"(B look $B$N:G8e$N0z?t$H$7$F(B file $B$rEO$5$J$$$H(B ($B>JN,$9$k(B
$B$H(B) $B6/@)E*$K0z?t(B -d $B$H(B -f $B$N5!G=$,M-8z$K$J$k!#$b$7(B look $B$r;W$$DL$j@)8f$7$?$1$l(B
$B$PE,@Z$J(B file $B$r;XDj$9$k$Y$-$G$"$k!#(B

 ($B@_DjNc(B)

 (setq skk-look-conversion-arguments \"-df %s /usr/share/dict/words\")
"
  :type 'string
  :group 'skk-look)

(defcustom skk-look-completion-arguments
  (concat "%s "
	  (cond ((file-exists-p "/usr/share/dict/words")
		 "/usr/share/dict/words")
		((file-exists-p "/usr/share/lib/dict/words")
		 "/usr/share/lib/dict/words")
		((file-exists-p "/usr/dict/words")
		 "/usr/dict/words")
		(t
		 "")))
  "*look $B%3%^%s%I$,1Q?t!VJd40!W;~$K8F$S=P$5$l$k:]$KEO$90z?t$r;XDj$9$kJQ?t!#(B
look $B%3%^%s%I$K4X$7$F$OJQ?t(B `skk-look-conversion-arguments' $B$N%I%-%e%a%s%H!"(B
$B5Z$S(B \\[man] look $B$r;2>H$5$l$?$$!#(B

 ($B@_DjNc(B)

 (setq skk-look-completion-arguments \"-d %s /usr/share/dict/words.case\")
"
  :type 'string
  :group 'skk-look)

(defcustom skk-look-recursive-search nil
  "*Non-nil $B$J$i$P!"(B look $B%3%^%s%I$,8+$D$1$?1QC18l$rJQ49%-!<$K$7$F:F8!:w$r9T$&!#(B
$B:F8!:w$N7k2L!"8uJd$,8+$D$+$i$J$1$l$P!"85$N1QC18l<+?H$r8uJd$H$7$F=PNO$9$k!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-expanded-word-only t
  "*Non-nil $B$J$i$P!"(B look $B$N=PNO$KBP$9$k:F8!:w$,@.8y$7$?8uJd$N$_$rI=<($9$k!#(B
`skk-look-recursive-search' $B$,(B non-nil $B$G$"$k$H$-$N$_M-8z!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-use-ispell nil
  "*look $B$K$h$k8!:w$N:]!"(Bispell $B$rJ;MQ$9$k$+$I$&$+$r;XDj$9$k!#(B
t $B$J$i$P!"Jd40;~$H1Q?t;zJQ49;~$K(B ispell $B$rJ;MQ$9$k!#(B
`completion' $B$J$i$P!"Jd40;~$@$1(B ispell $B$rJ;MQ$9$k!#(B
`conversion' $B$J$i$P!"1Q?t;zJQ49;~$@$1(B ispell $B$rJ;MQ$9$k!#(B
nil $B$J$i$P!"(Bispell $B$r;HMQ$7$J$$!#(B"
  :type '(radio (const :tag "$BJd40;~$H1Q?t;zJQ49;~$KM-8z(B" t)
		(const :tag "$BJd40;~$@$1M-8z(B" completion)
		(const :tag "$B1Q?t;zJQ49;~$@$1M-8z(B" conversion)
		(const :tag "$BL58z(B" nil))
  :group 'skk-look)

(defvar skk-look-completion-words nil)

;;;; skk-lookup.el related.
(defcustom skk-lookup-search-agents nil
  "*$B8!:w%(!<%8%'%s%H$N@_Dj$N%j%9%H!#(B
$B%j%9%H$N3FMWAG$O<!$N7A<0$r<h$k(B:

  (CLASS LOCATION [KEY1 VALUE1 [KEY2 VALUE2 [...]]])

CLASS $B$K$O!"%(!<%8%'%s%H$N<oN`$r%7%s%\%k$G;XDj$9$k!#(B
LOCATION $B$K$O!"%(!<%8%'%s%H$N=j:_$rJ8;zNs$G;XDj$9$k!#(B
KEY $B5Z$S(B VALUE $B$O>JN,2DG=$G!"%(!<%8%'%s%H$KBP$9$k%*%W%7%g%s$r;XDj$9$k!#(B

$BNc(B: (setq skk-lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\"))))"
  :type '(repeat (sexp :tag "Agent"))	; type $B$O$A$g$C$H$d$d$3$7$9$.!&!&(B
  :group 'skk-lookup)

(defcustom skk-lookup-option-alist
  '(;; "[spla -> splat]"
    ("ispell" exact nil nil (not skk-okuri-char) ("-> \\([^ ]+\\)]$" . 1)
     nil nil)
    ;; what's this?
    ("jedict" exact nil nil (not skk-okuri-char) nil nil nil)
    ;; $BCN7CB"(B
    ;; `$B"'#I#M#F!N(BInternational Monetary Fund$B!?(BInternational
    ;;            Metalworkers Federation$B!O(B'
    ;; `$B#I#M#F!J9q:]DL2_4p6b!K!Z(BInternational Monetary Fund$B![(B'
    ("CHIEZO" exact exact prefix t
     ("$B!J(B\\(.+\\)$B!K(B\\|$B!Z(B\\(.+\\)$B![(B$\\|$B!N(B\\(.+\\)$B!O(B$\\|^\\([^$B!J!Z!N!O![!K(B]+\\)$"
      .
      (cond ((match-beginning 1) 1)
	    ((match-beginning 2) 2)
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)))
     "$B!?(B\\|$B!"(B\\|, " nil)
    ;; $B!V<-!&E5!&HW!W(B
    ;; `$B$"$+#3(B $B^@(B", "ethanol'
    ("CHUJITEN" exact exact prefix t ("[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; `($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>'
    ("COLLOC" exact exact prefix t ("\\([^ $B!T!U(B]+\\) <[a-z]+>$" . 1) nil nil)
    ;; $B%8!<%K%"%91QOB(B, $B%8!<%K%"%91QOB!&OB1Q<-E5(B
    ;; `$B$"$+(B[$B^@(B]'
    ;; `$B$$$l$+$((B[$BF~$lBX$((B,$BF~$l49$((B]'
    ("GENIUS" exact exact prefix t
     ;;("\\[\\(.+\\)\\]$" . 1) ;;can I use `$' for GENIUS?
     ("\\[\\(.+\\)\\]" . 1)
     "," nil)
    ;; Super$BE}9g<-=q(B99 Disk1, 2/$B8=BeMQ8l$N4pACCN<1(B
    ;; `$B"!<k!&3t!&<l!&<n!L;w$?$b$N4A;z!M(B' ; `$B!&(B' $B$,6h@Z$jJ8;z$G$"$k$H$-$H(B
    ;;  $B$=$&$G$J$$$H$-$,$"$k$J$!(B...$B!#(B
    ;; `$B"!@V%o%$%s!&%V!<%`!L7r9/LdBj!M(B'
    ("GN99EP01" exact exact prefix t ("^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" . 1) nil nil)
    ("GN99EP02" exact exact prefix t ("^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" . 1) nil nil)
    ;; $B4dGH9q8l<-E5(B
    ;; `$B$7$?$$!Z;`BN!&;SBN![(B'
    ;; `$B$7$?$$!Z;YBb![!Z;^Bb![(B'
    ;; `$B$"$$!Z0&![(B'
    ;; `$B$"$$(B($B$"$p(B)$B!ZMu![(B'
    ;; `$B$"$$(B<gaiji=za52a>$B0%(B<gaiji=za52b>'
    ;; `$B$@$7!Z=P$7![!Z=P$7!&!R=P=A!S![!Z!P;3<V!Q![(B'
    ;; `$B$U$&$-$j!ZIu@Z(B($B$j(B)$B![(B'
    ("IWAKOKU" exact exact prefix t
     ;; cannot use `$' for this.
     ("$B!Z(B\\(.+\\)$B![(B" . 1)
     "$B![!Z(B\\|$B!&(B" "[$B!R!S!P!Q(B()]")
    ;; "$B9$(B", "$B@V(B"
    ("KANWA" exact exact prefix t nil nil nil)
    ;; KOUJIEN: $B9-<-1q(B $BBh(B4$BHG(B($B4dGH(B,EPWING) $B%^%k%A%a%G%#%"HG(B
    ;; `$B$"$$!Z9g$$!&2q$$![%"%R(B' ; $B$3$l$K$O(B `$B![(B$' $B$r;H$($J$$!#(B
    ;; `$B$"$$!Z4V![%"%R(B'
    ;; `$B%&%#!Z(Boui $B%U%i%s%9![(B'
    ;; `$B%=!Z(Bsol $B%$%?%j%"![(B'
    ;; `$B%"%j%9%H%F%l%9!>$7$e$.!Z!=<g5A![(B'
    ;; `$B%"!<%H%^%s!Z(B_tman $B[p![(B'; $BL$BP1~!#30;z$r4^$`8uJd!#(B_ $B$O30;z(B
    ;; "$B!{8W$N0R$r<Z$k8Q(B"
    ("KOUJIEN" exact exact prefix t
     ("^\\([^$B!Z![(B]+\\)$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\([^$B!Z![(B]+\\)$B![(B$\\|\
\$B!Z(B\\([a-zA-Z]+\\) [$B!<%!(B-$B%s(B]+$B![(B$\\|$B!Z(B\\([^$B!Z![(B]+\\)$B![(B\\|\
^$B!{(B\\(.+\\)$" .
      (cond ((match-beginning 2) '(1 2))
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)
	    ((match-beginning 5) 5)))
     "$B!&(B"
     ;;"$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\|$B![(B$"
     nil)
    ;; KOJIEN: $B9-<-1qBh(B5$BHG(B($B4dGH(B,EPWING)
    ;; `$B$G$s$7!>%V%C%/!ZEE;R!=![(B'
    ("KOJIEN" exact exact prefix t
     ("^\\([^$B!Z![(B]+\\)$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\([^$B!Z![(B]+\\)$B![(B$\\|\
\$B!Z(B\\([a-zA-Z]+\\) [$B!<%!(B-$B%s(B]+$B![(B$\\|\
\$B!Z(B\\([^$B!Z![(B]+\\)$B![(B\\|\
^[$B!<$!(B-$B$s(B]+$B!>(B\\([$B!<%!(B-$B%s(B]+\\)$B!Z(B\\([^$B!Z![(B]+\\)$B!=![(B$\\|\
^$B!{(B\\(.+\\)$" .
      (cond ((match-beginning 2) '(1 2))
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)
	    ((match-beginning 5) '(6 5))
	    ((match-beginning 7) 7)))
     "$B!&(B"
     ;;"$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\|$B![(B$"
     nil)
    ;; KOKUGO: $B;0>JF2(B $BF|K\8l<-E5!J8=Be9q8l!"30Mh8l!K(B
    ;; `$B!R(B' $B$O!"EvMQ4A;zI=$K$J$$4A;z$G!"(B`$B!T(B' $B$O!"EvMQ4A;zI=$K$O$"$k$,!"$=$N2;!"(B
    ;; $B71$,EvMQ4A;zI=$N2;71I=$K$J$$4A;z!#(B
    ("KOKUGO" exact exact prefix t ("$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" . 1) "$B!&(B" "[$B!T!R(B]")
    ;; $B!V<-!&E5!&HW!WImB0$N%^%$%Z%G%#%"(B
    ;;`$BBgOB74;3(B($B;T(B)'
    ;;`$B%o%7%s%H%s(B(George Washington)'
    ;;`$B%o%7%s%H%s(B($B=#(B)'
    ;;`$B%o%7%s%H%s(B Washington'
    ;;`$B%"%$%s%7%e%?%$%s(B(Albert Einstein)'
    ;;`$B9aNI='(B($BD.(B)'
    ;;`$B%+%i%9(B ($B1((B)'
    ;;`$B%+%i%9(B(Maria Callas)'
    ("MYPAEDIA" exact exact prefix t
     ("\\([^ ]+\\)(.+)$\\|.+ (\\([^ ]+\\))$\\|^\\([^ ()]+\\)$" .
      (cond ((match-beginning 1) 1)
	    ((match-beginning 2) 2)
	    ((match-beginning 3) 3)))
     nil nil)
    ;;  mypaedia-fpw $B$+$i@8@.$7$?(B PC Success $BHG%^%$%Z%G%#%"(B (FreePWING $B<-=q(B)
    ;; `$BBgOB74;3(B [$B$d$^$H$3$*$j$d$^(B] ($B;T(B)'
    ;; `$B%"%$%s%7%e%?%$%s(B (Albert Einstein)'
    ;; `$B%o%7%s%H%s(B (Washington) ($B=#(B)'
    ;; `$B%o%7%s%H%s(B (Washington)'
    ;; `$B%o%7%s%H%s(B (George Washington)'
    ;; `$B9aNI='(B [$B$+$i$9(B] ($BD.(B)'
    ;; `$B%+%i%9(B ($B1((B) [$B%+%i%9(B]'
    ;; `$B%+%i%9(B (Maria Callas)'
    ;;("MYPAEDIA" exact exact prefix t
    ;; ("^\\([^ ]+\\) \\[.+\\] (.+)$\\|^[^ ]+ (\\(.+\\)) \\[.+\\]$\\|\
    ;;   ^\\([^][() ]+\\)\\( .+\\)?$" .
    ;;  (cond ((match-beginning 1) 1)
    ;;        ((match-beginning 2) 2)
    ;;        ((match-beginning 3) 3)))
    ;; nil nil)
    ;;
    ;; $B%K%e!<%"%s%+!<1QOB(B
    ;; "$B$"$+#2(B $B9$(B"
    ("NEWANC" exact exact prefix t ("[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; what's this?
    ;; `$B!!$"$+(B <scud$B#2(B>',
    ;; `$B!!!V$"$+!W(B <rust>'
    ("PLUS" exact exact prefix t ("^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" . 1) nil nil)
    ("lsd" exact exact prefix t ("^\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" . 1) nil nil))
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: `lookup-dictionary-name' $B$,JV$9J8;zNs(B ($B<-=q<oJL$rI=$o$9(B)$B!#(B
  1th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B%*%W%7%g%s$r;XDj$7$F(B
       $B$$$J$$$H$-(B ($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B)
       $B$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$H!"(B
       $BAw$j$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  3th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B$G$"$k$H$-(B ($BAw$j2>L>(B
       $B7hDj$NA0$K8!:w$r3+;O$7$F$*$j!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B
       prefix $B$r=|$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B) $B$N(B search
       method $B$r<($9(B $B%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49(B
       $B$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  4th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  5th: `lookup-entry-heading' $B$,JV$9(B heading $B$+$i8uJd$H$7$F=PNO$9$kJ8;zNs$r@Z$j(B
       $B=P$9$?$a$N(B regexp $B;XDj5Z$S@Z$j=P$7%*%W%7%g%s!#(B
       car $B$K(B regexp $B$r<($9J8;zNs!"(Bcdr $B$K(B `match-string' $B$KEO$9(B count $B$r;XDj(B
       $B$9$k(B (5th $B$KJ8;zNs$@$1$r;XDj$7$?>l9g$O(B `match-string' $B$K$O(B 1 $B$,(B
       $BEO$5$l$k(B)$B!#(B
       cdr $BIt$K(B S $B<0$r;XDj$9$k$3$H$b2DG=!#2<5-$N$h$&$K(B cond $B<0$G>r7oH=Dj$9$l$P(B
       $BJ#?t$N(B regexp $B$r(B or $B;XDj$9$k$3$H$,2DG=!#(B

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
	        ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr $BIt$NI>2A7k2L$,?t;z$N%j%9%H$K$J$k$H$-$O!"$=$N?t;z$r=g$K(B match-string
       $B$KEO$7$FJ8;zNs$r@Z$j=P$7!"$=$l$iO"7k$7$?J8;zNs$r8uJd$H$7$FJV$9!#Nc$($P!"(B

          (cond ((match-beginning 5) '(6 5)))

       $B$H;XDj$9$k$H!"(B(match-beginning 5) $B$,(B non-nil $B$K$J$C$?>l9g!"(B
       (match-string 6) $B$H(B (match-string 5) $B$r$=$N=g$KO"7k$7$?J8;zNs$r8uJd$H$7(B
       $B$F=PNO$9$k!#(B
       $B@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(B5th $B$K(B nil $B$r;XDj$9$k!#(B
  6th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B
       regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B
  7th: $B@Z$j=P$5$l$?J8;zNs$+$iFCDj$NJ8;zNs$r<h$j=|$/>l9g$K;XDj$9$k(B regexp$B!#(B
       $B<-=q$N=PNO$,<-=qFCM-$N5-9fJ8;z$r4^$`>l9g$K;XDj$9$k!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O(B \"ispell\", \"jedict\", \"CHIEZO\", \"CHUJITEN\",
\"COLLOC\", \"GENIUS\", \"GN99EP01\", \"GN99EP02\", \"IWAKOKU\", \"KANWA\",
\"KOUJIEN\", \"KOJIEN\", \"KOKUGO\", \"MYPAEDIA\", \"NEWANC\", \"PLUS\" $B5Z$S(B
\"lsd\"$B!#(B
`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 (skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact)"
  ;; for checking.
  ;; (pp (mapcar (lambda (e)(cons (car e) (length e)))
  ;;	skk-lookup-option-alist))
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method for okuri nasi"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(choice
		 :tag "Search method for okuri ari (not process okuri early)"
		 (const exact) (const prefix)
		 (const suffix) (const substring)
		 (const keyword) (const text)
		 (const nil))
		(choice
		 :tag "Search method for okuri ari (process okuri early)"
		 (const exact) (const prefix)
		 (const suffix) (const substring)
		 (const keyword) (const text)
		 (const nil))
		(sexp :tag "S expression to search")
		(choice :tag "Regexp to substring candidate from heading"
			(cons regexp sexp) (const nil))
		(choice :tag "Regexp to split candidates"
			regexp (const nil))
		(choice :tag "Regexp to remove a string from candidates"
			regexp (const nil))))
  :group 'skk-lookup)

(defcustom skk-lookup-default-option-list
  '(exact exact prefix t ("$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" . 1) "$B!&(B" nil)
  ;; CRCEN: $B;0>JF2(B $B%K%e!<%;%s%A%e%j!<1QOB!&?7%/%i%&%sOB1Q<-E5(B
  ;; KANJIGEN: Super$BE}9g<-=q(B99 Disk2/$B4A;z8;(B : EPWING
  ;; RIKAGAKU: $BM}2=3X<-E5(B
  ;; WAEI: what's this?
  "*$B%G%U%)%k%H$N<-=q8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B$^$:<-=qL>$r%-!<$K$7$F(B `skk-lookup-option-alist' $B$r0z$-!"$=$3$K<-=q8!:w!"J8;z@Z(B
$B$j=P$7$N%*%W%7%g%s$,8+$D$+$l$P$=$l$r;HMQ$7!"8+$D$+$i$J$+$C$?>l9g$K$3$NJQ?t$G(B
$B;XDj$5$l$k<-=q8!:w!"J8;z@Z$j=P$7$N%*%W%7%g%s$r;HMQ$9$k!#(B

$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(B
  1th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B%*%W%7%g%s$r;XDj$7$F$$$J(B
       $B$$$H$-(B ($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B) $B$N(B
       search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$H!"Aw$j(B
       $B$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B$G$"$k(B ($BAw$j2>L>7hDj$NA0(B
       $B$K8!:w$r3+;O$7$F$*$j!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B prefix
       $B$r=|$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B) $B$H$-$N(B search method
       $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49$N:]$O$=(B
       $B$N<-=q$r8!:w$7$J$$!#(B
  3th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  4th: `lookup-entry-heading' $B$,JV$9(B heading $B$+$i8uJd$H$7$F=PNO$9$kJ8;zNs$r@Z$j(B
       $B=P$9$?$a$N(B regexp $B;XDj5Z$S@Z$j=P$7%*%W%7%g%s!#(B
       car $B$K(B regexp $B$r<($9J8;zNs!"(Bcdr $B$K(B `match-string' $B$KEO$9(B count $B$r;XDj$9(B
       $B$k(B (4th $B$KJ8;zNs$@$1$r;XDj$7$?>l9g$O(B `match-string' $B$K$O(B 1 $B$,EO$5$l$k(B)$B!#(B
       cdr $BIt$K(B S $B<0$r;XDj$9$k$3$H$b2DG=!#2<5-$N$h$&$K(B cond $B<0$G>r7oH=Dj$9$l$P(B
       $BJ#?t$N(B regexp $B$r(B or $B;XDj$9$k$3$H$,2DG=!#(B

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
	        ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr $BIt$NI>2A7k2L$,?t;z$N%j%9%H$K$J$k$H$-$O!"$=$N?t;z$r=g$K(B `match-string'
       $B$KEO$7$FJ8;zNs$r@Z$j=P$7!"$=$l$iO"7k$7$?J8;zNs$r8uJd$H$7$FJV$9!#Nc$($P!"(B

          (cond ((match-beginning 5) '(6 5)))

       $B$H;XDj$9$k$H!"(B(match-beginning 5) $B$,(B non-nil $B$K$J$C$?>l9g!"(B
       (match-string 6) $B$H(B (match-string 5) $B$r$=$N=g$KO"7k$7$?J8;zNs$r8uJd$H$7(B
       $B$F=PNO$9$k!#(B
       $B@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(B4th $B$K(B nil $B$r;XDj$9$k!#(B
  5th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B
        regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B
  6th: $B@Z$j=P$5$l$?J8;zNs$+$iFCDj$NJ8;zNs$r<h$j=|$/>l9g$K;XDj$9$k(B regexp$B!#(B
       $B<-=q$N=PNO$,<-=qFCM-$N5-9fJ8;z$r4^$`>l9g$K;XDj$9$k!#(B

$B$3$N%*%W%7%g%s$GBP1~$7$F$$$k<-=qL>$O!"(B\"CRCEN\", \"KANJIGEN\", \"RIKAGAKU\"
$B5Z$S(B \"WAEI\".
`lookup-entry-heading' $B$G<h$j=P$7$?J8;zNs$,2<5-$N$h$&$K$J$k$3$H$rA0Ds$K(B
$B$7$F$$$k!#(B

  \"$B$"!>$+!Z0!2J![!E%/%o(B\"
  \"$B$"$+!Zod2@![(B\"
  \"$B$3!>$7$g$&!Z>.@+!&>.@-![!E%7%d%&(B\"

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 (skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact)"
  :type '(list (choice :tag "Search method for okuri nasi"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (choice
		:tag "Search method for okuri ari (not process okuri early)"
		(const exact) (const prefix)
		(const suffix) (const substring)
		(const keyword) (const text)
		(const nil))
	       (choice :tag "Search method for okuri ari (process okuri early)"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (sexp :tag "S expression to search")
	       (choice :tag "Regexp to substring candidate from heading"
		       (cons regexp sexp) (const nil))
	       (choice :tag "Regexp to split candidates"
		       regexp (const nil))
	       (choice :tag "Regexp to remove a string from candidates"
		       regexp (const nil)))
  :group 'skk-lookup)

(defcustom skk-lookup-search-modules nil
  "*$B8!:w%b%8%e!<%k$N@_Dj$N%j%9%H!#(B"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup)

(defcustom skk-lookup-process-henkan-key-function nil
  "*Lookup $B$KEO$9:]$K8!:w%-!<$r2C9)$9$k%U%!%s%/%7%g%s!#(B
$BAw$j$"$jJQ49$N:]$N$_%3!<%k$5$l$k!#0z?t$O2C9)$9$Y$-J8;zNs(B HENKAN-KEY$B!#(B
$BJV$jCM$O(B car $B$K2C9)$7$?J8;zNs!"(Bcdr $B$KAw$j2>L>$N2C9)J}K!$r<($9%^%8%C%/%J%s%P!<(B
$B$rF~$l$?(B cons cell$B!#(B
$B%^%8%C%/%J%s%P!<$O!"(B0 $B$,Aw$j$J$7$rI=$o$9(B ($BK\(B function $B$G$O;HMQ$9$k$3$H$O$J$$(B)$B!#(B
1 $B$OAw$j$"$jJQ49$G(B `skk-process-okuri-early' $B$,(B nil $B$N>l9g!#(B
2 $B$OAw$j$"$jJQ49$G(B `skk-process-okuri-early' $B$,(B non-nil $B$N>l9g$rI=$o$9!#(B
$B6a$$>-Mh!"(Bskk-lookup.el $BA4BN$rDL$8$F$3$N$h$&$J%^%8%C%/%J%s%P!<$r;H$o$J$$$h$&$K(B
$B2~NI$5$l$k2DG=@-$,$"$k!#(B"
  :type '(radio (function :tag "$B4X?t(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-lookup)

(defcustom skk-lookup-kana-vector
  ["$B$!(B" "$B$"(B" "$B$#(B" "$B$$(B" "$B$%(B" "$B$&(B" "$B$'(B" "$B$((B" "$B$)(B" "$B$*(B"
   "$B$+(B" "$B$,(B" "$B$-(B" "$B$.(B" "$B$/(B" "$B$0(B" "$B$1(B" "$B$2(B" "$B$3(B" "$B$4(B"
   "$B$5(B" "$B$6(B" "$B$7(B" "$B$8(B" "$B$9(B" "$B$:(B" "$B$;(B" "$B$<(B" "$B$=(B" "$B$>(B"
   "$B$?(B" "$B$@(B" "$B$A(B" "$B$B(B" "$B$C(B" "$B$D(B" "$B$E(B" "$B$F(B" "$B$G(B" "$B$H(B" "$B$I(B"
   "$B$J(B" "$B$K(B" "$B$L(B" "$B$M(B" "$B$N(B"
   "$B$O(B" "$B$P(B" "$B$Q(B" "$B$R(B" "$B$S(B" "$B$T(B" "$B$U(B" "$B$V(B" "$B$W(B" "$B$X(B" "$B$Y(B" "$B$Z(B" "$B$[(B" "$B$\(B" "$B$](B"
   "$B$^(B" "$B$_(B" "$B$`(B" "$B$a(B" "$B$b(B"
   "$B$c(B" "$B$d(B" "$B$e(B" "$B$f(B" "$B$g(B" "$B$h(B"
   "$B$i(B" "$B$j(B" "$B$k(B" "$B$l(B" "$B$m(B"
   "$B$n(B" "$B$o(B" "$B$p(B" "$B$q(B" "$B$r(B" "$B$s(B"]
  "*`skk-kana-rom-vector' $B$N(B prefix $B$KBP1~$9$k$+$JJ8;z$N%Y%/%H%k!#(B
$B$"$k(B prefix $B$,$I$N$+$JJ8;z$KBP1~$9$k$+$N%^%C%W$r:n$k$?$a$K;2>H$9$k!#(B"
  :type 'sexp
  :group 'skk-lookup)

(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)
(defvar skk-lookup-prefix-and-kana-map nil)

(defvar skk-lookup-get-content-nth-dic 0
  "*$B4X?t(B `skk-lookup-get-content' $B$N=hM}BP>]$r?tCM$G;XDj$9$k(B.
$B?tCM$O!V4X?t(B `skk-lookup-default-module' $B$NI>2A7k2L$N$&$A2?HVL\$N(B agent $B$r(B
$B;HMQ$9$k$+!W$r!"%<%m$r5/E@$K?t$($k(B.

*scratch* $B%P%C%U%!$G<!$N(B S $B<0$rI>2A$7$F$_$k$H$h$$(B.
\(let ((n 0))
  (dolist (i (lookup-module-dictionaries (skk-lookup-default-module)))
    (insert (format \"%d %s\" n (lookup-dictionary-name i)) 10) ;10$B$O2~9T(B
    (setq n (1+ n))))

$B$J$*!"(BDDSKK $B$N5/F08e$KJQ?t$NCM$rJQ99$7$?>l9g$O!"(B*scratch* $B%P%C%U%!$G(B
$B4X?t(B `skk-lookup-get-content-setup-dic' $B$rI>2A$9$k$3$H(B.")

(defvar skk-lookup-get-content-default-dic nil)
(defvar skk-lookup-get-content-default-dic-name nil)

;;; skk-num.el related.
(defcustom skk-use-numeric-conversion t
  "*Non-nil $B$G$"$l$P!"?tCMJQ49$r9T$&!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-non-numeric-prog-list nil
  "*$B?tCMJQ49$K;H$o$J$$<-=q8!:w%W%m%0%i%`$N%j%9%H!#(B
`skk-use-numeric-conversion' $B$,(B non-nil $B$N>l9g$N$_M-8z!#%j%9%H$NMWAG$H$7$F$O!"(B

1. $B%W%m%0%i%`$N4X?tL>$rI=$9%7%s%\%k(B
2. $B%W%m%0%i%`$r0z?t$NCM$^$G;XDj$7$?7A$N%j%9%H(B

$B$N$$$:$l$G$b;XDj$G$-$k!#(B

$BA0<T$G$O!"4X?tL>$N0lCW$7$?A4%W%m%0%i%`$,0lCW$HH=CG$5$l$k!#8e<T$O(B
`skk-search-prog-list' $B$NMWAG$HF1$8=q<0$GI=$5$l!"F1%j%9%H$NMWAG$H4X?tL>5Z$S(B
$B$9$Y$F$N0z?t$,0lCW$7$?>l9g$N$_0lCW$HH=CG$5$l$k!#(B

$B0lCW$NI>2A$O!"(B 1 $B$O4X?t(B `eq' $B$K$h$C$F!"(B 2 $B$O%j%9%H$KBP$7$F(B `equal' $B$K$h$C$F(B
$B9T$o$l$k!#(B

 ($B@_DjNc(B)

 (setq skk-non-numeric-prog-list
       '(skk-look
	 skk-tankan-search
	 (skk-search-jisyo-file \"/usr/share/skk/SKK-JISYO.jinmei\" 10000)))
"
  :type '(repeat (radio (symbol :tag "$B4X?tL>$N$_$G;XDj(B")
			(list :tag "$B4X?tL>$H0z?t$N%j%9%H(B")))
  :group 'skk-num)

(defcustom skk-show-num-type-info t
  "*Non-nil $B$J$i$P!"?tCMJQ49%(%s%H%j$N<-=qEPO?;~$KJQ49%?%$%W$N0FFb$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-type-alist
  '((0 . identity)
    (1 . skk-num-jisx0208-latin)
    (2 . skk-num-type2-kanji)
    (3 . skk-num-type3-kanji)
    (4 . skk-num-recompute)
    (5 . skk-num-type5-kanji)
    (8 . skk-num-grouping)
    (9 . skk-num-shogi))
  "*$B?tCM$NJQ49$N$?$a$N!"%$%s%G%/%9$HJQ49$K;HMQ$9$k4X?t$H$NO"A[%j%9%H!#(B
$B4X?t(B `skk-num-exp' $B$,;2>H$7$F$$$k!#(B
$B3FMWAG$O!"(B`($B%$%s%G%/%9(B . $B4X?tL>(B)' $B$H$$$&9=@.$K$J$C$F$$$k!#(B
$B%$%s%G%/%9$K$O!"Nc$($P8+=P$78l$,(B \"$BJ?@.(B#1$BG/(B\" $B$N$H$-!"(B`#' $B5-9f$ND>8e$KI=<((B
$B$5$l$k(B integer `1' $B$rBeF~$9$k!#(B

$B%$%s%G%/%9$H4X?t$N4X78(B ($B%G%U%)%k%HCM(B) $B$O2<5-$NDL$j!#(B
    0 -> $BL5JQ49(B
    1 -> $BA43Q?t;z$XJQ49(B
    2 -> $B4A?t;z(B ($B0L<h$j$"$j(B) $B$XJQ49(B
    3 -> $B4A?t;z(B ($B0L<h$j$J$7(B) $B$XJQ49(B
    4 -> $B$=$N?t;z$=$N$b$N$r%-!<$K$7$F<-=q$r:F8!:w(B
    5 -> $B4A?t;z(B ($B<j7A$J$I$G;HMQ$9$kJ8;z$r;HMQ(B) $B$XJQ49(B
    8 -> $B7e6h@Z$j$XJQ49(B (1,234,567)
    9 -> $B>-4}$G;HMQ$9$k?t;z(B (\"$B#3;M(B\" $B$J$I(B) $B$KJQ49(B"
  :type '(repeat (cons (radio :tag "$B%$%s%G%/%9(B"
			      (const 0)
			      (const 1)
			      (const 2)
			      (const 3)
			      (const 4)
			      (const 5)
			      (const 8)
			      (const 9))
		       (function :tag "$B4X?t(B")))
  :group 'skk-num)

(defcustom skk-num-convert-float nil
  "*Non-nil $B$G$"$l$P!"IbF0>.?tE@?t$r;H$C$?8+=P$78l$KBP1~$7$FJQ49$r9T$&!#(B
$B$3$NCM$r(B non-nil $B$K$9$k$3$H$G!"(B\"#.# /#1$B!%(B#1/#0$B7n(B#0$BF|(B/\" $B$J$I$N<-=q8+=P$7$,;HMQ(B
$B$G$-$J$/$J$k$N$G!"Cm0U!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-uniq (or (assq 4 skk-num-type-alist)
			    (and (assq 2 skk-num-type-alist)
				 (or (assq 3 skk-num-type-alist)
				     (assq 5 skk-num-type-alist)))) "\
*Non-nil $B$G$"$l$P!"0[$J$k?tCMI=8=$G$bJQ497k2L$,F1$8?tCM$r=EJ#$7$F=PNO$7$J$$!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-load-hook nil
  "*skk-num.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-num)

(defconst skk-num-alist-type1
  '((?0 . "$B#0(B") (?1 . "$B#1(B") (?2 . "$B#2(B") (?3 . "$B#3(B")
    (?4 . "$B#4(B") (?5 . "$B#5(B") (?6 . "$B#6(B") (?7 . "$B#7(B")
    (?8 . "$B#8(B") (?9 . "$B#9(B")
    (?. . "$B!%(B")	; $B>.?tE@!#(B(?. . ".") $B$NJ}$,NI$$?M$b$$$k$+$b(B...$B!#(B
    (?  . ""))
  "ascii $B?t;z$N(B char type $B$HA43Q?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B#1#9#9#5(B\" $B$N$h$&$JJ8;zNs$XJQ49$9$k:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type2
  '((?0 . "$B!;(B") (?1 . "$B0l(B") (?2 . "$BFs(B") (?3 . "$B;0(B")
    (?4 . "$B;M(B") (?5 . "$B8^(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?\  . ""))
  "ascii $B?t;z$N(B char type $B$H4A?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B0l6e6e8^(B\" $B$N$h$&$JJ8;zNs$XJQ49$9$k:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type3
  (append
   '((ju . "$B==(B") (hyaku . "$BI4(B") (sen . "$B@i(B")
     (man . "$BK|(B") (oku . "$B2/(B") (cho . "$BC{(B") (kei . "$B5~(B"))
   skk-num-alist-type2)
  "$B?t;z$N4A;zI=5-$rI=$9O"A[%j%9%H!#(B
\"1995\" -> \"$B@i6eI46e==8^(B\" $B$N$h$&$JJ8;zNs$XJQ49$9$k:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type5
  '((ju . "$B=&(B") (hyaku . "$BI4(B") (sen . "$Bot(B")
    (man . "$Bh_(B") (oku . "$B2/(B") (cho . "$BC{(B") (kei . "$B5~(B")
    (?0 . "$BNm(B") (?1 . "$B0m(B") (?2 . "$BFu(B") (?3 . "$B;2(B")
    (?4 . "$B;M(B") (?5 . "$B8`(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?\  . ""))
  "$B?t;z$N4A;zI=5-$rI=$9O"A[%j%9%H!#(B
\"1995\" -> \"$B0mot6eI46e=&8`(B\" $B$N$h$&$JJ8;zNs$XJQ49$9$k:]$KMxMQ$9$k!#(B")

(skk-deflocalvar skk-num-list nil
  "`skk-henkan-key' $B$NCf$K4^$^$l$k?t;z$rI=$9J8;zNs$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$rJQ49$9$k$H$-!"(B`skk-henkan-key' $B$O(B
\"$B$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$G$"$j!"(B`skk-num-list' $B$O(B (\"7\" \"10\") $B$H$J$k!#(B
\(buffer local)")

(defvar skk-num-recompute-key nil
  "#4 $B%?%$%W$N%-!<$K$h$j?tCM$N:F7W;;$r9T$C$?$H$-$N8!:w%-!<!#(B")

(defcustom skk-num-grouping-separator ","
  "#8 $B%?%$%W(B ($B7e6h@Z$j(B) $B$G;HMQ$9$k5-9f(B"
  :type 'string
  :group 'skk-num)

(defcustom skk-num-grouping-places 3
  "#8 $B%?%$%W(B ($B7e6h@Z$j(B) $B$r2?7e$G6h@Z$k$+(B"
  :type 'integer
  :group 'skk-num)

;;; skk-server.el related.
(defcustom skk-server-host (or (getenv "SKKSERVER") "localhost")
  "*$B<-=q%5!<%P$,5/F0$7$F$$$k%[%9%HL>Kt$O(B IP $B%"%I%l%9!#(B"
  :type `(radio (string :tag "$B%[%9%HL>(B"
			,(or (getenv "SKKSERVER") "localhost"))
		(const nil))
  :group 'skk-server)

(defcustom skk-server-prog (getenv "SKKSERV")
  "*$B<-=q%5!<%P%W%m%0%i%`L>!#(B
$B%U%k%Q%9$G=q$/!#(B
`skk-server-inhibit-startup-server' $B$,(B nil $B$N;~$K;2>H$5$l!"(B
$B$3$N%W%m%0%i%`$,(B SKK $B$h$j5/F0$5$l$k!#(B"
  :type '(radio (file :tag "$B<-=q%5!<%PL>(B")
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-file
  :group 'skk-server)

(defcustom skk-server-jisyo (getenv "SKK_JISYO")
  "*$B<-=q%5!<%P%W%m%0%i%`$KEO$9<-=q%U%!%$%kL>!#(B
$B%U%k%Q%9$G=q$/!#(B
`skk-server-inhibit-startup-server' $B$,(B nil $B$N;~$K;2>H$5$l$k!#(B
$B<-=q%U%!%$%k$N;XDjK!$O<-=q%5!<%P$K$h$j0[$J$k$N$GCm0U!#(B
  % skkserv jisyo
$B$N7A<0$N;~$N$_MxMQ$G$-$k%*%W%7%g%s$G$"$k!#(B"
  :type `(radio (file :tag "$B<-=q%U%!%$%kL>(B" ,(or skk-aux-large-jisyo ""))
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-file
  :group 'skk-server)

(defcustom skk-server-portnum (if (eq system-type 'windows-nt)
				  1178
				nil)
  "*Non-nil $B$G$"$l$P!"$=$NCM$r(B port number $B$H$7$F(B skkserv $B$H(B TCP $B@\B3$9$k!#(B
/etc/services $B$rD>@\=q$-49$($k8"8B$,$J$$%f!<%6!<$N$?$a$NJQ?t!#(B
Microsoft Windows $B$G$O%G%U%)%k%HCM$H$7$F(B 1178 $B$,@_Dj$5$l$k!#(B"
  :type '(radio (integer :tag "$B%]!<%HHV9f(B" 1178)
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-server)

(defcustom skk-servers-list nil
  "*$B<-=q%5!<%PKh$N>pJs%j%9%H!#(B

$BJ#?t$N%[%9%H$GF0$$$F$$$k<-=q%5!<%P$K%"%/%;%9$G$-$k>l9g$K$O!"0J2<$N$h$&$K%j%9%H$N(B
$B3FMWAG$K=g$K%[%9%HL>!"%U%k%Q%9$G$N<-=q%5!<%PL>!"<-=q%5!<%P$KEO$9<-=q%U%!%$%kL>!"(B
$B<-=q%5!<%P$,;HMQ$9$k%]!<%HHV9f$r=q$-!"@_Dj$r$9$k$3$H$,$G$-$k!#(B

   (setq skk-servers-list
         '((\"host1\" \"/path/to/skkserv\" \"/path/to/SKK-JISYO.L\" 1178)
           (\"host2\" \"/path/to/skkserv\")))

$B$3$N>l9g!":G=i$K;XDj$7$?<-=q%5!<%P$K%"%/%;%9$G$-$J$/$J$k$H!"<+F0E*$K=g<!%j%9%H$K$"(B
$B$k;D$j$N<-=q%5!<%P$K%"%/%;%9$9$k$h$&$K$J$k!#(B
$B<-=q%5!<%P$N%G%U%)%k%H$N<-=q5Z$S%]!<%HHV9f$r;HMQ$9$k>l9g$O(B nil $B$r;XDj$9$k$+!"(B
$B2?$b=q$+$J$$$GNI$$!#(B

$B$J$*!"%f!<%6!<<+?H$K<B9T8"8B$N$J$$<-=q%5!<%P$r;XDj$9$k>l9g$O!"(B

   (setq skk-servers-list '((\"host1\") (\"host2\")))

$B$N$h$&$K!"%[%9%HL>$@$1$r=q$/$3$H$,$G$-$k!#>e5-$N@_DjNc$G$O!"(Bhost1, host2 $B$K$*(B
$B$1$k(B skkserv $B%5!<%S%9$N(B TCP $B@\B3$N3+;O$N$_;n$_!"<-=q%5!<%P$N5/F0$O;n$_$J$$!#(B"
  :type '(repeat
	  (list (string :tag "$B%[%9%HL>(B")
		(radio :tag "$B<-=q%5!<%PL>(B"
			file
			(const :tag "$B;XDj$7$J$$(B" nil))
		(radio :tag "$B<-=q%U%!%$%k(B"
			file
			(const :tag "$B;XDj$7$J$$(B" nil))
		(radio :tag "$B%]!<%HHV9f(B"
			integer
			(const :tag "$B;XDj$7$J$$(B" nil))))
  :group 'skk-server)

(defcustom skk-server-report-response nil
  "*Non-nil $B$G$"$l$P!"<-=q%5!<%P$N1~Ez>u67$rJs9p$9$k!#(B
$B6qBNE*$K$O!"JQ49;~$K<-=q%5!<%P$NAw=P$9$kJ8;z$r<u$1<h$k$^$G$K(B
`accept-process-output' $B$r2?2s<B9T$7$?$+$r%(%3!<%(%j%"$KJs9p$9$k!#(B"
  :type 'boolean
  :group 'skk-server)

(defcustom skk-server-remote-shell-program
  (or (getenv "REMOTESHELL")
      (and (boundp 'remote-shell-program)
	   (symbol-value 'remote-shell-program))
      (cond
       ((eq system-type 'berkeley-unix)
	(if (file-exists-p "/usr/ucb/rsh")
	    "/usr/ucb/rsh"
	  "/usr/bin/rsh"))
       ((eq system-type 'usg-unix-v)
	(if (file-exists-p "/usr/ucb/remsh")
	    "/usr/ucb/remsh"
	  "/bin/rsh"))
       ((eq system-type 'hpux)
	"/usr/bin/remsh")
       ((eq system-type 'EWS-UX/V)
	"/usr/ucb/remsh")
       ((eq system-type 'pcux)
	"/usr/bin/rcmd")
       (t
	"rsh")))
  "*$B%j%b!<%H%7%'%k$N%W%m%0%i%`L>!#(B"
  :type 'file
  :group 'skk-server)

(defcustom skk-server-inhibit-startup-server t
  "*Non-nil $B$G$"$l$P(B `call-process' $B$G$N<-=q%5!<%P5/F0$r6X;_$9$k!#(B"
  :type 'boolean
  :group 'skk-server)

(defcustom skk-server-load-hook nil
  "*skk-server.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-server)

;;(defvar skk-server-debug nil
;;  "*Non-nil $B$G$"$l$P!"<-=q%5!<%P%W%m%0%i%`$r%G%#%P%C%0%b!<%I$G5/F0$9$k!#(B
;;$B%G%#%P%C%0!&%b!<%I$G(B skkserv $B$rAv$i$;$k$H!"$=$N$^$^(B foreground $B$GAv$j!"(B
;;$B%a%C%;!<%8$r=PNO$9$k!#%-!<%\!<%I$+$i3d$j$3$_$r$+$1$k$3$H$b$G$-$k!#(B")

(defconst skkserv-working-buffer " *skkserv*")
(defvar skkserv-process nil)

;;; skk-sticky related.
(defcustom skk-sticky-key nil
  "*$BJQ493+;O0LCV$b$7$/$OAw$j3+;O0LCV$N;XDj$r$9$k%-!<!#(B

$B%-!<$N@_DjJ}K!$O3dEv$F$k%-!<$N<oN`$K$h$C$F0[$J$j$^$9!#(B

1. $BI=<(2DG=$J%-!<(B

  \";\" $B$J$I$NI=<($,2DG=$J%-!<$N>l9g$O(B

    (setq skk-sticky-key \";\")

  $B$N$h$&$K(B string $B$r@_Dj$7$F2<$5$$!#(B`skk-sticky-key' $B$K@_Dj$7$?J8(B
  $B;z$=$N$b$N$rF~NO$7$?$$>l9g$O(B2$B2sB3$1$FBG$D$HF~NO$G$-$^$9!#(B

2. $BI=<($5$l$J$$%-!<(B

  \"$BL5JQ49(B\" $B$N$h$&$JI=<($rH<$o$J$$%-!<$N>l9g$O(B

    (setq skk-sticky-key [muhenkan])	; Windows $B4D6-$@$H(B [noconvert]

  $B$N$h$&$K$=$N%-!<$rI=$o$9(B vector $B$r@_Dj$7$F2<$5$$!#(B

3. $BF1;~BG80(B

  2$B$D$N%-!<$rF1;~$KBG80$9$k$3$H$G$bJQ490LCV$r;XDj$G$-$^$9!#Nc$($P(B
  \"f\" $B$H(B \"j\" $B$NF1;~BG80$G;XDj$9$k>l9g$O(B

    (setq skk-sticky-key '(?f ?j))

  $B$N$h$&$K(B character $B$N%j%9%H$r@_Dj$7$F2<$5$$!#(B"
  :type '(radio (string :tag "$BI=<(2DG=$J%-!<(B")
		(vector :tag "$BI=<($5$l$J$$%-!<(B" symbol)
		(list :tag "$BF1;~BG80(B" character character)
		(const :tag "$B;XDj$7$J$$(B" nil))
  :group 'skk-sticky)

(defcustom skk-sticky-double-interval 0.1
  "*$B$3$N;~4V0JFb$KBG80$5$l$?$b$N$rF1;~BG80$HH=Dj$9$k!#(B
$BC10L$OIC!#%G%U%)%k%H$O(B 0.1 $BIC!#(B"
  :type 'number
  :group 'skk-sticky)

;;; skk-study.el related.
(defcustom skk-study-file (if skk-user-directory
			      (expand-file-name "study" skk-user-directory)
			    (convert-standard-filename "~/.skk-study"))
  "*$B3X=,7k2L$rJ]B8$9$k%U%!%$%k!#(B"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-backup-file (if skk-user-directory
				     (expand-file-name "study.bak"
						       skk-user-directory)
				   (convert-standard-filename
				    "~/.skk-study.BAK"))
  "*$B3X=,7k2L$rJ]B8$9$k%U%!%$%k$N%P%C%/%"%C%W!#(B"
  :type 'file
  :group 'skk-study)

(defcustom skk-study-associates-number 5
  "*$BJ]B8$9$k4XO"8l$N?t!#(B"
  :type 'integer
  :group 'skk-study)

(defcustom skk-study-sort-saving nil
  "*Non-nil $B$G$"$l$P3X=,7k2L$r%=!<%H$7$F%;!<%V$9$k!#(B"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-check-alist-format nil
  "*Non-nil $B$G$"$l$P!"3X=,7k2L$NFI$_9~$_;~$KO"A[%j%9%H$N%U%)!<%^%C%H$r%A%'%C%/$9$k!#(B"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-search-times 5
  "*$B8=:_$NJQ49%-!<$KBP$9$k4XO"JQ49%-!<$r$$$/$D$^$GAL$C$F8!:w$9$k$+!#(B"
  :type 'integer
  :group 'skk-study)

(defcustom skk-study-first-candidate t
  "*Non-nil $B$G$"$l$P!"Bh0l8uJd$G3NDj$7$?:]$b3X=,$9$k!#(B"
  :type 'boolean
  :group 'skk-study)

(defcustom skk-study-max-distance 30
  "*$BD>A0$K3NDj$7$?%]%$%s%H$H:#2s$NJQ49%]%$%s%H$,$3$N5wN%0J>eN%$l$F$$$k$H3X=,$7$J$$!#(B
nil $B$N>l9g$OD>A0$K3NDj$7$?%]%$%s%H$H$N5wN%$r9MN8$;$:$K3X=,$9$k!#(B"
  :type '(radio integer (const nil))
  :group 'skk-study)

;;; system internal variables and constants.
;; global variable
(defconst skk-study-file-format-version "0.2")
(defvar skk-kakutei-end-function nil)
(defvar skk-study-alist nil)
(defvar skk-study-data-ring nil
  "$BD>A0$N(B `skk-study-search-times' $B8DJ,$NJQ49%-!<$H3NDj8l%G!<%?!#(B
ring.el $B$rMxMQ$7$F$*$j!"6qBNE*$K$O!"2<5-$N$h$&$J9=B$$K$J$C$F$$$k!#(B

\(2 3 . [\(\"$B$3$&$>$&(B\" . \"$B9=B$(B\"\)\
 \(\"$B$0$?$$$F$-(B\" . \"$B6qBNE*(B\"\) \(\"$B$+$-(B\" . \"$B2<5-(B\"\)]\)")

(defvar skk-study-last-save nil)
(defvar skk-study-last-read nil)

;;; skk-tankan.el related.
(defcustom skk-tankan-search-key ?@
  "*$BC14A;zJQ49$r9T$&%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-jisx0213
  :group 'skk-tankan)

;;; $BJ8;z=89g$NJ8;z$KBP$7$F(B ($BIt<s(B $BIt<sFb2h?t(B $BAm2h?t(B) $B$rJV$94X?t$N(B alist
;; $BJd=u4A;zEy$K$b0l1~BP1~2DG=$J$h$&$KJQ?t$K$7$F$"$k(B
(defvar skk-tankan-get-char-data-functions
  '((japanese-jisx0208 . skk-tankan-get-char-data-0213-1)
    (japanese-jisx0213-1 . skk-tankan-get-char-data-0213-1)
    (japanese-jisx0213-2 . skk-tankan-get-char-data-0213-2)))

(put 'annotation 'char-table-extra-slots 0)
(defvar skk-tankan-annotation-table
  (make-char-table (if (featurep 'xemacs)
		       'generic
		     'annotation)))

(defvar skk-tankan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'skk-tankan-mode-prev)
    (define-key map "k" 'skk-tankan-mode-prev)
    (define-key map "n" 'skk-tankan-mode-next)
    (define-key map "j" 'skk-tankan-mode-next)
    (define-key map "w" 'skk-tankan-mode-copy)
    (define-key map "q" 'skk-tankan-mode-quit)
    (define-key map "?" 'skk-tankan-mode-usage)
    (define-key map "$" 'skk-tankan-mode-display-code)
    map)
  "Keymap used in skk-tankan mode.")

(defvar skk-tankan-mode-original-window-configuration nil
  "")

(defface skk-tankan-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-tankan-mode $B$N(B face $BB0@-!#(B"
  :group 'skk-tankan
  :group 'skk-visual)

(defface skk-tankan-radical-name-face
  '((((class color) (type tty))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((class color) (background dark))
     (:inherit default))
    (((class grayscale))
     (:inherit default)))
  "*skk-tankan-bushu-compread() $B$G;HMQ$9$k!VIt<s$NFI$_!W$N(B face $BB0@-!#(B"
  :group 'skk-tankan
  :group 'skk-visual)

(defvar skk-tankan-overlay nil)

;;; skk-tooltip related.
(defcustom skk-show-tooltip nil
  "*Non-nil $B$G$"$l$P!"%(%3!<%(%j%"$NBe$o$j$K(B tooltip $B$G8uJd$J$I$rI=<($9$k!#(B
$B$3$N5!G=$O(B GNU Emacs 21 $B0J>e$H(B XEmacs 21.5 $B0J>e$GF0:n$9$k!#(B
XEmacs 21.4 $B$G$O%(%i!<$K$J$i$J$$$+$b$7$l$J$$$,!"6K$a$FIT40A4$JF0:n$7$+$7$J$$!#(B"
  :type 'boolean
  :group 'skk-basic
  :group 'skk-tooltip)

(defcustom skk-tooltip-hide-delay 1000
  "*tooltip $B$r;H$C$F8uJd$J$II=<($9$k>l9g$K!"I=<($9$k;~4V(B ($BIC(B)$B!#(B
$B$3$N;~4V$,7P2a$9$k$H<+F0E*$K(B tooltip $B$O>C$($k!#(B"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-face nil
  "*$B%D!<%k%F%#%C%W$KI=<($9$kJ8;zNs$KE,MQ$9$k%U%'%$%9$r;XDj$9$kJQ?t!#(B
$B8uJdJ8;zNs$N%U%'%$%9B0@-!J(B`skk-treat-candidate-appearance-function' $B$K$h$k(B
$B2C9)$J$I!K$r$=$N$^$^;H$$$?$$>l9g$O(B nil $B$K@_Dj$9$k!#(B

 ($B@_DjNc(B)

 (setq skk-tooltip-face 'font-lock-doc-face)"
  :type '(radio (face :tag "$B%U%'%$%9$r;XDj(B" tooltip)
		(const :tag "$B8uJdJ8;zNs$N%U%'%$%9B0@-$r$=$N$^$^;HMQ(B" nil))
  :group 'skk-henkan
  :group 'skk-tooltip)

(defcustom skk-tooltip-parameters nil
  "*tooltip $B$r;H$&>l9g$N(B SKK $BFH<+$N(B tooltip $B%U%l!<%`%Q%i%a!<%?@_Dj!#(B

 ($B@_DjNc(B)

 (setq skk-tooltip-parameters
       '((foreground-color . \"navy blue\")
	 (background-color . \"alice blue\")
	 (border-color . \"royal blue\")
	 (border-width . 1)))
"
  :type '(radio (const :tag "$B@_DjNc$r;n$9(B"
		       ((foreground-color . "navy blue")
			(background-color . "alice blue")
			(border-color . "royal blue")
			(border-width . 1)))
		(repeat :tag "$BG$0U$N@_Dj(B"
			(cons (symbol :tag "$B%Q%i%a!<%?L>(B")
			      (sexp :tag "$BCM(B (S$B<0(B)"))))
  :group 'skk-tooltip)

(defcustom skk-tooltip-mouse-behavior
  (cond ((featurep 'xemacs)
	 'banish)
	(t
	 'banish))
  "*Tooltip $B$rI=<($9$k>l9g$N!"%^%&%9%]%$%s%?$N5sF0!#(B
`follow' $B$J$i$P!"(B tip $B$N0LCV$K0\F0$9$k!#(B
`avoid' $B$J$i$P!"%&%#%s%I%&$NC<$KB`Hr$9$k!#(B
`avoid-maybe' $B$J$i$P!"%&%#%s%I%&>e$K$"$k%^%&%9%]%$%s%?$N$_B`Hr$9$k!#(B
`banish' $B$J$i$P!"%&%#%s%I%&$NC<$KB`Hr$7$?$^$^5"$C$F$3$J$$!#(B
`nil' $B$J$i$P!"B`Hr$7$J$$!#$3$N>l9g!"(Btip $B$N%F%-%9%H$H%^%&%9%]%$%s%?$,(B
$B=E$J$C$?$j!"$&$^$/(B tip $B$,I=<($G$-$J$+$C$?$j$9$k$N$GCm0U!#(B

$B$3$N5!G=$O(B GNU Emacs 22 $B0J>e$+(B XEmacs 21.5 $B0J>e$GF0:n$9$k!#(B"
  :type '(radio (const :tag "Tip $B$K=>$&(B" follow)
		(const :tag "$B%&%#%s%I%&$NC<$KF($2$k(B" avoid)
		(const :tag "$BF($2$?$[$&$,$h$5$=$&$J$H$-$@$1F($2$k(B" avoid-maybe)
		(const :tag "$BF($2$?$^$^5"$i$J$$(B" banish)
		(const :tag "$B5o:B$k(B" nil))
  :group 'skk-tooltip)

(defcustom skk-tooltip-x-offset
  (if (featurep 'xemacs)
      0
    (/ (1+ (frame-char-height)) 2))
  "*Tooltip $B$NI=<(0LCV$r1&$K$:$i$9%T%/%;%k?t!#(B
$BIi$N@0?t$r;XDj$9$k$H:8$K$:$l$k!#(B"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-y-offset 0
  "*Tooltip $B$NI=<(0LCV$r2<$K$:$i$9%T%/%;%k?t!#(B
$BIi$N@0?t$r;XDj$9$k$H>e$K$:$l$k!#(B"
  :type 'integer
  :group 'skk-tooltip)

(defcustom skk-tooltip-function
  (lambda (tooltip-str)
    (skk-tooltip-show-at-point tooltip-str 'listing))
  "*Tip $BIA2h5!9=$N4X?t$r;XDj$9$k!#(B
$B%G%U%)%k%H$G$O(B Emacs $BI8=`$N(B Tooltip $B$r;HMQ$9$k!#(B
$BB>$N(B Tip $BIA2h5!9=(B $B$G$"$k(B pos-tip $B$d(B popup-tip $B$b;XDj$G$-$k!#(B"
  :type 'function
  :group 'skk-tooltip)

;;; skk-tut.el related.
(defcustom skk-tut-file
  (cond ((featurep 'xemacs)
	 (or (locate-data-file "SKK.tut")
	     "/usr/local/share/skk/SKK.tut"))
	((fboundp 'locate-file)
	 (or (locate-file "skk/SKK.tut"
			  (list (expand-file-name "../../.."
						  data-directory)))
	     (locate-file "skk/SKK.tut" (list data-directory))
	     "/usr/local/share/skk/SKK.tut"))
	(t
	 "/usr/local/share/skk/SKK.tut"))
  "*SKK $BF|K\8l%A%e!<%H%j%"%k$N%U%!%$%kL>(B ($B%Q%9$r4^$`(B)$B!#(B"
  :type 'file
  :group 'skk-tut)

(defcustom skk-tut-lang "Japanese"
  "*SKK $B%A%e!<%H%j%"%k$GMQ$$$k8@8l!#(B
\\[universal-argument] \\[skk-tutorial] $B$K$h$k8@8l;XDj$O!"$3$NJQ?t$h$j$bM%@h(B
$B$9$k!#(B"
  :type '(radio (string "Japanese")
		(string "English"))
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       (set-default symbol value))
	   (setq skk-tut-current-lang nil)))
  :group 'skk-tut)

(defvar skk-tut-file-suffix-alist
  `(("Japanese" . "")
    ("English" . ".E"))
  "Alist of (LANGUAGE . suffix) pairs.
For example, if filename of the Japanese version is \"SKK.tut\",
then filename of the English version will be \"SKK.tut.E\".")

(defvar skk-tut-current-lang nil)

(defcustom skk-tut-use-face skk-use-face
  "*Non-nil $B$G$"$l$P!"%A%e!<%H%j%"%k$G(B face $B$rMxMQ$7$FI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-tut)

(defface skk-tut-section-face
  '((((class color) (background light))
     (:foreground "yellow" :background "dodgerblue"))
    (((class color) (background dark))
     (:foreground "yellow" :background "slateblue"))
    (((class grayscale))
     (:bold t) (:italic t)))
  "*$B%A%e!<%H%j%"%kCf$N%;%/%7%g%s$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-tut)

(defface skk-tut-do-it-face
  '((((class color) (background light))
     (:foreground "DarkGoldenrod"))
    (((class color) (background dark))
     (:foreground "LightGoldenrod"))
    (((class grayscale))
     (:bold t)))
  "*$B%A%e!<%H%j%"%kCf$N;X<(9`L\$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-tut)

(defface skk-tut-question-face
  '((((class color) (background light))
     (:foreground "Blue"))
    (((class color) (background dark))
     (:foreground "LightSkyBlue"))
    (((class grayscale))
     (:underline t)))
  "*$B%A%e!<%H%j%"%kCf$NLdBj$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-tut)

(defface skk-tut-key-bind-face
  '((((class color) (background light))
     (:foreground "Firebrick"))
    (((class color) (background dark))
     (:foreground "OrangeRed"))
    (((class grayscale))
     (:bold t)))
  "*$B%A%e!<%H%j%"%kCf$N%-!<%P%$%s%I$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-tut)

(defface skk-tut-hint-face
  '((((class color) (background light))
     (:foreground "CadetBlue"))
    (((class color) (background dark))
     (:foreground "Aquamarine"))
    (((class grayscale))
     (:italic t)))
  "*$B%A%e!<%H%j%"%kCf$N%R%s%H$NI=<(ItJ,$N(B face$B!#(B
$B8=:_$N$H$3$m!"(BSKK.tut.E $B$G$7$+;HMQ$5$l$F$$$J$$!#(B"
  :group 'skk-tut)

;;; skk-show-mode.el related.
(defvar skk-show-mode-invoked nil)
(defvar skk-show-mode-functions '((inline . skk-show-mode-inline)
				  (tooltip . skk-show-mode-tooltip)))
(defcustom skk-show-mode-show nil
  "*Non-nil $B$G$"$l$P!"$+$J%b!<%I$d%"%9%-!<%b!<%I$X@Z$jBX$o$C$?$H$-$K%+!<%=%kIU6a$K(B skk-*-mode-string $B$rI=<($9$k!#(B

$BI=<(%9%?%$%k$O(B `skk-show-mode-style' $B$G;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-visual)

(defvar skk-show-mode-enable t
  "$BFbItMQ!#%A%e!<%H%j%"%k<B9TCf$N$_(B nil $B$H$J$k!#(B")

(defcustom skk-show-mode-style 'inline
  "*skk-show-mode $B$NI=<(%9%?%$%k!#(B"
  :type '(radio (const :tag "tooltip" tooltip)
		(const :tag "inline" inline))
  :group 'skk-visual)

(defvar skk-show-mode-inline-overlays nil
  "$BFbIt%9%?%C%/MQ(B")

(defface skk-show-mode-inline-face
  '((((class color) (type tty))
     (:inherit default :background "gray"))
    (((class color) (background light))
     (:inherit default :background "gray"))
    (((class color) (background dark))
     (:inherit default :background "dark slate gray" :box t))
    (((class grayscale))
     (:inherit default)))
  "*inline $B8~$1$NGX7J?'(B"
  :group 'skk-visual)

;;; skk-get related.
(defvar skk-get-jisyo-direcroty "~/.emacs.d/skk-get-jisyo"
  ;; (expand-file-name "../../../skk" data-directory)
  "`skk-get'$B$NJ]B8@h(B")

;;; skk-search-web related.
(defvar skk-use-search-web nil
  "*Non-nil $B$G$"$l$P!"(Bskk-search-web $B$rM-8z$K$9$k(B.")

;; XXX workaround
;; face $B$N(B property $B$,0lIt$N>u67$GH?1G$5$l$J$$$3$H$KBP=h(B
(when (and (not noninteractive)
	   window-system)
  (dolist (f '(skk-tut-section-face
	       skk-tut-section-face
	       skk-tut-do-it-face
	       skk-tut-question-face
	       skk-tut-key-bind-face
	       skk-tut-hint-face))
      (set-face-foreground f (face-foreground f))))

;;; skk-viper.el related.
(defcustom skk-use-viper nil
  "*Non-nil $B$G$"$l$P!"(BVIPER $B$KBP1~$9$k!#(B"
  :type 'boolean
  :group 'skk-viper)

(defvar skk-viper-saved-cursor-color
  (when (and (featurep 'viper)
	     (boundp 'viper-insert-state-cursor-color))
    (symbol-value 'viper-insert-state-cursor-color)))
(make-variable-buffer-local 'viper-insert-state-cursor-color)

(defconst skk-viper-use-vip-prefix
  (not (fboundp 'viper-normalize-minor-mode-map-alist)))

(defconst skk-viper-normalize-map-function
  (if skk-viper-use-vip-prefix
      'vip-normalize-minor-mode-map-alist
    'viper-normalize-minor-mode-map-alist)
  "Viper $B$,(B `minor-mode-map-alist' $B$rD4@0$9$k$?$a$N4X?t!#(B")

(provide 'skk-vars)

;;; skk-vars.el ends here
