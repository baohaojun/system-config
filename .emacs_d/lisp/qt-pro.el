;;; qt-pro.el --- Qt Pro/Pri major mode

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Todd Neal <tolchz@gmail.com>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;
;; Commentary:
;;
;; Version 1.0 (7 January 2007)
;;
;; Based off simple.el
;;
;; Add the following to your .emacs to install
;; (require 'qt-pro)
;; (add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;;; Code:
(defvar qt-pro-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map [foo] 'qt-pro-do-foo)
		map)
	"Keymap for `qt-pro-mode'.")

(defvar qt-pro-mode-syntax-table
	(let ((st (make-syntax-table)))
		(modify-syntax-entry ?# "<" st)
		(modify-syntax-entry ?\n ">" st)
		st)
	"Syntax table for `qt-pro-mode'.")

(defvar qt-pro-font-lock-keywords
	`(("$$(\\s *\\w+\\s *)" (0 font-lock-function-name-face t))
		("$$\\[\\s *\\w+\\s *\\]" (0 font-lock-function-name-face t))
(,(regexp-opt '("basename" "contains" "count" "dirname" "error" "eval" "exists""find"
										"for" "include" "infile" "isEmpty" "join" "member" "message" "prompt" "quote"
										"replace" "sprintf" "system" "unique" "warning") `words)
		 (0 font-lock-function-name-face))
		(,(regexp-opt '("CONFIG" "DEFINES" "DEF_FILE" "DEPENDPATH" "DEPLOYMENT" "DESTDIR"
										"DESTDIR_TARGET" "DLLDESTDIR" "DISTFILES" "DSP_TEMPLATE" "FORMS"
										"FORMS3" "HEADERS" "INCLUDEPATH" "INSTALLS" "LEXIMPLS" "LEXOBJECTS"
										"LEXSOURCES" "LIBS" "LITERAL_HASH" "MAKEFILE" "MAKEFILE_GENERATOR"
										"MOC_DIR" "OBJECTS" "OBJECTS_DIR" "OBJMOC" "POST_TARGETDEPS"
										"PRE_TARGETDEPS" "PRECOMPILED_HEADER" "QMAKE" "QMAKESPEC"
										"QMAKE_APP_FLAG" "QMAKE_APP_OR_DLL" "QMAKE_AR_CMD" "QMAKE_BUNDLE_DATA"
										"QMAKE_BUNDLE_EXTENSION" "QMAKE_CC" "QMAKE_CFLAGS_DEBUG"
										"QMAKE_CFLAGS_MT" "QMAKE_CFLAGS_MT_DBG" "QMAKE_CFLAGS_MT_DLL"
										"QMAKE_CFLAGS_MT_DLLDBG" "QMAKE_CFLAGS_RELEASE" "QMAKE_CFLAGS_SHLIB"
										"QMAKE_CFLAGS_THREAD" "QMAKE_CFLAGS_WARN_OFF" "QMAKE_CFLAGS_WARN_ON"
										"QMAKE_CLEAN" "QMAKE_CXX" "QMAKE_CXXFLAGS" "QMAKE_CXXFLAGS_DEBUG"
										"QMAKE_CXXFLAGS_MT" "QMAKE_CXXFLAGS_MT_DBG" "QMAKE_CXXFLAGS_MT_DLL"
										"QMAKE_CXXFLAGS_MT_DLLDBG" "QMAKE_CXXFLAGS_RELEASE"
										"QMAKE_CXXFLAGS_SHLIB" "QMAKE_CXXFLAGS_THREAD"
										"QMAKE_CXXFLAGS_WARN_OFF" "QMAKE_CXXFLAGS_WARN_ON" "QMAKE_DISTCLEAN"
										"QMAKE_EXTENSION_SHLIB" "QMAKE_EXT_MOC" "QMAKE_EXT_UI" "QMAKE_EXT_PRL"
										"QMAKE_EXT_LEX" "QMAKE_EXT_YACC" "QMAKE_EXT_OBJ" "QMAKE_EXT_CPP"
										"QMAKE_EXT_H" "QMAKE_FAILED_REQUIREMENTS" "QMAKE_FILETAGS"
										"QMAKE_FRAMEWORK_BUNDLE_NAME" "QMAKE_FRAMEWORK_VERSION" "QMAKE_INCDIR"
										"QMAKE_INCDIR_OPENGL" "QMAKE_INCDIR_QT" "QMAKE_INCDIR_THREAD"
										"QMAKE_INCDIR_X11" "QMAKE_LFLAGS" "QMAKE_LFLAGS_CONSOLE"
										"QMAKE_LFLAGS_CONSOLE_DLL" "QMAKE_LFLAGS_DEBUG" "QMAKE_LFLAGS_PLUGIN"
										"QMAKE_LFLAGS_QT_DLL" "QMAKE_LFLAGS_RELEASE" "QMAKE_LFLAGS_SHAPP"
										"QMAKE_LFLAGS_SHLIB" "QMAKE_LFLAGS_SONAME" "QMAKE_LFLAGS_THREAD"
										"QMAKE_LFLAGS_WINDOWS" "QMAKE_LFLAGS_WINDOWS_DLL" "QMAKE_LIBDIR"
										"QMAKE_LIBDIR_FLAGS" "QMAKE_LIBDIR_OPENGL" "QMAKE_LIBDIR_QT"
										"QMAKE_LIBDIR_X11" "QMAKE_LIBS" "QMAKE_LIBS_CONSOLE"
										"QMAKE_LIBS_OPENGL" "QMAKE_LIBS_OPENGL_QT" "QMAKE_LIBS_QT"
										"QMAKE_LIBS_QT_DLL" "QMAKE_LIBS_QT_OPENGL" "QMAKE_LIBS_QT_THREAD"
										"QMAKE_LIBS_RT" "QMAKE_LIBS_RTMT" "QMAKE_LIBS_THREAD"
										"QMAKE_LIBS_WINDOWS" "QMAKE_LIBS_X11" "QMAKE_LIBS_X11SM"
										"QMAKE_LIB_FLAG" "QMAKE_LINK_SHLIB_CMD" "QMAKE_POST_LINK"
										"QMAKE_PRE_LINK" "QMAKE_LN_SHLIB" "QMAKE_MAC_SDK"
										"QMAKE_MACOSX_DEPLOYMENT_TARGET" "QMAKE_MAKEFILE" "QMAKE_MOC_SRC"
										"QMAKE_QMAKE" "QMAKE_QT_DLL" "QMAKE_RESOURCE_FLAGS" "QMAKE_RUN_CC"
										"QMAKE_RUN_CC_IMP" "QMAKE_RUN_CXX" "QMAKE_RUN_CXX_IMP" "QMAKE_TARGET"
										"QMAKE_UIC" "QT" "QTPLUGIN" "QT_VERSION" "QT_MAJOR_VERSION"
										"QT_MINOR_VERSION" "QT_PATCH_VERSION" "RC_FILE" "RCC_DIR" "REQUIRES"
										"RES_FILE" "SIGNATURE_FILE" "SOURCES" "SRCMOC" "SUBDIRS" "TARGET"
										"TARGET_EXT" "TARGET_x" "TARGET_x.y.z" "TEMPLATE" "TRANSLATIONS"
										"UICIMPLS" "UICOBJECTS" "UI_DIR" "UI_HEADERS_DIR" "UI_SOURCES_DIR"
										"VERSION" "VER_MAJ" "VER_MIN" "VER_PAT" "VPATH" "YACCIMPLS"
										"YACCOBJECTS" "YACCSOURCES" "LANGUAGE") `words) 
		 (0 font-lock-keyword-face)))
	"Keyword highlighting specification for `qt-pro-mode'.")

;;;###autoload
(define-derived-mode qt-pro-mode fundamental-mode "Qt-pro"
	"A major mode for editing Qt-pro files."
	:syntax-table qt-pro-mode-syntax-table
	(set (make-local-variable 'comment-start) "# ")
	(set (make-local-variable 'comment-start-skip) "#+\\s-*")
	(set (make-local-variable 'font-lock-defaults) '(qt-pro-font-lock-keywords))
	)



(provide 'qt-pro)
;;; qt-pro.el ends here