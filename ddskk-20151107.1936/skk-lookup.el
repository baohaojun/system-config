;;; skk-lookup.el --- SKK lookup gateway -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-lookup.el,v 1.41 2013/01/13 09:45:48 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Created: Sep. 23, 1999
;; Last Modified: $Date: 2013/01/13 09:45:48 $

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

;;; Commentary

;; Keisuke Nishida <kxn30@po.cwru.edu> さんの作られた辞書検索ツール
;; Lookup と SKK との gateway を行い、Lookup で検索できる辞書を使っ
;; て候補を出力するプログラムです。
;;
;; <HOW TO INSTALL>
;; make を実行する際に、lookup.el にパスが通っていて require できる
;; ときは、本プログラムも自動的にインストールされます。lookup.el が
;; インストールされているのに Emacs が検出してくれないときは、
;; SKK-CFG を編集して ADDITIONAL_LISPDIR にそのパスを書くと良
;; いでしょう。
;;
;; <HOW TO USE>
;; 当然ですが、Lookup がインストールされていて、かつ、対応する辞書が
;; マウントされていないと使えません。
;;
;; 次のように skk-search-prog-list に加えて指定し使用します。
;; SKK が用意している検索プログラムの中で最も重いので、
;; skk-seach-server の検索の後に持ってくるのがセオリーです。
;;
;;  (setq skk-search-prog-list
;;        '((skk-search-jisyo-file skk-jisyo 0 t)
;;          (skk-search-server skk-aux-large-jisyo 10000)
;;          (skk-lookup-search)))
;;
;; ディフォルトの設定では、lookup の変数である `lookup-search-agents'
;; をコピーして ndkks, ndcookie, ndnmz を取り去り、
;; `skk-lookup-search-agents' にセットしてこれを検索するようにしてい
;; ます。もちろん lookup の検索とは異なる設定を
;; `skk-lookup-search-agents' に明示することも可能です。
;;
;; `lookup-entry-heading' が返す heading (辞書見出し。辞書毎にフォーマ
;; ットが異なる) から正規表現を使い、候補として出力する文字列を切り出し
;; ています。現在対応している辞書は下記の通り (`lookup-dictionary-name'
;; が返す値で標記しています) ですが、下記に記載のない辞書でも正規表現を
;; 指定することで使用可能です。
;;
;;    "CHIEZO" ;知恵蔵
;;    "CHUJITEN" ;辞・典・盤
;;    "COLLOC" ;
;;    "CRCEN" ;三省堂 ニューセンチュリー英和・新クラウン和英辞典
;;    "GENIUS" ; ジーニアス英和, ジーニアス英和・和英辞典
;;    "GN99EP01" ;Super統合辞書99 Disk1/現代用語の基礎知識
;;    "GN99EP02" ;Super統合辞書99 Disk2/現代用語の基礎知識
;;    "IWAKOKU" ;岩波国語辞典
;;    "KANJIGEN"; Super統合辞書99 Disk2/漢字源 : EPWING
;;    "KANWA";
;;    "KOJIEN" ; 広辞苑第5版(岩波,EPWING)
;;    "KOKUGO" ;三省堂 日本語辞典（現代国語、外来語）
;;    "KOUJIEN"; 広辞苑第4版(岩波,EPWING) マルチメディア版
;;    "MYPAEDIA" ;「辞・典・盤」附属のマイペディア
;;               ; mypaedia-fpw から生成した PC Success 版マイペディア
;;                 (FreePWING 辞書)
;;    "NEWANC" ; ニューアンカー英和
;;    "PLUS";
;;    "RIKAGAKU" ;理化学辞典
;;    "WAEI";
;;    "ispell";
;;    "jedict";
;;
;; ご自分で使用している辞書の出力が上手く取り込めないときは、
;; `skk-lookup-pickup-headings' を使用して例えば、
;;
;;   (skk-lookup-pickup-headings "こしょう" 'exact)
;;
;; などと評価して ("こしょう" の文字列部分は問題となっている検索対象と
;; 入れ替えましょう) `lookup-dictionary-name' と
;; `lookup-entry-heading' が返す値を参考に、`skk-lookup-option-alist'
;; に必要なリストを加えましょう。新たなリストを加えられたら是非
;; skk@ring.gr.jp 宛てに知せて下さい。default value に取り込みたいと思
;; います。よろしくお願いいたします。
;;
;; kakasi ("KAKASI" を利用する代りに skk-kakasi.el を使いましょう),
;; "ndcookie", "ndnmz" には対応していませんし、対応の必要はないと考え
;; ています (メリットがあれば教えて下さい)。
;;
;; 末尾ながら、Lookup を作られた Lookup Development Team の皆様、
;; Lookup の 原作者であり、本プログラムの開発にもいくつか貴重なご意見を
;; いただきました Keisuke Nishida <kxn30@po.cwru.edu> さん、開発の初期
;; からデバッグを手伝っていただいた、NEMOTO Takashi
;; <tnemoto@mvi.biglobe.ne.jp> さん、sphere <sphere@pop12.odn.ne.jp> さ
;; んに深く感謝いたします。

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'skk-num))

(require 'lookup)

(eval-and-compile
  (autoload 'lookup-vse-search-query "lookup-vse"))

(unless (fboundp 'skk-okurigana-prefix)
  (defalias 'skk-okurigana-prefix 'skk-auto-okurigana-prefix))

;;;; inline functions.
(defsubst skk-lookup-get-1 (name index)
  (let ((list
	 (cdr
	  (or (assoc name skk-lookup-option-alist)
	      (save-match-data
		;; If you search via ndtpd, book's name and slash are attached
		;; to NAME as prefix, like `IWANAMI/KOJIEN'.  The following
		;; forms will truncate it to `KOJIEN'.
		(when (string-match "/\\(.+\\)$" name)
		  (assoc (match-string 1 name) skk-lookup-option-alist)))))))
    (nth index (or list skk-lookup-default-option-list))))

(defsubst skk-lookup-get-method (name okuri-process)
  (let ((sex (skk-lookup-get-1 name okuri-process)))
    (cond ((symbolp sex) sex)
	  (t (eval sex)))))

(defsubst skk-lookup-get-nonsearch-sex (name)
  (skk-lookup-get-1 name 3))

(defsubst skk-lookup-get-pickup-regexp (name)
  (skk-lookup-get-1 name 4))

(defsubst skk-lookup-get-split-regexp (name)
  (skk-lookup-get-1 name 5))

(defsubst skk-lookup-get-cleanup-regexp (name)
  (skk-lookup-get-1 name 6))

;;;; funcitions.
;;;###autoload
(defun skk-lookup-search ()
  (unless (or skk-num-list
	      skk-num-recompute-key)
    ;; 数値変換のときは変換キーが `#' を含むものなので、lookup で検索しない。
    (let ((module (skk-lookup-default-module))
	  ;; if `lookup-enable-gaiji' is nil, gaiji tag like
	  ;; `<gaiji=za52a>' is put out.
	  ;;(lookup-enable-gaiji nil)
	  (lookup-gaiji-alternate "")
	  (henkan-key skk-henkan-key)
	  okuri-process v)
      (cond
       ((not (or skk-henkan-okurigana skk-okuri-char))
	;; okuri-nasi
	(setq okuri-process 0))
       ;; okuri-ari and `skk-lookup-process-henkan-key-function' is non-nil.
       (skk-lookup-process-henkan-key-function
	(setq v (funcall skk-lookup-process-henkan-key-function
			 henkan-key)
	      henkan-key (car v)
	      okuri-process (cdr v)))
       ;; okuri-ari and (not skk-process-okuri-early)
       (skk-henkan-okurigana
	;; 送り仮名のかな prefix を捨て、送り仮名を足して lookup に渡す。
	(setq henkan-key (concat (substring henkan-key
					    0 (1- (length henkan-key)))
				 skk-henkan-okurigana)
	      okuri-process 1))
       ;; okuri-ari and skk-process-okuri-early
       (skk-okuri-char
	;; 送り仮名のかな prefix を捨てて lookup に渡す。
	(setq henkan-key (substring henkan-key 0 (1- (length henkan-key)))
	      okuri-process 2)))
      (delete skk-henkan-key (skk-lookup-search-1 module henkan-key okuri-process)))))

(defun skk-lookup-search-1 (module key okuri-process)
  ;; search pattern.
  (let (name method entries candidates-string candidates-list)
    (setq lookup-search-pattern key)
    ;; setup modules.
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (when (and (lookup-dictionary-selected-p dictionary)
		  (setq name (lookup-dictionary-name dictionary))
		  (eval (skk-lookup-get-nonsearch-sex name))
		  (setq method (skk-lookup-get-method name okuri-process))
		  ;; valid method or not?
		  (memq method (lookup-dictionary-methods dictionary))
		  ;; actual search.
		  (setq entries (lookup-vse-search-query
				 dictionary
				 (lookup-make-query method
						    lookup-search-pattern))))
	 (lookup-foreach
	  (lambda (entry)
	    ;; pickup necessary string for SKK.
	    (setq candidates-string (lookup-entry-heading entry))
	    (if (or (skk-lookup-get-pickup-regexp name)
		    (skk-lookup-get-split-regexp name))
		(setq candidates-list
		      (nconc (skk-lookup-process-heading
			      name candidates-string okuri-process)
			     candidates-list))
	      (setq candidates-string (skk-lookup-process-okurigana
				       candidates-string
				       okuri-process))
	      (if (and candidates-string
		       (not (string= lookup-search-pattern candidates-string)))
		  (setq candidates-list (cons candidates-string
					      candidates-list)))))
	  entries)))
     ;; dictionaries to be searched.
     (lookup-module-dictionaries module))
    (nreverse candidates-list)))

(defun skk-lookup-process-okurigana (string process-type)
  (cond
   ((string= string "")
    ;; KOUJIEN has a heading like `ま‐き【真木・(GAIJI)・槙】'
    ;; As GAIJI cannot be processed by skk-lookup.el, the heading
    ;; is equal to `ま‐き【真木・・槙】' for skk-lookup.el.
    ;; It causes to produce a null string candidate.
    ;;   (split-string "真木・・槙" "・") -> ("真木" "" "槙")
    ;; So return nil if STRING is a null string.
    nil)
   ;; okuri-nasi
   ((zerop process-type)
    string)
   ;; okuri-ari
   (t
    (let* ((okuri-length
	    (cond
	     ;; has `skk-henkan-okurigana'.
	     ((= process-type 1) (length skk-henkan-okurigana))
	     ;; `skk-process-okuri-early' is non-nil.
	     ((= process-type 2)
	      ;; don't know exactly how long okurigana is.
	      ;; truncate length of one character anyway.
	      1)))
	   (okurigana (and (> (length string) okuri-length)
			   (substring string (- okuri-length)))))
      (cond (
	     ;; cannot detect okurigana in STRING.
	     (not okurigana) nil)
	    (skk-henkan-okuri-strictly
	     (and (string= skk-henkan-okurigana okurigana)
		  ;; cut okurigana off.
		  (substring string 0 (- okuri-length))))
	    ;; `skk-process-okuri-early' or not `skk-henkan-okuri-strictly'.
	    ((string= (skk-okurigana-prefix okurigana) skk-okuri-char)
	     ;; cut okurigana off.
	     (substring string 0 (- okuri-length))))))))

(defun skk-lookup-process-heading (name heading okuri-process-type)
  ;; heading しか取り出さないのはもったいない？  他にも情報を取り出し
  ;; ておいて、必要に応じて参照するか？
  (save-match-data
    (do* ((pickup (skk-lookup-get-pickup-regexp name))
	  (pickup-regexp (if (consp pickup) (car pickup)))
	  (match (if (consp pickup) (cdr pickup) 1))
	  (split-regexp (skk-lookup-get-split-regexp name))
	  (cleanup-regexp (skk-lookup-get-cleanup-regexp name))
	  (candidates-list (if (not pickup) (list heading)))
	  candidates-string)
	;; `だし【出し】【出し・〈出汁〉】【｛山車｝】' などのように
	;; 1 つの heading に対し、複数の切り出し作業が必要になる場合
	;; があるのでループで作業する。
	((or (string= heading "")
	     (and pickup-regexp
		  (not (string-match pickup-regexp heading))))
	 candidates-list)
      (setq match (eval match))
      (cond
       ((and pickup-regexp
	     (listp match))
	(setq candidates-string
	      (mapconcat (lambda (num)
			    (match-string-no-properties num heading))
			 match "")
	      ;; XXX MATCH が複数だったら、heading 切り出しは
	      ;; 一度だけしかできない...。
	      heading ""))
       (pickup-regexp
	(setq candidates-string (match-string-no-properties match heading)
	      heading (substring heading
				 (min (1+ (match-end match))
				      (length heading)))))
       (t
	;; XXX never be used?
	(setq candidates-string heading
	      heading "")))
      (when cleanup-regexp
	(while (string-match cleanup-regexp candidates-string)
	  (setq candidates-string
		(concat (substring candidates-string 0 (match-beginning 0))
			(substring candidates-string (match-end 0))))))
      (cond
       (split-regexp
	(lookup-foreach
	 (lambda (c)
	   (unless (string= lookup-search-pattern c)
	     (setq c (skk-lookup-process-okurigana c okuri-process-type))
	     (when c
	       (setq candidates-list (cons c (delete c candidates-list))))))
	 (split-string candidates-string split-regexp)))
	((string= lookup-search-pattern candidates-string)
	 nil)
	(t
	 (setq candidates-string (skk-lookup-process-okurigana
				  candidates-string okuri-process-type))
	 (when candidates-string
	   (setq candidates-list
		 (cons candidates-string
		       (delete candidates-string candidates-list)))))))))

;; The following four functions were imported from lookup.el and
;; lookup-types.el.
(defun skk-lookup-default-module ()
  (or skk-lookup-default-module
      (setq skk-lookup-default-module (car (skk-lookup-module-list)))))

(defun skk-lookup-module-list ()
  (or skk-lookup-module-list
      (setq skk-lookup-module-list
	    (mapcar 'skk-lookup-new-module (or skk-lookup-search-modules
					       '(("%SKK-EVERY" "")))))))
(defun skk-lookup-new-module (spec)
  (let ((name (car spec))
	(id-list (cdr spec))
	module agents match start)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (setq match (concat "^" (regexp-quote id))
			    start agents)
		      (lookup-foreach
		       (lambda (e)
			 (when (string-match match (lookup-agent-id e))
			   (setq agents (cons e agents))))
		       (skk-lookup-agent-list))
		      (when (eq start agents)
			(error "No match agent: %s" id)))
		    ;; get a list of agent-IDs
		    (lookup-nunique
		     (mapcar (lambda (id)
			       (string-match "^[^:]*" id)
			       (substring id 0 (match-end 0)))
			     id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun skk-lookup-agent-list ()
  (or skk-lookup-agent-list
      (progn
	(unless skk-lookup-search-agents
	  ;; copy-list is a C primitive of XEmacs, but FSFmacs has it
	  ;; in cl.el.
	  (setq skk-lookup-search-agents
		(let ((agents (copy-sequence lookup-search-agents))
		      e)
		  ;; use `skk-kakasi.el' instead of ndkks.
		  (setq agents (delete '(ndkks) agents))
		  (while (setq e (assq 'ndcookie agents))
		    (setq agents (delq e agents)))
		  (while (setq e (assq 'ndnmz agents))
		    (setq agents (delq e agents)))
		  agents)))
	(setq skk-lookup-agent-list
	      (mapcar 'lookup-new-agent skk-lookup-search-agents)))))

;; the following two are to check dictionary output of heading for
;; creating new regexp.
(defun skk-lookup-test-regexp (regexp place string)
  "Search STRING by REGEXP and pick up a part of STRING in PLACE."
  (string-match regexp string)
  (match-string-no-properties place string))

(defun skk-lookup-pickup-headings (pattern method)
  "Search PATTERN by METHOD.
METHOD は変数`lookup-search-methods'を参照のこと."
  (let ((module (skk-lookup-default-module))
	(lookup-gaiji-alternate "")
	;;lookup-enable-gaiji ;  not to put out gaiji.
	var)
    (lookup-module-setup module)
    (lookup-foreach
     (lambda (dictionary)
       (lookup-foreach
	(lambda (entry)
	  (setq var
		(nconc
		 (list
		  (list (lookup-dictionary-name dictionary)
			(lookup-dictionary-id dictionary)
			(lookup-entry-heading entry)
			;;(lookup-dictionary-command dictionary 'content entry)
			))
		 var)))
	(lookup-vse-search-query
	 dictionary (lookup-make-query method pattern))))
     (lookup-module-dictionaries module))
    var))

(defun skk-lookup-map-prefix-and-kana ()
  (let ((lenv (length skk-lookup-kana-vector))
	(n 0) kana prefix prefix-kana alist)
    (while (> lenv n)
      (setq kana (aref skk-lookup-kana-vector n)
	    prefix (aref skk-kana-rom-vector n)
	    prefix-kana (assoc prefix alist)
	    n (1+ n))
      (if prefix-kana
	  (setcdr prefix-kana (cons kana (cdr prefix-kana)))
	(setq alist (cons (cons prefix (list kana)) alist))))
    alist))

;;
;; 
(defun skk-lookup-get-content-setup-dic ()
  (interactive)
  (let ((module (skk-lookup-default-module)))
    (lookup-module-setup module)
    (setq skk-lookup-get-content-default-dic-name
	  (lookup-dictionary-name
	   (setq skk-lookup-get-content-default-dic
		 (nth skk-lookup-get-content-nth-dic
		      (lookup-module-dictionaries module))))))
  (message "skk-lookup-get-content: %s" skk-lookup-get-content-default-dic-name))

;;;###autoload
(defun skk-lookup-get-content (word &optional listing-p)
  (unless skk-lookup-get-content-default-dic
    (skk-lookup-get-content-setup-dic))
  (let* ((query (lookup-vse-search-query skk-lookup-get-content-default-dic
					(lookup-make-query 'exact word)))
	 (content (if query
		      (lookup-dictionary-command
		       skk-lookup-get-content-default-dic
		       'content (car query))
		    nil)))
    (when content
      (setq content (replace-regexp-in-string
		     "<[^>]*>" "" 
		     (if listing-p
			 (nth 1 (split-string content "\n"))
		       content))
	    content (format "%s [%s]"
			    content
			    skk-lookup-get-content-default-dic-name)))
    content))

(provide 'skk-lookup)

;;; skk-lookup.el ends here
