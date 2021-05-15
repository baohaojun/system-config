;;; skk-emacs.el --- GNU Emacs support for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2004 Masatake YAMATO <jet@gyve.org>
;; Copyright (C) 2004-2010 SKK Development Team

;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'skk)

(eval-when-compile
  (require 'cl-lib)
  (require 'ja-dic-utl)
  (require 'tooltip)

  (defvar tool-bar-border)
  (declare-function x-show-tip "xfns.c"))

(eval-and-compile
  (autoload 'mouse-avoidance-banish-destination "avoid")
  (autoload 'mouse-avoidance-point-position "avoid")
  (autoload 'mouse-avoidance-set-mouse-position "avoid")
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-emacs-modeline-menu-items
  (when window-system
    '("Daredevil SKK Menu"
      ["Hiragana"
       (skk-j-mode-on)
       :selected (and skk-j-mode (not skk-katakana))
       :style radio
       :keys nil
       :key-sequence nil]
      ["Katakana"
       (skk-j-mode-on t)
       :selected (and skk-j-mode skk-katakana)
       :style radio
       :keys nil
       :key-sequence nil]
      ["Hankaku alphabet"
       skk-latin-mode
       :selected skk-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      ["Zenkaku alphabet"
       skk-jisx0208-latin-mode
       :selected skk-jisx0208-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      "--"
      ["Find kanji by radicals" skk-tankan t]
      ["Show list of characters" (skk-list-chars nil) t]
      ["Lookup word in region or at point"
       skk-annotation-lookup-region-or-at-point t]
      ["SKK Clock" (skk-clock nil t) t]
      "--"
      ["Read Manual" skk-emacs-info t]
      ["Start Tutorial" skk-tutorial t]
      ["Customize SKK" skk-customize-group-skk t]
      ["Customize SKK (simple)" skk-customize t]
      "--"
      ["About Daredevil SKK.." skk-version t]
      ["Visit Web Site" skk-emacs-visit-website t])))

(defvar skk-emacs-menu-resource-ja
  '(("Daredevil SKK Menu" . "Daredevil SKK メニュー")
    ("Convert Region and Echo" . "領域を変換してミニバッファに表示")
    ("Gyakubiki" . "逆引き")
    ("to Hiragana" . "ひらがなに変換")
    ("to Hiragana, All Candidates" . "ひらがなに変換、全ての候補を表示")
    ("to Katakana" . "カタカナに変換")
    ("to Katakana, All Candidates" . "カタカナに変換、全ての候補を表示")
    ("Hurigana" . "ふりがな")
    ("Convert Region and Replace" . "領域を変換して置き換える")
    ("Hiragana" . "ひらがな")
    ("Katakana" . "カタカナ")
    ("Hiragana to Katakana" . "ひらがなをカタカナに変換")
    ("Katakana to Hiragana" . "カタカナをひらがなに変換")
    ("Kana and Zenkaku to Romaji" . "かな・カナ・全角をローマ字に変換")
    ("Ascii to Zenkaku" . "ASCII を全角英数に変換")
    ("Zenkaku to Ascii" . "全角英数を ASCII に変換")
    ("Count Jisyo Candidates" . "辞書中の候補数を数える")
    ("Save Jisyo" . "辞書を保存する")
    ("Undo Kakutei" . "確定を取り消す (アンドゥ)")
    ("Restart SKK" . "SKK の再起動")
    ("Version" . "SKK のバージョン")
    ("Daredevil SKK Menu" . "Daredevil SKK メニュー")
    ("Hankaku alphabet" . "半角英数")
    ("Zenkaku alphabet" . "全角英数")
    ("Read Manual" . "マニュアルを読む")
    ("Start Tutorial" . "チュートリアル")
    ("Customize SKK" . "SKK をカスタマイズ")
    ("Customize SKK (simple)" . "SKK をカスタマイズ (簡易版)")
    ("Find kanji by radicals" . "漢字を部首から調べる")
    ("Show list of characters" . "文字コード表")
    ("SKK Clock" . "SKK 時計")
    ("Lookup word in region or at point" . "領域またはポイントの語句を調べる")
    ("About Daredevil SKK.." . "Daredevil SKK について..")
    ("Visit Web Site" . "SKK のサイトへ")))

(defvar skk-emacs-max-tooltip-size '(80 . 40)
  "Not used if `x-max-tooltip-size' is bound.")

(defvar skk-emacs-modeline-menu nil)

(defvar skk-emacs-tool-bar-height
  (+ (if (and (boundp 'tool-bar-images-pixel-height)
              (integerp tool-bar-images-pixel-height))
         tool-bar-images-pixel-height
       0)
     (if (and (boundp 'tool-bar-button-margin)
              (integerp tool-bar-button-margin))
         (* 2 tool-bar-button-margin)
       0)
     (if (and (boundp 'tool-bar-button-relief)
              (integerp tool-bar-button-relief))
         (* 2 tool-bar-button-relief)
       0)
     (if (boundp 'tool-bar-border)
         (cond ((integerp tool-bar-border)
                tool-bar-border)
               ((symbolp tool-bar-border)
                (or (frame-parameter (selected-frame)
                                     tool-bar-border)
                    0))
               (t
                0))
       0)
     (if (featurep 'gtk)
         ;; inaccurate (seems each few pixels (top and bottom) are used)
         6
       0)))

(defvar skk-emacs-menu-bar-height
  (+ (frame-char-height) ; menu font is not the frame font, though
     (if (featurep 'gtk)
         ;; inaccurate (seems each few pixels (top and bottom) are used)
         4
       0)))

;; Functions.

;;@@ Menu related functions.

;;;###autoload
(defun skk-emacs-prepare-menu ()
  (unless skk-emacs-modeline-menu
    (setq skk-emacs-modeline-menu
          (easy-menu-create-menu (car skk-emacs-modeline-menu-items)
                                 (cdr skk-emacs-modeline-menu-items))))
  ;;
  (unless (or (null window-system)
              (eq window-system 'w32)
              (boundp 'mac-carbon-version-string) ; Carbon Emacs
              (featurep 'ns) ; Cocoa Emacs
              (and (eq window-system 'x)
                   (boundp 'gtk-version-string)
                   (stringp (symbol-value 'gtk-version-string))
                   (string< "2.0" (symbol-value 'gtk-version-string))))
    (setq skk-show-japanese-menu nil))
  ;;
  (when skk-show-japanese-menu
    (skk-emacs-menu-replace skk-emacs-modeline-menu)
    (dolist (map (list skk-j-mode-map
                       skk-latin-mode-map
                       skk-jisx0208-latin-mode-map
                       skk-abbrev-mode-map))
      (skk-emacs-menu-replace (or (assq 'skk (assq 'menu-bar map))
                                  (assq 'SKK (assq 'menu-bar map)))))))

(defun skk-emacs-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-emacs-modeline-menu-items)
        7
        (cond (skk-katakana
               (skk-emacs-find-func-keys 'skk-toggle-characters))
              ((not skk-mode)
               (skk-emacs-find-func-keys 'skk-mode))
              ((not skk-j-mode)
               (skk-emacs-find-func-keys 'skk-kakutei))
              (t
               nil)))
  (aset (nth 2 skk-emacs-modeline-menu-items)
        7
        (if (and skk-j-mode
                 (not skk-katakana))
            (skk-emacs-find-func-keys 'skk-toggle-characters)
          nil))
  (aset (nth 3 skk-emacs-modeline-menu-items)
        7
        (if skk-j-mode
            (skk-emacs-find-func-keys 'skk-latin-mode)
          nil))
  (aset (nth 4 skk-emacs-modeline-menu-items)
        7
        (if skk-j-mode
            (skk-emacs-find-func-keys 'skk-jisx0208-latin-mode)
          nil))
  ;;
  (let ((easy-menu-converted-items-table
         (make-hash-table :test 'equal)))
    (popup-menu skk-emacs-modeline-menu)))

(defun skk-emacs-circulate-modes (&optional arg)
  (interactive "P")
  (cond (skk-henkan-mode
         nil)
        ((not skk-mode)
         (skk-mode arg))
        (skk-j-mode
         (if skk-katakana
             (skk-jisx0208-latin-mode arg)
           (skk-toggle-characters arg)))
        (skk-jisx0208-latin-mode
         (skk-latin-mode arg))
        (skk-latin-mode
         (skk-j-mode-on))))

(defun skk-emacs-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-emacs-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-emacs-visit-website ()
  (interactive)
  (browse-url "https://github.com/skk-dev"))

;;;###autoload
(defun skk-emacs-prepare-modeline-properties ()
  (setq skk-icon
        (let* ((dir (ignore-errors
                      (file-name-directory
                       (or (locate-file "skk.xpm" load-path)
                           (locate-file "skk/skk.xpm"
                                        (list (expand-file-name
                                               "../../.."
                                               data-directory)))
                           (locate-file "skk/skk.xpm"
                                        (list data-directory))))))
               (image (when dir
                        (find-image
                         `((:type xpm
                                  :file ,(expand-file-name "skk.xpm" dir)
                                  :ascent center)))))
               (string "skk"))
          (if (and skk-show-icon window-system image)
              (apply 'propertize string
                     (cons 'display (cons image skk-emacs-modeline-property)))
            nil)))
  ;;
  (unless skk-use-color-cursor
    (setq skk-indicator-use-cursor-color nil))
  ;;
  (when window-system
    (let (face)
      (dolist (mode '(hiragana
                      katakana
                      jisx0208-latin
                      jisx0201
                      abbrev))
        (setq face (intern (format "skk-emacs-%s-face" mode)))
        (unless (facep face)
          (make-face face)
          (when skk-indicator-use-cursor-color
            (set-face-foreground face
                                 (symbol-value
                                  (intern
                                   (format "skk-cursor-%s-color" mode))))))
        (push (cons mode (append skk-emacs-modeline-property
                                 (list 'face face)))
              skk-emacs-property-alist)))))

(defun skk-emacs-find-func-keys (func)
  (let ((keys
         (or (cl-do ((spec (nth 4 skk-rule-tree) (cdr spec))
                     (list nil (car spec))
                     (str nil (when (eq (nth 3 list) func)
                                (nth 1 list))))
                 ((or str (null spec))
                  (if (stringp str)
                      str
                    nil)))
             (car (where-is-internal func skk-j-mode-map)))))
    (if keys
        (format "%s" (key-description keys))
      nil)))

(defun skk-emacs-menu-replace (list)
  (let ((running-ntemacs (and (eq window-system 'w32)
                              (not (fboundp 'Meadow-version))))
        (workaround '("Hiragana"
                      "Katakana"
                      "Hankaku alphabet"
                      "Zenkaku alphabet"))
        cons)
    (while (and list (listp list))
      (cond
       ((and (car-safe list)
             (listp (car list)))
        (skk-emacs-menu-replace (car list)))
       ((and (stringp (car-safe list))
             (setq cons (assoc (car list) skk-emacs-menu-resource-ja)))
        (setcar list (if (and running-ntemacs
                              (member (car list) workaround))
                         ;; NTEmacs で Widget 付きメニューアイテムの
                         ;; 日本語がうまく表示できない問題への対策
                         ;; (NTEmacs 22.1, 23.1)
                         (encode-coding-string (cdr cons) 'shift_jis)
                       (cdr cons))))
       ((and (vectorp (car-safe list))
             (setq cons (assoc (aref (car list) 0) skk-emacs-menu-resource-ja)))
        (aset (car list) 0 (if (and running-ntemacs
                                    (member (aref (car list) 0) workaround))
                               ;; NTEmacs で Widget 付きメニューアイテムの
                               ;; 日本語がうまく表示できない問題への対策
                               ;; (NTEmacs 22.1, 23.1)
                               (encode-coding-string (cdr cons) 'shift_jis)
                             (cdr cons)))))
      (setq list (cdr list)))))

;;@@ Tooltip related functions.

(defun skk-emacs-mouse-position ()
  "ポイントの位置を (FRAME X . Y) の形で返す。
これは `mouse-avoidance-point-position' とほぼ同じだが、SKK ▼モードのときは
▼のポイントを返す。"
  (let* ((w (if skk-isearch-switch
                (minibuffer-window)
              (selected-window)))
         (edges (window-edges w))
         (list
          (compute-motion (max (window-start w) (point-min))   ; start pos
                          ;; window-start can be < point-min if the
                          ;; latter has changed since the last redisplay
                          '(0 . 0)       ; start XY
                          (if (eq skk-henkan-mode 'active)
                              (ignore-errors
                                (marker-position skk-henkan-start-point))
                            (point))       ; stop pos
                          (cons (window-width w)
                                (window-height w)); stop XY: none
                          (1- (window-width w))       ; width
                          (cons (window-hscroll w) 0)     ; 0 may not be right?
                          w)))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
          (cons (+ (car edges) (cadr list))
                (+ (cadr edges) (car (cddr list)))))))

(defun skk-tooltip-max-tooltip-size ()
  (if (boundp 'x-max-tooltip-size)
      (symbol-value 'x-max-tooltip-size)
    ;; Workaround.
    ;; Cocoa Emacs 23.2 で x-max-tooltip-size が定義されていないのを確認
    skk-emacs-max-tooltip-size))

(defun skk-tooltip-resize-text (text)
  (let ((lines 0)
        (max-lines (- (/ (/ (display-pixel-height) 2) ;ディスプレイの半分 (ex.512)
                         (frame-char-height)) ;の行数(ex.16) => 32
                      2))       ;基準とする最大高 => 16
        (max-columns (- (car (skk-tooltip-max-tooltip-size)) 2)) ;ex.78
        (columns 0)
        current-column indent)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert text)
      (setq indent (if (and (memq (downcase (char-after (point-min)))
                                  skk-henkan-show-candidates-keys)
                            (eq ?: (char-after (1+ (point-min)))))
                       "  "
                     ""))
      (goto-char (point-min))
      (while (not (eobp))
        (setq lines (1+ lines))
        (cond ((= lines max-lines)  ;長すぎる
               (beginning-of-line)
               (insert "(長すぎるので省略されました)")
               (delete-region (point) (point-max))
               (goto-char (point-max)))
              ;;
              (t
               (when (> (progn (end-of-line)
                               (current-column))
                        max-columns)
                 (move-to-column max-columns)
                 (backward-char)
                 (if (member (char-to-string (following-char))
                             skk-auto-start-henkan-keyword-list)
                     (forward-char))
                 (insert "\n" indent)
                 (forward-line -1))
               (end-of-line)
               (setq current-column (current-column))
               (when (> current-column columns)
                 (setq columns current-column))
               (forward-line 1))))
      (goto-char (point-min))
      (while (re-search-forward "\n *\n" nil t)
        (replace-match "\n" nil t))
      (setq text (buffer-string)))
    ;; (text . (x . y))
    (cons text (cons columns lines))))

(defun skk-tooltip-show-at-point (text &optional situation)
  "TEXT を tooltip で表示する。"
  (require 'tooltip)

  (and (null (eq situation 'annotation))
       skk-tooltip-show-at-point-decor
       (setq text (funcall skk-tooltip-show-at-point-decor text)))

  (let* ((P (cdr (skk-emacs-mouse-position)))
         (oP (cdr (mouse-position)))
         event
         parameters
         (avoid-destination (if (memq skk-tooltip-mouse-behavior
                                      '(avoid avoid-maybe banish))
                                (mouse-avoidance-banish-destination)))
         win
         tip-destination
         fontsize
         left top
         tooltip-info tooltip-size
         spacing border-width internal-border-width
         text-width text-height
         screen-width screen-height
         (inhibit-quit t))
    ;;
    (when (null (car P))
      (unless (memq skk-tooltip-mouse-behavior '(avoid-maybe banish nil))
        (setq oP (cdr (mouse-avoidance-point-position)))))
    ;;
    (when (eq skk-tooltip-mouse-behavior 'follow)
      (mouse-avoidance-set-mouse-position P))
    ;;
    (when (or (and (memq skk-tooltip-mouse-behavior '(avoid banish))
                   (not (equal (mouse-position) avoid-destination)))
              (and (eq skk-tooltip-mouse-behavior 'avoid-maybe)
                   (cadr (mouse-position))
                   (not (equal (mouse-position) avoid-destination))))
      (set-mouse-position (selected-frame)
                          (car avoid-destination)
                          ;; XXX pending
                          ;; マウスポインタはどこへいくべきか
                          ;; (cdr avoid-destination)
                          0))
    ;;
    (cond
     ((eq skk-tooltip-mouse-behavior 'follow)
      (setq tooltip-info (skk-tooltip-resize-text text)
            text (car tooltip-info)))
     (t
      ;; マウスポインタに依存せず tooptip の位置を決定する。
      (setq win (if skk-isearch-switch
                    (minibuffer-window)
                  (selected-window))
            tip-destination (posn-x-y
                             (if skk-isearch-switch
                                 (posn-at-point
                                  (with-current-buffer
                                      (window-buffer (minibuffer-window))
                                    (point-min))
                                  (minibuffer-window))
                               (posn-at-point (point))))
            fontsize (frame-char-height)
            spacing (let ((val (or (cdr-safe (assq 'line-spacing
                                                   skk-tooltip-parameters))
                                   (cdr-safe (assq 'line-spacing
                                                   tooltip-frame-parameters))
                                   (frame-parameter (selected-frame)
                                                    'line-spacing)
                                   (default-value 'line-spacing)
                                   0)))
                      (if (integerp val)
                          val
                        (truncate (* fontsize spacing))))
            border-width (or (cdr-safe (assq 'border-width
                                             skk-tooltip-parameters))
                             (cdr-safe (assq 'border-width
                                             tooltip-frame-parameters))
                             (frame-parameter (selected-frame) 'border-width)
                             0)
            internal-border-width (or (cdr-safe (assq 'internal-border-width
                                                      skk-tooltip-parameters))
                                      (cdr-safe (assq 'internal-border-width
                                                      tooltip-frame-parameters))
                                      (frame-parameter (selected-frame)
                                                       'internal-border-width)
                                      0)

            ;; 以下 left と top は、X Window System 下では画面全体の中での座標を
            ;; 指定する。 Apple OS X においても、Carbon Emacs 22.3 では同様だが
            ;; Cocoa Emacs 23.2 では Emacs フレーム内での座標を指定する必要がある。

            ;; x 座標 (左からの)
            left (+ (car tip-destination)
                    (nth 0 (window-inside-pixel-edges win))
                    (eval (frame-parameter (selected-frame) 'left))
                    skk-tooltip-x-offset)
            ;; y 座標 (上からの)
            top  (+ (cdr tip-destination)
                    (nth 1 (window-inside-pixel-edges win))
                    (+ (if tool-bar-mode
                           skk-emacs-tool-bar-height
                         0)
                       (if (and menu-bar-mode
                                (not (or (boundp 'mac-carbon-version-string)
                                         (featurep 'ns))))
                           skk-emacs-menu-bar-height
                         0)
                       (eval (frame-parameter (selected-frame) 'top))
                       (+ fontsize spacing))
                    skk-tooltip-y-offset)
            tooltip-info (skk-tooltip-resize-text text)
            text (car tooltip-info)
            tooltip-size (cdr tooltip-info)
            text-width (+ (* (/ (1+ fontsize) 2) (car tooltip-size))
                          (* 2 (+ border-width internal-border-width)))
            text-height (+ (* (+ fontsize spacing) (cdr tooltip-size))
                           (* 2 (+ border-width internal-border-width)))
            screen-width (display-pixel-width)
            screen-height (display-pixel-height))
      ;;
      (when (> (+ left text-width) screen-width)
        ;; 右に寄りすぎて欠けてしまわないように
        (setq left (- left (- (+ left text-width
                                 ;; 少し余計に左に寄せないと avoid
                                 ;; したマウスポインタと干渉する
                                 (* 2 fontsize))
                              screen-width))))
      (when (> (+ top text-height) screen-height)
        ;; 下に寄りすぎて欠けてしまわないように
        (setq top (- top
                     ;; 十分上げないとテキストと重なるので、
                     ;; いっそテキストの上にしてみる
                     text-height (* 2 (+ fontsize spacing))))
        ;; さらに X 座標を...
        (let ((right (+ left
                        text-width
                        skk-tooltip-x-offset))
              (mouse-x (+ (frame-parameter (selected-frame) 'left)
                          (* (frame-pixel-width)))))
          (when (and (<= left mouse-x) (<= mouse-x right))
            ;; マウスポインタと被りそうなとき
            (setq left (- left (- right mouse-x) fontsize)))))))
    ;; END **マウスポインタに依存せず tooptip の位置を決定する**
    ;;
    (setq parameters (if (eq skk-tooltip-mouse-behavior 'follow)
                         skk-tooltip-parameters
                       (append skk-tooltip-parameters
                               (list (cons 'top top)
                                     (cons 'left left)))))
    ;;
    (skk-tooltip-show-1 text parameters)
    ;;
    (when (eq situation 'annotation)
      (skk-annotation-message situation))
    ;;
    (setq event (read-event))
    (cond ((skk-key-binding-member (skk-event-key event)
                                   '(keyboard-quit
                                     skk-kanagaki-bs
                                     skk-kanagaki-esc)
                                   skk-j-mode-map)
           (tooltip-hide)
           (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
                      (car oP))
             (mouse-avoidance-set-mouse-position oP))
           (setq skk-henkan-count 0)
           (cond ((eq skk-henkan-mode 'active) ; ▼モード
                  (skk-unread-event
                   (aref (car (where-is-internal 'skk-previous-candidate
                                                 skk-j-mode-map))
                         0))
                  (when (eq situation 'listing)
                    (throw 'unread nil))) ; skk-henkan まで一気に throw する。
                 (t
                  (skk-unread-event event))))

          (t
           (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
                      (car oP))
             (mouse-avoidance-set-mouse-position oP))
           (tooltip-hide)
           (skk-unread-event event)))))

(defun skk-tooltip-show-1 (text skk-params)
  "TEXT を tooltip で表示する。
SKK-PARAMS は `skk-tooltip-parameters' 又は `tooltip-frame-parameters' のいずれか。
TEXT には `skk-tooltip-face' が適用される。"
  (condition-case error
      (let ((params (or skk-params tooltip-frame-parameters))
            fg bg)
        (if skk-params
            ;; ユーザが独自に tooltip 表示設定する
            (dolist (cell tooltip-frame-parameters)
              (unless (assq (car cell) skk-params)
                (setq params (cons cell params))))
          ;; tooltip のデフォルトの設定をする
          (setq fg (face-attribute 'tooltip :foreground))
          (setq bg (face-attribute 'tooltip :background))
          (when (stringp fg)
            (setq params (skk-put-alist 'foreground-color fg params))
            (setq params (skk-put-alist 'border-color fg params)))
          (when (stringp bg)
            (setq params (skk-put-alist 'background-color bg params))))
        ;;
        (when (facep skk-tooltip-face)
          (setq text (propertize text 'face skk-tooltip-face)))
        ;; ミニバッファにいるとき余計なメッセージをクリアする
        (when (or skk-isearch-switch
                  (skk-in-minibuffer-p))
          (message nil))
        ;;
        (let ((x-gtk-use-system-tooltips nil))
          ;; GTK 付 Emacs で、GTK のツールティップを利用すると
          ;; 現状フェイス属性が適用されないので、Emacs のツール
          ;; ティップを用いる。
          (x-show-tip text (selected-frame) params skk-tooltip-hide-delay
                      ;;
                      (if (eq skk-tooltip-mouse-behavior 'follow)
                          skk-tooltip-x-offset
                        tooltip-x-offset)
                      ;;
                      (if (eq skk-tooltip-mouse-behavior 'follow)
                          skk-tooltip-y-offset
                        tooltip-y-offset))))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))


;;@@ Other functions.

;;;###autoload
(defun skk-search-ja-dic ()
  "GNU Emacs に付属するかな漢字変換辞書を用いて検索する。
現在の Emacs には SKK-JISYO.L を基に変換された ja-dic.el が付属している。
この辞書データを用いて送りあり、送りなし、接頭辞、接尾辞の変換を行う。
ただし、SKK-JISYO.L のような英数変換、数値変換などはできない。"
  (require 'ja-dic-utl)
  ;; Mostly from ja-dic-utl.el.
  (when (and (not skkdic-okuri-nasi)
             (locate-library "ja-dic/ja-dic"))
    (ignore-errors
      (load-library "ja-dic/ja-dic")))
  (when skkdic-okuri-nasi
    (let* ((len (length skk-henkan-key))
           (vec (make-vector len 0))
           (i 0)
           entry result)
      ;; At first, generate vector VEC from SEQ for looking up SKK
      ;; alists.  Nth element in VEC corresponds to Nth element in SEQ.
      ;; The values are decided as follows.
      ;;   If SEQ[N] is `ー', VEC[N] is 0,
      ;;   else if SEQ[N] is a Hiragana character, VEC[N] is:
      ;;     ((The 2nd position code of SEQ[N]) - 32),
      ;;   else VEC[N] is 128.
      (while (< i len)
        (let ((ch (aref skk-henkan-key i))
              code)
          (cond ((= ch ?ー)
                 (aset vec i 0))
                ((and (>= ch (car skkdic-jisx0208-hiragana-block))
                      (<= ch (cdr skkdic-jisx0208-hiragana-block))
                      (setq code (encode-char ch 'japanese-jisx0208)))
                 (aset vec i (- (logand code #xFF) 32)))
                (t
                 (aset vec i 128))))
        (setq i (1+ i)))
      (cond
       (skk-henkan-okurigana ; 送りあり変換
        (let ((okurigana (assq (aref skk-henkan-okurigana 0)
                               skkdic-okurigana-table))
              orig-element)
          (when okurigana
            (setq orig-element (aref vec (1- len)))
            (aset vec (1- len) (- (cdr okurigana)))
            (when (and (setq entry (lookup-nested-alist vec
                                                        skkdic-okuri-ari
                                                        len 0 t))
                       (consp (car entry)))
              (setq entry (nreverse (copy-sequence (car entry))))))))
       ((string-match ">$" skk-henkan-key) ; 接頭辞
        (setq entry (lookup-nested-alist vec skkdic-prefix (1- len) 0 t)))
       ((string-match "^>" skk-henkan-key) ; 接尾辞
        (setq entry (lookup-nested-alist vec skkdic-postfix len 1 t)))
       (t ; 通常の送りなし変換
        (setq entry (lookup-nested-alist vec skkdic-okuri-nasi len 0 t))))
      ;;
      (when (consp (car entry))
        (setq entry (car entry)))
      (while entry
        (when (stringp (car entry))
          (setq result (nconc result (list (car entry)))))
        (setq entry (cdr entry)))
      result)))

;; advices.

(defadvice tooltip-hide (after ccc-ad activate)
  (ccc-update-buffer-local-frame-params))


(provide 'skk-emacs)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-emacs.el ends here
