;;; -*- Emacs-Lisp -*-
;;; Window manager for GNU Emacs.
;;; $Id: windows.el,v 2.48 2010/05/23 12:33:40 yuuji Exp $
;;; (c) 1993-2010 by HIROSE Yuuji [yuuji@gentei.org]
;;; Last modified Sun May 23 21:27:31 2010 on firestorm

;;;		Window manager for GNU Emacs
;;;
;;;[What is Windows?]
;;;
;;;	  You can divide  the screen of GNU Emacs  as many as you  like.
;;;	Since efficiency of implementation or so  depends  much  on  the
;;;	style  of  window division,  you  may have  your  own  style  of
;;;	partitioning.   But  if you  switch the  mode to  e-mail mode or
;;;	NetNews mode, they break your favorite style.
;;;	
;;;	  Windows.el  enables  you  to  have  multiple  favorite  window
;;;	configurations at the same  time, and switch them.  Furthermore,
;;;	it  can  save  all  window  configurations and  some  global  or
;;;	buffer-local variables into a file and restore them correctly.
;;;
;;;  **For Emacs 19 or later**
;;;
;;;	 This package provides  the `named(numbered) frame'  that can be
;;;	selected directly with their name.   With revive.el, all frames'
;;;	displaying buffers,  window configurations and size can be saved
;;;	and restored.
;;;
;;;[Installation]
;;;
;;;	  Put windows.el into the directory load-path indicates, and put
;;;	the following expressions into your ~/.emacs.
;;;
;;;	   (require 'windows)
;;;	   (win:startup-with-window)
;;;	   (define-key ctl-x-map "C" 'see-you-again)
;;;
;;;[Hack]
;;;
;;;	  If your are thinking of creating patch for windows.el, please
;;;	don't forget to consider about compatibility of various emacsen.
;;;	
;;;		* Do not rely on other package.  e.g., 'when relies on
;;;		  'cl package.
;;;		* Test your patch on as old/new emacs as you can have.
;;;	
;;;	Anyway, feel free to hack.  I'll make a glue for various emacsen
;;;	upon your patches if you can't test.
;;;
;;;[Key Bindings]
;;;
;;;	  The default prefix key stroke for Windows is `C-c C-w'.  If it
;;;	causes  you some  troubles, see  the  section  `Customizations'.
;;;	Here are the default key bindings.
;;;
;;;		C-c C-w 1		Switch to window 1 (Q)
;;;		C-c C-w 2		Switch to window 2 (Q)
;;;		   :
;;;		C-c C-w 9		Switch to window 9 (Q)
;;;		C-c C-w 0		Swap windows with the buffer 0 (Q)
;;;					(Select unallocated frame(Emacs 19))
;;;		C-c C-w SPC		Switch to window previously shown (Q)
;;;		C-c C-w C-n		Switch to next window
;;;		C-c C-w C-p		Switch to previous window
;;;		C-c C-w !		Delete current window (Q)
;;;		C-c C-w C-w		Window operation menu
;;;		C-c C-w C-r		Resume menu
;;;		C-c C-w C-l		Local resume menu
;;;		C-c C-w C-s		Switch task
;;;		C-c C-w =		Show window list (Q)
;;;
;;;	  The  key strokes  to  select  windows from  1    to 9 must  be
;;;	frequently used, so the alternative key strokes `C-c [Num.]' are
;;;	available  by default  (And  any  function  with (Q)mark can  be
;;;	invoked without  C-w).  To disable these  quick key strokes, set
;;;	the variable win:quick-selection to `nil' in your ~/.emacs.
;;;
;;;[Description]
;;;(for Emacs19 or later)
;;;
;;;	  Windows.el manages   maximum  10 (or   more if you  configure)
;;;	frames.  The first frame is `window#1'.  If you want to create a
;;;	new frame as  `window#2', type `C-c  C-w 2'.  You can  create or
;;;	switch  numbered frames as  many  as you  want.  Unlike standard
;;;	Emacs's frame selection (`C-x  5 o'), windows.el enables you  to
;;;	go to the desired frame directly.
;;;
;;;	  For    detailed  frame  management,   read the  section `Other
;;;	functions'.  In that section, the term `window' is used to refer
;;;	the frame.
;;;
;;;[Description]
;;;(for Emacs18 or emacs -nw)
;;;
;;;	  Windows.el has 10 window buffers,  from 0 to  9.   But you can
;;;	use the buffer from 1 to 9,  the  buffer 0 is reserved  for  the
;;;	most recently  used window  (This  is  not true  for  the  frame
;;;	environment of Emacs 19).
;;;
;;;	  If you  have  wrote `win:startup-with-window' in your  .emacs,
;;;	the initial  window configuration is memorized  in the buffer 1.
;;;	Otherwise you    have to memorize   it by   typing `C-c  C-w 1'.
;;;	Anyway, say you are editing some program in buffer 1.
;;;
;;;	  After a  while, a mail has arrived.   You may wish to assign a
;;;	mail mode configuration  to the  buffer 2.   Type `C-c C-w 2' to
;;;	create the buffer 2,  and  you  will  see  the  `window creation
;;;	menu':
;;;
;;;  C)reate D)uplicate P)reserve F)indfile B)uff X)M-x k)KeyFunc&exit N)o:
;;;
;;;	in the  minibuffer.   Since  you don't need  the current  window
;;;	configuration  (programming  configuration) to read  mails, type
;;;	`c', stands for Create, to create the new  window configuration.
;;;	Then, after invoke the mail reader, current window configuration
;;;	will turn to mail oriented one. (Of course you can directly call
;;;	e-mail mode by typing `x' at the window creation menu.) When you
;;;	finish  to  read  your  mail,  you  can  return  to  programming
;;;	configuration with `C-c C-w 1'.
;;;
;;;	  Let's read the NetNews.  M-x gnus...  O.K.!  You've read.  You
;;;	may have typed `q' to quit gnus until yesterday.   But you don't
;;;	have to do it  from  today.   You  can go  back  to  programming
;;;	immediately by typing  `C-c C-w 1'!  Oops, you  have not created
;;;	the  new  configuration buffer  yet.  So the  window  you see is
;;;	still the window 1.  If you switch the window to another, you'll
;;;	lose the  configuration in the buffer 1.  In this case, type `p'
;;;	(stands for  Preserve) at the window creation  menu after typing
;;;	`C-c C-w  3'(%1).   Windows.el doesn't  update the  buffer 1 and
;;;	saves  the  current window configuration of the NetNews into the
;;;	buffer 3.
;;;   **On Emacs-19,  you can't preserve  the buffer contents unless you 
;;;	have saved it into the resume file described below.
;;;
;;;	  Then, type `C-c C-w 2' to read the mail, type `C-c C-w 3' when
;;;	you are  tired with    programming.   `C-c  C-w  SPC' is    very
;;;	convenient when you want to exchange two configurations.
;;;
;;;	  If you forget what windows are  allocated to the buffers, type
;;;	`C-c C-w =' to display  the configuration buffer names and their
;;;	corresponding  editing buffer names in    the menu buffer.   The
;;;	entry preceded by `*' is the selected  buffer and the entry with
;;;	`+'  is the buffer previously  selected(that is, the buffer `C-c
;;;	C-w SPC' will select).  In this buffer, you can move cursor with
;;;	`n' or `p', and select that window  with SPC.  Type `?' for more
;;;	information.
;;;
;;;	(%1)
;;;	At a point of this time, while window configuration buffer holds
;;;	the  programming environment,  the screen of GNUS  is displayed.
;;;	And,  notice  that `p'(Preserve)  at  the  window creation  menu
;;;	doesn't  work  when the  Windows uses `frame' as window unit  on
;;;	Emacs 19.
;;;
;;;[Other functions]
;;;
;;;	  Typing `C-c C-w C-w' displays the menu as follows.
;;;
;;;	N)ext P)rev R)ecent D)elete K)ill S)ave2buf L)oad-from-buf A)save-As
;;;
;;;	In this menu, `n',  `p' is for  switching window to the next  or
;;;	previous, `r' is for recovering the  window recently seen before
;;;	the window switching operation(%2), `d' is for  deleting current
;;;	window.  `k' is same as `d' except  closing all visible file(s).
;;;	And type `s' to save  the current window  configuration into the
;;;	corresponding   buffer,  `a'   to    save  the   current  window
;;;	configuration into specified buffer.  Note that `s' and `l' is
;;;	unavailable on frame environment.
;;;
;;;	(%2)
;;;	When using frame on Emacs 19, it is impossible  to store a frame
;;;	recently seen, so `r'  (win-recent-window)  selects a frame that
;;;	is not  allocated to Windows instead, if any (equivalent to `C-c
;;;	C-w 0').
;;;
;;;	  When you  create  the  new window, you see the window creation
;;;	menu which  ask you  how to  handle  the  current  configuration
;;;	buffer  and  what  configuration the  new  window must  be.  The
;;;	entries of the prompt stand for the followings.
;;;
;;;		Create		After saving the current window
;;;				configuration into the current buffer,
;;;				create a newly allocated window.
;;;		Duplicate	After saving the current window
;;;				configuration into the current buffer,
;;;				create the new window with the same
;;;				configuration as the current one.
;;;		Preserve	Without updating the current buffer,
;;;				use the current window configuration
;;;				as the new window.  This function is not
;;;				available on Emacs 19 with frame.
;;;		Findfile	Find-file on new window.
;;;		Buff		Switch to buffer on new window.
;;;		M-x		Call a command on new window.
;;;		No		Cease window creation.
;;;
;;;[Resume]
;;;
;;;	  Windows.el  can  resume  your  environment  with  a  help   of
;;;	`revive.el'.  You can see the menu by typing `C-c C-w C-r'.
;;;
;;;	     A)save-all R)estore-all S)ave-this L)oad-this N)Load# ~)read-~
;;;
;;;	Type `a' to save all window configurations into a file, and type
;;;	`r' to restore configurations saved  in that  file.  `s' to save
;;;	the  current window configuration, `l'  to  load current  window
;;;	from file, `n' to load a window specified by a  number.  And use
;;;	`~' when you type `a' instead of `r' by mistake.  And it is much
;;;	more convenient  to kill emacs with `C-x  C'.   If you  want  to
;;;	resume that  context, call  `resume-windows' (or C-c C-w  C-r r)
;;;	just after starting Emacs.
;;;
;;;[Local resume]
;;;
;;;	  If you want to have  many sets of window  configurations, type
;;;	`C-c   C-w  C-l'   to  specify  the  directory   where   another
;;;	configuration file is to resid,  and operate in the same way  as
;;;	you  do in resume menu described  above.  You can change sets of
;;;	configurations directory by directory.
;;;
;;;	  By  the  way,  most  of  tasks  are being  done  in a  certain
;;;	directory.   If you have a couple of or more jobs to do at once,
;;;	the  function  `win-switch-task' is very useful  to  switch your
;;;	tasks.  It  saves the current  set of window configurations into
;;;	current configuration file, flushes buffers, and reads  the next
;;;	set of window  configurations for  the  next task  from  another
;;;	configuration file.
;;;
;;;[Customizations]
;;;
;;;	  To change  the prefix key stroke  to  `C-c w' for example,
;;;	put the following expressions into your ~/.emacs.
;;;
;;;		(setq win:switch-prefix "\C-cw")
;;;		(define-key global-map win:switch-prefix nil)
;;;		(define-key global-map "\C-cw1" 'win-switch-to-window)
;;;
;;;	And you can also change the key stroke of window selection to
;;;	`a' to `z' other than `1' to `9'.
;;;
;;;		(setq win:switch-prefix "\C-cw")
;;;		(define-key global-map win:switch-prefix nil)
;;;		(define-key global-map "\C-cwb" 'win-switch-to-window)
;;;		(setq win:base-key ?`)		;; '`' is before 'a'
;;;		(setq win:max-configs 27)	;; '`' to 'z' are 27 chars.
;;;		(setq win:quick-selection nil)	;; Not assign `C-c LETTER'
;;;
;;;	Notice that '`' is the previous character  of 'a' in ASCII code,
;;;	and  that C-c w  ` is bound  to swap  the configuration  in  the
;;;	buffer '`' and the current buffer.
;;;
;;;	  If you don't use `frame' even on Emacs 19 with X Window.  Set
;;;	win:use-frame to nil in ~/.emacs.
;;;
;;;	  If you hate raising of  frames at win-save-all-configurations,
;;;	set win:no-raise-at-save to t.
;;;
;;;[For frame users]
;;;
;;;	  When you start to use windows.el,  you may create  a new frame
;;;	with  old  operation  `C-x  5 f'  or  so.  Frames  created  with
;;;	standard frame operation are not marked in the windows ring.  To
;;;	incorporate sucn an  orphan frame into  windows  ring, type `C-c
;;;	C-w C-w' and select `A)save-as' from the menu.   And if you want
;;;	to switch to  orphan frames, type  `C-c C-w 0',  which  switches
;;;	frame to isolated frames and rotate them.
;;;
;;;	By default, windows.el name a frame title (bar) as follows.
;;;	
;;;		Emacs[1]:*scartch*
;;;	
;;;	This causes frame  title  always representing the frame  number.
;;;	So you can switch   to any of  them with  key operation  of your
;;;	window manager, `fvwm' for example:
;;;	--- .fvwmrc ---
;;;	key 1 A C Warp "" Emacs[1]
;;;	key 2 A C Warp "" Emacs[2]
;;;	  :
;;;	key 9 A C Warp "" Emacs[9]
;;;	
;;;	If you use fvwm2
;;;	AddToFunc DeiconifyFocusAndWarp "I" Iconify -1
;;;	+				"I" FocusAndWarp $0 $1
;;;	
;;;	Key 1 A C	Next [Emacs?1?:*] DeiconifyFocusAndWarp
;;;	 :
;;;	Key 9 A C	Next [Emacs?9?:*] DeiconifyFocusAndWarp
;;;	#(fvwm2 can't escape [ and ], so cause matches with them by ?s.)
;;;
;;;	Thus C-1, C-2, ..., C-9 directly select window frame `Emacs[1]'
;;;	through `Emacs[9]'.
;;;	In this  hook, two  variables  `frame'  and `index', which  have
;;;	frame object, frame number respectively, are available.
;;;
;;;[Bugs]
;;;
;;;	  This  program  was inspired from  `screen', the window manager
;;;	for VT100  emulation  terminal, and operations are fundamentally
;;;	based on it.  But there is no compatibility.
;;;
;;;	  When  restoring  frames  of  Emacs  19,  the  order  of  frame
;;;	allocation is  not restored, that is,  its order depends on  the
;;;	frame  where  win-load-all-configurations  is called.  No  frame
;;;	positions are not recovered neither.
;;;
;;;[Copying]
;;;
;;;	  This program is distributed as a free  software. The author is
;;;	not responsible  for  any  possible   defects   caused  by  this
;;;	software.
;;;
;;;	  Comments and bug reports  are  welcome.   Don't  hesitated  to
;;;	report.  My possible e-mail address is following.
;;;
;;;							yuuji@gentei.org
;;;
;;; Japanese Document follows:
;;;
;;;		GNU Emacs 用編集画面マネージャ [windows]
;;;
;;;【できる事】
;;;
;;;	  GNU Emacs では縦横任意の数だけウィンドウを分割して作業をする事
;;;	ができます。プログラムを開発する時などのウィンドウ分割は効率に大
;;;	きく影響するので、人によって好みの分割形態を持っている事でしょう。
;;;	しかしその途中で、メイルやニュースを読むとその分割形態を壊されて
;;;	しまいます。正しい手順でメイルリーダモードを終われば良いのですが、
;;;	それだとまたメイルが来た時に再びメイルリーダモードを起動しなけれ
;;;	ばなりません。
;;;	  windows.el をロードすると、好みのウィンドウ分割形態を複数持ち、
;;;	それらを切替えながら Emacs を使う事ができます。さらに、その分割
;;;	形態の全てをファイルにセーブし、いつでもそれらを復元することがで
;;;	きます。
;;;
;;;	  Emacs 19(Mule2) 以降では、同様の操作体系で frame を単位として
;;;	ウィンドウ切替え操作を行います。さらに分割形態復元時にはフレーム
;;;	のサイズと位置も忠実に再現します。フレームの嫌いな人は変数の設定
;;;	によりフレームを使わないウィンドウの切り替えで利用することもでき
;;;	ます。
;;;
;;;【準備】
;;;
;;;	  windows.el を load-path の通ったディレクトリに入れてください。
;;;	そして以下の行を .emacs に入れてください。
;;;
;;;	   (require 'windows)
;;;	   (define-key global-map "\C-xC" 'see-you-again)
;;;	   (win:startup-with-window)
;;;
;;;
;;;【キー定義】
;;;
;;;	  デフォルトのプリフィクスキーは C-c C-w です。これでは不都合な
;;;	場合は『キーカスタマイズ』の項を参照してください。標準状態のキー
;;;	バインドは以下のようになっています。
;;;
;;;		C-c C-w 1	分割状態 1 へ (Q)
;;;		C-c C-w 2	分割状態 2 へ (Q)
;;;		   :
;;;		C-c C-w 9	分割状態 9 へ (Q)
;;;		C-c C-w 0	直前の分割状態へ(バッファ0と交換) (Q)
;;;		C-c C-w SPC	分割状態1〜nのうち、直前用いたものへ (Q)
;;;		C-c C-w n	次の分割状態へ(C-c SPC)
;;;		C-c C-w p	前の分割状態へ
;;;		C-c C-w !	現在のウィンドウを破棄 (Q)
;;;		C-c C-w -	ちょっと前のウィンドウ状態を復活(Q)
;;;		C-c C-w C-w	ウィンドウ操作メニュー
;;;		C-c C-w C-r	リジュームメニュー
;;;		C-c C-w C-l	ローカルリジュームメニュー
;;;		C-c C-w C-s	タスク切替え
;;;		C-c C-w =	分割状態保存バッファ一覧表示 (Q)
;;;
;;;	  1 番から 9 番までのウィンドウ選択は非常に頻繁に用いられるので、
;;;	「C-c 番号」でも切り替えができるようになっています(その他Qマーク
;;;	の付いているキーバインド全ては C-w を打たなくてもよい)。これを無
;;;	効にするためには、~/.emacs などで変数 win:quick-selection を nil 
;;;	にセットしてください。
;;;
;;;【説明】
;;;
;;;	  windows.el には0から9までの10個のバッファが用意されています。
;;;	このうちユーザが好みの分割状態を保存させるために使えるのは1〜9 
;;;	で、0番は自動的に直前の分割状態をセーブするために使われます
;;;	(Emacs-19以降ではwindowsに割り当てられていないframeへのジャンプ)。
;;;
;;;	  win:startup-with-window を .emacs に書いている場合は起動時の編
;;;	集状態がそのまま1番のバッファに格納されています。そうでない場合
;;;	は何か C-c C-w 1 を押して最初の編集状態を1番のバッファに記憶させ
;;;	ます。
;;;
;;;	  さて、1番に現状を保存した状態で、メイルを読みましょう。メイル
;;;	は2番のウィンドウに割り当てます。C-c C-w 2 を押すと今度は
;;;
;;;		C)reate D)uplicate P)reserve F)indfile B)uff X)M-x N)o:
;;;
;;;	と出て来ます(これをウィンドウ生成メニューと呼ぶことにします)。メ
;;;	イルを読むためには現在の分割状態は必要ありませんから、Create の 
;;;	c を押して新規ウィンドウを作成します。そこでメイルリーダを起動す
;;;	ると、ウィンドウの分割状態がメイル専用になります(もちろんメニュー
;;;	でxを押して直接メイルリーダを起動してもかまいません)。読み終わっ
;;;	たら C-c C-w 1 を押すと、1番のバッファに保存されている、最初のプ
;;;	ログラム編集状態に切り替わります。これでプログラム作成に直ちに戻
;;;	れます。
;;;
;;;	  今度はニュースを読みましょう。M-x gnus ・・・。はい読み終わり
;;;	ました。いままでは q で終了していましたが、今日からは違います。 
;;;	C-c C-w 1 で復帰できるのです。はい、C-c C-w 1。おっと、新規ウィ
;;;	ンドウを作成するのを忘れていたので、まだ1番のウィンドウにいたの
;;;	でした。このまま(%1)ではせっかく1番に保存したプログラム編集状態
;;;	が消えてしまいます。そんな時は、C-c C-w 3 を押してウィンドウ生成
;;;	メニューが出たところで、Preserve の p を押してください。1番のバッ
;;;	ファの内容は更新せずに、現状のウィンドウ状態を3番のバッファに保
;;;	存します。
;;;   **Mule2ではリジューム(後述)ファイルに現在の状態がセーブされていな
;;;	い限り、分割状態を復旧することができません。
;;;
;;;	  あとはメイルが来たら C-c C-w 2 を、プログラムに飽きたら C-c
;;;	C-w 3 を押して一日を過ごしましょう。メイルとニュースだけを読み続
;;;	ける場合のように二つの状態を行ったり来たりする場合は C-c C-w SPC 
;;;	が便利です。
;;;
;;;	  「あれ、メイルはいま何番のウィンドウだ?」と分からなくなったら、
;;;	C-c C-w = を押しましょう。ミニバッファに番号と、対応するバッファ
;;;	名がメニューバッファに表示されます。このうち、 バッファ名の頭に*
;;;	が付いているものが現在選択しているバッファで、 +が付いているもの
;;;	が直前に選択していたバッファです(つまり  C-c C-w SPC の行き先)。
;;;	メニューバッファでは、 n や p を押して切替えたいウィンドウの位置
;;;	まで移り、 SPC でそのウィンドウに切替えることができます。また d,
;;;	k, s, l を押すと行頭に D, K,  S, L マークが付きます。ここで x を
;;;	押すとマークを付けたバッファに対し、それぞれ「削除」、「表示され
;;;	ているバッファも含めて削除(kill)」、「状態をファイルにセーブ」、
;;;	「状態をファイルからロード」を行います。詳しくは、メニューバッファ
;;;	で ? を押してください。
;;;
;;;	(%1)
;;;	この状態ではバッファにプログラム編集状態が保存され、ウィンドウに
;;;	GNUSの画面が表示されている。なお、Preserve は Emacs 19 でframeを
;;;	切替え単位とする場合には利用できないので注意して下さい。
;;;
;;;【その他細かい機能】
;;;
;;;	  C-c C-w C-w を押すと以下のようなメニューが出て来ます。
;;;
;;;	N)ext P)rev R)ecent D)elete K)ill S)ave2buf L)oad-from-buf A)save-As
;;;
;;;	n, p はそれぞれ一つ前/次のバッファの選択します。r はウィンドウ切
;;;	り替えする直前に表示していた画面へ復帰します(%2)。d は現在選択し
;;;	ている番号のバッファを消去します。k は d と同じですが、現在見え
;;;	ているファイルも同時にクローズします。l は間違えて C-x 1 してし
;;;	まった時などに、バッファにセーブされているウィンドウ状態を強制的
;;;	に読み直す時に使います。s は現在見ている状態を強制的に対応するバッ
;;;	ファにセーブする為に使い、a はセーブするバッファ番号を別途指定す
;;;	る時に使います。
;;;
;;;	(%2)
;;;	Emacs 19 の frame 機能でウィンドウ切替えを行う場合、直前の状態を
;;;	保存する事はできないため、r を押した場合(関数win-recent-window)
;;;	windows 用に割り当てられていない frame があればそれに表示を切替
;;;	えます(C-c C-w 0 と同じ)。
;;;
;;;	  新規ウィンドウ作成時に、現在のバッファ内容の更新と、新ウィンド
;;;	ウの状態を指示するためにウィンドウ生成メニューが出ます。これらの
;;;	意味は以下のようになっています。
;;;
;;;		Create		現画面を現バッファに保存後、新規ウィンド
;;;				ウを生成
;;;		Duplicate	現画面を現バッファに保存後、現画面と同じ
;;;				ウィンドウを生成
;;;		Preserve	現バッファは更新せず、現画面を新規ウィン
;;;				ドウとして登録
;;;		Findfile	新規ウィンドウで find-file を行なう
;;;		Buff		新規ウィンドウで switch-to-buffer を行なう
;;;		M-x		新規ウィンドウでコマンド実行
;;;		No		新規ウィンドウ生成を中止
;;;
;;;【リジューム】
;;;
;;;	  revive.el と組み合わせて使うことにより、リジューム機能が有効で
;;;	す。C-c C-w C-r を押すと以下のメニューが現れます。
;;;
;;;	     A)save-all R)estore-all S)ave-this L)oad-this N)Load# ~)read-~
;;;
;;;	ここで a を押すと現在の全てのウィンドウの情報をファイルにセーブ
;;;	することができます。r を押すとファイルにセーブしたものをロードす
;;;	ることができます。s,l を押すと現在選択しているウィンドウ状態をそ
;;;	れぞれ セーブ/ロード します。n はファイルから数字で指定したウィ
;;;	ンドウ状態をロードします。また Emacs 起動直後に r を押すところを
;;;	間違えて a を押してしまった場合などは ~ を利用してください。
;;;
;;;	  このメニューからセーブするよりも「C-x C」で Emacs を終了し、次
;;;	回 Emacs を起動した直後に resume-windows (または C-c C-w C-r r)
;;;	を起動することで直ちに以前の状態に戻ることができるのでこちらの方
;;;	が便利な使い方といえましょう。
;;;
;;;【ローカルリジューム】
;;;
;;;	  リジュームでは全てのウィンドウ情報をファイルにセーブしますが、
;;;	そのファイルをさらに複数持ち、それぞれを切り替えて使うことができ
;;;	ます。「C-c C-w C-l」をタイプし情報ファイルをセーブ/ロードするデ
;;;	ィレクトリを入力した後、通常のリジュームメニューの操作を行ないま
;;;	す。
;;;
;;;	  ところで、多くの仕事はその仕事特有のディレクトリをベースに行わ
;;;	れます。この性質を利用して、一度に複数個の仕事をこなす場合などに、
;;;	関数 win-switch-task を使うと仕事の切替えをスムーズに行うことが
;;;	できます。この関数を呼ぶと現在の環境を現在の情報ファイルに保存す
;;;	るか確認後、次の仕事を行っているディレクトリの入力を促します。
;;;
;;;【カスタマイズ】
;;;
;;;	  プリフィクスキーを例えば C-c w に変更する時は .emacs に次のよ
;;;	うな記述をいれます。
;;;
;;;		(setq win:switch-prefix "\C-cw")
;;;		(define-key global-map win:switch-prefix nil)
;;;		(define-key global-map "\C-cw1" 'win-switch-to-window)
;;;
;;;	  ウィンドウの選択を1〜9ではなくて、a〜zにすることもできます。
;;;
;;;		(setq win:switch-prefix "\C-cw")
;;;		(define-key global-map win:switch-prefix nil)
;;;		(define-key global-map "\C-cwa" 'win-switch-to-window)
;;;		(setq win:base-key ?`)		;; ` は「直前の状態」
;;;		(setq win:max-configs 27)	;; ` 〜 z は27文字
;;;		(setq win:quick-selection nil)	;; C-c英字 に割り当てない
;;;
;;;	ここで ` はアスキーコードで a の一つ前にあることに注意してくださ
;;;	い。C-c C-w ` は直前状態保存用バッファと、カレントウィンドウの内
;;;	容の交換に割り当てられます。
;;;
;;;	  windows の動作を調整する以下の変数があります。
;;;
;;;	win:switch-prefix	windows.el 操作用のprefixキー
;;;	win:menu-key-stroke	window生成menuのキー(prefixキーに続けて)
;;;	win:resume-key-stroke	resume menu のキー (〃)
;;;	win:resume-local-key-stroke	local resume menu のキー (〃)
;;;	win:switch-task-key-stroke	switch task menu のキー (〃)
;;;	win:quick-selection	C-c 数字 などでwindow選択できる
;;;	win:mode-line-format	window番号を示す mode-line format
;;;	win:configuration-file	resume file のパス名
;;;	win:make-backup-files	resume file のバックアップを取るか
;;;	win:buffer-depth-per-win window毎にbuffer-listの上位何個を記憶するか
;;;				nilの時はbuffer-list優先順位を保存しない
;;;	win:inhibit-switch-in-minibuffer
;;;				ミニバッファではwindow切り替えしない
;;;	win:memorize-winconf-timing
;;;				C-c C-w - で復帰すべきウィンドウ状態を保
;;;				存するタイミングを指定する 'save (ファイ
;;;				ル保存時)または 'change (バッファ修正時)
;;;
;;;	-- 以下 mule2 以降でフレームを利用する場合のみ有効 --
;;;	win:no-raise-at-save	全windowのセーブ時にフレームをraiseするか
;;;	win:frame-parameters-to-save-private
;;;				frame parameter のうちセーブしたいパラメー
;;;				タ(win:frame-parameters-to-save-default 
;;;				の値以外のものを設定する)
;;;	win:auto-position	フレーム新規作成時に
;;;				nil なら手動で位置を確定
;;;				'absolute なら絶対座標を計算して配置
;;;				'relative なら現フレームとの相対位置で配置
;;;	win:new-frame-offset-x	新規フレーム自動配置時のX座標のオフセット
;;;	win:new-frame-offset-y	新規フレーム自動配置時のY座標のオフセット
;;;	win:resumed-frame-offset-x フレームリジューム時のX座標のオフセット
;;;				fvwmのデフォルトの BoundaryWidth を使う
;;;				時はこの変数の値を3にしておくと良い。
;;;	win:resumed-frame-offset-y フレームリジューム時のY座標のオフセット
;;;	win:mouse-position	フレーム移動時のマウスカーソルの座標
;;;				'(x y) というリストで指定
;;;	win:frame-title-function 各フレームのタイトルを決定する関数
;;;				フレーム番号が引数として渡される。
;;;				この値をnilにするとタイトルはいじらない。
;;;	win:title-with-buffer-name
;;;				フレームタイトルにバッファ名を付加するか
;;;
;;;【バグ】
;;;
;;;	  ウィンドウを切替えると言うアイデアは screen コマンドに基づいて
;;;	いますが、操作体系はかなり違います。
;;;
;;;【通な使い方】
;;;	
;;;	windows.el 2.11 以降では、frameに
;;;
;;;		mule[1]:*scratch*
;;;	
;;;	のようなタイトルを付けます。これを利用すると、ウィンドウマネージャ
;;;	から一撃で目的のframeに飛ぶことが出来るようになります。fvwm2を使っ
;;;	ているときは、
;;;	
;;;	AddToFunc DeiconifyFocusAndWarp "I" Iconify -1
;;;	+				"I" FocusAndWarp $0 $1
;;;	
;;;	Key 1 A C	Next [mule?1?:*] DeiconifyFocusAndWarp
;;;	 :
;;;	Key 9 A C	Next [mule?9?:*] DeiconifyFocusAndWarp
;;;	
;;;	のように ~/.fvwm2rc に書くことで、C-1 〜 C-9 を押すことで目的フ
;;;	レームに飛ぶことが出来るようになります(詳しくはウィンドウマネー
;;;	ジャのマニュアルを見てください)。
;;;	
;;;【あとがき】
;;;
;;;	  似たようなの他にもありそう。 とおもいつつ fj.editor.emacs に投
;;;	稿したところ screens.el というまさに screen コマンド互換のものが
;;;	ありました。 また wicos.el という後継版が出ているようですが…(多
;;;	くは語るまい)…。windows.el では、 (全)ウィンドウ状態の保存/復元
;;;	が可能なので、ただちに前回の編集環境を取り戻すことができます。そ
;;;	れゆえ論文にインプリメントにと忙しい時期には欠かせないユーティリ
;;;	ティとなるでしょう。
;;;
;;;
;;;【謝辞】
;;;
;;;	  このプログラムを作るきっかけと適切なコメントを下さった、ASCII-
;;;	NETのたりゃー佐々木さん、 Emacs-19 でミニバッファを分離させている
;;;	時の挙動に関するデバッグに協力下さったfujixerox.co.jpの廣瀬陽一さ
;;;	ん、メニューによるウィンドウ切り替えを提案してくださった(株) 東芝
;;;	の小林勉さん、XEmacs関係の動作報告などを下さった中島幹夫さん、各
;;;	ウィンドウに名前をつける機能などのパッチを下さった早稲田大学の西
;;;	本さん、Mew(5以降)を win:use-frame がnilで使うときの不具合に関す
;;;	る助言を下さった増田さん、Emacs22以降 -nw のときに同一バッファを
;;;	別ウィンドウに出している場合に current-window-configuration がポ
;;;	イントを復帰できない問題の回避策と複数の *shell* バッファを復帰可
;;;	能とするパッチを下さった千葉大学の桜井貴文さんに感謝致します。
;;;
;;;【取り扱い】
;;;
;;;	  このプログラムは、フリーソフトウェアとして配布いたします。この
;;;	プログラムを使用して生じたいかなる結果に対しても作者は一切の責任
;;;	を負わないものといたしますが、コメントやバグレポートは大いに歓迎
;;;	いたします。お気軽にご連絡下さい。連絡は以下のアドレスまでお願い
;;;	いたします(2008/6現在)。
;;;							yuuji@gentei.org

;;;
;; Code
;;;

;; ---------- Customizable variables
(defvar win:max-configs 10
  "*Number of window configurations to hold")
(defvar win:base-key ?0
  "*Base of window buffer name")
(if (> win:max-configs 27) (error "win:max-configs too large!"))
(defvar win:switch-prefix "\C-c\C-w"
  "*Prefix key stroke to switch windows")
(defvar win:menu-key-stroke "\C-w"
  "*Key assignment of win-menu")
(defvar win:resume-key-stroke "\C-r"
  "*Key assignment of win-resume-menu")
(defvar win:resume-local-key-stroke "\C-l"
  "*Key assignment of win-resume-local-menu")
(defvar win:switch-task-key-stroke "\C-s"
  "*Key assignment of win-switch-task")
(defvar win:quick-selection t
  "*Non-nil enables a short cut for window selection;
Not only with `C-c C-w 1' but also with `C-c 1'.")
(defvar win:mode-line-format "[%c%s%s]"
  "*Format of mode line that shows the selected window number")

;;Variables for resume
(defvar win:configuration-file
  (if (eq system-type 'ms-dos) "~/_windows" "~/.windows")
  "*File to save window configurations")
(defvar win:local-config-file win:configuration-file
  "*Default local configuration file")
(defvar win:make-backup-files t
  "*Create a backup of window configuration file or not")
(defvar win:config-loaded nil
  "Flag if some configuration file is loaded or not")

;;Variables for Emacs 19
(defvar win:no-raise-at-save nil
  "*Non-nil inhibits win-save-all-configurations from raising each frame.")
;(Rev.1.4)
(defvar win:frame-parameters-to-save-default
  (delq nil
	(list
	 'top 'left 'minibuffer 'vertical-scroll-bars
	 (if (string< "19.27" emacs-version) 'menu-bar-lines)
	 (if (featurep 'mule) 'line-space)
	 ;;	These parameters below should go to
	 ;;	win:frame-parameters-to-save-private
	 ;; 'cursor-type 'auto-lower 'auto-raise 'cursor-color 'mouse-color
	 ;; 'background-color 'foreground-color 'font
	 ))
  "Which frame parameters to save; Do not modify this variable.")
(defvar win:frame-parameters-to-save-private nil
  "*User defined list of frame parameters to save;
You don't have to include neither 'heght nor 'width in this list
because they are automatically saved in other method.")
(defvar win:auto-position 'absolute
  "*Non-nil automatically positions the newly created frame;
There are two methods of auto positioning,
'absolute for this variable puts the new frame by calculating its absolute
coordinates.
'relative for this variable puts the new frame relative to currently
selected frame.")
(defvar win:new-frame-offset-x 50
  "*X-Offset of new frame to currently selected frame; See win:auto-position.")
(defvar win:new-frame-offset-y 10
  "*Y-Offset of new frame to currently selected frame; See win:auto-position.")
(defvar win:allocate-frame-hook nil
  "*Hook running at allocating new frame")
(defvar win:resumed-frame-offset-x 0
  "*X-Offset of resumed frame to compensate the window manager's border")
(defvar win:resumed-frame-offset-y 0
  "*Y-Offset of resumed frame to compensate the window manager's border")
(defvar win:mouse-position '(0 -5)
  "*Mouse position list (X Y) in *pixel* at window selection")
(defvar win:title-with-buffer-name t
  "*T means naming frame title with buffer name")
(defvar win:wipe-also-frames nil
  "*T means wipe all frames but one when wipe-windows.")
(defvar win:need-uptodate-frame-title t
  "*Set this to nil to disable frequent rewriting of frame title.")
(defvar win:buffer-depth-per-win 5
  "*Default depth of buffer stack per window.
If nil, do not preserve buffer-list priority")
(defvar win:names-maxl 20 "*Title length in window list")
(defvar win:inhibit-switch-in-minibuffer nil
  "*Non-nil inhibits window switching in minibuffer window")


;;Variables for XEmacs
(defvar win:frame-title-function
  '(lambda (index)
     (format (if (featurep 'mule) "mule[%d]" "emacs[%d]") index))
  "*Window title creation function.
One argument of frame number should be taken.")

;;; ---------- Internal work variables
(defvar win:configs (make-vector win:max-configs nil)
  "Array of window configurations; 0th always has previous configuration.")
(defvar win:names (make-vector win:max-configs ""))
(defvar win:names-prefix (make-vector win:max-configs ""))
(defvar win:sizes (make-vector win:max-configs nil))
(defvar win:buflists (make-vector win:max-configs nil))
(let ((i 0))
  (while (< i win:max-configs)
    (aset win:buflists i (make-vector win:buffer-depth-per-win nil))
    (setq i (1+ i))))

(defvar win:current-config 0 "Current window buffer number")
(defvar win:last-config 0 "Buffer number for a window previously seen")

(defvar win:switch-map nil "Key map for window switcher")
(defvar win:switch-menu-map nil "Keymap used in window selection menu")

;; for XEmacs
(defvar win:xemacs-p (string-match "XEmacs" emacs-version))
(defvar win:XEmacs-remake-initial-frame win:xemacs-p
  "*Non-nil delets initial frame and rebuild same frame with name mule[1]")
(defun win:remake-frame ()
  (if win:XEmacs-remake-initial-frame
      (let ((old (selected-frame)))
	(prog1
	    (make-frame
	     (cons
	      (cons 'name "mule[1]")
	      (mapcar '(lambda (s)
			 (cons s (frame-property old s)))
		      '(top left height width))))
	  (delete-frame old)))))

;; for Nemacs(Emacs-18)
(if (fboundp 'add-hook)
    nil
  (defun win:add-hook (hook funcs &optional append local)
    "Append funcs to hook's value keeping its uniquness."
    ;;Derived from add-hook.el by Daniel LaLiberte.
    (if (boundp hook)
	(let ((value (symbol-value hook)))
	  (if (and (listp value) (not (eq (car value) 'lambda)))
	      (and (not (memq funcs value))
		   (set hook
			(append value (list funcs))))
	    (and (not (eq funcs value))
		 (set hook
		      (list value funcs)))))
      (set hook funcs)))
  (fset 'add-hook 'win:add-hook))

(defun win:make-frame (index &optional prop)
  "Wrapper function for make-frame;
INDEX is a window number.  Optional second argument PROP is passed to
make-frame function."
  (if win:frame-title-function
      (setq prop
	    (cons (cons 'name (funcall win:frame-title-function index)) prop)))
  (if win:xemacs-p
      (make-frame (alist-to-plist prop))
    (let ((frame (make-frame prop)) (count 100))
      ;;(modify-frame-parameters frame prop)
      (if (string-match "^2[01]\\." emacs-version)
	  (progn
	    (while (and (> count 0) (not (frame-visible-p frame)))
	      (setq count (1- count))
	      (sit-for (string-to-number "0.1")))
	    ;;Emacs 20 becomes not to support top/left for initial parameter.
	    (modify-frame-parameters frame prop)))
      frame)))

;; for Emacs-19
(defvar win:use-frame
  (and (string< "18" (substring emacs-version 0 2)) window-system)
  "*Non-nil means switch windows with frame.")
(and (fboundp 'eval-when-compile)
     (eval-when-compile (require 'revive)))

(if win:switch-map nil
  (setq win:switch-map (make-sparse-keymap)
	win:switch-menu-map (make-keymap))
  (suppress-keymap win:switch-menu-map)
  (define-key global-map win:switch-prefix nil)
  (define-key global-map win:switch-prefix win:switch-map)
  (let ((key win:base-key) (max (+ win:base-key win:max-configs)))
    (while (< key max)
      (define-key win:switch-map (char-to-string key) 'win-switch-to-window)
      (setq key (1+ key)))		;`key' increasing
    (if (and win:quick-selection (> (length win:switch-prefix) 1))
	(let ((prefix (substring win:switch-prefix 0 1)))
	  (while (>= key win:base-key)
	    (setq key (1- key))
	    (define-key global-map
	      (concat prefix (char-to-string key))
	      'win-switch-to-window))
	  (define-key global-map (concat prefix "=") 'win-switch-menu)
	  (define-key global-map (concat prefix " ") 'win-toggle-window)
	  (define-key global-map (concat prefix "!")
	    'win-delete-current-window)
	  (define-key global-map (concat prefix "-")
	   'win-recover-recent-winconf)))
    (setq key win:base-key)
    (while (< key max)
      (define-key win:switch-menu-map (char-to-string key)
	'win-switch-menu-select-directly)
      (setq key (1+ key))))
  (define-key win:switch-map "=" 'win-switch-menu)
  (define-key win:switch-map win:menu-key-stroke 'win-menu)
  (define-key win:switch-map win:resume-key-stroke 'win-resume-menu)
  (define-key win:switch-map win:resume-local-key-stroke 'win-resume-local)
  (define-key win:switch-map win:switch-task-key-stroke 'win-switch-task)
  (define-key win:switch-map " " 'win-toggle-window)
  (define-key win:switch-map "\C-n" 'win-next-window)
  (define-key win:switch-map "n" 'win-next-window)
  (define-key win:switch-map "\C-p" 'win-prev-window)
  (define-key win:switch-map "p" 'win-prev-window)
  (define-key win:switch-map "-" 'win-recover-recent-winconf)
  (define-key win:switch-map "!" 'win-delete-current-window)
  ;; Key map of window selection menu.
  (define-key win:switch-menu-map "n"		'next-line)
  (define-key win:switch-menu-map "p"		'previous-line)
  (define-key win:switch-menu-map "\C-m"	'win-switch-menu-select)
  (define-key win:switch-menu-map " "		'win-switch-menu-select)
  (define-key win:switch-menu-map "f"		'win-switch-menu-select)
  (define-key win:switch-menu-map "s"		'win-switch-menu-mark-job)
  (define-key win:switch-menu-map "l"		'win-switch-menu-mark-job)
  (define-key win:switch-menu-map "d"		'win-switch-menu-mark-job)
  (define-key win:switch-menu-map "k"		'win-switch-menu-mark-job)
  (define-key win:switch-menu-map "u"		'win-switch-menu-unmark-job)
  (define-key win:switch-menu-map "e"           'win-switch-menu-edit-name-prefix)
  (define-key win:switch-menu-map "x"		'win-switch-menu-execute-job)
  (define-key win:switch-menu-map "q"		'win-switch-menu-quit)
  (define-key win:switch-menu-map "?"		'describe-mode)
  ;;Next lines prevent accidents
  (mapcar
   (function
    (lambda (func)
      (mapcar (function (lambda (key)
			  (define-key win:switch-menu-map key "")))
	      (where-is-internal func))))
    '(switch-to-buffer other-window other-frame kill-buffer save-buffer))
  (define-key win:switch-menu-map win:switch-prefix "")
  (define-key win:switch-menu-map (substring win:switch-prefix 0 1) "")
  (run-hooks 'windows-keymap-setup-hook)
  )

;; define menu-bar
(if (or (null (fboundp 'make-frame))
	(or (not (boundp 'window-system)) (not window-system)))
    nil					;Emacs 18 or emacs -nw
  (cond
   (win:xemacs-p
    (defvar win:xemacs-resume-menu
      '("Resume menu"
	["Save all"			(win-resume-menu nil ?a)	t]
	["Resume"			(win-resume-menu nil ?r)	t]
	["Save this window"		(win-resume-menu nil ?s)	t]
	["Load this window"		(win-resume-menu nil ?l)	t]
	["Load from backup file"	(win-resume-menu nil ?~)	t]
	["Wipe"				(win-resume-menu nil ?w)	t]))
    (defvar win:xemacs-top-menu
      '("Windows"
	; :filter windows-menu-filter
	["Next window frame"		win-next-window			t]
	["Preious window frame"	win-prev-window			t]
	["Switch task"			win-switch-task			t]
	["local"			win-resume-local		t]
	["--"				nil				nil]
	["See you! (Revivable)"	see-you-again			t]
	["Revive!"			resume-windows			t]))
    (add-submenu nil win:xemacs-top-menu "Edit")
    (add-submenu '("Windows") win:xemacs-resume-menu)
    (set-menubar current-menubar)
    (set-menubar-dirty-flag) )
   (t					;GNU Emacs-19 or later
    (defvar win:menu-bar-buffer-map (make-sparse-keymap "Windows"))
    (defvar win:menu-bar-file-map (make-sparse-keymap "Windows resume menu"))
    (fset 'win:update-menu-bar
	  (function
	   (lambda ()
	     (define-key-after (lookup-key global-map [menu-bar buffer])
	       [windows] (cons "Windows" win:menu-bar-buffer-map) [frames]))))
    (define-key win:menu-bar-buffer-map [resume]
      (cons "Resume menu" (make-sparse-keymap "Resume menu")))
    (mapcar
     (function
      (lambda (bind)
	(define-key win:menu-bar-buffer-map (vector 'resume (car bind))
	  (cons (nth 1 bind)
		(list 'lambda nil '(interactive)
		      (list 'win-resume-menu nil (nth 2 bind)))))))
     (nreverse
      '((save-all	"Save all"		?a)
	(resume	"Resume"		?r)
	(save	"Save this window"	?s)
	(load	"Load this window"	?l)
	(back	"Load from backup file"	?~)
	(wipe	"Wipe"			?w))))
    (mapcar
     (function
      (lambda (bind)
	(define-key win:menu-bar-buffer-map (vector (car bind)) (cdr bind))))
     (nreverse
      '((next "Next window frame" . win-next-window)
	(prev "Previous window frame" . win-prev-window)
	(delete "Delete current window frame" . win-delete-current-window)
	;;(load "Load all configuration" . win-load-all-configurations)
	;;(save "Save all configuration" . win-save-all-configurations)
	(switch "Switch task" . win-switch-task)
					;(resume "Resume menu" . win-resume-menu)
	(local "Local resume" . win-resume-local)
	)))
    (define-key-after
      (or (lookup-key global-map [menu-bar file])
	  (lookup-key global-map [menu-bar files]))
      [see-you] (cons "See you!(revivable)" 'see-you-again)
      'kill-buffer)
    (define-key-after
      (or (lookup-key global-map [menu-bar file])
	  (lookup-key global-map [menu-bar files]))
      [revive] (cons "Revive!" 'resume-windows)
      'see-you)
    (add-hook 'menu-bar-update-hook 'win:update-menu-bar t))))

(defun win:No-of-windows ()
  (let ((i 1) (num 0))
    (while (< i win:max-configs)
      (if (aref win:configs i) (setq num (1+ num)))
      (setq i (1+ i)))
    num))

(defun win:free-window-min ()
  "Search a window which is not displayed currently nor previously."
  (let ((i 1) num)
    (catch 'free
      (while (< i win:max-configs)
	(if (and (aref win:configs i)
		 (/= i win:current-config)
		 (/= i win:last-config))
	    (throw 'free i))
	(setq i (1+ i)))
      (throw 'free win:last-config))))

(defun win:delete-window (n &optional killbufmsg force)
  "Delete Nth entry of window configuration buffer."
  (if (= n 0) (error "Can't delete window %c" win:base-key))
  (if (and (<= (win:No-of-windows) 1)    ;;if there's 1 window and it is
	   (aref win:configs n))         ;;currently selected window
      (error "Can't delete sole window"))
  (if (or force
	  (y-or-n-p (format "Erase %sconfiguration of %c{%s}?"
			    (if killbufmsg "buffer and " "")
			    (+ n win:base-key) (aref win:names n))))
      (progn
	(cond
	 (win:use-frame (delete-frame (aref win:configs n)))
	  ;; no use-frame
	 ((and (fboundp 'delete-frame) (null window-system)
	       (string< "20" emacs-version))
	  (save-window-excursion
	    (win:set-window-configuration (aref win:configs n))
	    (delete-frame (selected-frame)))))
	(aset win:configs n nil)
	(aset win:names-prefix n "")
	(if (= n win:current-config)
	    (let ((free (win:free-window-min)))
	      (setq win:current-config win:last-config
		    win:last-config free)
	      (win:set-wc win:current-config)))
	(win:update-mode-line win:current-config)))
  (message ""))

(defun win:store-buffer-list (index)
  "(Windows internal)Store current window's buffer-list top 5"
  (let ((d 0) (bl (buffer-list)))
    (while (and (< d win:buffer-depth-per-win) bl)
      (or (eq (window-buffer (minibuffer-window)) (car bl))
          ;(string-match "\\*$" (buffer-name (car bl)))
	  (progn			;buffer-listの順序保存
	    (aset (aref win:buflists index) d (buffer-name (car bl)))
	    (setq d (1+ d))))
      (setq bl (cdr bl)))))

(defun win:restore-buffer-list (index)
  "(Windows internal)Restore current window's buffer-list previously saved."
  (save-window-excursion
    (if (eq (selected-window) (minibuffer-window))
	;;escape from minibuffer
	(let ((wl (revive:window-list)))
	  (while wl
	    (select-window (car wl))
	    (if (eq (selected-window) (minibuffer-window))
		(setq wl (cdr wl))
	      (setq wl nil)))))
    (let ((d win:buffer-depth-per-win) b)
      ;(select-window (get-largest-window))
      (delete-other-windows)
      (while (> d 0)
	(setq d (1- d)
	      b (aref (aref win:buflists index) d))
	(and b
	     (get-buffer b)
	     (switch-to-buffer b))))))

(defun win:switch-window (to &optional preserve nosetwc)
  "(windows internal) Switch to window TO, changing win:{last,current}-config.
Optional second arg PRESERVE non-nil inhibits updation of win:current-config.
Optional third arg NOSETWC non-nil means do not set-window-configuration TO.
"
  (win:adjust-window t)
  (if win:buffer-depth-per-win (win:store-buffer-list win:current-config))
  (cond
   (win:use-frame
    (or preserve
	(if (win:frame-window (selected-frame))
	    (win:store-config win:current-config)))
    (if (aref win:configs to) nil
      (aset win:configs to 
	    (if (win:selected-window)
		(win:allocate-frame to)
	      (selected-frame))))
    (if (win:select-frame to)		;if succeeded
	(if win:buffer-depth-per-win (win:restore-buffer-list to))
      (aset win:configs to nil)		;failed
      (error "Window frame [%c] does not exist." (+ win:base-key to))))
   (t					;NOT use-frame
    (or preserve (win:store-config win:current-config))
    (or nosetwc (win:set-wc to))
    (if win:buffer-depth-per-win
	(save-window-excursion (win:restore-buffer-list to)))
    (and win:frames-in-nw
	 (null (aref win:configs to))
	 (select-frame
	  (make-frame (list (cons 'name (format "W:%d" to))))))))
  (win:update-mode-line to)
  (if (/= to win:current-config)
      (setq win:last-config win:current-config
	    win:current-config to))
  (win:memorize-current-conf-1)
  (message "Switch to window[%c]" (+ to win:base-key)))

(defun win-delete-current-window (arg)
  "Delete selected window configuration buffer.
Non-nil for ARG kills all visible buffer."
  (interactive "P")
  (if win:use-frame
      (win:adjust-window t)
    (win:set-window-name win:current-config)) ;set window name
  (let ((cwin (selected-window)) win (cc win:current-config)
	(blist (list (current-buffer))))
    (if arg
	(progn
	  (setq win cwin)
	  (while (not (eq cwin (setq win (next-window win))))
	    (setq blist (cons (window-buffer win) blist)))))
    (win:delete-window win:current-config arg)
    (if (and arg (/= cc win:current-config))
	(progn
	  (while blist
	    (if (get-buffer (car blist))
		(kill-buffer (car blist)))
	    (setq blist (cdr blist)))))))

(defun win:kill-buffer-with-window ()
  "Kill current window if it was created by KeyFunc&Exit."
  (and win:kill-buffer-with
       (equal win:current-config win:kill-buffer-with)
       (win:delete-window win:current-config nil t)))

(defvar win:kill-buffer-with nil
  "If win:current-config is equal to this, delete this window.")
(make-variable-buffer-local 'win:kill-buffer-with)
;;;###autoload
(defun win-switch-to-window (arg &optional window)
  "Switch window configurations to a buffer specified by keyboard.
If calling from program, optional second argument WINDOW can specify
the window number."
  (interactive "p")
  (let*((window (or window
                    (if (and (> ?\M-0 0) (<= ?\M-0 last-command-char))
                        (- last-command-char ?\M-0)
                      (- last-command-char win:base-key))))
	(wc (aref win:configs window)))
    (cond
     ((and win:inhibit-switch-in-minibuffer
	   (eq (minibuffer-window) (selected-window)))
      (message "Really? Then, set win:inhibit-switch-in-minibuffer to nil!")
      (sit-for 1))
     ;;if ARG=0, switch to recent window.
     ((= window 0) (win-recent-window))
     ;;if ARG<0, delete specified(by invoke key) window.
     ((< arg 0)
      (win:delete-window window))
     ;;if ARG>0, update specified(by invoke key) window.
     ((> arg 1)
      (win:switch-window window t t)
      (message "Update window[%c]" (+ window win:base-key)))
     ;;if specified window is current window.
     ((= win:current-config window)
      (if (= arg 0)
	  (win:set-wc window)
	(if win:use-frame (win:switch-window window)
	  (message "This is window[%c]." (+ window win:base-key)))))
     ;;else...
     ;;switch to specified window.
     (t
      (if (aref win:configs window)
	  ;;if target window already exists.
	  (win:switch-window window)
	;;if target window does not exist.
	(let (ans)
	  (if (= (+ win:current-config win:last-config) 0)
	      (message "Create window[%c]?(y or n):" (+ window win:base-key))
	    (message
	     (concat
	      "[%c] is nil. C)reate D)up "
	      (if win:use-frame "" "P)reserve ")
	      "F)indfile B)uff X)M-x k)KeyFunc&exit N)o:")
	     (+ window win:base-key)))
	  ;;(setq ans (if (interactive-p) (read-char) ?y))
	  (setq ans (read-char))
	  (if (string-match (char-to-string ans) "cdpyfbxk")
	      (let (inhibitmsg)
		(cond
		 ((equal ans ?f)
		  (let ((file (read-file-name "Find file on new window: ")))
		    (win:switch-window window nil t)
		    (delete-other-windows)
		    (switch-to-buffer (get-buffer-create "*scratch*"))
		    (sit-for 0)
		    (find-file file)))
		 ((equal ans ?b)
		  (let ((buf (read-buffer "Switch to buffer on new window: "
					  (other-buffer))))
		    (win:switch-window window nil t)
		    (delete-other-windows)
		    (switch-to-buffer buf)))
		 ((equal ans ?x)
		  (let ((cmd (read-command "M-x on new window: ")))
		    (win:switch-window window nil t)
		    (delete-other-windows)
		    (switch-to-buffer (get-buffer-create "*scratch*"))
		    (sit-for 0)
		    (setq inhibitmsg t)
		    (call-interactively cmd)))
		 ((equal ans ?k)
		  (let ((cmd (key-binding
			      (read-key-sequence
			       "Keysequence function on new window(deleted in the future): "))))
		    (win:switch-window window nil t)
		    (delete-other-windows)
		    (switch-to-buffer (get-buffer-create "*scratch*"))
		    (sit-for 0)
		    (setq inhibitmsg t)
		    (call-interactively cmd)
		    (setq win:kill-buffer-with win:current-config)
		    (add-hook 'kill-buffer-hook 'win:kill-buffer-with-window)))
		 ((and (string-match (char-to-string ans) "py")
		       (null win:use-frame))
		  (win:switch-window window t t))
		 ((equal ans ?d)
		  (require 'revive)
		  (let ((conf (current-window-configuration-printable)))
		    (win:switch-window window nil t)
		    (restore-window-configuration conf)))
		 ((string-match (char-to-string ans) "cy")
		  (win:switch-window window nil t)
		  (delete-other-windows)
		  (switch-to-buffer (get-buffer-create "*scratch*")))
		 (nil t))
		(or inhibitmsg
		    (message "Memorize current window configuration into %c"
			     (+ window win:base-key))))))
	)))))

(defun win-menu (arg)
  "Windows menu for operations on window configuration."
  (interactive "p")
  (message
   "N)ext P)rv R)ecent D)el recoV)er K)ill S)ave2buf L)oad-from-buf saveA)s sW)ap")
  (let ((c (downcase (read-char))))
    (cond
     ((= c ?n) (win-next-window arg))
     ((= c ?p) (win-prev-window arg))
     ((= c ?r) (win-recent-window))
     ((= c ?v) (win-recover-recent-winconf arg))
     ((= c ?d) (win-delete-current-window nil))
     ((= c ?k) (win-delete-current-window t))
     ((= c ?l) (win-load-window-buffer))
     ((= c ?s) (win-update-window-buffer))
     ((= c ?a) (win-save-as))
     ((= c ?w) (call-interactively 'win-swap-with))
     (t nil))))

;; Interactive functions.
(defun win-delete-current-window-force ()
  (interactive)
  (win-delete-current-window t))
(defun win-load-window-buffer ()
  (interactive)
  (win:set-wc win:current-config))
(defun win-update-window-buffer ()
  (interactive)
  (win:store-config win:current-config)
        (message "Update window [%d]" win:current-config))
(defun win-save-as ()
  (interactive)
  (let (c)
    (message "Save current window configuration into buffer # ?")
    (setq c (- (read-char) win:base-key))
    (if (or (< c 1) (> c win:max-configs))
	(error "Buffer number out of range"))
    (if (and win:use-frame
	     ;;(aref win:configs c)
	     ;;(frame-live-p (aref win:configs c))
	     (win:selected-window)
	     )
	(error "Window frame [%c] already used" (+ win:base-key c))
      (win:store-config c)
      (win:switch-window c))
    (message "Save current window configuration into buffer[%c]"
	     (+ win:base-key c))))

(defun win-swap-with (win)
  "Swap current window configuration with WIN-th window's."
  (interactive "cSwap with: ")
  (if (or (< win win:base-key) (> win (+ win:base-key win:max-configs)))
      (error "Window number out of range"))
  (let* ((n (- win win:base-key)) (tmp (aref win:configs n)) conf1 conf2)
    (or tmp (error "Window [%c] is null." win))
    (win:adjust-window)
    (if (= n win:current-config) (error "[%c] is current window." win))
    (cond
     (win:use-frame
      (setq conf1 (current-window-configuration-printable))
      (select-frame (aref win:configs n))
      (setq conf2 (current-window-configuration-printable))
      (restore-window-configuration conf1)
      (select-frame (aref win:configs win:current-config))
      (restore-window-configuration conf2))
     (t
      (aset win:configs n (win:current-window-configuration))
      (win:set-window-name win:current-config)
      (aset win:names n (aref win:names win:current-config))
      (win:set-window-configuration tmp))))
)

(defun win:read-config-file ()
  "Read window configuration file from minibuffer."
  (let ((dir (file-name-directory win:local-config-file)))
    (setq dir (expand-file-name
	       (read-file-name "Configuration file directory: " dir dir 1)))
    (if (not (string-match "[\\/]$" dir))
	(setq dir (concat dir "/")))
    (if (not (file-directory-p dir)) (error "No such directory [%s]" dir))
    (setq win:local-config-file
	  (concat dir (file-name-nondirectory win:configuration-file)))))

(defun win-switch-task (force-save &optional config-file)
  "Switch to other configuration environments.
If FORCE-SAVE is non-nil, save current environment without query.
Optional second arg CONFIG-DIR specifies the configuration file to switch.
Using from program, non-interactively, don't forget to specify second arg."
  (interactive "P")
  (if (or force-save
	  (y-or-n-p (format "Save current configuration in [%s]? "
			    win:local-config-file)))
      (let ((win:configuration-file win:local-config-file))
	(win-save-all-configurations)))
  (let ((win:configuration-file (or config-file (win:read-config-file))))
    (setq win:local-config-file win:configuration-file)
    (if (file-exists-p win:local-config-file)
	(win-load-all-configurations)
      (wipe-windows t)
      (message "New task started"))))

(defun win-resume-local (arg)
  "Call resume operations with local configuration file."
  (interactive "P")
  (let (dir (win:configuration-file (win:read-config-file)))
    (win-resume-menu arg)))

(defun win-resume-menu (&optional arg key)
  "Windows menu for resume operations."
  (interactive "P")
  (message
   "A)save-all R)esume-all S)ave-this L)oad-this N)Load# ~)read-~ W)ipe")
  (let ((c (or key (downcase (read-char)))))
    (win:adjust-window)
    (cond
     ((= c ?a) (win-save-all-configurations))
     ((= c ?r) (win-load-all-configurations arg))
     ((= c ?s) (win:save-window win:current-config))
     ((= c ?l) (win:load-window win:current-config))
     ((= c ?n)
      (message "Load the window No.?")
      (win:load-window (- (read-char) win:base-key)))
     ((= c ?~)
      (let ((win:configuration-file
	     (make-backup-file-name win:configuration-file)))
	(win-load-all-configurations)))
     ((= c ?w)
      (require 'revive)
      (if (y-or-n-p "Do you wish to save this configurations? ")
	  (let ((win:configuration-file (win:read-config-file)))
	    (win-save-all-configurations)))
      (wipe-windows t))
     )))

(defun win-toggle-window ()
  "Toggle current window and most recently used one."
  (interactive)
  (win:adjust-window)
  (if (or (= win:last-config win:current-config)
	  (equal (aref win:configs win:last-config) nil))
      (message "No other window")
    (win:switch-window win:last-config)))

(defun win-recent-window ()
  "Switch to recently displayed window saved in temporary buffer.
If Windows uses frame, switch to frame unallocated to Windows."
  (interactive)
  ;;(let ((last-wc (aref win:configs 0))(last-sz (aref win:sizes 0)))
  ;;  (if last-wc (win:set-wc last-wc last-sz)))
  (if win:use-frame
      (let ((cf (selected-frame)) (frame (next-frame)))
	(if (catch 'found
	      (while (not (eq frame cf))
		(if (null (win:frame-window frame)) (throw 'found t))
		(setq frame (next-frame frame))))
	    (progn
	      (select-frame frame)
	      (raise-frame frame)
	      (focus-frame frame)
	      (apply 'set-mouse-pixel-position
		     (if win:xemacs-p (selected-window) frame)
		     win:mouse-position)
	      (win:update-mode-line 0))
	  (message "There are no %sunallocated frames for Windows."
		   (if (win:frame-window (selected-frame)) "" "other ")))
	frame)
    (win:set-wc 0)
    (win:update-mode-line win:current-config)))

(defun win-next-window (arg)
  "Switch to window saved in the ARG-th next configuration buffer."
  (interactive "p")
  (if (<= (win:No-of-windows) 1)
      (message "There is no other window.")
    (let ((count 0)(num win:current-config)
	  (incr (if (> arg 0) 1 (1- win:max-configs))))
      (setq arg (max arg (- arg)))
      (while (< count arg)
	(setq num (% (+ num incr) win:max-configs))
	(if (and (aref win:configs num) (/= num 0))
	    (setq count (1+ count))))
      (setq num (% num win:max-configs))
      (if (= num win:current-config)
	  (message "Next window by %d is this window." num)
	(win:switch-window num)))))

(defun win-prev-window (arg)
  "Switch to window saved in the ARG-th previous congiguration buffer."
  (interactive "p")
  (win-next-window (- arg)))

;;;###autoload
(defun win:set-wc (num)
  "(Windows low level internal) Set the NUM-th windows configuration.
If Windows uses frame(Emacs 19), Select the NUM-th window frame."
  (let*((size (aref win:sizes num))
	(oldh (nth 0 size)) (oldw (nth 1 size))
	(wc (aref win:configs num)))
    (win:store-config 0)  ;;save current configuration into array[0]
    (if win:use-frame
	(win:select-frame num)
      (win:set-window-configuration wc))))

(cond
 ((fboundp 'screen-height)
  (fset 'win:screen-height 'screen-height)
  (fset 'win:screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'win:screen-height 'frame-height)
  (fset 'win:screen-width 'frame-width))
 (t (error "I don't know how to run windows.el on this Emacs...")))

(defun win:store-config (index)
  "(Windows low level intenal)Save current configuration in INDEX-th of array."
  (aset win:configs index
	(if win:use-frame (selected-frame) (win:current-window-configuration)))
  (win:set-window-name index)
  (aset win:sizes index (list (win:screen-height) (win:screen-width))))

(defun win:current-window-configuration ()
  (cons (current-window-configuration) (point-marker)))

(defun win:set-window-configuration (wc)
  (prog1
      (set-window-configuration (car wc))
    (if (marker-buffer (cdr wc))
	(progn (goto-char (cdr wc))
	       (set-marker (cdr wc) nil)))))

(defun win:get-frame (index)
  "(windows internal) Get INDEX-th wnidows frame object"
  (let ((obj (aref win:configs index)))
    (if win:frames-in-nw
	(if obj (window-configuration-frame (car-safe obj)) (selected-frame))
      obj)))

(defun win:set-window-name (index)
  "(Windows low level intenal)Set current window name to INDEX-th of array."
  (require 'revive)
  (aset win:names index
	(mapconcat (function (lambda (w) (buffer-name (window-buffer w))))
		   (revive:window-list) " / ")))

(defvar win:mode-string nil)
(defun win:update-mode-line (number)
  "Update mode line for selected window number NUMBER."
  (let ((prefix (aref win:names-prefix number)))
    (setq win:mode-string (format win:mode-line-format
                                  (+ number win:base-key)
                                  (if (string= prefix "") "" ":")
                                  prefix
                                )))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))


(defun win-edit-name-prefix (index)
  "Edit win:names-prefix of INDEXth window."
  (interactive (list win:current-config))
  (aset win:names-prefix index
        (read-string "Window name: " (aref win:names-prefix index)))
  (if (= win:current-config index) (win:update-mode-line index)))
  
(defun win:recover-deleted-frames ()
  "Detect frames deleted at initialization, and allocate new one for Windows.
Do not call this function."
  (let ((i 1) newframe)
    (if (catch 'deleted
	  (while (< i win:max-configs)
	    (if (and (aref win:configs i)
		     (not (frame-live-p (aref win:configs i))))
		(throw 'deleted t))
	    (setq i (1+ i))))
	(progn
	  (aset win:configs i (setq newframe (win:allocate-frame i)))
	  (modify-frame-parameters (aref win:configs i) '((name)))
	  (select-frame newframe)
	  (let ((index i)(frame newframe))
	    (run-hooks 'win:allocate-frame-hook))
	  (win:load-window i))
      )))

;;;###autoload
(defun win:startup-with-window ()
  "Start up Emacs with window[1] selected."
  (if (aref win:configs 1) nil
    (and (boundp 'default-frame-plist)	;for XEmacs initial variable
	 (listp (car default-frame-plist))
	 (setq default-frame-plist (alist-to-plist default-frame-plist)))
    (cond
     ((and win:xemacs-p
	   (lax-plist-get default-frame-plist 'minibuffer))
      (win:remake-frame)
      (setq win:current-config 1)
      (win:store-config 1)
      (win:update-mode-line 1)
      (message "Startup with window [1]"))
     ((and win:use-frame (not (interactive-p)))
      ;; When called from .emacs, since the base frame at that time
      ;; will be deleted by the function frame-notice-user-settings,
      ;; allocating frame here won't work.  So we set window-setup-hook to
      ;; do all the jobs after frame initialization has been done.
      (add-hook 'window-setup-hook
		'(lambda ()
		   (if (aref win:configs 1) ;Already resumed by resume-windows
		       ;; `emacs -e resume-windows' leads to this section.
		       (progn
			 (win:recover-deleted-frames)
			 (if (get 'resume-windows 'lc)
			     ;; Adjust window visiting list.
			     ;; See also the comment of resume-windows.
			     (let ((lc (get 'resume-windows 'lc)))
			       (win:switch-window (car (cdr lc)))
			       (put 'resume-windows 'lc nil)
			       (setq win:last-config (car lc))
			       (message "Startup with window[%d]"
					win:current-config))))
		     (let ((count 30))
		       (while (and (null (visible-frame-list))
				   (> count 0))
			 (message "Waiting for frames to be shown...")
			 (sit-for (string-to-number "0.1"))
			 (setq count (1- count))))
		     (select-frame
		      (car
		       (or (filtered-frame-list
			    (function
			     (lambda (f)
			       (and
				(eq (cdr (assq 'visibility
					       (frame-parameters f)))
				    t)
				(not (member f (minibuffer-frame-list)))))))
			   (visible-frame-list))))
		     (win:remake-frame) ;to change frame title with "mule[1]"
		     (setq win:current-config 1)
		     (win:store-config 1)
		     (win:update-mode-line 1)
		     (and
		      (assq 'minibuffer default-frame-alist)
		      (null (cdr (assq 'minibuffer default-frame-alist)))
		      (boundp 'minibuffer-frame-alist)
		      minibuffer-frame-alist
		      (modify-frame-parameters
		       (car (minibuffer-frame-list))
		       minibuffer-frame-alist))
		     (message "Startup with window [1]")
		     (let ((index 1) (frame (aref win:configs 1)))
		       (run-hooks 'win:allocate-frame-hook))
		     (sit-for 1))
		   'append)))
     (t					;not frame environment
      (win:store-config 1)
      (win:update-mode-line 1)
      (setq win:current-config 1)
      (message "Startup with window[1]")))))

;;;
;; Functions for resume.
;;;
(defconst win:revision
  "$Revision: 2.48 $"
  "Revision string of windows.el")
(defvar win:revision-prefix ";win;")

(defun win:frame-parameters (&optional frame)
  "Return frame parameters of FRAME (defaults to nil) which you want to save."
  (let ((parm (frame-parameters frame))
	(tosave (append win:frame-parameters-to-save-default
			win:frame-parameters-to-save-private)))
    (delq nil
	  (mapcar
	   (function 
	    (lambda (s)
	      (if (memq (car s) tosave)
		  (if (and (eq 'minibuffer (car s))
			   (windowp (cdr s)))
		      (cons 'minibuffer t)
		    s))))
	   parm))))

(defun win:save-window (winnum)
  "Save window WINNUM into configuration file.
Configuration to be saved is a list of
 (WINNUM (current-window-configuration-printable))"
  (require 'revive)
  (let ((conf (current-window-configuration-printable))
	(make-backup-files win:make-backup-files)
	(version-control 'never)
	(hilit-auto-highlight nil)
	i)
    (set-buffer (find-file-noselect win:configuration-file))
    (widen)
    (goto-char (point-min))
    (if (re-search-forward "^([0-9]" nil t)
	(goto-char (match-beginning 0))
      (goto-char (point-max))
      (if (not (bolp)) (newline 1)))
    (if (re-search-forward (format "^(%d " winnum) nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (kill-sexp 1))
      (catch 'here
	(while (re-search-forward "^(\\([0-9]+\\)" nil 1)
	  (setq i (string-to-int
		   (buffer-substring (match-beginning 1) (match-end 1))))
	  (if (> i winnum)
	      (progn (open-line 1) (throw 'here t))))))
    (if (not (bolp)) (newline 1))
    (delete-blank-lines)
    ;; Now save the current window configuration.
    (insert
     (format "%s\n"
	     (prin1-to-string
	      (list winnum conf
		    (if win:use-frame
			(win:frame-parameters (aref win:configs winnum)))
		    (aref win:names-prefix winnum) ;;2.26
		    ))))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    (message "Saved window [%d]" winnum)))

;;;###autoload
(defun win-save-all-configurations ()
  "Save all window configurations into the configuration file."
  (interactive)
  (message "Saving all window configurations")
  (require 'revive)
  (win:adjust-window)
  (win:store-config win:current-config)
  (let ((i 1) (buflist (revive:buffer-property-list))
	(bakfile (concat win:configuration-file ".~~"))
	(hilit-auto-highlight nil)
	(version-control 'never))
    (if (and win:make-backup-files
	     (file-exists-p win:configuration-file))
	(copy-file win:configuration-file bakfile t))
    (set-buffer (find-file-noselect win:configuration-file))
    (widen)
    (erase-buffer)
    (insert (format "%s%s\n\n" win:revision-prefix win:revision))
    (insert (format ";;buffer-list\n%s\n\n;;window-configurations\n"
		    (prin1-to-string
		     (list buflist win:current-config win:last-config))))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    (while (< i win:max-configs)
      (if (aref win:configs i)
	  (progn
	    (if win:use-frame
		(progn
		  (select-frame (aref win:configs i))
		  (or win:no-raise-at-save (raise-frame (aref win:configs i))))
	      (win:set-window-configuration (aref win:configs i)))
	    (message "Saving window [%d]..." i)
	    (win:save-window i)
	    ))
      (setq i (1+ i)))
    (if (and win:make-backup-files
	     (file-exists-p bakfile))
	(let ((bk (make-backup-file-name win:configuration-file)))
	  (rename-file bakfile bk t)))
    (message "Saved all configurations in [%s]" win:configuration-file)
    (if win:use-frame
	(win:select-frame win:current-config)
      (win:set-window-configuration (aref win:configs win:current-config)))))

(defun win:load-window (arg)
  "Load the ARG-th window configuration from the configuration file."
  (require 'revive)
  (if (or (< arg 0) (> arg win:max-configs))
      (error "Window number %d out of range" arg))
  (if (null (file-exists-p win:configuration-file))
      (error "Configuration file %s not found" win:configuration-file))
  (let ((curb (current-buffer)) confb config)
    (set-buffer (find-file-noselect win:configuration-file))
    (setq confb (current-buffer))
    (widen)
    (goto-char (point-min))
    (if (null (re-search-forward (format "^(%d " arg) nil t))
	(error "Window [%s] is not found in configuration file." arg))
    (goto-char (match-beginning 0))
    (setq config (read (current-buffer)))
    (kill-buffer confb)
    (switch-to-buffer curb)
    (if (/= win:current-config arg)
	(win:switch-window arg nil t))
    (win:set-frame-configuration config)
    (win:store-config win:current-config)
    (setq win:config-loaded t)
    (win:switch-window arg)))

;;;###autoload
(defun wipe-windows (&optional no-ask)
  "Kill all buffers.  Optional argument NO-ASK non-nil skips query."
  (interactive "P")
  (if (and (null no-ask)
	   (not (y-or-n-p "Are you sure to wipe Emacs? ")))
      (error "Aborted"))
  (save-some-buffers)
  (let ((i 2))
    (while (< i win:max-configs)
      (and win:use-frame win:wipe-also-frames (aref win:configs i)
	   (delete-frame (aref win:configs i)))
      (aset win:configs i nil)
      (setq i (1+ i))))
  (mapcar (function kill-buffer) (buffer-list))
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (funcall initial-major-mode)
  (delete-other-windows)
  (win:store-config 1)
  (win:update-mode-line 1))

(defun win:numericify (obj)
  "Force to numericify object of frame position parameter."
  (cond
   ((integerp obj) obj)
   ((and (listp obj) (= (length obj) 2) (memq (car obj) '(+ -)))
    (eval obj))
   (t 100)))

(defun win:set-frame-configuration (config)
  (if win:use-frame
      (let ((conf (car (cdr config))) x y (params (nth 2 config)) frame)
	;;win:switch-window is the easiest way to resume frames.
	(win:switch-window (car config))
	(setq frame (aref win:configs (car config)))
	(set-frame-width frame (car conf))
	(set-frame-height frame (car (cdr conf)))
	(if (setq x (assq 'left params))
	    (setcdr (car (member x params))
		    (+ (win:numericify (cdr x)) win:resumed-frame-offset-x)))
	(if (setq y (assq 'top params))
	    (setcdr (car (member y params))
		    (+ (win:numericify (cdr y)) win:resumed-frame-offset-y)))
	(if win:xemacs-p		;XEmacs is unwilling to set minibuffer
	    (setq params (delq (assq 'minibuffer params) params)))
	(modify-frame-parameters frame params))
    (win:switch-window (car config) t t))
  (if (nth 3 config) (aset win:names-prefix (car config) (nth 3 config)))
  (restore-window-configuration (car (cdr config))))

;;;###autoload
(defun win-load-all-configurations (&optional preserve)
  "Load all window configurations from the configuration file.
Non-nil for optional argument PRESERVE keeps all current buffers."
  (interactive "P")
  (if (null (file-exists-p win:configuration-file))
      (error "Configuration file %s not found" win:configuration-file))
  (message "Loading all window configurations")
  (require 'revive)
  (let ((i 0) buflist wconflist buf)
    (if (null preserve)
	(wipe-windows t))
    (if (aref win:configs 1) (win:switch-window 1))
    (set-buffer (find-file-noselect win:configuration-file))
    (setq buf (current-buffer))
    (widen)
    (goto-char (point-min))
    (if (null (search-forward win:revision-prefix nil t))
	(error "Configuration file collapsed"))
    (if (and (not (string= win:revision
		      (buffer-substring
		       (point)
		       (progn (end-of-line)
			      (skip-chars-backward "^$") (point)))))
	     (not (y-or-n-p
		   "Configuration file version conflicts. Continue?")))
	(error "Version of configuration file conflicts.  Please update."))
    (if (null (re-search-forward "^(" nil t))
	(error "Configuration empty"))
    (goto-char (match-beginning 0))
    (setq buflist (read (current-buffer)))
    (setq win:current-config (nth 1 buflist) win:last-config (nth 2 buflist))

    ;;read all configs
    (while (re-search-forward "^([0-9]+ " nil t)
      (goto-char (match-beginning 0))
      (setq wconflist (cons (read (current-buffer)) wconflist)))
    (setq wconflist (nreverse wconflist))
    (set-buffer-modified-p nil)
    (kill-buffer buf)

    ;;Start restore
    (revive:restore-buffers (car buflist))
    (while wconflist
      (message "Restoring window [%d]..." (car (car wconflist)))
      (let ((win:current-config win:current-config)
	    win:last-config frame)
	(win:set-frame-configuration (car wconflist)))
      (win:store-config (car (car wconflist)))
      (win:restore-buffer-list (car (car wconflist)))
      (setq wconflist (cdr wconflist)))
    (win:set-wc win:current-config)
    (win:update-mode-line win:current-config)
    (setq win:config-loaded t))
  (message "Finish loading.  Resume in window [%d]" win:current-config))

(or global-mode-string (setq global-mode-string '("")))
(or (memq 'win:mode-string global-mode-string)
    (setq global-mode-string (append global-mode-string '(win:mode-string))))

;;;
;; For Emacs 19 frame feature
;;;
(defun win:select-frame (num)
  "Select the NUM-th window frame."
  (if (= (length (frame-list)) 1)
      (if (eq (selected-frame) (aref win:configs num)) (selected-frame) nil)
    (let ((goal (aref win:configs num)))
      (if (null (frame-live-p goal))
	  (aset win:configs num nil)	;returns NIL
	;(if (eq (cdr (assq 'visibility (frame-parameters goal))) 'icon)
	;    (make-frame-visible goal))	;to de-iconify(if iconified)
	;;'visibility attribute is not defined in XEmacs...
	(or (eq t (frame-visible-p goal))
	    (make-frame-visible goal))
	(while (not (frame-visible-p goal)) (sit-for 0))
	(raise-frame goal)
	(select-frame goal)
	(if (fboundp 'x-focus-frame) (x-focus-frame goal))
	(if (not (eq (selected-frame) goal))
	    nil
	  (or win:xemacs-p (unfocus-frame))
	  ;; Emacs-21.0.9x sometimes fails to display some part of frame
	  (if (string< "21.0.92" emacs-version)
	      (sit-for (string-to-number "1e-5")))
	  (apply 'set-mouse-pixel-position
		 (if win:xemacs-p (selected-window) goal)
		 win:mouse-position)
	  goal)))))

(defun win:selected-window ()
  "Return the window number if selected-frame is a member of window list."
  (let ((i 1) (sf (selected-frame)) found)
    (while (and (< i win:max-configs) (not found))
      (if (eq sf (win:get-frame i))
	  (setq found t))
      (setq i (1+ i)))
    (if found (1- i))))

(defconst win:frames-in-nw
  (and (null window-system)
       (fboundp 'window-configuration-frame) ;not in XEmacs
       (fboundp 'make-frame)
       (string< "20" emacs-version))
  "Flags whether using frame feature in -nw mode or not.")
(defun win:adjust-window (&optional noerror)
  "Adjust win:current-config to (selected-frame).
If optional argument NOERROR is non-nil, do not stop even if the selected
window is not a member of window list."
  (interactive)
  (cond
   ((or win:use-frame win:frames-in-nw)
    (catch 'escape
      (if (eq (win:get-frame win:current-config) (selected-frame))
	  nil				;selected frame is current window.
	(let ((sw (win:selected-window)))
	  (if (not sw)
	      (if noerror
		  (throw 'escape t)
		(error "This frame is not under control of windows."))
	    (setq win:last-config win:current-config
		  win:current-config sw)
	    (win:update-mode-line win:current-config))))
      (if (and win:title-with-buffer-name (not win:xemacs-p)
	       win:frame-title-function
	       (not win:frames-in-nw))
	  (modify-frame-parameters
	   nil
	   (list
	    (cons 'name
		  (concat (funcall
			   win:frame-title-function win:current-config)
			  ":" (aref win:names-prefix win:current-config)
			  "(" (buffer-name) ")")))))))))

(defun win:title-mode-line-updater ()
  (if (eq this-command (get 'win:title-mode-line-updater 'last))
      nil				;Continuous call(maybe) canceled
    (condition-case err
	(progn
	  (win:adjust-window t)
	  (put 'win:title-mode-line-updater 'buffer (current-buffer)))
      (error nil))))

(defun win:title-mode-line-updater2 ()
  (if (eq this-command (get 'win:title-mode-line-updater 'last))
      nil
    (put 'win:title-mode-line-updater 'last this-command)
    (or (eq (get 'win:title-mode-line-updater 'buffer) (current-buffer))
	(condition-case err
	    (progn
	      (win:adjust-window t))
	  (error nil)))))

(if (and win:use-frame win:need-uptodate-frame-title)
    (progn
      (add-hook 'pre-command-hook 'win:title-mode-line-updater)
      (add-hook 'post-command-hook 'win:title-mode-line-updater2)))

(defvar win:memorized-winconf (make-vector win:max-configs nil)
  "Vector for temporary window-configurations of each frames.")

(defun win:memorize-current-conf-1 ()
  "(windows.el internal) Memorize recent window-configuration."
  (win:adjust-window t)
  (aset win:memorized-winconf win:current-config (win:current-window-configuration)))

(defun win:memorize-current-conf (&optional from to old)
  "Memorize recent window-configuration.
Called by after-change-functions"
  (if (or (eq (minibuffer-window) (selected-window))
	  (null (get-buffer-window (current-buffer)))
	  (and (stringp (buffer-name))
	       ;;(string-match "^[ *]" (buffer-name))
	       ;;String-match causes error, why!!??
	       (memq (aref (buffer-name) 0) '(?* ? ?\t))))
      t
    (win:memorize-current-conf-1))
  nil)

(defun win-recover-recent-winconf (arg)
  "Recover recent window configuration
This is useful when some elisp destroys your editing window configuration.
With prefix ARG non-nil, restore all window configuration including
selected window.  Without ARG, selected window and its point reserved."
  (interactive "p")
  (win:adjust-window)
  (let ((memorized (aref win:memorized-winconf win:current-config))
	(sw (selected-window))
	(ws (window-start))
	(p (point)))
    (if (null memorized)
	(message "No conf. yet.  Modify this buffer, and you get conf.")
      (aset win:memorized-winconf win:current-config
	    (win:current-window-configuration))
      (win:set-window-configuration memorized)
      (if (equal 1 arg)
	  (progn
	    (select-window sw)
	    (set-window-start sw ws)
	    (goto-char p))))))

(defvar win:memorize-winconf-timing 'save
  "*Timing of memorizing window configuration of selected frame.
Choose one of 'save 'change.
'save	Use write-file-hook.
'change	Use after-change-functions.")

(cond
 ((eq win:memorize-winconf-timing 'change)
  (add-hook 'after-change-functions 'win:memorize-current-conf nil))
 ((eq win:memorize-winconf-timing 'save)
  (add-hook 'write-file-hooks 'win:memorize-current-conf nil)))

;(remove-hook 'after-change-functions 'win:memorize-current-conf nil)

(defun win:adjust-names ()
  "Adjust window names stored in win:names (for frame environment)."
  (cond
   (win:use-frame
    (let ((i 1))
      (while (< i win:max-configs)
	(if (aref win:configs i)
	    (progn
	      (select-frame (aref win:configs i))
	      (win:set-window-name i)))
	(setq i (1+ i))))
    (select-frame (aref win:configs win:current-config)))))

(defun win:other-frame (arg)
  "Switch to other frame for windows."
  (interactive "p")
  (win:adjust-window t)
  (other-frame arg)
  (if (null (win:adjust-window t)) (win:update-mode-line 0)))
(mapcar (function
	 (lambda (s)
	   (define-key global-map s 'win:other-frame)))
	(where-is-internal 'other-frame))

(defun win:frame-window (frame)
  "If FRAME is a member of window list return that index, else return nil."
  (let ((i 1))
    (catch 'found
      (while (< i win:max-configs)
	(if (eq frame (aref win:configs i)) (throw 'found i))
	(setq i (1+ i))))))

(defun win:allocate-frame (index)
  "Allocate a new frame for buffer list of Windows.
If all frames are members of window buffer, create new frame.
If there is a frame which don't belong to window buffer, return it.
INDEX is referred to decide the frame position."
  (let ((flist (reverse (frame-list))) frame)
    (if (catch 'found
	  (while flist
	    (or (member (car flist) (minibuffer-frame-list))
		(window-dedicated-p (frame-selected-window (car flist)))
		(if (null (win:frame-window (car flist)))
		    (throw 'found (setq frame (car flist)))))
	    (setq flist (cdr flist))))
	frame
      (setq
       frame
       (win:make-frame
	index
	(if (and win:auto-position (numberp index)
		 (memq 'top (append win:frame-parameters-to-save-default
				    win:frame-parameters-to-save-private)))
	    (let (frame (minn 1) minl mint left top)
	      (cond
	       ((eq win:auto-position 'absolute)
		(or (catch 'found	;search minimumly numbered window
		      (while (< minn win:max-configs)
			(if (aref win:configs minn) (throw 'found t))
			(setq minn (1+ minn))))
		    (setq minn 1 minl 0 mint 0)) ;not found
		(setq frame (aref win:configs minn)
		      minl (cdr (assq 'left (frame-parameters frame)))
		      minl (- (win:numericify minl)
			      (* win:new-frame-offset-x (1- minn)))
		      mint (cdr (assq 'top (frame-parameters frame)))
		      mint (- (win:numericify mint)
			      (* win:new-frame-offset-y (1- minn)))
		      left (+ (* win:new-frame-offset-x index) minl)
		      top  (+ (* win:new-frame-offset-y index) mint)))
	       (t			;not absolutely
		(setq
		 left (cdr (assq 'left (frame-parameters (selected-frame))))
		 left (+ (win:numericify left)
			 win:new-frame-offset-x)
		 top (cdr (assq 'top (frame-parameters (selected-frame))))
		 top (+ (win:numericify top)
			win:new-frame-offset-y))))
	      (list (cons 'left left) (cons 'top top)))))))
    (run-hooks 'win:allocate-frame-hook)
    frame))

;;;
;; For Utility
;;;
;;;###autoload
(defun see-you-again ()
  "Save all of the window configurations and kill-emacs."
  (interactive)
  (if win:current-config
      (progn (win-save-all-configurations)
	     (message "See you again!")
	     (sit-for 1)
	     (save-buffers-kill-emacs))))

;;;###autoload
(defun resume-windows (&optional preserve)
  "Restore all window configurations reading configurations from a file.
Non-nil for optional argument PRESERVE keeps current buffers."
  (interactive "P")
  (win:store-config 0)
  (win-load-all-configurations preserve)
  ;; This section depends highly on the version of `frame.el'.
  ;; It might be better to check boundp of variables frame-*.
  ;; But I think the case that those variables is not defined makes
  ;; confusion.  So we refer them without checking boundp.
  (if (and win:use-frame (aref win:configs 1)
	   frame-initial-frame (boundp 'frame-initial-geometry-arguments))
      ;;Modify initial-frame-alist so that the alternative frame
      ;;which will be created by frame-notice-user-settings
      ;;has the same geometry as that of the resumed first frame.
      (let ((frame (aref win:configs 1)))
	(setq frame-initial-geometry-arguments
	      (append (list (assq 'height (frame-parameters frame))
			    (assq 'width (frame-parameters frame))
			    (assq 'top (frame-parameters frame))
			    (assq 'left (frame-parameters frame)))
		      frame-initial-geometry-arguments))
	;; The new frame created in frame-notice-user-settings is to
	;; have the same buffer as that of selected frame at that time.
	;; So we temporarily select the window#1 for the new frame,
	;; and save the current window visiting list into the property
	;; of the symbol 'resume-windows.
	(put 'resume-windows 'lc (list win:last-config win:current-config))
	(win:switch-window 1))))

;;;
;; Window selection menu
;;;
(defvar win:switch-menu-buffer " * window list *")
(defvar win:switch-menu-saved-config (make-vector 2 nil)
  "(windows internal variable) vector of [NUM CONF];
where NUM is the selected window number when window selection is invoked,
CONF is the window configuration at the time.")

(defun win-switch-menu-edit-name-prefix ()
  "Edit win:names-prefix in win-switch-menu."
  (interactive)
  (let* ((goal (progn
                 (beginning-of-line)
                 (re-search-forward "(\\([A-Za-z0-9:-@]\\))" nil t)
                 (- (char-after (match-beginning 1)) win:base-key))) 
         (prefix (read-string "Window name: " (aref win:names-prefix goal))))
    (if (> (length prefix) win:names-maxl) ;日本語知らない…
	(setq prefix (substring prefix 0 win:names-maxl)))
    (aset win:names-prefix goal prefix)
    (win:switch-menu-prepare-menu)
    (goto-char (point-min))
    (search-forward (concat "(" (format "%c" (+ goal win:base-key)) ")"))
    (beginning-of-line)
  ))

(defun win-switch-menu-select (kill)
  "Select the window of cursor position in *window list*.
If interactive argument KILL is non-nil, kill menu buffer and no select."
  (interactive "P")
  (beginning-of-line)
  (if (and (eq (get-buffer win:switch-menu-buffer) (current-buffer))
	   (looking-at "[ A-Z]*(\\([A-Za-z0-9:-@]\\))"))
      (let ((goal (buffer-substring (match-beginning 1) (match-end 1))))
	(setq goal (- (string-to-char goal) win:base-key))
	(if win:use-frame (win:adjust-window))
	(if (eq win:current-config (aref win:switch-menu-saved-config 0))
	    (win:set-window-configuration (aref win:switch-menu-saved-config 1)))
	(or kill (win-switch-to-window 1 goal)))))

(defun win-switch-menu-select-directly ()
  "Select the window directly from the keyboard in window selection menu."
  (interactive)
  (let ((num (- last-command-char win:base-key)))
    (and
     (eq (get-buffer win:switch-menu-buffer) (current-buffer))
     (< num win:max-configs)
     (> num 0)
     (goto-char (point-min))
     (progn
       (if win:use-frame (win:adjust-window))
       (if (eq win:current-config (aref win:switch-menu-saved-config 0))
	   (win:set-window-configuration (aref win:switch-menu-saved-config 1)))
       (win-switch-to-window 1 num)))))

(defun win-switch-menu-mark-job (unmark)
  "Put job symbol in window selection menu buffer."
  (interactive "P")
  (and
   (eq (get-buffer win:switch-menu-buffer) (current-buffer))
   (progn (beginning-of-line) (looking-at "[ A-Z]+(.)"))
   (let (buffer-read-only)		;bound to nil
     (delete-char 1)
     (insert (if unmark " " (char-to-string (upcase last-command-char))))
     (forward-line 1)
     (and (eobp) (forward-line -1)))))

(defun win-switch-menu-unmark-job ()
  "Remove job symbol in window selection menu buffer."
  (interactive)
  (win-switch-menu-mark-job t))

(defun win-switch-menu-execute-job ()
  "Do real jobs in window selection menu buffer."
  (interactive)
  (let*((current win:current-config) job num (name (aref win:names current))
	(buf (get-buffer win:switch-menu-buffer)) (mes "Doing marked jobs..."))
    (save-excursion
      (goto-char (point-min))
      (message mes)
      (while (re-search-forward "^[A-Z]" nil t)
	(setq job (char-after (match-beginning 0)))
	(re-search-forward "(\\([A-Za-z0-9:-@]\\))" nil t)
	(setq num (- (char-after (match-beginning 1)) win:base-key))
	(cond
	 ((= job ?D)
	  (if (aref win:configs num)
	      (if (or (/= current num)
		      (y-or-n-p "Are you sure to kill this window?"))
		  (win:delete-window num nil t)))) ;without query(3rd arg t)
	 ((= job ?S) (and (aref win:configs num) (win:save-window num)))
	 ((= job ?L)
	  (let ((win:last-config win:last-config))
	    (win:load-window num)
	    (if (= num current)		;override previous configuration.
		(aset win:switch-menu-saved-config 1
		      (win:current-window-configuration)))))
	 ((= job ?K)
	  (if (aref win:configs num)
	      (let ((win:last-config win:last-config))
		(win:switch-window num)
		(win-delete-current-window t)))) ;with query
	 (t nil))
	(and (aref win:configs num) (/= num current)
	     (progn (win:switch-window current)	;back to menu buffer
		    (win:set-window-name current))) ;preserve window name
	(set-buffer buf)))		;might be killed by `D'
    (message "%sDone." mes)
    (if (= current win:current-config) ;menu is still alive
	(win:switch-menu-prepare-menu))))

(defun win-switch-menu-quit ()
  "Quit from window menu buffer."
  (interactive)
  (win-switch-menu-select t))

(defun win:switch-menu-prepare-menu ()
  (switch-to-buffer win:switch-menu-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((i 1) (this (make-marker))
	(form (format " (%%c)%%s %%-%ds [%%s]\n" (+ win:names-maxl 2))))
    (while (< i win:max-configs)
      (if (= win:current-config i) (set-marker this (point)))
      (insert (format form
		      (+ win:base-key i)
		      (cond ((= i win:current-config) "*")
			    ((= i win:last-config) "+")
			    (t " "))
		      (if (aref win:configs i) 
			  (format "\"%s\"" (aref win:names-prefix i))
			"")
		      (if (aref win:configs i) (aref win:names i)
			"")
		      ))
      (setq i (1+ i)))
    (use-local-map win:switch-menu-map)
    (goto-char this))
  (win:switch-menu-mode)
  (setq buffer-read-only t))

(defun win-switch-menu ()
  "Show the menu of all windows and select one of them."
  (interactive)
  (if (string= (buffer-name (current-buffer)) win:switch-menu-buffer) () 
    (if (= (+ win:current-config win:last-config) 0)
        (error "No windows allocated"))
    (win:adjust-window)
    (win:store-config win:current-config)		;save current state
    (win:adjust-names)
    (or win:use-frame (win:store-config win:current-config)) ;save current state
    (aset win:switch-menu-saved-config 0 win:current-config)
    (aset win:switch-menu-saved-config 1 (win:current-window-configuration))
    (if (< (window-height) (1+ win:max-configs))
        (enlarge-window (- win:max-configs (window-height)))))
  (win:switch-menu-prepare-menu)
  (message
   "N)ext P)rev SPC)select S)ave L)oad D)elete K)ill E)dit-name e(X)ec NUM Q)uit ?)help"))

(defun win:switch-menu-mode ()
  "===== Window selection buffer for windows.el =====

	n	next-line
	p	previous-line
	\\[win-switch-menu-select]	select this window
	\\[win-switch-menu-quit]	quit from window selection menu
	NUMBER	select NUMBER-th window directly
	s	mark this window to be saved into configuration file
	l	mark this window to be loaded from configuration file
	d	mark this window to be deleted
	k	mark this window to be killed (kill also displayed buffers)
        e       edit window configuration name
	\\[win-switch-menu-execute-job]	do jobs on each marked window
"
  (setq major-mode 'win:switch-menu-mode
	mode-name "window selection"))

(if (fboundp 'wrap-function-to-control-ime)
    (progn
      (wrap-function-to-control-ime 'win-menu t "p")
      (wrap-function-to-control-ime 'win-switch-to-window t "p")
      (wrap-function-to-control-ime 'win-resume-local t "P")
      (wrap-function-to-control-ime 'win-resume-menu t "P")
      (wrap-function-to-control-ime 'win-switch-menu t nil)))

;;;
;; Work-arounds
;;;
;for mew-5.2 or later (Suggested by masutaka)
(if (fboundp 'defadvice)
    (defadvice mew-buffer-message
      (after mew-buffer-message-after-advice (&rest args) activate)
      (or win:use-frame
	  (setq ad-return-value
		(format "%s%d" ad-return-value win:current-config)))))

;;;
;; Final setup.
;;;
(provide 'windows)
(run-hooks 'win-load-hook)

;; $Log: windows.el,v $
;; Revision 2.48  2010/05/23 12:33:40  yuuji
;; Workaround for frame focus on CarbonEmacs.
;; Thanks to nabechan.
;;
;; Revision 2.47  2009/10/17 01:49:05  yuuji
;; Fix for XEmacs and emacs-20.
;;
;; Revision 2.46  2009/09/02 05:09:33  yuuji
;; ?\M-0 is no longer negative.
;; Thanks to patch from rubikitch>at<ruby-lang.org.
;;
;; Revision 2.45  2009/09/02 04:48:06  yuuji
;; Frame use in -nw mode fixed.
;; In -nw mode, we should adjust window number after switching windows.
;; Thanks to the comment from Francis Moreau.
;;
;; Revision 2.44  2008/06/08 21:55:54  yuuji
;; In non-frame mode, displaying different point of the same buffer
;; in two or more window can't be restored by set-window-configuration.
;; For this work-around, define win:current-window-configuration and
;; win:set-window-configuration. (thanks to sakurai>at<math.s.chiba-u.ac.jp)
;;
;; Revision 2.43  2008/05/14 13:08:51  yuuji
;; We cannot use 'unless here.  Replaced it with or.
;;
;; Revision 2.42  2008/05/13 01:21:09  yuuji
;; Work-around for Mew5 or later.
;; Utilize make-frame in `emacs -nw'.
;;
;; Revision 2.41  2007/06/06 11:28:05  yuuji
;; Fix hook setup for KeyFunc&Exit.
;; Suggested by Debian Project.
;;
;; Revision 2.40  2004/10/30 07:18:29  yuuji
;; New flag win:config-loaded introduced for external utility.
;;
;; Revision 2.39  2003/03/28 11:34:41  yuuji
;; add-hook alternative definition for Emacs-18(Nemacs)
;;
;; Revision 2.38  2002/09/17 15:14:13  yuuji
;; For Meadow, wrap-function-to-control-ime against
;; 'win-menu, 'win-switch-to-window, 'win-resume-local, 'win-resume-menu
;; 'win-switch-menu
;;
;; Revision 2.37  2002/06/30 08:01:15  yuuji
;; New variable win:inhibit-switch-in-minibuffer;
;;  Inhibit window switching from minibuffer
;; win:restore-buffer-list works well when called from minibuffer.
;;
;; Revision 2.36  2002/06/26 16:23:26  yuuji
;; expand-file-name in win:read-config-file
;;
;; revision 2.35
;; date: 2002/03/08 01:12:13;  author: yuuji;  state: Exp;  lines: +31 -14
;; By Sean Champ <schamp@users.sourceforge.net>;
;; win:delete-frame-with-window : added
;; 
;; #'win:delete-window :
;;    if win:delete-frame-with-window is nil:
;;          1) do not delete the frame of the window
;;             2) do not call #'win:set-wc
;; 
;; these changes have been tested, and appear to work well enough.
;; 
;; (xemacs 21.5.4, with gtk widgets, might make some strange widgets appear
;; in the buffer that is selected after the key-sequence C-c C-w 9 c )
;;
;; Revision 2.34  2002/01/29 14:09:21  yuuji
;; Call win:restore-buffer-list in win-load-all-configurations so that
;; buffer-list order in each window can be back at resume-windows.
;;
;; Revision 2.33  2001/10/28 07:57:49  yuuji
;; Emacs21 sometimes fails to locate new frame in expected position; fixed.
;;
;; Revision 2.32  2001/03/07 03:44:37  yuuji
;; Convert to EUC-JP for non-mule.
;;
;; Revision 2.31  2001/01/30 03:30:28  yuuji
;; For Emacs-21.0.96 in win:switch-window;
;; do win:restore-buffer-list after selecting target window(not frame mode)
;;
;; Revision 2.30  2001/01/29 09:06:10  yuuji
;; Sit-for 1e-10 in 2.29 is too little... Change to 1e-5.
;;
;; Revision 2.29  2001/01/29 09:04:02  yuuji
;; Emacs-21.0.9x sometimes fails to display some part of frame.
;; Add sit-for to win:selected-window for workaround.
;;
;; Revision 2.28  2001/01/24 12:57:54  yuuji
;; Check win:frame-title-function's void or not in win:adjust-window.
;;
;; Revision 2.27  2000/11/24 03:52:06  yuuji
;; Shorten window-creation message.
;;
;; Revision 2.26  1999/05/14 05:43:38  yuuji
;; Fix the case as below;
;; Start Emacs with win:startup-with-window,
;; and C-c 2 f SomeFile,
;; and immediately delete window by C-c !. it complain that
;; there is only one window.
;;
;; Revision 2.25  1999/01/16 06:19:25  yuuji
;; bug in win:store-buffer-list, fixed
;;
;; Revision 2.24  1999/01/16 05:13:20  yuuji
;; win:buffer-depth-per-win
;; 	preserves buffer-list priority per window
;;
;; Revision 2.23  1998/11/12 13:42:01  yuuji
;; Now runs correctly on Emacs without screen-height/screen-width.
;;
;; Revision 2.22  1998/10/26 13:25:02  yuuji
;; For Emacs20, avoid infinite loop at selecting iconified frame.
;;
;; Revision 2.21  1998/09/22 15:16:08  yuuji
;; Now win:auto-position works correctly on Emacs20.
;;
;; Revision 2.20  1998/09/22 14:46:29  yuuji
;; *** empty log message ***
;;
;; Revision 2.19  1998/04/01 03:52:24  yuuji
;; FSF's address fixed
;;
;; Revision 2.18  1997/12/22 02:34:04  yuuji
;; win:wipe-also-frames works correctly
;;
;; Revision 2.17  1997/12/15 02:52:55  yuuji
;; Free variable `frame' in win:set-frame-configuration localized
;;
;; Revision 2.16  1997/12/08 06:51:52  yuuji
;; Small fix
;;
;; Revision 2.15  1997/12/08 06:43:15  yuuji
;; Try to keep frame title up to date.
;; win:need-uptodate-frame-title
;;
;; Revision 2.14  1997/12/04 03:30:27  yuuji
;; win:wipe-also-frames
;; Set win:current-config to 1 at win:startup-with-window.
;;
;; Revision 2.13  1997/12/01 19:36:53  yuuji
;; Fix document of the part which says about combination with window
;; manager.
;;
;; Revision 2.12  1997/12/01 01:51:28  yuuji
;; Switch to selected frame at loading.
;; Don't pass 'minibufffer property at frame creation on XEmacs.
;;
;; Revision 2.11  1997/11/29 13:19:42  yuuji
;; XEmacs support
;;
;; Revision 2.10  1997/10/23 13:53:52  yuuji
;; Support Emacs20
;;
;; Revision 1.94  1997/01/27 03:50:31  yuuji
;; split-window-safe fixed
;; mew support fixed again
;;
;; Revision 1.93  1997/01/17 03:10:38  yuuji
;; Support mew(revive.el)
;;
; Revision 1.92  1996/08/16  16:23:18  yuuji
; Adjust window at win-resume-menu and win-swap-with.
;
; Revision 1.91  1996/05/05  16:48:50  yuuji
; small fix for emacs 19.30
;
; Revision 1.9  1995/09/07  02:27:16  yuuji
; Revise document.
;
; Revision 1.8  1995/07/13  01:33:19  yuuji
; Fix typos and setq of frame-initial-geometry-arguments.
;
; Revision 1.7  1995/02/02  17:37:05  yuuji
; New variables for customization, win:resumed-frame-offset-x,
; win:resumed-frame-offset-y and win:mouse-position added.
;
; Revision 1.6  1995/01/12  14:55:37  yuuji
; Enhanced window selection menu
;
; Revision 1.5  1994/11/19  17:50:30  yuuji
; Resume window number correctly when splitting minibuffer.
;
; Revision 1.4  1994/09/26  17:16:09  yuuji
; Full support for Emacs-19.
;
; Revision 1.3  1994/06/06  07:51:52  yuuji
; Window #0 no longer shows up in window-list.
;
; Revision 1.2  1994/05/06  21:38:33  yuuji
; Omit window#0 on win-{next,prev}-window.
; Disable deleting sole window.
; win:last-config has no longer #0.
;
; Revision 1.1  1994/04/18  02:41:22  yuuji
; Fix the bug on `Preserve' of window creation menu.
;
; Revision 1.0  1994/03/07  07:39:01  yuuji
; Support `frame environment' of Emacs 19.
;
; Revision 0.9  1994/02/17  08:48:41  yuuji
; Make win-startup-with-window smart.
; Local resume.
;
; Revision 0.8  1994/02/09  10:57:20  yuuji
; Update the document.
; Fix win:startup-with-window.
;
; Revision 0.7  1994/02/07  12:41:05  yuuji
; win-resume-menu
;
; Revision 0.6  1994/02/05  07:56:37  yuuji
; Add the functions to resume Emacs.
;
; Revision 0.5  1994/02/03  10:08:03  yuuji
; Allow resize of Emacs's window.
;
;; Revision 0.2  93/10/12  02:43:15  yuuji
;; Change prefix key to `C-c C-w'.
;; 

; Local variables:
; fill-prefix: ";;;	"
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:
