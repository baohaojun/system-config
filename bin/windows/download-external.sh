#!/bin/bash
test "$DOWN" == yes && set -e

function download-all()
{
    mkdir -p /c/download
    cd /c/download
    file_list=( 
        http://download.sysinternals.com/Files/ProcessMonitor.zip
        http://download.sysinternals.com/Files/ProcessExplorer.zip
        http://download.sysinternals.com/Files/PsTools.zip
        http://download.sysinternals.com/Files/WinObj.zip
        http://download.sysinternals.com/Files/DebugView.zip
        http://the.earth.li/~sgtatham/putty/latest/x86/putty.zip
        http://www.nirsoft.net/utils/resourcesextract.zip
        http://www.nirsoft.net/utils/iconsext.zip
        http://www.winterdrache.de/freeware/png2ico/data/png2ico-win-2002-12-08.zip
    )

    for x in "${file_list[@]}"; do
        if ! [[ -f `basename "$x"` ]]; then
            wget "$x"
        fi
    done
            

    cd ~/bin/windows/lnks
    for x in /c/download/*.zip; do 
        if [[ -f "$x" ]]; then
            /bin/unzip -o "$x";
        fi
    done
    mv ./*/* . || true
    chmod a+x ./*
}

test "$DOWN" == yes && download-all

function emacs-site-lisps()
{
    mkdir -p ~/tools/emacs-site-lisp/
    cd ~/tools/emacs-site-lisp/
    rm */ -rf
    file_list=(
        http://me.in-berlin.de/~myrkr/dictionary/dictionary-1.8.7.tar.gz
        http://cvs.savannah.gnu.org/viewvc/*checkout*/emacsweblogs/weblogger/lisp/xml-rpc.el
        http://debian.cn99.com/debian/pool/main/e/emacs-goodies-el/emacs-goodies-el_30.3.orig.tar.gz
        http://mwolson.org/static/dist/muse-latest.tar.gz
        http://debian.cn99.com/debian/pool/main/w/w3m-el-snapshot/w3m-el-snapshot_1.4.364+0.20090802.orig.tar.gz
    )
    for x in "${file_list[@]}"; do
        if ! [[ -f "$(basename "$x")" ]]; then
            wget "$x"
        fi
    done
    
    for x in *.tar.gz *.tgz; do
        if [[ -f "$x" ]] ;
        then 
            tar zxfv "$(basename "$x")"; 
        fi
    done    
}

test "$DOWN" == yes && emacs-site-lisps


function patch-puttycyg()
{
    (cd /c/download/puttycyg-read-only/putty-0.60/ &&
        patch -p1 <<EOF
diff -urN putty-orig/version.c putty-patched/version.c
--- putty-orig/version.c	2007-02-04 13:30:39.000000000 +0100
+++ putty-patched/version.c	2008-09-19 18:22:14.000000000 +0200
@@ -30,7 +30,7 @@
 
 #else
 
-char ver[] = "Unidentified build, " __DATE__ " " __TIME__;
+char ver[] = "imnotalamer.com version, " __DATE__ " " __TIME__;
 char sshver[] = "PuTTY-Local: " __DATE__ " " __TIME__;
 
 #endif
diff -urN putty-orig/windows/winctrls.c putty-patched/windows/winctrls.c
--- putty-orig/windows/winctrls.c	2007-02-18 20:50:41.000000000 +0100
+++ putty-patched/windows/winctrls.c	2008-09-19 16:19:51.000000000 +0200
@@ -1955,8 +1955,11 @@
 	    cf.lStructSize = sizeof(cf);
 	    cf.hwndOwner = dp->hwnd;
 	    cf.lpLogFont = &lf;
+		cf.Flags = CF_BOTH;
+		/*
 	    cf.Flags = CF_FIXEDPITCHONLY | CF_FORCEFONTEXIST |
 		CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;
+		*/
 
 	    if (ChooseFont(&cf)) {
 		strncpy(fs.name, lf.lfFaceName,
diff -urN putty-orig/windows/windlg.c putty-patched/windows/windlg.c
--- putty-orig/windows/windlg.c	2007-01-16 19:48:47.000000000 +0100
+++ putty-patched/windows/windlg.c	2008-09-20 03:22:27.000000000 +0200
@@ -217,7 +217,7 @@
 	  case IDA_WEB:
 	    /* Load web browser */
 	    ShellExecute(hwnd, "open",
-			 "http://www.chiark.greenend.org.uk/~sgtatham/putty/",
+			 "http://www.imnotalamer.com/mtp/putty/",
 			 0, 0, SW_SHOWDEFAULT);
 	    return 0;
 	}
diff -urN putty-orig/windows/window.c putty-patched/windows/window.c
--- putty-orig/windows/window.c	2007-02-25 01:50:24.000000000 +0100
+++ putty-patched/windows/window.c	2008-09-19 16:32:24.000000000 +0200
@@ -75,6 +75,14 @@
 #define WHEEL_DELTA 120
 #endif
 
+/* Font size & position modifiers */
+#define FONTSIZE_MOD_X -1 /* for fixing font sizes if putty thinks your fonts aren't the same size as you */
+#define FONTSIZE_MOD_Y -1
+#define FONTSLIDE_X 0 /* fix terminal window placement in the main putty window */
+#define FONTSLIDE_Y -1
+#define FONTBOX_MOD_TOP 1 /* fix if fonts are at bad position in their rectangle */
+#define FONTBOX_MOD_BOTTOM 1
+
 static Mouse_Button translate_button(Mouse_Button button);
 static LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
 static int TranslateKey(UINT message, WPARAM wParam, LPARAM lParam,
@@ -1407,8 +1415,8 @@
     GetObject(fonts[FONT_NORMAL], sizeof(LOGFONT), &lfont);
 
     if (pick_width == 0 || pick_height == 0) {
-	font_height = tm.tmHeight;
-	font_width = tm.tmAveCharWidth;
+	font_height = tm.tmHeight+FONTSIZE_MOD_X;
+	font_width = tm.tmAveCharWidth+FONTSIZE_MOD_Y;
     }
     font_dualwidth = (tm.tmAveCharWidth != tm.tmMaxCharWidth);
 
@@ -3178,8 +3186,8 @@
 
     x *= fnt_width;
     y *= font_height;
-    x += offset_width;
-    y += offset_height;
+    x += offset_width + FONTSLIDE_X;
+    y += offset_height + FONTSLIDE_Y;
 
     if ((attr & TATTR_ACTCURS) && (cfg.cursor_type == 0 || term->big_cursor)) {
 	attr &= ~(ATTR_REVERSE|ATTR_BLINK|ATTR_COLOURS);
@@ -3284,9 +3292,9 @@
     else
 	SetBkMode(hdc, OPAQUE);
     line_box.left = x;
-    line_box.top = y;
+    line_box.top = y + FONTBOX_MOD_TOP;
     line_box.right = x + char_width * len;
-    line_box.bottom = y + font_height;
+    line_box.bottom = y + font_height + FONTBOX_MOD_BOTTOM;
 
     /* Only want the left half of double width lines */
     if (line_box.right > font_width*term->cols+offset_width)
EOF
    )
}

function get-putty()
{
    
    cd /c/download/
    if [[ -d puttycyg-read-only ]]; then
        svn update puttycyg-read-only
    else
        svn checkout http://puttycyg.googlecode.com/svn/trunk/ puttycyg-read-only
        patch-puttycyg
    fi

    cd /c/download/puttycyg-read-only/putty-0.60/
    /bin/perl mkfiles.pl
    make -C windows -f Makefile.cyg putty.exe
    make -C windows/cthelper
    size windows/putty.exe windows/cthelper/cthelper.exe
    cp ./windows/cthelper/cthelper.exe ~/bin/windows/lnks
    cp ./windows/putty.exe ~/bin/windows/lnks
}

test "$DOWN" == yes && get-putty
