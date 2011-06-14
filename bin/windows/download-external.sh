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
        http://www.nirsoft.net/utils/resourcesextract.zip
        http://www.nirsoft.net/utils/iconsext.zip
        http://www.winterdrache.de/freeware/png2ico/data/png2ico-win-2002-12-08.zip
        http://download.sysinternals.com/Files/Handle.zip
    )

    for x in "${file_list[@]}"; do
        if ! [[ -f `basename "$x"` ]]; then
            wget -N "$x"
        fi
    done
            

    cd ~/bin/windows/lnks
    for x in /c/download/*.zip; do 
        if [[ -f "$x" ]]; then
            /bin/unzip -o "$x";
        fi
    done
    mv ./*/* . || true
    chmod a+x ./* || true
}

test "$DOWN" == yes && download-all

function emacs-site-lisps()
{
    mkdir -p ~/tools/emacs-site-lisp/
    cd ~/tools/emacs-site-lisp/
    rm ./*/ -rf
    cp /usr/share/emacs/site-lisp/subdirs.el . 
    

    source_list=(
        http://ftp.us.debian.org/debian/dists/sid/main/source/Sources.bz2 
        http://ftp.us.debian.org/debian/dists/sid/contrib/source/Sources.bz2 
        http://ftp.us.debian.org/debian/dists/sid/non-free/source/Sources.bz2
    )

    y=0
    for x in "${source_list[@]}"; do 
        ((y++)) || true
        test -e Sources.bz2.$y || ( lftp -c "pget -n 10 $x" && mv Sources.bz2 Sources.bz2.$y )
    done

    file_list=(
        http://me.in-berlin.de/~myrkr/dictionary/dictionary-1.8.7.tar.gz
        `get-deb-src-dir emacs-goodies-el`
        `get-deb-src-dir cscope`
        `get-deb-src-dir muse-el`
        `get-deb-src-dir w3m-el-snapshot`
    )
    for x in "${file_list[@]}"; do
        if ! [[ -f "$(basename "$x")" ]]; then
            lftp -c "pget $x"
        fi
    done
    
    for x in *.tar.gz *.tgz; do
        if [[ -f "$x" ]] ;
        then 
            tar zxfv "$(basename "$x")"; 
        fi
    done    
    cd emacs-goodies-el*/elisp/emacs-goodies-el/ && mkdir themes
}

test "$DOWN" == yes && emacs-site-lisps

function manual-download()
{
    page_list=(
        http://www.riverbankcomputing.co.uk/software/pyqt/download
        http://python.net/crew/mhammond/win32/Downloads.html
        http://www.python.org/download/
        http://www.andlinux.org/downloads.php
        http://www.microsoft.com/whdc/devtools/debugging/default.mspx
        http://www.gimp.org/windows/
        http://java.sun.com/javase/downloads/index.jsp
        http://sourceforge.net/projects/com0com/files/
        http://nsis.sourceforge.net/Download
        http://sourceforge.net/projects/py2exe/files/
        http://sourceforge.net/projects/visemacs/files/
        http://code.google.com/p/zscreen/downloads/list
        http://www.avira.com/en/download/
    )
    for x in "${page_list[@]}"; do 
        cygstart "$x";
    done
}

function get-ms-addons()
{
    cd /c/download/
    addon_list=(
        http://download.microsoft.com/download/5/6/7/567758a3-759e-473e-bf8f-52154438565a/dotnetfx.exe
        http://download.microsoft.com/download/0/6/1/061F001C-8752-4600-A198-53214C69B51F/dotnetfx35setup.exe
    )
    for x in "${page_list[@]}"; do 
        wget -c "$x";
        `basename "$x"` /q
    done
}

test "$DOWN" == yes && manual-download

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

test "$DOWN" == yes && (gcc-switch.sh 3; get-putty; gcc-switch.sh 4)

function get-cscope()
{
    if ! test -d /c/download/cscope/cscope; then
        mkdir -p /c/download/cscope
        cd /c/download/cscope
        cvs -d:pserver:anonymous@cscope.cvs.sourceforge.net:/cvsroot/cscope login
        cvs -z3 -d:pserver:anonymous@cscope.cvs.sourceforge.net:/cvsroot/cscope co -P cscope
    fi
}
