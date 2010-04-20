#!/bin/bash

#把windows-config下载回来，并全部拷到~下
#cd ~
#svn co http://windows-config.googlecode.com/svn/trunk
#mv ~/trunk/* ~/trunk/.* ~ 

#先把一些常用的软件装上，包括编译Android需要用到的软件。
sudo apt-get install -y ant ascii automake1.9 bash-doc binutils binutils-dev binutils-doc binutils-static bison build-essential cramfsprogs cscope curl dialog dictionary-el dict-jargon dict-moby-thesaurus dict-web1913 dict-wn dovecot-imapd dwm emacs-goodies-el emacs-snapshot expect exuberant-ctags fakeroot flashplugin-installer flex fontforge g++-4.2 g++-4.2-multilib gcc-4.2 gcc-4.4-doc gcc-multilib git-core git-svn glibc-doc g++-multilib gnupg gperf grub2 grun htop ia32-libs imagemagick iotop lib32ncurses5-dev lib32readline5-dev lib32z-dev libc6-dev-i386 libcurses-perl libcurses-ruby libcurses-ui-perl libcurses-widgets-perl libesd0-dev libltdl7 libncurses5-dev libncursesw5-dev libruby1.8 libsdl-dev libtool libwxgtk2.6-dev libx11-dev libz-dev lynx make-doc manpages manpages-dev meld minicom mplayer msn-pecan msttcorefonts muse-el ncurses-dev ncurses-hexedit ncurses-term offlineimap openssh-server p7zip p7zip-full pbzip2 pidgin python2.6-doc python3.1 python-qt4 python-soappy python-svn samba sawfish screen sharutils smbnetfs ssh subversion subversion-tools sysv-rc-conf texlive-full tkdiff traceroute ubuntu-desktop unrar unzip valgrind vim w3m-el-snapshot wget wmii x11proto-core-dev xchm xclip youtube-dl zip zlib1g-dev 

#把一些软件默认的版本换掉
sudo update-alternatives --set emacs /usr/bin/emacs-snapshot
sudo update-alternatives --set emacsclient /usr/bin/emacsclient.emacs-snapshot 
sudo update-alternatives --set etags /usr/bin/etags.emacs-snapshot 

#我们用的桌面系统是gnome，但是窗口管理器要换一个，gnome自带的是metacity，我们要用sawfish
gconftool-2 -s /desktop/gnome/applications/window_manager/default --type string /usr/bin/sawfish

#我们要用的字体
mkdir ~/.fonts
cp ~/doc/monaco-linux.ttf ~/.fonts
fc-cache -v
