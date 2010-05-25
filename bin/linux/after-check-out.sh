#!/bin/bash

set -e
#把windows-config下载回来，并全部拷到~下
#cd ~
#svn co http://windows-config.googlecode.com/svn/trunk
#mv ~/trunk/* ~/trunk/.* ~ 

#先把一些常用的软件装上，包括编译Android需要用到的软件。
packages='
ant
ascii
automake1.9
bash-doc
binutils
binutils-dev
binutils-doc
binutils-static
bison
build-essential
cramfsprogs
cscope
curl
dialog
dictionary-el
dict-jargon
dict-moby-thesaurus
dict-web1913
dict-wn
dovecot-imapd
dwm
emacs-goodies-el
emacs-snapshot
expect
exuberant-ctags
fakeroot
flashplugin-installer
flex
fontforge
g++-4.2
g++-4.2-multilib
gcc-4.2
gcc-4.4-doc
gcc-multilib
git-core
git-svn
glibc-doc
global
g++-multilib
gnupg
gperf
grub2
grun
hibernate
htop
ia32-libs
imagemagick
iotop
lib32ncurses5-dev
lib32readline5-dev
lib32z-dev
libc6-dev-i386
libcurses-perl
libcurses-ruby
libcurses-ui-perl
libcurses-widgets-perl
libesd0-dev
libltdl7
libncurses5-dev
libncursesw5-dev
libruby1.8
libsdl-dev
libtool
libwxgtk2.6-dev
libx11-dev
libz-dev
lynx
make-doc
manpages
manpages-dev
meld
minicom
mplayer
msn-pecan
msttcorefonts
muse-el
ncurses-dev
ncurses-hexedit
ncurses-term
nmap
offlineimap
openssh-server
p7zip
p7zip-full
pbzip2
pidgin
postfix
python2.6-doc
python3.1
python-qt4
python-soappy
python-svn
samba
sawfish
screen
sharutils
smbnetfs
ssh
subversion
subversion-tools
sysv-rc-conf
texlive-full
tkdiff
traceroute
ubuntu-desktop
unrar
unzip
valgrind
vim
w3m-el-snapshot
wget
wmii
x11proto-core-dev
xchm
xclip
youtube-dl
zip
zlib1g-dev
'
#lunar-applet

sudo apt-get udpate
sudo apt-get dist-upgrade -y
sudo apt-get install -y $packages || for x in $packages; do sudo apt-get install -y $x; done || (echo 'Some packages failed to install!'; sleep 3)

#把一些软件默认的版本换掉
sudo update-alternatives --set emacs /usr/bin/emacs-snapshot
sudo update-alternatives --set emacsclient /usr/bin/emacsclient.emacs-snapshot 
sudo update-alternatives --set etags /usr/bin/etags.emacs-snapshot 

#我们用的桌面系统是gnome，但是窗口管理器要换一个，gnome自带的是metacity，我们要用sawfish
gconftool-2 -s /desktop/gnome/applications/window_manager/default --type string /usr/bin/sawfish
#gconftool-2 -s /desktop/gnome/session/required_components/panel --type string gnome-panel-zh
#sudo cp ~/doc/gnome-panel-zh.desktop /usr/share/applications/
#put zh_CN.UTF-8 into /var/lib/locales/supported.d/local like this:
#en_US.UTF-8 UTF-8
#zh_CN.GBK GBK
#zh_CN.UTF-8 UTF-8


#我们要用的字体
mkdir ~/.fonts
cp ~/doc/monaco-linux.ttf ~/.fonts
fc-cache -v

#设置gmail邮件

#.gnus

function config_email()
{
    #config .gnus
    read -p 'Please input your gmail account (for e.g., somebody@gmail.com): ' Gmail

    perl -npe 's/baohaojun@gmail.com/'$Gmail'/g' -i ~/.gnus

    #config smtp:
    read -s -p 'Please input your gmail password: ' Gpass
    cat <<EOF >~/.postfix.cf


################################################################
relayhost = [smtp.gmail.com]:587
default_transport = smtp
relay_transport = smtp
inet_protocols = ipv4
smtp_sasl_auth_enable=yes
smtp_sasl_password_maps = hash:/etc/postfix/sasl_passwd
smtp_use_tls = yes
smtp_sasl_security_options = noanonymous
smtp_sasl_tls_security_options = noanonymous
smtp_tls_note_starttls_offer = yes
tls_random_source = dev:/dev/urandom
smtp_tls_scert_verifydepth = 5
smtpd_tls_ask_ccert = yes
smtpd_tls_req_ccert =no
smtp_tls_enforce_peername = no
EOF
    sudo bash -c "cat ~/.postfix.cf >>/etc/postfix/main.cf"
    sudo bash -c "echo '[smtp.gmail.com]:587' $Gmail:$Gpass > /etc/postfix/sasl_passwd"
    cd /etc/postfix/
    sudo postmap sasl_passwd
    sudo postfix reload

#config dovecot
#先把这些配置内容放到我自己有写权限的文件里去，下面再用sudo bash -c搞到/etc/dovecot/dovecot.conf里。
#上面的postfix/main.cf也是这样做的。

cat <<EOF > ~/.dovecot
protocols = imap imaps
log_timestamp = "%Y-%m-%d %H:%M:%S "
   mail_location = maildir:~/Maildir
mail_privileged_group = mail
protocol imap {
}
  
protocol pop3 {
  pop3_uidl_format = %08Xu%08Xv
}
protocol managesieve {
}
auth default {
  mechanisms = plain
  passdb pam {
  }
  userdb passwd {
  }
  user = root
}
dict {
}
plugin {
}
EOF

    sudo bash -c "cat ~/.dovecot > /etc/dovecot/dovecot.conf"


# 配置offlineimap
    read -p "Please input your password of this PC: " lpass
    cat <<EOF > ~/.offlineimaprc
[general]
accounts = Gmail
maxsyncaccounts = 1

ui = Noninteractive.Basic, Noninteractive.Quiet

[Account Gmail]
localrepository = Local
remoterepository = Remote

[Repository Local]
type = IMAP
remotehost = localhost
port = 143
remoteuser = `whoami`
remotepass = $lpass

[Repository Remote]
type = IMAP
remotehost = imap.gmail.com
remoteuser = $Gmail
remotepass = $Gpass
ssl = yes
maxconnections = 1
realdelete = no
folderfilter = lambda foldername: all([x.lower() not in foldername.lower() for x in ('all mail', 'spam', 'trash')])

EOF

    sudo /etc/init.d/dovecot restart
    offlineimap

#now you should be able to start gnus in Emacs


}
read -p 'Do you want to configure your Gmail? [Y/n]: ' Want

if test "$Want" = "" -o "$Want" = "Y" -o "$Want" = "y"
then
    echo "OK. Configure email"
    config_email
else
    echo "OK. Not configure email"
fi

#wget -e 'robots=off' --timeout=30 -c -m --tries=0 http://antlr.org
#wget -e 'robots=off' --timeout=30 -c -m --tries=0 http://www.stringtemplate.org/    
#wget -e 'robots=off' --timeout=30 -c -m --tries=0 http://serghei.net/

download-external.sh

echo OK.
