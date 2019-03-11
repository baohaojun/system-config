export TZ="Asia/Shanghai"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:en
export BROWSER=firefox
if test -e ~/.local-config/.profile; then
    . ~/.local-config/.profile
fi
export PATH=/usr/lib/jvm/java-8-openjdk-amd64/bin:~/src/github/emacs/bin:$PATH
