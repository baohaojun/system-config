export TZ="Asia/Shanghai"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:en
export BROWSER=firefox
if test -e ~/src/github/private-config/.profile; then
    . ~/src/github/private-config/.profile
fi
if test "$(lsb_release -is 2>&1)" = Debian; then
    export PATH=/usr/lib/jvm/java-8-openjdk-amd64/bin:~/external/local/emacs25/bin:$PATH
fi
