export TZ="Asia/Shanghai"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:en
export BROWSER=firefox
if test -e ~/src/github/private-config/.profile; then
    . ~/src/github/private-config/.profile
fi
if test "$(uname)" = Linux -a "$(lsb_release -rs 2>&1)" = testing; then
    export PATH=/usr/lib/jvm/java-8-openjdk-amd64/bin:~/external/local/emacs25/bin:$PATH
fi
