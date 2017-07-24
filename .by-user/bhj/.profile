export TZ="Asia/Shanghai"
export LC_ALL=ja_JP.UTF-8
export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP:en_US:en
export BROWSER=firefox
if test "$(uname)" = Linux -a "$(lsb_release -rs 2>&1)" = testing; then
    export PATH=/usr/lib/jvm/java-8-openjdk-amd64/bin:~/external/local/emacs25/bin:$PATH
fi
