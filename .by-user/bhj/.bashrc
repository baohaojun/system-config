DEBEMAIL="baohaojun@gmail.com"
DEBFULLNAME="Bao Haojun"
export DEBEMAIL DEBFULLNAME
export SC_ACCOUNT_USER=bhj

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:
export INFOPATH=${HOME}/src/github/org-mode/doc:${INFOPATH}
export ANDROID_HOME=${ANDROID_HOME:-~/external/bin/Linux/ext/android-sdk-linux}
export ANDROIDSDK_HOME=${ANDROIDSDK_HOME:-$ANDROID_HOME}
export ANDROID_NDK_HOME=${ANDROID_NDK_HOME:-~/external/bin/Linux/ext/ndk}
. ~/system-config/.by-user/bhj/.profile
if test -e ~/src/github/private-config/.bashrc; then
    . ~/src/github/private-config/.bashrc
fi

if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
    source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
fi

