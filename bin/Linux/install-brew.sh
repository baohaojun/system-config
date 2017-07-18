#!/bin/bash

set -e
export HOMEBREW_NO_AUTO_UPDATE=true

if ! which brew >/dev/null 2>&1; then
    hint "Please make sure homebrew is installed into your ~/.linuxbrew/"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)" </dev/null
fi

brew install atk
brew install glib
brew install gmime
brew install gtk+
brew install libgpg-error
brew install lzlib
brew install giflib
brew install mono --without-fsharp
(
    cd ~/.linuxbrew/bin
    relative-link -f mcs gmcs
)

export LD_LIBRARY_PATH=~/.linuxbrew/lib:$LD_LIBRARY_PATH
download-debian-source -p gtk-sharp2
(
    cd ~/src/debian-sources/gtk-sharp2/*/
    ./configure --prefix=${HOME}/.linuxbrew
    make -j8
    make install
)

download-debian-source -p libgmime2.6-cil-dev
(
    cd ~/src/debian-sources/libgmime2.6-cil-dev/*/
    autoreconf -i
    ./configure --prefix=${HOME}/.linuxbrew
    make -j8 CFLAGS=-I${HOME}/.linuxbrew/include LDFLAGS=-L${HOME}/.linuxbrew/lib
    make install
)

if false; then
    download-debian-source -p emacs25
    (
        cd ~/src/debian-sources/emacs25/*/
        ./autogen.sh
        CFLAGS=$(pkg-config --cflags x11) \
              LIBS=$(pkg-config --libs x11) \
              ./configure --prefix=${HOME}/.linuxbrew \
              --x-includes=$(pkg-config --cflags x11|perl -npe 's,-I,\n,g'|xargs string-join ":" $HOME/.linuxbrew/include/) \
              --x-libraries=$HOME/.linuxbrew/Cellar/libx11/1.6.5/lib:$HOME/.linuxbrew/lib/

        make -j8
        make install
    )
fi

for x in ~/.linuxbrew/Cellar/shared-mime-info/*/; do
    (
        cd "$x"
        relative-link ./share/pkgconfig/shared-mime-info.pc ~/.linuxbrew/lib/pkgconfig/ -f
    )
done

compile_beagrep --no-sudo -- --prefix=${HOME}/.linuxbrew
compile_ctags --no-sudo -- --prefix=${HOME}/.linuxbrew --program-suffix=-exuberant
if ! grep -P '^\Qexport LD_LIBRARY_PATH=~/.linuxbrew/lib:$LD_LIBRARY_PATH\E' ~/.bashrc; then
    echo export LD_LIBRARY_PATH=\~/.linuxbrew/lib:\$LD_LIBRARY_PATH >> ~/.bashrc
fi
mkdir -p ~/.mono/MimeKit.1.0.3.0/lib/net40
chmod 700 ~/.mono/

for p in String::ShellQuote String::Approx; do
    check-perl-module --no-apt-get $p
done
