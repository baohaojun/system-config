#!/bin/bash
set -e

function die() {
    echo Error: "$@"
    exit -1
}

cd $(dirname $(readlink -f $0))
export DOING_T1WRENCH_RELEASE=true

## start code-generator "^\\s *#\\s *"
# generate-getopts ssmb r:ReleaseVersion
## end code-generator
## start generated code
smb=true
ReleaseVersion=
while getopts 'sr:' opt; do
    case "$opt" in
        s)    smb=false;;
        r)    ReleaseVersion=$OPTARG;;
        *)    echo Usage Error;;
    esac
done
shift $(($OPTIND - 1))

## end generated code
export ReleaseVersion
export shortVersion=$ReleaseVersion

ReleaseVersion="$shortVersion $@"
if test ! "$shortVersion"; then
    die "No shortVersion defined"
fi

. .gitx

if git st -s | grep . -q; then
    git st -s
    die "Can't do release build when git not clean: see output above"
fi

oldVersion=$(perl -ne 'print $1 if m!<string>Smartisan T1聊天小扳手\s*(.*)</string>!' t1wrenchmainwindow.ui)
perl -npe 's!<string>Smartisan T1聊天小扳手.*</string>!<string>Smartisan T1聊天小扳手 $ENV{shortVersion}</string>!' -i t1wrenchmainwindow.ui

if test $(compare-version "$oldVersion" "$shortVersion") != '<'; then
    if test $(compare-version "$oldVersion" "$shortVersion") = "=" &&
            yes-or-no-p -n "Use the same version $shortVersion = $oldVersion?"; then
        true
    else
        die "old version $oldVersion >= new version $shortVersion"
    fi
fi

if is-tty-io; then
    src_version=$(cd ~/src/github/T1Wrench-linux; cat .src-version.txt)
    git log $src_version..HEAD
    yes-or-no-p -y "Continue '$oldVersion' -> '$shortVersion'?"
fi

git clean -xfd
git submodule foreach 'git clean -xfd'

(
    rm ~/tmp/build-t1-windows -rf
    ./build-wine.sh
    touch ~/tmp/build-t1-windows/build-ok
)&

(
    rm ~/tmp/build-t1 -rf
    ./build-linux.sh
    touch ~/tmp/build-t1/build-ok
)&

(
    ssh bhj-mac rm $(up) -rf
    rm ~/tmp/build-t1-mac -rf
    mkdir ~/tmp/build-t1-mac -p
    ./build-mac.sh
    touch ~/tmp/build-t1-mac/build-ok
)&

wait

if test ! -e ~/tmp/build-t1-windows/build-ok; then
    die "Windows build failed"
fi

if test ! -e ~/tmp/build-t1/build-ok; then
    die "Linux build failed"
fi

if test ! -e ~/tmp/build-t1-mac/build-ok; then
    die "Mac build failed"
fi

for x in ~/src/github/T1Wrench-linux ~/src/github/T1Wrench-macos/T1Wrench.app/Contents/MacOS/ ~/src/github/T1Wrench-windows; do
    (
        cd $x
        if test -e last-pic-notes.png; then
            rm -f last-pic-notes.png
        fi
        ./update-md5s.sh
        cd $(dirname $(lookup-file -e .git))
        dir=$(basename $PWD)
        cd ..

        file=~/tmp/$dir.zip
        if test "$dir" = T1Wrench-linux; then
            file=~/tmp/$dir.tgz
            tar czfv $file $dir --exclude-vcs
        else
            zip -r $file $dir -x '*/.git/*'
        fi
        if test $smb = true; then
            smb-push $file ~/smb/share.smartisan.cn/share/baohaojun/T1Wrench
            rsync $file rem:/var/www/html/baohaojun/ -v
        fi
    )
done

echo all done
