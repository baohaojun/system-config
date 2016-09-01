#!/bin/bash
set -e

function die() {
    echo Error: "$@"
    exit -1
}

cd $(dirname $(readlink -f $0))
export DOING_WRENCH_RELEASE=true

## start code-generator "^\\s *#\\s *"
# generate-getopt ssmb r:ReleaseVersion p:platforms=all
## end code-generator
## start generated code
TEMP=$(getopt -o sr:p:h \
              --long smb,ReleaseVersion:,platforms:,help,no-smb \
              -n $(basename -- $0) -- "$@")
smb=false
ReleaseVersion=
platforms=all
eval set -- "$TEMP"
while true; do
    case "$1" in

        -s|--smb|--no-smb)
            if test "$1" = --no-smb; then
                smb=false
            else
                smb=true
            fi
            shift
            ;;
        -r|--ReleaseVersion)
            ReleaseVersion=$2
            shift 2
            ;;
        -p|--platforms)
            platforms=$2
            shift 2
            ;;
        -h|--help)
            set +x
            echo
            echo
            echo Options and arguments:
            printf %06s '-p, '
            printf %-24s '--platforms=PLATFORMS'
            echo
            printf %06s '-r, '
            printf %-24s '--ReleaseVersion=RELEASEVERSION'
            echo
            printf %06s '-s, '
            printf %-24s '--[no-]smb'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done


## end generated code
export ReleaseVersion
export shortVersion=$ReleaseVersion

ReleaseVersion="$shortVersion $@"
if test ! "$shortVersion"; then
    die "No shortVersion defined"
fi

export T1_GIT_HASH=$(git log --pretty=%h -1)

. .gitx

if git st -s | grep . -q; then
    git st -s
    die "Can't do release build when git not clean: see output above"
fi

atexit() {
    cd ~/src/github/Wrench
    git checkout -- .
}
trap atexit 0

oldVersion=$(perl -ne 'print $1 if m!<string>Wrench\s*(V.*)</string>!' wrenchmainwindow.ui)
perl -npe 's!<string>Wrench\s*V.*</string>!<string>Wrench $ENV{shortVersion} ($ENV{T1_GIT_HASH})</string>!' -i wrenchmainwindow.ui

if test $(compare-version "$oldVersion" "$shortVersion") != '<'; then
    if test $(compare-version "$oldVersion" "$shortVersion") = "=" &&
            yes-or-no-p -n "Use the same version $shortVersion = $oldVersion?"; then
        true
    else
        die "old version $oldVersion >= new version $shortVersion"
    fi
fi

if is-tty-io; then
    src_version=$(cd ~/src/github/Wrench-debian; cat .src-version.txt)
    git log $src_version..HEAD || true
    yes-or-no-p -y "Continue '$oldVersion' -> '$shortVersion'?"
fi

git clean -xfd
git submodule foreach 'git clean -xfd'

function is-platform-needed() {
    test "$platforms" = all -o -z "$(arg1-arg2 "$1" "$platforms")"
}

if is-platform-needed wine; then
    (
        rm ~/tmp/build-wrench-windows -rf
        ./build-wine.sh
        touch ~/tmp/build-wrench-windows/build-ok
    )&
fi

if is-platform-needed ubuntu; then
    (
        rm ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-wrench/ -rf
        ssh trusty "export DOING_WRENCH_RELEASE=true; cd $PWD; ./build-linux.sh -r Wrench-ubuntu-14.04"
        touch ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-wrench/build-ok
    )&
fi

if is-platform-needed linux; then
    (
        rm ~/tmp/build-wrench -rf
        ./build-linux.sh -b ~/tmp/build-wrench
        touch ~/tmp/build-wrench/build-ok
    )&
fi

if is-platform-needed mac; then
    (

        ssh bhj-mac rm $(up) -rf
        rm ~/tmp/build-wrench-mac -rf
        mkdir ~/tmp/build-wrench-mac -p
        ./build-mac.sh
        touch ~/tmp/build-wrench-mac/build-ok
    )&
fi

wait

if is-platform-needed wine && test ! -e ~/tmp/build-wrench-windows/build-ok; then
    die "Windows build failed"
fi

if is-platform-needed ubuntu && test ! -e ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-wrench/build-ok; then
    die "ubuntu build failed"
fi

if is-platform-needed linux && test ! -e ~/tmp/build-wrench/build-ok; then
    die "Linux build failed"
fi

if is-platform-needed mac &&  test ! -e ~/tmp/build-wrench-mac/build-ok; then
    die "Mac build failed"
fi

for x in $(
              is-platform-needed linux && echo ~/src/github/Wrench-debian;
              is-platform-needed mac && echo ~/src/github/Wrench-macos/Wrench.app/Contents/MacOS/;
              is-platform-needed wine && echo ~/src/github/Wrench-windows;
              is-platform-needed ubuntu && echo ~/src/github/Wrench-ubuntu-14.04;
          ); do
    (
        cd $x
        if test -e last-pic-notes.png; then
            rm -f last-pic-notes.png
        fi
        ./update-md5s.sh
        lookup-file -e .git
        cd $(dirname $(lookup-file -e .git))
        dir=$(basename $PWD)
        cd ..

        file=~/tmp/$dir.zip
        if test "$dir" = Wrench-debian -o "$dir" = Wrench-ubuntu-14.04; then
            file=~/tmp/$dir.tgz
            tar czfv $file $dir --exclude-vcs
        else
            rm -f $file
            zip -r $file $dir -x '*/.git/*'
        fi
        if test $smb = true; then
            bfile=$(basename $file)
            (
                cd ~/smb/share.smartisan.cn/share/baohaojun/Wrench
                mkdir -p old-versions
                mv ${bfile%.*}* old-versions || true
            )
            smb-push $file ~/smb/share.smartisan.cn/share/baohaojun/Wrench/${bfile%.*}-$shortVersion.${bfile##*.}
            rsync $file rem:/var/www/html/baohaojun/Wrench/ -v
        fi || true&
    )
done


echo all done
