#!/bin/bash
set -e

function die() {
    echo Error: "$@"
    exit -1
}

cd $(dirname $(readlink -f $0))
export DOING_WRENCH_RELEASE=true

## start code-generator "^\\s *#\\s *"
# generate-getopts ssmb r:ReleaseVersion p:platforms=all
## end code-generator
## start generated code

smb=false
ReleaseVersion=
platforms=all
OPTIND=1
while getopts "sr:p:h" opt; do
    case "$opt" in

        s) smb=true ;;
        r) ReleaseVersion=$OPTARG ;;
        p) platforms=$OPTARG ;;
        h)
            echo
            echo
            printf %06s '-p '
            printf %-24s 'PLATFORMS'
            echo ''
            printf %06s '-r '
            printf %-24s 'RELEASEVERSION'
            echo ''
            printf %06s '-s '
            printf %-24s ''
            echo ''
            shift
            exit 0
            ;;
        *)
            echo
            echo
            printf %06s '-p '
            printf %-24s 'PLATFORMS'
            echo ''
            printf %06s '-r '
            printf %-24s 'RELEASEVERSION'
            echo ''
            printf %06s '-s '
            printf %-24s ''
            echo ''
            exit 2
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
        rm ~/tmp/build-t1-windows -rf
        ./build-wine.sh
        touch ~/tmp/build-t1-windows/build-ok
    )&
fi

if is-platform-needed ubuntu; then
    (
        rm ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-t1/ -rf
        ssh trusty "export DOING_WRENCH_RELEASE=true; cd $PWD; ./build-linux.sh -r Wrench-ubuntu-14.04"
        touch ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-t1/build-ok
    )&
fi

if is-platform-needed linux; then
    (
        rm ~/tmp/build-t1 -rf
        ./build-linux.sh -d ~/tmp/build-t1
        touch ~/tmp/build-t1/build-ok
    )&
fi

if is-platform-needed mac; then
    (

        ssh bhj-mac rm $(up) -rf
        rm ~/tmp/build-t1-mac -rf
        mkdir ~/tmp/build-t1-mac -p
        ./build-mac.sh
        touch ~/tmp/build-t1-mac/build-ok
    )&
fi

wait

if is-platform-needed wine && test ! -e ~/tmp/build-t1-windows/build-ok; then
    die "Windows build failed"
fi

if is-platform-needed ubuntu && test ! -e ~/external/cowbuilder/ubuntu-trusty-amd64/chroot/home/bhj/tmp/build-t1/build-ok; then
    die "ubuntu build failed"
fi

if is-platform-needed linux && test ! -e ~/tmp/build-t1/build-ok; then
    die "Linux build failed"
fi

if is-platform-needed mac &&  test ! -e ~/tmp/build-t1-mac/build-ok; then
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
