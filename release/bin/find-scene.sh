#!/usr/bin/env bash

resdir=.
if test "${WRENCH_APP_DIR}"; then
    resdir=${WRENCH_APP_DIR}/res
fi

unset EMACS

if test "$WRENCH_APP_DIR"; then
    set -x
    exec 2>> ~/tmp/find-scene.sh.log
fi

function picture-matches() {

    ## start code-generator "^\\s *#\\s *"
    # generate-getopt -P t:default-tolerance=unset
    ## end code-generator
    ## start generated code
    TEMP=$(POSIXLY_CORRECT=true getopt -o t:h \
                          --long default-tolerance:,help \
                          -n "$(basename -- "$0")" -- "$@")
    declare default_tolerance=unset
    eval set -- "$TEMP"
    while true; do
        case "$1" in

            -t|--default-tolerance)
                default_tolerance=$2
                shift 2

                ;;
            -h|--help)
                set +x
                echo -e
                echo
                echo Options and arguments:
                printf %6s '-t, '
                printf %-24s '--default-tolerance=DEFAULT_TOLERANCE'
                echo
                exit
                shift
                ;;
            --)
                shift
                break
                ;;
            *)
                die "internal error: $(. bt; echo; bt | indent-stdin)"
                ;;
        esac
    done


    ## end generated code

    if test "${default_tolerance}" = unset; then
        if test -e $1.tol; then
            default_tolerance=$(cat $1.tol)
            # log "scene tol changed to $default_tolerance"
        else
            default_tolerance=15
        fi
    fi

    if test ! -e "$1" -o ! -e "$2"; then
        die "Usage: picture-matches FILE1.PNG FILE2.PNG"
    fi

    set -- "$(absp "$1")" "$(absp "$2")"

    export g_onmyoji_location=Wrench
    export onmyoji_parallel=0

    look_same_postfix=${ANDROID_SERIAL}.${g_onmyoji_location}${onmyoji_parallel}
    # ^^^^ this is because look-same.server is written like this ^^^^
    local sock_name=look-same/${look_same_postfix}

    local node_try
    for node_try in $(seq 1 2); do
        if nodejs_ret=$(
                nc.openbsd -U /tmp/$sock_name <<<"$1 $2 ${default_tolerance}"
                     ); then
            if test "$nodejs_ret" = yes; then
                return 0;
            elif test "${nodejs_ret}" -a "${nodejs_ret}" != no; then
                echo "nodejs_ret: $nodejs_ret" 1>&2
            elif test "${nodejs_ret}" = no; then
                cp "$2" "$(dirname "$1")"/"$(bp "$1")".failed.png || true
            fi
            break
        else
            mkdir -p ~/tmp/${sock_name%/*}
            (
                cd ~/tmp/
                set -x
                flock -n 9 || exit
                rm /tmp/$sock_name -f
                mkdir -p /tmp/${sock_name%/*}
                nohup nodejs ~/system-config/bin/look-same.server || (bhj-notify Onmyoji "You need to make sure nodejs works") 9>/dev/null
            ) 9> ~/tmp/$sock_name.lock >> ~/tmp/$sock_name.log 2>&1 &
            if test "${node_try}" = 2; then
                return 1
            fi
            sleep 1
        fi
    done

    local x
    for x in $(seq 1 100); do
        x_png=${1%.png}.$x.png
        if test ! -e "${x_png}"; then
            if test -L "$1"; then
                if picture-matches "$(readlink -f "$1")" "$2"; then
                    return 0
                else
                    return 1
                fi
            fi
            return 1
        elif picture-matches ${x_png} $2; then
            return 0
        fi
    done
    return 1
}

get-image-size() {
    local image_size=$(identify "$1" | pn 3)
    echo "$image_size"
}

function find-scene() {

    ## start code-generator "^\\s *#\\s *"
    # generate-getopt d:dir='${WRENCH_DATA_DIR:-~/tmp/}' s:scene @:scene-dir='${resdir}' @:at-xy
    ## end code-generator
    ## start generated code
    TEMP=$( getopt -o d:s:h \
                   --long at-xy:,dir:,scene:,scene-dir:,help \
                   -n $(basename -- $0) -- "$@")
    declare at_xy=
    declare dir=${WRENCH_DATA_DIR:-~/tmp/}
    declare scene=
    declare scene_dir=${resdir}
    eval set -- "$TEMP"
    while true; do
        case "$1" in

            --at-xy)
                at_xy=$2
                shift 2

                ;;
            -d|--dir)
                dir=$2
                shift 2

                ;;
            -s|--scene)
                scene=$2
                shift 2

                ;;
            --scene-dir)
                scene_dir=$2
                shift 2

                ;;
            -h|--help)
                set +x
                echo -e
                echo
                echo Options and arguments:
                printf "%6s" " "
                printf %-24s '--at-xy=AT_XY'
                echo
                printf %6s '-d, '
                printf %-24s '--dir=DIR'
                echo
                printf %6s '-s, '
                printf %-24s '--scene=SCENE'
                echo
                printf "%6s" " "
                printf %-24s '--scene-dir=SCENE_DIR'
                echo
                exit
                shift
                ;;
            --)
                shift
                break
                ;;
            *)
                die "internal error: $(. bt; echo; bt | indent-stdin)"
                ;;
        esac
    done


    ## end generated code

    local scene_png=${resdir}/${scene}.png

    if test ! -e "${scene_png}"; then
        return 1
    fi

    local full_jpg=${dir}/find-scene.${ANDROID_SERIAL}.jpg

    adb-screenshot $full_jpg >/dev/null 2>&1

    local matched_xy

    if test "${at_xy}"; then
        matched_xy=${at_xy}
    else
        matched_xy=$(
            match_image ${full_jpg} ${scene_png}
                  )
    fi

    local matched_x=$(echo $matched_xy | pn 1)
    local matched_y=$(echo $matched_xy | pn 2)

    convert -crop $(get-image-size ${scene_png})+$matched_x+$matched_y ${full_jpg} ${full_jpg}.png


    local ret=1
    if picture-matches ${scene_png} ${full_jpg}.png ; then
        ret=0
    fi

    # rm -f ${full_jpg} ${full_jpg}.png
    if test "${ret}" = 0; then
        echo ${matched_xy}
    fi
    return $ret
}

is-scene() {

    ## start code-generator "^\\s *#\\s *"
    # generate-getopt x:scene-x=0 y:scene-y=0 s:scene
    ## end code-generator
    ## start generated code
    TEMP=$( getopt -o s:x:y:h \
                   --long scene:,scene-x:,scene-y:,help \
                   -n $(basename -- $0) -- "$@")
    declare scene=
    declare scene_x=0
    declare scene_y=0
    eval set -- "$TEMP"
    while true; do
        case "$1" in

            -s|--scene)
                scene=$2
                shift 2

                ;;
            -x|--scene-x)
                scene_x=$2
                shift 2

                ;;
            -y|--scene-y)
                scene_y=$2
                shift 2

                ;;
            -h|--help)
                set +x
                echo -e
                echo
                echo Options and arguments:
                printf %6s '-s, '
                printf %-24s '--scene=SCENE'
                echo
                printf %6s '-x, '
                printf %-24s '--scene-x=SCENE_X'
                echo
                printf %6s '-y, '
                printf %-24s '--scene-y=SCENE_Y'
                echo
                exit
                shift
                ;;
            --)
                shift
                break
                ;;
            *)
                die "internal error: $(. bt; echo; bt | indent-stdin)"
                ;;
        esac
    done


    ## end generated code

    if test -z "$scene"; then
        (is-scene --help) 1>&2
        exit 1
    fi

    local scene_png=${resdir}/${scene}.png
    local scene_size=$(get-image-size ${scene_png})

    tmp_png=~/tmp/is-scene.${ANDROID_SERIAL}.${scene}.png
    mkdir -p "${tmp_png%/*}"
    adb-screenshot -x ${scene_x} -y ${scene_y} -s ${scene_size} ${tmp_png} >/dev/null 2>&1
    if picture-matches ${scene_png} ${tmp_png}; then
        return 0
    fi
    return 1
}

new-clip() {
    scene=$1
    clip 2png > ${resdir}/${scene}.png
}

add-alias() {
    scene=$1

    for x in $(seq 1 100); do
        if test ! -e $resdir/${scene}.$x.png; then
            mv $resdir/${scene}.png.failed.png $resdir/${scene}.$x.png
            exit
        fi
    done

    bhj-notify "find-scene.sh" "Failed to add alias"
}

new-scene() {
    scene=$1

    if test ! -e "${resdir}/${scene}".png; then
        adb-screenshot -e &
        if EMACS=t yes-or-no-p "Finished copying the scene?"; then
            mkdir -p "$(dirname "${resdir}/${scene}")"
            clip 2png > "${resdir}/${scene}".png
        fi
    fi

    if [[ $scene =~ send-button/.*-(on|off) ]]; then
        for x in on off; do
            x=${scene%-*}-${x}
            if test ! -e "${resdir}/${x}".png; then
                relative-link "${resdir}/${scene}".png "${resdir}/${x}".png
            fi
        done
    fi
}

"$@"
