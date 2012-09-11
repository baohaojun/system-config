# /* vim: set ai ts=4 ft=sh: */
#
# Copyright 2011, The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

_adb() {
    unset -v have
    type $1 &> /dev/null && have="yes"

    if [ "$have" != "yes" ]; then
        return
    fi

    local where i cur serial
    COMPREPLY=()

    serial="${ANDROID_SERIAL:-none}"
    where=OPTIONS
    for ((i=1; i <= COMP_CWORD; i++)); do
        cur="${COMP_WORDS[i]}"
        case "${cur}" in
            -s)
                where=OPT_SERIAL
                ;;
            -p)
                where=OPT_PATH
                ;;
            -*)
                where=OPTIONS
                ;;
            *)
                if [[ $where == OPT_SERIAL ]]; then
                    where=OPT_SERIAL_ARG
                elif [[ $where == OPT_SERIAL_ARG ]]; then
                    serial=${cur}
                    where=OPTIONS
                else
                    where=COMMAND
                    break
                fi
                ;;
        esac
    done

    if [[ $where == COMMAND && $i -ge $COMP_CWORD ]]; then
        where=OPTIONS
    fi

    OPTIONS="-d -e -s -p"
    COMMAND="devices connect disconnect push pull sync shell emu logcat lolcat forward jdwp install uninstall bugreport help version start-server kill-server get-state get-serialno status-window remount reboot reboot-bootloader root usb tcpip"

    case $where in
        OPTIONS|OPT_SERIAL|OPT_PATH)
            COMPREPLY=( $(compgen -W "$OPTIONS $COMMAND" -- "$cur") )
            ;;
        OPT_SERIAL_ARG)
            local devices=$(command adb devices '2>' /dev/null | grep -v "List of devices" | awk '{ print $1 }')
            COMPREPLY=( $(compgen -W "${devices}" -- ${cur}) )
            ;;
        COMMAND)
            if [[ $i -eq $COMP_CWORD ]]; then
                COMPREPLY=( $(compgen -W "$COMMAND" -- "$cur") )
            else
                i=$((i+1))
                case "${cur}" in
                    install)
                        _adb_cmd_install "$serial" $i
                        ;;
                    pull)
                        _adb_cmd_pull "$serial" $i
                        ;;
                    push)
                        _adb_cmd_push "$serial" $i
                        ;;
                    reboot)
                        if [[ $COMP_CWORD == $i ]]; then
                            args="bootloader recovery"
                            COMPREPLY=( $(compgen -W "${args}" -- "${COMP_WORDS[i]}") )
                        fi
                        ;;
                    shell)
                        _adb_cmd_shell "$serial" $i
                        ;;
                    uninstall)
                        _adb_cmd_uninstall "$serial" $i
                        ;;
                esac
            fi
            ;;
    esac

    return 0
}

_adb_cmd_install() {
    local serial i cur where

    serial=$1
    i=$2

    where=OPTIONS
    for ((; i <= COMP_CWORD; i++)); do
        cur="${COMP_WORDS[i]}"
        case "${cur}" in
            -*)
                where=OPTIONS
                ;;
            *)
                where=FILE
                break
                ;;
        esac
    done

    cur="${COMP_WORDS[COMP_CWORD]}"
    if [[ $where == OPTIONS ]]; then
        COMPREPLY=( $(compgen -W "-l -r -s" -- "${cur}") )
        return
    fi

    _adb_util_complete_local_file "${cur}" '!*.apk'
}

_adb_cmd_push() {
    local serial IFS=$'\n' i cur

    serial=$1
    i=$2

    cur="${COMP_WORDS[COMP_CWORD]}"

    if [[ $COMP_CWORD == $i ]]; then
        _adb_util_complete_local_file "${cur}"
    elif [[ $COMP_CWORD == $(($i+1)) ]]; then
        if [ "${cur}" == "" ]; then
            cur="/"
        fi
        _adb_util_list_files $serial "${cur}"
    fi
}

_adb_cmd_pull() {
    local serial IFS=$'\n' i cur

    serial=$1
    i=$2

    cur="${COMP_WORDS[COMP_CWORD]}"

    if [[ $COMP_CWORD == $i ]]; then
        if [ "${cur}" == "" ]; then
            cur="/"
        fi
        _adb_util_list_files $serial "${cur}"
    elif [[ $COMP_CWORD == $(($i+1)) ]]; then
        _adb_util_complete_local_file "${cur}"
    fi
}

_adb_cmd_shell() {
    local serial IFS=$'\n' i cur
    local -a args

    serial=$1
    i=$2

    cur="${COMP_WORDS[i]}"
    if [ "$serial" != "none" ]; then
        args=(-s $serial)
    fi

    if [[ $i -eq $COMP_CWORD && ${cur:0:1} != "/" ]]; then
        paths=$(command adb ${args[@]} shell echo '$'PATH 2> /dev/null | tr -d '\r' | tr : '\n')
        COMMAND=$(command adb ${args[@]} shell ls $paths '2>' /dev/null | tr -d '\r' | {
            while read -r tmp; do
                command=${tmp##*/}
                printf '%s\n' "$command"
            done
        })
        COMPREPLY=( $(compgen -W "$COMMAND" -- "$cur") )
        return 0
    fi

    i=$((i+1))
    case "$cur" in
        ls)
            _adb_shell_ls $serial $i
            ;;
        /*)
            _adb_util_list_files $serial "$cur"
            ;;
        *)
            COMPREPLY=( )
            ;;
    esac

    return 0
}

_adb_cmd_uninstall() {
    local serial i where cur packages

    serial=$1
    i=$2
    if [ "$serial" != "none" ]; then
        args=(-s $serial)
    fi

    where=OPTIONS
    for ((; i <= COMP_CWORD; i++)); do
        cur="${COMP_WORDS[i]}"
        case "${cur}" in
            -*)
                where=OPTIONS
                ;;
            *)
                where=FILE
                break
                ;;
        esac
    done

    cur="${COMP_WORDS[COMP_CWORD]}"
    if [[ $where == OPTIONS ]]; then
        COMPREPLY=( $(compgen -W "-k" -- "${cur}") )
    fi

    packages="$(
        command adb ${args[@]} shell pm list packages '2>' /dev/null 2> /dev/null | tr -d '\r' | {
            while read -r tmp; do
                local package=${tmp#package:}
                echo -n "${package} "
            done
        }
    )"

    COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "${packages}" -- "${cur}") )
}

_adb_shell_ls() {
    local serial i cur file
    local -a args

    serial=$1
    i=$2
    if [ "$serial" != "none" ]; then
        args=(-s $serial)
    fi

    where=OPTIONS
    for ((; i <= COMP_CWORD; i++)); do
        cur="${COMP_WORDS[i]}"
        case "${cur}" in
            -*)
                where=OPTIONS
                ;;
            *)
                where=FILE
                break
                ;;
        esac
    done

    file="${COMP_WORDS[COMP_CWORD]}"
    if [[ ${file} == "" ]]; then
        file="/"
    fi

    case $where in
        OPTIONS)
            COMPREPLY=( $(compgen -W "$OPTIONS" -- "$cur") )
            _adb_util_list_files $serial "$file"
            ;;
        FILE)
            _adb_util_list_files $serial "$file"
            ;;
    esac

    return 0
}

_adb_util_list_files() {
    local serial dir IFS=$'\n'
    local -a toks
    local -a args

    serial="$1"
    file="$2"

    if [ "$serial" != "none" ]; then
        args=(-s $serial)
    fi

    toks=( ${toks[@]-} $(
        command adb ${args[@]} shell ls -dF ${file}"*" '2>' /dev/null 2> /dev/null | tr -d '\r' | {
            while read -r tmp; do
                filetype=${tmp%% *}
                filename=${tmp:${#filetype}+1}
                if [[ ${filetype:${#filetype}-1:1} == d ]]; then
                    printf '%s/\n' "$filename"
                else
                    printf '%s\n' "$filename"
                fi
            done
        }
    ))

    # Since we're probably doing file completion here, don't add a space after.
    if [[ $(type -t compopt) = "builtin" ]]; then
        compopt -o nospace
    fi

    COMPREPLY=( ${COMPREPLY[@]:-} "${toks[@]}" )
}

_adb_util_complete_local_file()
{
    local file xspec i j
    local -a dirs files

    file=$1
    xspec=$2

    # Since we're probably doing file completion here, don't add a space after.
    if [[ $(type -t compopt) = "builtin" ]]; then
        compopt -o plusdirs
        if [[ "${xspec}" == "" ]]; then
            COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -f -- "${cur}") )
        else
            compopt +o filenames
            COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -f -X "${xspec}" -- "${cur}") )
        fi
    else
        # Work-around for shells with no compopt

        dirs=( $(compgen -d -- "${cur}" ) )

        if [[ "${xspec}" == "" ]]; then
            files=( ${COMPREPLY[@]:-} $(compgen -f -- "${cur}") )
        else
            files=( ${COMPREPLY[@]:-} $(compgen -f -X "${xspec}" -- "${cur}") )
        fi

        COMPREPLY=( $(
            for i in "${files[@]}"; do
                local skip=
                for j in "${dirs[@]}"; do
                    if [[ $i == $j ]]; then
                        skip=1
                        break
                    fi
                done
                [[ -n $skip ]] || printf "%s\n" "$i"
            done
        ))

        COMPREPLY=( ${COMPREPLY[@]:-} $(
            for i in "${dirs[@]}"; do
                printf "%s/\n" "$i"
            done
        ))
    fi
}


if [[ $(type -t compopt) = "builtin" ]]; then
    complete -F _adb adb
else
    complete -o nospace -F _adb adb
fi
