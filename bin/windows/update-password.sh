#!/bin/bash
set -e
echo
read -s -p "Please input your password for .gnus/.fetchmailrc: " PASS
echo
read -s -p "Please input again to confirm: " PASS2
echo

if [[ "$PASS" != "$PASS2" ]]; then
    echo "Your input does not match"
    exit -1
fi

if [[ -z "$PASS" ]]; then
    echo "you don't want to change password"
    exit 0
fi

/bin/perl -npe 's/XXXX/'"$PASS"'/g' ~/.fetchmailrc-template > ~/.fetchmailrc
/bin/perl -npe 's/XXXX/'"$PASS"'/g' ~/.gnus-template > ~/.gnus
