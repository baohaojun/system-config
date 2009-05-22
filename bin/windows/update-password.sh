#!/bin/bash
echo
read -s -p "Please input your password for .gnus/.fetchmailrc: " PASS
echo
read -s -p "Please input again to confirm: " PASS2
echo

if [[ "$PASS" != "$PASS2" ]]; then
    echo "Your input does not match"
    exit -1
fi

/bin/perl -npe 's/XXXX/'"$PASS"'/g' ~/.fetchmailrc-template > ~/.fetchmailrc
/bin/perl -npe 's/XXXX/'"$PASS"'/g' ~/.gnus-template > ~/.gnus
