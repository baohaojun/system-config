#!/bin/sh



# rm /tmp/screen-exchange.bak

# foos=/tmp/screen-exchange
# cat $foos.* >/tmp/screen-exchange

# #rm $foos.*
FILE=/tmp/screen-exchange
# if [[ -f "$1" ]]; then
#     FILE="$1"
# fi

/bin/perl -e '$sec=0; $nsec=0; 
    while (<>) {
        if (m/bhj [0-9a-fA-F]+ (\d+) (\d+)/) {
            $tmp_sec=$1-$sec; $tmp_nsec=($2-$nsec)/1000;
            if ($tmp_nsec<0) {
                $tmp_sec-=1; $tmp_nsec+=1000000;
            }
            $sec=$1; $nsec=$2;
            s/bhj ([0-9a-fA-F]+) (\d+) (\d+)/bhj \1 $tmp_sec $tmp_nsec/;

        }
        print
    }' -i "$FILE"
/bin/perl -i ~/system-config/bin/screen-exchange-format.pl "$FILE"
cd /tmp
findexec.exe -F 1 -p emacs
efindfile.sh "$FILE"
