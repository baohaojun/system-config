#!/bin/bash

wlan0_ip=$(ifconfig wlan0 | perl -ne '
chomp;
if (s/.*Default Gateway . . . . . . . . . : (.*)/$1/) { 
    if ($_ eq "0.0.0.0") {
        $_ = <>;
        s/\s//g;
    }         
    print $_;
}
')
route print -4|perl -ne '
# 12...00 ff 68 ea 3a 88 ......Juniper Network Connect Virtual Adapter
# 11...08 11 96 49 3f 80 ......Intel(R) Centrino(R) Advanced-N 6205
# 10...00 21 cc 6c b2 73 ......Intel(R) 82579LM Gigabit Network Connection

if (m/^\s*(\d+).*Juniper/) {
    $vpn = $1;
} elsif (m/^\s*(\d+).*Centrino/) {
    $wifi = $1;
} elsif (m/\s*(\d+).*82579LM/) {
    $eth0 = $1;
}

END {
    print "route delete 0.0.0.0\n";
    print "route add -p 0.0.0.0 mask 0.0.0.0 '$wlan0_ip' metric 1 IF $wifi\n";
    print "route add -p 0.0.0.0 mask 0.0.0.0 10.21.128.2 metric 30 IF $eth0\n";
    print "route delete 10.21.128.0\n";
    print "route add -p 10.21.128.0 mask 255.255.255.0 0.0.0.0 metric 1 IF $eth0\n";
}'
