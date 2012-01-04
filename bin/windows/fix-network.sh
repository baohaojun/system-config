#!/bin/bash

wlan0_ip=$(ifconfig wlan0 | perl -ne '
BEGIN {
    sub debug(@) {
        print STDERR "@_\n";
    }
}
chomp;
if (m/IPv4 Address. . . . . . . . . . . : (\d+)\.(\d+)\.(\d+)\.(\d+)$/) {
    @ip = ($1, $2, $3, $4);
}

if (m/Subnet Mask . . . . . . . . . . . : (\d+)\.(\d+)\.(\d+)\.(\d+)$/) {
    @sm = ($1, $2, $3, $4);
}

if (m/Default Gateway . . . . . . . . . : (\d+)\.(\d+)\.(\d+)\.(\d+)$/) {
    do {
        @gw = ($1, $2, $3, $4);
        debug "gw is @gw, sm is @sm, ip is @ip";
        $right_gw = 1;
        for (0, 1, 2, 3) {
            debug "gw[$_] & sm[$_] is " . ($gw[$_] & $sm[$_]);
            debug "ip[$_] & sm[$_] is " . ($ip[$_] & $sm[$_]);
            if (($gw[$_] & $sm[$_]) != ($ip[$_] & $sm[$_])) {
                debug "not equal!";
                $right_gw = 0;
                last;
            }
        }
        if ($right_gw) {
            print join(".", @gw);
            exit;
        }
        $_ = <>;
    } while (m/(\d+)\.(\d+)\.(\d+)\.(\d+)$/);
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
    print "route add -p 0.0.0.0 mask 0.0.0.0 '$wlan0_ip' metric 50 IF $wifi\n";
    print "route add -p 0.0.0.0 mask 0.0.0.0 10.21.128.2 metric 80 IF $eth0\n";
    print "route delete 10.21.128.0\n";
    print "route add -p 10.21.128.0 mask 255.255.255.0 0.0.0.0 metric 1 IF $eth0\n";
}'

netsh interface show interface|tr -d '\r'|grep Local | perl -npe 's/.*Local/Local/' |xargs -d '\n' bash -c 'for x in "$@"; do netsh interface set interface name="$x" admin=disabled; done' xx > /dev/null 2>&1
