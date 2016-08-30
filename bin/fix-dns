#!/bin/bash
if ! grep -q 'nameserver 8.8.8.8' /etc/resolv.conf; then
    old_resolv_conf=$(
        cat /etc/resolv.conf
                   )

    cat <<EOF | sudo tee /etc/resolv.conf
nameserver 8.8.8.8
nameserver 4.4.4.4
$old_resolv_conf
EOF
fi
