#!/bin/bash

echo -n 'BTW, if you can'\''t visit `\\bhj1`, the current ('`date|tr -d '\r\n'`') ip address is: '
ipconfig |tr -d '\n\r '|perl -npe 's/.*eth0.*?IPA.*?:(.*?)S.*/\1/'
echo -n ', but since DHCP is used, the ip might change.'
