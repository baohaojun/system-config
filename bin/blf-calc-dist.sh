#!/bin/bash

off1=$(blf-to-human $BLF | grep -P -e "^$1\s" | pn 5)
off2=$(blf-to-human $BLF | grep -P -e "^$2\s" | pn 5)

pretty $off2-$off1|pn 1
