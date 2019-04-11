#!/bin/bash

rm -f .config/system-config/.bashrc-path
. ~/system-config/.bashrc
~/system-config/bin/after-co-ln-s.sh
