#!/usr/bin/env bash
set -e

gr assign-permissions \
   -r saos/mall/mall-product \
   -b refs/heads/master \
   -b "refs/heads/release/*" \
   -p "read" \
   -p "push" \
   -g user/scmtest \
   -g admin \
   --exclusive
