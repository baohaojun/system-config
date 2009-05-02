#!/bin/bash -x
ln -sf "`readlink \"$1\"`" "$1"