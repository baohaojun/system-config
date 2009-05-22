#!/bin/bash
ln -sf "`/bin/readlink \"$1\"`" "$1"
