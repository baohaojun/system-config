#!/bin/bash
cat |awk -F, '{print "echo "  "\""$1"\"" "\"<"$2">\" >" "\""$1"\""}'
