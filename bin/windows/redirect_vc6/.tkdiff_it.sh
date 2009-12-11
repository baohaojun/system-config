#!/bin/bash
set -- "$(cygpath -au $1)" "$(cygpath -au $2)"
cd #must change to where _tkdiff.rc is?

tkdiff "$@"

