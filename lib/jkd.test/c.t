#!/usr/bin/env bash
set -e

jkd c -p TJS -t "故障" --fields-json "$(
cat <<'EOFf1e002924c68' | perl -ne 'print unless m,^#,;'
# {%json-mode%}
{
  "Summary" : "hello world",
  "Description" : "Hello World"
}
# {%/json-mode%}
EOFf1e002924c68

)"
