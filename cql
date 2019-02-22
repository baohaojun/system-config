#!/bin/bash
# [[file:~/src/github/org-kungfu/org-kungfu.org::cql][cql]]
set -e
get-config() {
    git config -f ~/.config/org-kungfu/config.ini "$@"
}

user=$(get-config ok.username)
pass=$(get-config ok.password)
wikiurl=$(get-config ok.wikiurl)

cql_str=$(
    urlify "$@"
       )

curl -u "${user}:${pass}" "${wikiurl}/rest/api/search?cql=$cql_str"
# cql ends here
