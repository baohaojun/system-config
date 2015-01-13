#!/bin/bash

exec >atom.xml
site=http://baohaojun.github.io/
if test -e .do-blog-site; then
    site=$(cat .do-blog-site)
fi
function github-pagep()
{
    echo ${site}${1/%.org/.html}
}

cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">

  <title><![CDATA[Bao Haojun - Happy Hacking]]></title>
  <link href="${site}atom.xml" rel="self"/>
  <link href="${site}"/>
  <updated>2013-01-09T16:02:42+00:00</updated>
  <id>${site}</id>
  <author>
    <name><![CDATA[Bao Haojun]]></name>

  </author>
  <generator uri="http://octopress.org/">Octopress</generator>
EOF

for x in $(find blog -name '*.org'|sort -n|reverse); do grep -P -e "\Q#+title:\E" -q $x &&
    (
        git log --pretty=format:"%ai $x%n" $x 2>/dev/null | tail -n 1 | grep . ||
        echo $(date +%Y-%M-%d\ %H:%m:%S\ +0800) $x
    ); done|perl -ne 'print if 1..10' |tee /dev/stderr | while read date time tz name; do
        cat <<EOF
  <entry>
    <title type="html"><![CDATA[$(grep -P -e "\Q#+title:\E" $name | perl -npe "s/.*:\s+//")]]></title>
    <link href="$(github-pagep "$name")"/>
    <updated>${date}T$time${tz/%00/:00}</updated>
    <id>$(github-pagep "$name")</id>
  </entry>
EOF
done

echo "</feed>"
