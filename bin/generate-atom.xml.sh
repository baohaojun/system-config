#!/bin/bash

function github-pagep()
{
    echo http://baohaojun.github.io/${1/%.org/.html}
}

cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">

  <title><![CDATA[Bao Haojun - Happy Hacking]]></title>
  <link href="http://baohaojun.github.io/atom.xml" rel="self"/>
  <link href="http://baohaojun.github.io/"/>
  <updated>2013-01-09T16:02:42+00:00</updated>
  <id>http://baohaojun.github.io/</id>
  <author>
    <name><![CDATA[Bao Haojun]]></name>

  </author>
  <generator uri="http://octopress.org/">Octopress</generator>
EOF

for x in *.org; do grep -P -e "\Q#+title:\E" -q $x && git log -1 --pretty=format:"%ai $x%n" $x; done|sort -n |reverse | while read date time tz name; do
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
