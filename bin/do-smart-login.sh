#!/bin/bash
curl -c ~/.logs/cookie.txt -L -v 'https://login.alibaba-inc.com/ssoLogin.htm?APP_NAME=smart&BACK_URL=http://smart.corp.taobao.com/&CONTEXT_PATH=site' > 2.txt
perl -npe 's/\s+/ /gm' 2.txt |
perl -ne '
    while (m/<input([^<>]+)>/g) {
        my ($name, $val);
        $input = $1;
        if ($input =~ m/name=['\''"](.*?)['\''"]/) {
            $name = $1;
        }
        if ($input =~ m/value=['\''"](.*?)['\''"]/) {
            $val = $1;
        }
        if ($input =~ m/id=.account./) {
            $val = "%u";
        } elsif ($input =~ m/id=.password./) {
            $val = "%p";
        }

        print "$name=$val\n"
    }' |
perl -npe 's/%u/haojun.bhj/; s/%p/'"$(get-authinfo imap.alibaba-inc.com)"'/; s/(login_maintain)=/$1=on/; s/^/--data-urlencode /'|
xargs bash -x -c ' curl -A "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36" --request POST -c ~/.logs/cookie2.txt -L -v "$@" "https://login.alibaba-inc.com/ssoLogin.htm?APP_NAME=smart&BACK_URL=http://smart.corp.taobao.com/&CONTEXT_PATH=site"' true > 1.html
of 1.html
