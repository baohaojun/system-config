#!/bin/bash
diff unicode.ord wx.ord | perl -ne '{print if 2..-1}; END{ print "1c1\n"}' |
perl -ne '
    our $unicode;
    if (m/^\d+c\d+/) {
        print "\"; #  ";
        system("grep", "$unicode", glob("~/.emacs_d/UnicodeData.txt"));
    };
    use Encode;
    if (m/^< (\d+),/) {
        $unicode = sprintf("^%X;", $1);
        print "\$unicode_wx_map{\"" . encode_utf8(chr $1). "\"} = \"";
    };
    if (m/^> (\d+)/) {
        print encode_utf8 chr($1)
    }'
