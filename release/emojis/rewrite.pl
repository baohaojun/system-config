#!/usr/bin/perl

use Encode;

while (<>) {
    print if s|git mv u([0-9a-f]+)_u([0-9a-f]+).png unicode/(.*)|
        $c1 = encode_utf8 chr(hex("0x$1"));
        $c2 = encode_utf8 chr(hex("0x$2"));
        $uni1 = qx(unicode-name $1);
        $uni2 = qx(unicode-name $2);
        sprintf "{['%s%s', '%s unicode', '%s']},", $c1, $c2, $uni1 . $uni2, "emojis/iphone-new/$3"
            |ei;

    print if s|git mv u([0-9a-f]+).png unicode/(.*)|
        $c = encode_utf8 chr(hex("0x$1"));
        $uni = qx(unicode-name $1);
        sprintf "{['%s', '%s unicode', '%s']},", $c, $uni, "emojis/iphone-new/$2"
            |ei;

    @colors = (
        "white skin",
        "light brown skin",
        "olive toned skin",
        "deep brown skin",
        "black skin",
        );
    print if s|git mv u([0-9a-f]+).([0-5]).png unicode/(.*)|
        $c1 = encode_utf8 chr(hex("0x$1"));
        $type = $2;
        $c2 = "";
        $color = "";
        if ($type > 0) {
            $c2 = encode_utf8 chr(hex("0x1f3fa") + $type);

            $color = " in $colors[$type - 1]";
        }
        $uni = qx(unicode-name $1);
        sprintf "{['%s%s', '%s%s unicode', '%s']},", $c1, $c2, $uni, $color, "emojis/iphone-new/$3"
            |ei;

    %p_map = (
        B => encode_utf8 chr(hex("0x1F466")),
        G => encode_utf8 chr(hex("0x1F467")),
        M => encode_utf8 chr(hex("0x1F468")),
        W => encode_utf8 chr(hex("0x1F469")),
        );
    $zwj = encode_utf8 chr(hex("0x200d"));
    $varsel = encode_utf8 chr(hex("0xfe0f"));
    $heart = encode_utf8 chr(hex("0x2764"));
    $kiss = encode_utf8 chr(hex("0x1f48b"));

    print if s|git mv u([0-9a-f]+).([0-5])\.(\w+)\.png unicode/(.*)|
        $c1 = encode_utf8 chr(hex("0x$1"));
        $type = $2;
        $combo = $3;
        $file = $4;
        $uni = qx(unicode-name $1);
        if ($combo =~ m/DEF/) {
            sprintf "{['%s', '%s unicode', '%s']},", $c1, $uni, "emojis/iphone-new/$file";
        } else {
            $joiner = $zwj;
            if ($uni =~ m/KISS/) {
                $joiner = "$zwj$heart$varsel$zwj$kiss$zwj";
            } elsif ($uni =~ m/COUPLE/) {
                $joiner = "$zwj$heart$varsel$zwj";
            }
            sprintf "{['%s', '%s unicode', '%s']},",
                join($joiner, map{ $p_map{$_} } split("", "$combo")),
                "$uni with $combo",
                "emojis/iphone-new/$file";
        }
    |ei;
}
