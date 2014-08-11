#!/usr/bin/perl

use Getopt::Long;
my $debug;
GetOptions(
    "d!" => \$debug,
    );
sub debug(@) {
    print STDERR "@_\n" if $debug;
}

my %emoticons;

$emoticons{big_laugh} = [":-))", "😝"]; # Very happy or double chin[6]
$emoticons{smile} = [":-) :) :o) :] :3 :c) :> =] 8)", "😊"];
$emoticons{laugh} = [":-D :D 8-D 8D x-D xD X-D XD =-D =D =-3 =3 B^D", "😄"];
$emoticons{sad} = [">:[ :-( :( :-c :c :-< :っC :< :-[ :[ :{", "😢"]; # Frown,[3][4][5] sad[7]
$emoticons{wink} = [";(", "😉"];   # Winky frowny, used to signify sadness, with a bit of sarcasm. It is easily misunderstood.[8]
$emoticons{angry} = [":-|| :@ >:(", "😠"]; # Angry[6]
$emoticons{crying} = [":'-( :'(", "😭"]; # Crying[7]
$emoticons{tears_of_happiness} = [":'-) :')", "😂"]; # Tears of happiness[7]
$emoticons{horror} = ["D:< D: D8 D; D= DX v.v D-':", "😱"]; # Horror, disgust, sadness, great dismay[4][5]
$emoticons{shock} = [">:O :-O :O :-o :o 8-0 O_O o-o O_o o_O o_o O-O", "😲"]; #   Surprise,[2] shock,[3][9] yawn[10]
$emoticons{kiss} = [":* :^*\n( '}{' )", "😗"]; #         Kiss, couple kissing[6]
$emoticons{wink} = [";-) ;) *-) *) ;-] ;] ;D ;^) :-,", "😉"]; #         Wink,[3][4][5] smirk[9][10]
$emoticons{tongue_out} = [">:P :-P :P X-P x-p xp XP :-p :p =p :-Þ :Þ :þ :-þ :-b :b d:", "😛"]; # Tongue sticking out, cheeky/playful,[3] blowing a raspberry
$emoticons{uneasy} = [">:\ >:/ :-/ :-. :/ :\ =/ =\ :L =L :S >.<", ]; # Skeptical, annoyed, undecided, uneasy, hesitant[3]
$emoticons{straight_face} = [":| :-|", ]; # Straight face[4] no expression, indecision[7]
$emoticons{blushing} = [":\$", "😳"]; # Embarrassed,[5] blushing[6]
$emoticons{sealed_lips} = [":-X :X :-# :#", ]; #   Sealed lips or wearing braces[3]
$emoticons{angel} = ["O:-) 0:-3 0:3 0:-) 0:) 0;^)", "😇"]; # Angel,[3][4][9] saint,[7] innocent
$emoticons{evil} = [">:) >;) >:-)", "😏"]; #    Evil[4]
$emoticons{devil} = ["}:-) }:) 3:-) 3:)", "👿"]; # Devilish[7]
$emoticons{high_five} = ["o/\\o ^5 >_>^ ^<_<", ]; # High five[10]
$emoticons{cool} = ["|;-) |-O", ]; # Cool,[7] bored/yawning[9]
$emoticons{tongue_tied} = [":-& :&", ]; # Tongue-tied[7]
$emoticons{partied_all_night} = ["#-)", ]; # Partied all night[7]
$emoticons{confused} = ["%-) %)", "😕"]; # Drunk,[7] confused
$emoticons{sick} = [":-###.. :###..", "😷"]; # Being sick[7]
$emoticons{dumb} = ["<:-|", ]; # Dumb, dunce-like[9]
$emoticons{disapproval} = ["ಠ_ಠ", ]; #Look of disapproval[11] The Unicode character ಠ is from the Kannada alphabet and can be called differently in HTML notation: &#3232; and &#x0CA0; (for Unicode)
$emoticons{fish} = ["<*)))-{ ><(((*> ><>", ]; # Fish, something's fishy,[9] Christian fish[6]
$emoticons{yay} = ["\\o/", ]; # Cheer "Yay, yay."[6]
$emoticons{cheerlearder} = ["*\\0/*", ]; # Cheerleader[6]
$emoticons{rose} = ["@}-;-'--- @>-->--", ]; # Rose[3][9]
$emoticons{homer_simpson} = ["~(_8^(I)", ];    # Homer Simpson[9]
$emoticons{elvis_presley} = ["5:-) ~:-\\", ]; # Elvis Presley[9][10]
$emoticons{jonh_lennon} = ["//0-0\\\\", ]; #         John Lennon[9]
$emoticons{santa_claus} = ["*<|:-)", ]; #  Santa Claus[12]
$emoticons{bill_clinton} = ["=:o]", ]; # Bill Clinton[12]
$emoticons{ronald_reagan} = [",:-) 7:^]", ]; #       Ronald Reagan[12]
$emoticons{heart} = ["<3 [heart] [心]", "💘"];
$emoticons{heart_broken} = ["</3 [heart broken] [broken heart] [心碎] [心碎了]", "💔"]; # Heart and broken-heart (reverse-rotation)[13]
$emoticons{good} = ["[good] [强]", "👍"];
$emoticons{weak} = ["[weak] [弱]", "👎"];
$emoticons{cow} = ["[cow] [牛] [牛逼]", "🐮"];
$emoticons{sweat} = ["[汗] [sweat]", "😓"];
$emoticons{baby} = ["[baby] [orz]", "🚼"];

my %unicode_emoticons;
for (keys %emoticons) {
    my $separator = ' ';
    $unicode_emoticons{$_} = $emoticons{$_}[1];
    $emoticons{$_} = $emoticons{$_}[0];
    if ($emoticons{$_} =~ m/\n/) {
        $separator = "\n";
    }
    $emoticons{$_} = join("|", map {$_ = "\Q$_\E"} split($separator, $emoticons{$_}));
    $emoticons{$_} = qr($emoticons{$_});
}

while (<>) {
    for my $emoji (sort {$a cmp $b} keys %emoticons) {
        if ($unicode_emoticons{$emoji}) {
            s/$emoticons{$emoji}/$unicode_emoticons{$emoji}/ge;
        }
    }
    print;
}
