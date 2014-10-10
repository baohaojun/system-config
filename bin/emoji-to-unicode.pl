#!/usr/bin/perl

# [Smile][Grimace][Drool][Scowl][CoolGuy][Sob][Shy][Silent][Sleep][Cry][Awkward][Angry][Tongue][Grin][Surprise][Frown][Ruthless][Blush][Scream][Puke][Chuckle][Joyful][Slight][Smug][Hungry][Drowsy][Panic][Sweat][Laugh][Commando][Determined][Scold][Shocked][Shhh][Dizzy][Tormented][Toasted][Skull][Hammer][Wave][Speechless][NosePick][Clap][Shame][Trick][Bah ! L][Bah ! R][Yawn][Pooh-pooh][Shrunken][TearingUp][Sly][Kiss][Wrath][Whimper][Cleaver][Watermelon][Beer][Basketball][PingPong][Coffee][Rice][Pig][Rose][Wilt][Lips][Heart][BrokenHeart][Cake][Lightning][Bomb][Dagger][Soccer][Ladybug][Poop][Moon][Sun][Gift][Hug][Strong][Weak][Shake][Peace][Fight][Beckon][Fist][Pinky][RockOn][NO][OK][InLove][Blowkiss][Waddle][Tremble][Aaagh][Twirl][Kotow][Dramatic][JumpRope][Surrender][Exciting][HipHot][ShowLove][Tai Chi L][Tai Chi R]
# /微笑/撇嘴/色/发呆/得意/流泪/害羞/闭嘴/睡/大哭/尴尬/发怒/调皮/呲牙/惊讶/难过/酷/冷汗/抓狂/吐/偷笑/可爱/白眼/傲慢/饥饿/困/惊恐/流汗/憨笑/大兵/奋斗/咒骂/疑问/嘘/晕/折磨/衰/骷髅/敲打/再见/擦汗/抠鼻/鼓掌/糗大了/坏笑/左哼哼/右哼哼/哈欠/鄙视/委屈/快哭了/阴险/亲亲/吓/可怜/菜刀/西瓜/啤酒/篮球/乒乓/咖啡/饭/猪头/玫瑰/凋谢/示爱/爱心/心碎/蛋糕/闪电/炸弹/刀/足球/瓢虫/便便/月亮/太阳/礼物/拥抱/强/弱/握手/胜利/抱拳/勾引/拳头/差劲/爱你/NO/OK/爱情/飞吻/跳跳/发抖/怄火/转圈/磕头/回头/跳绳/挥手/激动/街舞/献吻/左太极/右太极
# /::)/::~/::B/::|/:8-)/::</::$/::X/::Z/::'(/::-|/::@/::P/::D/::O/::(/::+/:--b/::Q/::T/:,@P/:,@-D/::d/:,@o/::g/:|-)/::!/::L/::>/::,@/:,@f/::-S/:?/:,@x/:,@@/::8/:,@!/:!!!/:xx/:bye/:wipe/:dig/:handclap/:&-(/:B-)/:<@/:@>/::-O/:>-|/:P-(/::'|/:X-)/::*/:@x/:8*/:pd/:<W>/:beer/:basketb/:oo/:coffee/:eat/:pig/:rose/:fade/:showlove/:heart/:break/:cake/:li/:bome/:kn/:footb/:ladybug/:shit/:moon/:sun/:gift/:hug/:strong/:weak/:share/:v/:@)/:jj/:@@/:bad/:lvu/:no/:ok/:love/:<L>/:jump/:shake/:<O>/:circle/:kotow/:turn/:skip/:oY/:#-0/:hiphot/:kiss/:<&/:&>

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
$emoticons{smile} = [":-) :)", "😊"];
$emoticons{laugh} = [":-D :D", "😄"];
$emoticons{sad} = [":-( :(", "😞"]; # Frown,[3][4][5] sad[7]
$emoticons{angry} = [":-|| :@ >:(", "😠"]; # Angry[6]
$emoticons{crying} = [":'(", "😭"]; # Crying[7]
$emoticons{tears_of_happiness} = [":')", "😂"]; # Tears of happiness[7]
$emoticons{horror} = ["[horror] [惊] [害怕] [尖叫]", "😱"]; # Horror, disgust, sadness, great dismay[4][5]
$emoticons{shock} = [":-O :O", "😲"]; #   Surprise,[2] shock,[3][9] yawn[10]
$emoticons{kiss} = [":* :^*", "😗"]; #         Kiss, couple kissing[6]
$emoticons{wink} = [";-)", "😉"]; #         Wink,[3][4][5] smirk[9][10]
$emoticons{tongue_out} = [":-P", "😛"]; # Tongue sticking out, cheeky/playful,[3] blowing a raspberry
$emoticons{uneasy} = [">:\ >:/ :-/ :-. :/ :\ =/ =\ :L =L :S >.<", ]; # Skeptical, annoyed, undecided, uneasy, hesitant[3]
$emoticons{straight_face} = [":| :-|", "😒"]; # Straight face[4] no expression, indecision[7]
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
$emoticons{yay} = ["\\o/", "🙌" ]; # Cheer "Yay, yay."[6]
$emoticons{cheerleader} = ["*\\0/*", ]; # Cheerleader[6]
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
