#!/usr/bin/perl
use Encode;
use utf8;

open($fwubi_hf_src, "<", "wubi.txt") or die $!;
open($fpy, "<", "py.txt") or die $!;
open($freverse, ">", "wubi86_reverse.py") or die $!;
open($fquail, ">", "wubi86.py") or die $!;


################ build data structure from wubi.txt ################
$line = 0;
while (<$fwubi_hf_src>) {
    $line += 1;
    chomp;
    $_ = decode_utf8($_);

    next unless m/^([^a-zA-Z ]+)([a-zA-Z ]+)$/;
    @comps = split /\s+/, encode_utf8 lc $2;
    $candidate = encode_utf8 $1;

    for $comp (@comps) {
	#海峰五笔对一些常见字也给一个一二级短码，但保证出现在真正的简码字后面。
        if (length(decode_utf8 $comp) < 3) { #这可能是一个一级或二级简码单字。
            next if $done{$comp}; #此码的正主儿之前已经出现过。
            $done{$comp} = 1;
        }
        next if $done{$comp.$candidate};
        $done{$comp.$candidate} = 1;

        push @{$quail_hash{$comp}}, $candidate;

        if (length (decode_utf8 $candidate) == 1) { #这是一个单字
            $seq{$candidate} = $line unless $seq{$candidate}; #$line是行号，出现的越早，说明该字的频率越高，在拼音反查的时候有用。
            push @{$pinyin_helper_hash{$candidate}}, $comp; #我们把其全码记下来，拼音反查用
            next if (length (decode_utf8 $comp) == 1); #码长为1，不能用于自组词
            $comp2 = substr $comp, 0, 2; #记录前二码，用于自组词
            next if $done_reverse{$comp2.$candidate};
            $done_reverse{$comp2.$candidate} = 1;
            push @{$reverse_hash{$candidate}}, $comp2;
        } else {
	    die "comp is not full" unless length($comp) == 4;
	    if (length (decode_utf8 $candidate) == 2) {
		my $char = encode_utf8 substr(decode_utf8 ($candidate), 0, 1);
		my $comp2 = substr($comp, 0, 2);
		die "组词的时候 " . decode_utf8($char) . " 有多于一种码: $comp2 and " . $single_reverse_hash{$char} 
		  if $single_reverse_hash{$char} and $single_reverse_hash{$char} ne $comp2;
		$single_reverse_hash{$char} = $comp2;
	    }
	    if (length (decode_utf8 $candidate) < 4) { #最后一个汉字也有效
		my $char = encode_utf8 substr(decode_utf8 ($candidate), -1);
		my $comp2 = substr($comp, -2);
		die "组词的时候 " . decode_utf8 ($char) . " 有多于一种码: $comp2 and " . $single_reverse_hash{$char} 
		    if $single_reverse_hash{$char} and $single_reverse_hash{$char} ne $comp2;
		$single_reverse_hash{$char} = $comp2;
	    }
	}
    }
}

for (keys %single_reverse_hash) {
    my $char = $_;
    my $comp2 = $single_reverse_hash{$char};

    @{$reverse_hash{$char}} = ($comp2);

    my @helper;
    for my $comp (@{$pinyin_helper_hash{$char}}) {
	if (length($comp) >= 2 and substr($comp, 0, 2) ne $comp2) {
	  #print "remove reverse " .  $comp . " for " . $char . " because of $comp2\n";
	  # remove $char from quail_hash{$comp}
	  @{$quail_hash{$comp}} = grep {$_ ne $char} @{$quail_hash{$comp}};
	  if (not @{$quail_hash{$comp}}) {
	    delete $quail_hash{$comp};
	  }
	  next;
	}
	push @helper, $comp;
    }
    @{$pinyin_helper_hash{$char}} = @helper;    
}

sub print_reverse()
{
################ output the reverse (for constructing chinese phrase in wubi) ################
   
    $head = <<EOC;
#!/bin/env python
# -*- coding: utf-8 -*-
g_reverse_map = {
EOC

    $tail = <<EOC;
}
EOC

    print $freverse $head;
    @keys = sort {$seq{$a} <=> $seq{$b}} keys %reverse_hash;
    foreach $key (@keys) {
        $" = qq(", ");
        print $freverse qq/"$key" : ("@{$reverse_hash{$key}}",),/, "\n";
    }
    print $freverse encode_utf8($tail);
}

sub print_quail()
{

################ build py data struct (pinyin) ################
while (<$fpy>) {
    chomp();
    ($key, $chinese) = split;
    local $"=" ";
    push @{$py_hash{"z".$key}}, {chinese=>$chinese, wubi=>"(@{$pinyin_helper_hash{$chinese}})"};
}

    $head = <<EOC;
#!/bin/env python
# -*- coding: utf-8 -*-

g_quail_map = {
EOC

    $tail = <<EOC;
". " : ("。",),
", " : ("，",),
"? " : ("？",),
"``" : ("“",),
"`` " : ("“",),
"''" : ("”",),
"'' " : ("”",),
": " : ("：",),
":`` " : ("：“",),
":``" : ("：“",),
"` " : ("‘",),
"' " : ("’",),
"< " : ("《",),
"> " : ("》",),
"<<" : ("《",),
">>" : ("》",),
"\\\\ " : ("、",),
"! " : ("！",),
"\$ " : ("￥",),
"^ " : ("…",),
"^^ " : ("……",),
"^^" : ("……",),
"* " : ("·",),
"**" : ("×",),
"** " : ("×",),
"_ " : ("—",),
"__" : ("——",),
"( " : ("（",),
") " : ("）",),
"{ " : ("｛",),
"} " : ("｝",),
"[ " : ("［",),
"] " : ("］",),
".\\" " : ("。”",),
":\\" " : ("：“",),
".'' " : ("。”",),
".''" : ("。”",),
",'' " : ("，”",),
",''" : ("，”",),
"'', " : ("”，",),
"''. " : ("”。",),
}
EOC

    print $fquail $head;

    @keys = sort keys %quail_hash;

    foreach $key (@keys) {
        $" = qq(", ");
        print $fquail qq("$key" : ("@{$quail_hash{$key}}",),), "\n";
    }


    @keys = sort keys %py_hash;

    foreach $key (@keys) {
        @py_data = map {$_ = ${$_}{"chinese"} . ${$_}{"wubi"} } sort {$seq{$a{"chinese"}} <=> $seq{$b{"chinese"}}} @{$py_hash{$key}};
        $" = qq(", ");
        print $fquail qq/"$key" : ("@py_data",),/, "\n";
    }
    print $fquail encode_utf8($tail);
}


################ refine the data struct, most comp with len < 4 should have only 1 candidate ################
sub refine_quail()
{
    foreach $chinese (keys %pinyin_helper_hash) {
        @comps = sort @{$pinyin_helper_hash{$chinese}};
        foreach $comp (@comps) {
            if (grep /$comp./, @comps) {
                @comps = grep {$_ !~/$comp./} @comps;
            }
        }

        foreach $comp (@{$pinyin_helper_hash{$chinese}}) {
            if (!grep {$_ eq $comp} @comps) {
                @{$quail_hash{$comp}} = grep {$_ ne $chinese} @{$quail_hash{$comp}};
                if (! @{$quail_hash{$comp}}) {
                    delete $quail_hash{$comp};
                }
            }
        }
        @{$pinyin_helper_hash{$chinese}} = @comps;

        
    }
}

refine_quail();

print_reverse();
print_quail();



# #("aaaa" ["工" "恭恭敬敬"])


