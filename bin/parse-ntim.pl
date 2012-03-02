#!/usr/bin/perl

use strict;

open(my $ntim, "<", $ARGV[0]) or die "can not open " . $ARGV[0];

sub read_data($$$) {
  my ($ntim, $template, $bytes) = @_;
  read($ntim, my $data, $bytes) == $bytes or die "can not read 4 bytes";
  if (length($template) == 1) {
    return (unpack($template, $data))[0];
  } else {
    return unpack($template, $data);
  }

}

my $version = read_data($ntim, "I", 4);
my @file_sig = read_data($ntim, "CCCC", 4);
my $all_0 = read_data($ntim, "I", 4);
die "all_0 is not 0" unless $all_0 == 0;
my $date = read_data($ntim, "I", 4);
my $oem_id = read_data($ntim, "I", 4);
my @ffff = read_data($ntim, "IIIII", 20);
my $boot_flash_sig = read_data($ntim, "I", 4);
my $num_imgs = read_data($ntim, "I", 4);
my $all_0 = read_data($ntim, "I", 4);
die "all_0 is not 0" unless $all_0 == 0;
my $remaining_after_imgs = read_data($ntim, "I", 4);

printf "version = %08x\n", $version;
printf "file sig = %c%c%c%c\n", reverse @file_sig;
printf "date is %08x\n", $date;
printf "oem_id is %08x\n", $oem_id;
printf "boot_flash_sig is %08x\n", $boot_flash_sig;
printf "num of imgs is %d\n", $num_imgs;
printf "remaining_after_imgs is %d\n", $remaining_after_imgs;

for (1..$num_imgs) {
  print "\n" . "*" x 16 . "\n";
  my @img_id = read_data($ntim, "CCCC", 4);
  my @next_id = read_data($ntim, "CCCC", 4);
  my $flash_addr = read_data($ntim, "I", 4);
  my $ddr_addr = read_data($ntim, "I", 4);
  my $img_size = read_data($ntim, "I", 4);
  my @all_0 = read_data($ntim, "I" x 10, 40);

  printf "img = %c%c%c%c\n", reverse @img_id;

  my $img_id = sprintf("%c" x 4, reverse @img_id);
  if ($ARGV[1]) {
    my $blf = $ARGV[1];
    my $img_path = qx(get-blf-img-val $blf $img_id Path);
    print "img_path is $img_path\n";
    chomp(my $img_stat_size = qx(stat -c %s $img_path));
    die "img size error for $img_id (ntim: $img_size; real: $img_stat_size)"
      unless $img_stat_size == $img_size;
  }
  my $image_name = 
  printf "next img = %c%c%c%c\n", reverse @next_id unless $next_id[0] == 0xff;
  printf "flash_addr is %08x\n", $flash_addr;
  printf "ddr_addr is %08x\n", $ddr_addr;
  printf "img size is %d\n", $img_size;
  if (grep {$_ != 0} @all_0) {
    die "cantained !0 in all_0";
  }
}

my $current = tell($ntim);
printf "we are currently on byte %x\n", $current;
use Fcntl qw(SEEK_END SEEK_SET);
seek($ntim, 0, SEEK_END);

my $total = tell($ntim);
my $remaining = $total - $current;
my $after_imgs_offset = $current;
printf "remaining bytes: %x\n", tell($ntim) - $current;
die "remaining is not remaining_after_imgs" unless $remaining >= $remaining_after_imgs;

seek($ntim, $current, SEEK_SET);
my @opt_sig = read_data($ntim, "CCCC", 4);
printf "opt_sig = %c%c%c%c\n", reverse @opt_sig;
die "opt sig error" unless sprintf("%c%c%c%c", reverse @opt_sig) eq "OPTH";

my $remaining_big_entries = read_data($ntim, "I", 4);
printf "remaining_big_entries is %08x\n", $remaining_big_entries;
die "remaining_big_entries error" unless $remaining_big_entries == 3;

my @ddrc_sig = read_data($ntim, "CCCC", 4);
printf "ddrc_sig is %c%c%c%c\n", reverse @ddrc_sig;
die "ddrc sig error" unless sprintf("%c%c%c%c", reverse @ddrc_sig) eq "DDRC";

my $ddrc_bytes = read_data($ntim, "I", 4);
printf "ddrc bytes = %d %08x\n", $ddrc_bytes, $ddrc_bytes;
die "ddrc_bytes not multiple of 8" unless $ddrc_bytes % 8 == 0;
printf "total %d entries of ddrc config\n", $ddrc_bytes/8 - 1;
for (1..($ddrc_bytes/8 - 1)) {
  my $ddrc_key = read_data($ntim, "I", 4);
  my $ddrc_val = read_data($ntim, "I", 4);
  printf "ddrc key %02d = %08x\n", $ddrc_key, $ddrc_val;
}

my @cmcc_sig = read_data($ntim, "CCCC", 4);
die "cmcc sig error" unless sprintf("%c%c%c%c", reverse @cmcc_sig) eq "CMCC";

my $cmcc_bytes = read_data($ntim, "I", 4);
die "cmcc bytes error" unless $cmcc_bytes == 0x20;

my @cmcc_configs = (1, 1, 0x4F424D49);

for (1..($cmcc_bytes/8 - 1)) {
  my $cmcc_key = read_data($ntim, "I", 4);
  die "cmcc_key error" unless $cmcc_key == ($_ - 1);

  my $cmcc_val = read_data($ntim, "I", 4);
  die "cmcc_val error" unless $cmcc_val == $cmcc_configs[$_ - 1];
}

my @term_sig = read_data($ntim, "CCCC", 4);
printf "term_sig is %c%c%c%c\n", reverse @term_sig;
die "term sig error" unless sprintf("%c%c%c%c", reverse @term_sig) eq "Term";

my $term_bytes = read_data($ntim, "I", 4);
die "term_bytes error" unless $term_bytes == 8;
$current = tell($ntim);
die "remaining_after_imgs error" unless $current - $after_imgs_offset == $remaining_after_imgs;

my $seen_ff = 0;
while (tell($ntim) < $total) {
  my $zero_or_ff = read_data($ntim, "I", 4);
  if ($zero_or_ff == 0 && not $seen_ff) {
    next;
  } elsif ($zero_or_ff == 0xffffffff) {
    $seen_ff = 1;
  } else {
    my $current = tell($ntim);
    printf "warning: remaining zero_or_ff : %08x at %08x\n", $zero_or_ff, $current;
  }

}
print "ok\n";
