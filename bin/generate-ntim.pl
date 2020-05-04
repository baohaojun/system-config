#!/usr/bin/env perl

use strict;

my $blf_path = $ARGV[0];
my $ntim_path = ($ARGV[1] or qx(get-blf-img-val $blf_path TIMH Path) . ".out");
open(my $ntim, ">", $ntim_path) or die "can not open ntim: $ntim_path\n";

sub debug(@) {
  print STDERR "@_\n";
}

sub write_data($$$) {
  my $packed = pack_data(@_);
  print $ntim $packed;
}

sub pack_data($$$) {
  my ($template, $bytes, $data) = @_;
  die "data is empty" unless defined $data;
  if ($template eq "I") {
    die "data format error" unless $data =~ m/^(0x)?[a-f0-9]+$/i;
    $data = eval($data);
  }

  my @data = ($data);
  if ($template =~ m/^C+$/) {
    @data = map(ord, reverse split(//, $data));
  }
  my $packed = pack($template, @data);
  print "data is @data\n";
  die "pack bytes error" unless length($packed) == $bytes;
  return $packed;
}

sub get_int($) {
  my ($i) = @_;
  die "int format error $i" unless $i =~ m/^\s*(0x)?[a-f0-9]+\s*$/i;
  return eval($i);
}

sub int_to_tag($) {
  my ($arg) = @_;
  my $x = pack("I", $arg);
  return join("", map(chr, reverse unpack("CCCC", $x)));
}


open(my $blf, "<", $blf_path) or die "can not open blf";

my %blf_setting;

my $ddrc_config_bytes;
my $cmcc_config_bytes;

my %ddrc_id_names =
  ("TTC_PHYCTLREG10_ID" => 26,
   "TTC_SDRTMGREG1_ID" => 5,
   "TTC_SDRCTLREG2_ID" => 11,
   "TTC_DLLCTLREG3_ID" => 31,
   "TTC_ADRMAPREG0_ID" => 17,
   "TTC_PHYCTLREG7_ID" => 23,
   "TTC_PHYCTLREG3_ID" => 22,
   "TTC_SDRCFGREG1_ID" => 3,
   "TTC_SDRTMGREG4_ID" => 8,
   "TTC_PHYCTLREG14_ID" => 28,
   "TTC_PHYCTLREG8_ID" => 24,
   "TTC_SDRTMGREG3_ID" => 7,
   "TTC_SDRTMGREG5_ID" => 9,
   "TTC_SDRTMGREG2_ID" => 6,
   "TTC_MCBCTLREG4_ID" => 35,
   "TTC_ADRMAPREG1_ID" => 18,
   "TTC_SDRCFGREG0_ID" => 2,
   "TTC_SDRCTLREG4_ID" => 13,
   "TTC_PHYCTLREG13_ID" => 27,
   "TTC_USRCMDREG0_ID" => 20,
   "TTC_PHYCTLREG9_ID" => 25,
   "TTC_DLLCTLREG2_ID" => 30,
   "TTC_DLLCTLREG1_ID" => 29,
  );
while (<$blf>) {
  chomp;
  next unless m/^\s*(.*?)\s*(?:=|:)\s*(.*?)\s*$/;
  my ($key, $val) = ($1, $2);
  if ($key =~ m/^TTC.*_ID$/) {
    debug "doing ddrc key $key";
    die "ddrc key error" unless exists $ddrc_id_names{$key};
    $ddrc_config_bytes .= pack_data("I", 4, $ddrc_id_names{$key});
    $ddrc_config_bytes .= pack_data("I", 4, $val);
  } elsif ($key =~ m/^CMCC_.*_ID$/) {
    my $id = length($cmcc_config_bytes) / 8;
    $cmcc_config_bytes .= pack_data("I", 4, $id);
    $cmcc_config_bytes .= pack_data("I", 4, $val);
  } else {
    $blf_setting{$key} = $val;
  }
}

my $num_ddrc_bytes = length($ddrc_config_bytes) + 8;
$ddrc_config_bytes = pack_data("CCCC", 4, "DDRC") .
  pack_data("I", 4, $num_ddrc_bytes) .
  $ddrc_config_bytes;

my $num_cmcc_bytes = length($cmcc_config_bytes) + 8;
$cmcc_config_bytes = pack_data("CCCC", 4, "CMCC") .
  pack_data("I", 4, $num_cmcc_bytes) .
  $cmcc_config_bytes;


write_data("I", 4, $blf_setting{"Version"});
printf "version is %s\n", $blf_setting{"Version"};
write_data("CCCC", 4, "TIMH");
write_data("I", 4, 0); # fixme
write_data("I", 4, $blf_setting{"Issue Date"});
write_data("I", 4, $blf_setting{"OEM UniqueID"});

for (1..5) {
  write_data("I", 4, 0xffffffff);
}

write_data("I", 4, $blf_setting{"Boot Flash Signature"});
write_data("I", 4, $blf_setting{"Number of Images"});
write_data("I", 4, 0); # fixme

my $remaining_bytes_after_imgs = 16 # OPTH tag 8 bytes; Term tag 8 bytes
  + length($ddrc_config_bytes)
  + length($cmcc_config_bytes);
write_data("I", 4, $remaining_bytes_after_imgs);

my $ntim_file_size = tell($ntim)
  + $remaining_bytes_after_imgs
  + $blf_setting{"Number of Images"} * 60;

for (1..eval($blf_setting{"Number of Images"})) {
  my $img_id = qx(get-blf-img-val $blf_path $_ "ID Name");
  debug "doing img $img_id";
  my $next_id = qx(get-blf-img-val $blf_path $_ "Next Image ID");

  write_data("CCCC", 4, $img_id);
  write_data("I", 4, $next_id);
  write_data("I", 4, $blf_setting{"$_ Image Flash Entry Address"});
  write_data("I", 4, $blf_setting{"$_ Image Load Address"});

  my $img_size;
  if ($img_id eq "TIMH") {
    $img_size = $ntim_file_size;
  } else {
    my $img_path = qx(get-blf-img-val $blf_path $img_id "Path");
    if (not $img_path) {
      $img_size = 0;
    } else {
      die "img $img_path not exist!" unless -e $img_path;
      chomp($img_path = qx(readlink -f $img_path)); # must not be a symlink!
      $img_size = -s $img_path;
      chomp(my $blf_erase_size = qx(get-blf-img-val $blf_path $_ "Erase Size"));
      if ($blf_erase_size) {
	$img_size = get_int($blf_erase_size);
      }
      die "img $img_path size is 0" unless $img_size;
    }
  }
  write_data("I", 4, $img_size);

  for (1..10) {
    write_data("I", 4, 0);
  }
}

write_data("CCCC", 4, "OPTH");
write_data("I", 4, 3);

print $ntim $ddrc_config_bytes;
print $ntim $cmcc_config_bytes;

write_data("CCCC", 4, "Term");
write_data("I", 4, 8);
print "OK\n";


# [Tavor Flasher Properties]
# Flash Configuration File Path = Tavor_NAND.ini
# FlasherLoadAddress = 0xD100B000
# MEP File Path = ReliableData\IMEI_Generator\MEP.bin
# IMEI File Path = ReliableData\IMEI_Generator\IMEI.bin
# Flasher Path = Flasher\TavorFlasher_NAND_B000.bin
# [TIM Configuration]
# Number of Images = 16
# Size of Reserved in bytes = 0
# Erase All Flash = 0
# Boot Flash Signature = 0x4E414E04
# Processor Type = PXA92x
# OEM UniqueID = 0x736B7920
# Issue Date = 0x06102009
# Version = 0x00030101
# Trusted = 0
# [Extended Reserved Data]
# Configure Memory Control:
# CMCC_CONFIG_ENA_ID:0x00000001
# CMCC_MEMTEST_ENA_ID:0x00000001
# CMCC_CONSUMER_ID:0x4F424D49
# End Configure Memory Control:
# DDR Custom:
# TTC_SDRCFGREG0_ID:0x00001430
# TTC_PHYCTLREG3_ID:0x0000C411
# TTC_ADRMAPREG0_ID:0x000C0001
# TTC_SDRTMGREG1_ID:0x488C00B6
# TTC_SDRTMGREG2_ID:0x43540132
# TTC_SDRTMGREG3_ID:0x201D1D09
# TTC_SDRTMGREG4_ID:0x12820002
# TTC_SDRTMGREG5_ID:0x00090022
# TTC_PHYCTLREG7_ID:0x0FF00331
# TTC_PHYCTLREG8_ID:0x07700330
# TTC_SDRCTLREG4_ID:0x0080C011
# TTC_SDRCTLREG2_ID:0x00080000
# TTC_PHYCTLREG13_ID:0x20007C04
# TTC_DLLCTLREG1_ID:0x20007C04
# TTC_DLLCTLREG2_ID:0x20007C04
# TTC_DLLCTLREG3_ID:0x20007C04
# TTC_PHYCTLREG14_ID:0x20000000
# TTC_MCBCTLREG4_ID:0x0200EE0E
# TTC_PHYCTLREG9_ID:0xC0000077
# TTC_PHYCTLREG10_ID:0x0010210C
# TTC_USRCMDREG0_ID:0x00000001
# End DDR Custom:
# End Extended Reserved Data:
# [Skip Block Configuration]
# Skip Block Number = 
# [Image List]
# 16 Image Image Size To CRC in bytes = 0
# 16 Image Partition Number = 0
# 16 Image Erase Size = 0x00840000
# 16 Image ID Name = ERAN
# 16 Image Type = RAW
# 16 Image Load Address = 0xFFFFFFFF
# 16 Image Flash Entry Address = 0x1e3c0000
# 16 Image Path = panic.part
# 16 Image Next Image ID = 0xFFFFFFFF
# 16 Image Image ID = 0x4552414E
# 16 Image Enable = 1
