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
my $trusted = read_data($ntim, "I", 4);
my $date = read_data($ntim, "I", 4);
my $oem_id = read_data($ntim, "I", 4);
my @ffff = read_data($ntim, "IIIII", 20);
my $boot_flash_sig = read_data($ntim, "I", 4);
my $num_imgs = read_data($ntim, "I", 4);
my $num_keys = read_data($ntim, "I", 4);
my $remaining_after_imgs = read_data($ntim, "I", 4);

printf "version = %08x\n", $version;
printf "file sig = %c%c%c%c\n", reverse @file_sig;
printf "trusted = %d\n", $trusted;
printf "date is %08x\n", $date;
printf "oem_id is %08x\n", $oem_id;
printf "boot_flash_sig is %08x\n", $boot_flash_sig;
printf "num of imgs is %d\n", $num_imgs;
printf "num of keys is %d\n", $num_keys;
printf "remaining_after_imgs is %d\n", $remaining_after_imgs;

for (1..$num_imgs) {
  print "\n" . "*" x 16 . "\n";

 # UINT_T ImageID;					// Indicate which Image
 # UINT_T NextImageID;				// Indicate next image in the chain
 # UINT_T FlashEntryAddr;			 	// Block numbers for NAND
 # UINT_T LoadAddr;
 # UINT_T ImageSize;
 # UINT_T ImageSizeToHash;
 # UINT_T HashAlgorithmID;            // See HASHALGORITHMID_T
 # UINT_T Hash[8];					// Reserve 256 bits for the hash
 # UINT_T PartitionNumber;

  my @img_id = read_data($ntim, "CCCC", 4);
  my @next_id = read_data($ntim, "CCCC", 4);
  my $flash_addr = read_data($ntim, "I", 4);
  my $ddr_addr = read_data($ntim, "I", 4);
  my $img_size = read_data($ntim, "I", 4);
  my $img_size_to_hash = read_data($ntim, "I", 4);
  my $hash_id = read_data($ntim, "I", 4);
  my @hash = read_data($ntim, "I" x 8, 32);
  my $part_num = read_data($ntim, "I", 4);

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
  printf "img size is %d (%s)\n", $img_size, substr(qx(pretty $img_size), 0, -1);

  printf "image size to hash is %08x\n", $img_size_to_hash;
  printf "hash id is %d\n", $hash_id;
  printf "hash is " . "%x " x 8 . "\n", @hash;
  printf "partition number is %d\n", $part_num;
}

for (1..$num_keys) {
 # UINT_T	KeyID;						// Associate an ID with this key
 # UINT_T HashAlgorithmID;            // 160 - SHA1, 256 - SHA2
 # UINT_T KeySize;					// Specified in bits
 # UINT_T PublicKeySize;				// Specified in bits
 # UINT_T RSAPublicExponent[MAXRSAKEYSIZEWORDS]; // Contents depend on PublicKeySize
 # UINT_T RSAModulus[MAXRSAKEYSIZEWORDS]; // Up to 2K bits
 # UINT_T KeyHash[8]; 				// Reserve 256 bits for the hash
    print "\n" . "*" x 16 . "\n";

    my $key_id = read_data($ntim, "I", 4);
    my $hash_id = read_data($ntim, "I", 4);
    my $key_size = read_data($ntim, "I", 4);
    my $public_key_size = read_data($ntim, "I", 4);
    my @rsa_public_exponent = read_data($ntim, "I" x 64, 64 * 4);
    my @rsa_modulus = read_data($ntim, "I" x 64, 64 * 4);
    my @key_hash = read_data($ntim, "I" x 8, 32);

    printf "key id is 0x%x (%s)\n", $key_id, qx(int-to-tag $key_id);
    printf "hash id is 0x%x\n", $hash_id;
    printf "key size is %d\n", $key_size;
    printf "public key size is %d\n", $public_key_size;
    printf "rsa public exponent is " . "%x " x 64 . "\n", @rsa_public_exponent;
    printf "rsa modulus is " . "%x " x 64 . "\n", @rsa_modulus;
    printf "key hash is " . "%x " x 8 . "\n", @key_hash;
	
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

for (1..$remaining_big_entries) {
    print "\n" . "*" x 16 . "\n";
    my @tag = read_data($ntim, "CCCC", 4);
    printf "tag is %c%c%c%c\n", reverse @tag;
    my $entry_size = read_data($ntim, "I", 4);
    printf "entry size is 0x%x (%d)\n", $entry_size, $entry_size;
    if ($entry_size > 8) {
	$entry_size -= 8;	
	my @entry_data = read_data($ntim, "I" x ($entry_size / 4) , $entry_size);
	printf "entry data is " . "%x " x ($entry_size / 4) . "\n", @entry_data;
    }
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
