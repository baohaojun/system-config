#!/bin/perl

use POSIX;

@ps_args_all = split /\r*\n/, `cat ~/doc/.bash_completion_words.ps.pl`;

for (@ps_args_all) {
  $ps_args_all{$_} = 1;
}

my @ps_args, @unknown_args, %ps_args;
$ps_args{"CommandLine"} = 1;
my $index_CreationDate;
for (("ProcessId", "ParentProcessId", "CreationDate", @ARGV)) {
  if ($ps_args_all{$_} and not $ps_args{$_}) {
    $index_CreationDate = @ps_args if $_ eq "CreationDate";
    $ps_args{$_} = 1;
    push @ps_args, $_;
  } else {
    push @unknown_args, $_;
  }
}
push @ps_args, "CommandLine";

sub debug(@) {
  print STDERR "@_\n";
}

$wmi_arg = join(",", @ps_args);
$Processes=`wmic.exe path win32_process get $wmi_arg </dev/null`;
@Processes = split /\r*\n/, $Processes;

my %max_ps_arg;
for (@ps_args) {
  $Processes[0] =~ m/\b$_\s*/ or #the order of pid/ppid seems to be random with wmic?
    die "can't match the first line for '$_': " . $Processes[0];

  $start_ps_arg{$_} = $-[0];
  $end_ps_arg{$_} = $+[0];
  $size_ps_arg{$_} = $end_ps_arg{$_} - $start_ps_arg{$_};
  $max_ps_arg{$_} = length($_);
}

my $width = 12;
sub my_mktime($) {
  my $create= $_[0];
  my ($year, $mon, $mday, $hour, $min, $sec) =
    (
     substr($create, 0, 4) - 1900,
     substr($create, 4, 2) - 1,
     substr($create, 6, 2),
     substr($create, 8, 2),
     substr($create, 10, 2),
     substr($create, 12, 2)
    );

  return mktime($sec, $min, $hour, $mday, $mon, $year);
}
push @to_print, \@ps_args;
for (@Processes[1..$#Processes]) {
    chomp();
    my @ps_fields;
    my %ps_fields;
    for $arg (@ps_args) {
      my $f = substr($_, $start_ps_arg{$arg}, $size_ps_arg{$arg});
      $f =~ s/(\s*$|^\s*)//g;
      push @ps_fields, $f;
      $ps_fields{$arg} = $f;
    }

    if (@unknown_args) {
        my $match = 1;
        for (@unknown_args) {
            unless ($ps_fields{"CommandLine"} =~ m/$_/i or $ps_fields{"ProcessId"} eq $_) {
                $match = 0;
                last;
            }
        }
        next unless $match;
    }

    if ($ps_args{"CreationDate"}) {
      my $create = $ps_fields{"CreationDate"};
      my $start_second = my_mktime($create);
      my $now = my_mktime(qx(date +'%Y%m%d%H%M%S'));
      $ps_fields{"CreationDate"} = $ps_fields[$index_CreationDate] = $now - $start_second;
    }

    for (@ps_args) {
      if (length($ps_fields{$_}) > $max_ps_arg{$_}) {
        $max_ps_arg{$_} = length($ps_fields{$_});
      }
    }

    next if $ps_fields{"ParentProcessId"} == $$;

    push @to_print, \@ps_fields;
}

my $format_str;
for (@ps_args) {
  $format_str .= "%" . ($max_ps_arg{$_} + 1) . "s";
}

$format_str =~ s/(.*)%.*s/$1 %-s/;
print STDERR sprintf("$format_str\n", @{shift @to_print});
for (@to_print) {
  printf "$format_str\n", @{$_};
}
if (@to_print) {
    exit 0;
} else {
    exit 1;
}
