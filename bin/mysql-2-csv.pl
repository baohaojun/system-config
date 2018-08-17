#!/usr/bin/env perl

## start code-generator "^\\s *#\\s *"
# generate-getopt -s perl @:defaults-file @:host u:user p:password d:database t:table e:select-exp o:output-file=/dev/stdout
## end code-generator
## start generated code
use Getopt::Long;

my $database = '';
my $defaults_file = '';
my $host = '';
my $output_file = '/dev/stdout';
my $password = '';
my $select_exp = '';
my $table = '';
my $user = '';

GetOptions (
            'database|d=s' => \$database,
            'defaults-file=s' => \$defaults_file,
            'host=s' => \$host,
            'output-file|o=s' => \$output_file,
            'password|p=s' => \$password,
            'select-exp|e=s' => \$select_exp,
            'table|t=s' => \$table,
            'user|u=s' => \$user,
            'help|h!' => \&handler_help,
           );



sub handler_help {
  print ;
  print "\n\n选项和参数：\n";
  printf "%6s", '-d, ';
  printf "%-24s", '--database=DATABASE';
  if (length('--database=DATABASE') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '';
  printf "%-24s", '--defaults-file=DEFAULTS-FILE';
  if (length('--defaults-file=DEFAULTS-FILE') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '';
  printf "%-24s", '--host=HOST';
  if (length('--host=HOST') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-o, ';
  printf "%-24s", '--output-file=OUTPUT-FILE';
  if (length('--output-file=OUTPUT-FILE') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-p, ';
  printf "%-24s", '--password=PASSWORD';
  if (length('--password=PASSWORD') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-e, ';
  printf "%-24s", '--select-exp=SELECT-EXP';
  if (length('--select-exp=SELECT-EXP') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-t, ';
  printf "%-24s", '--table=TABLE';
  if (length('--table=TABLE') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-u, ';
  printf "%-24s", '--user=USER';
  if (length('--user=USER') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";

  exit(0);
}

## end generated code

use DBIx::Dump;
use DBI;


# my $dbh = DBI->connect("DBI:mysql:mysql_read_default_file=$defaults_file;database=$database;host=$host", "$user", '$password', {RaiseError => 1});
my $dbh = DBI->connect("DBI:mysql:mysql_read_default_file=$defaults_file;database=$database;host=$host",
                       "$user", "$password",
                       {
                        RaiseError => 1,
                        mysql_enable_utf8 => 1
                       }
                      );

# ... connect to your database as normal
$sth = $dbh->prepare("SELECT * from $table");
$sth->execute( );

$out = DBIx::Dump->new('format' => "csv",
                       'output' => "$output_file",
                       'sth'    => $sth,
                       encoding => 'utf-8'
                      );
$out->dump( );
