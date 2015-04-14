#!/usr/bin/perl

use strict;

my $dir = $ENV{PWD};
my $db_dir = glob("~/.cache/system-config/for-code-reading" . $dir);
my $db_slags = $db_dir . "/SLAGS";
my $db_paths = $db_dir . "/SPATHS";

sub debug(@) {
    print STDERR "@_\n";
}

unless (-e $db_slags) {
    system("mkdir", "-p", glob("~/.cache/system-config/for-code-reading" . $dir));

    system("sqlite3", $db_slags, 
	   "create table methods(name, class, prototype, path_key INTEGER);"
	   . "create table fields(name, class, prototype, path_key INTEGER);"
	   . "create table classes(name, prototype, path_key INTEGER);");

    system("sqlite3", $db_paths, "create table paths(path_key INTEGER PRIMARY KEY AUTOINCREMENT, path, timestamp);");
}

open(my $find_pipe, "-|", "find . -type f -name '*.jar'") or
    die "Can not open find pipe";
open(my $sqlite3_pipe, ">", "$db_slags.sql") or
    die "Can not open sqlite3 pipe";

my $id_re = qr(\b[a-zA-Z_][a-zA-Z0-9_]*\b);
my $qualified_re = qr($id_re(?:(?:\.|\$)$id_re)*\b);

while (<$find_pipe>) {
    chomp;
    my $jar = $_;
    $jar =~ s!^\./!!;

    my $jar_time = (stat $jar)[9];

    chomp(my $sql_res = qx(sqlite3 $db_paths 'select path_key, timestamp from paths where path = \"$jar\";'));
    
    my ($sql_id, $sql_time) = split('\|', $sql_res);
    if (not $sql_time or $jar_time > $sql_time ) {
	debug "need update $jar";
	if ($sql_time) {
	    for ("methods", "classes", "fields") {
		debug "delete from $_ where path_key = $sql_id;\n";
		print $sqlite3_pipe "delete from $_ where path_key = $sql_id;\n";
	    }
	}
    } else {
	next;
    }

    if (not $sql_res) {
	system("sqlite3", $db_paths, "insert into paths(path, timestamp) values(\"$jar\", 0);");
	chomp(my $sql_res = qx(sqlite3 $db_paths 'select path_key, timestamp from paths where path = \"$jar\";'));
	debug "sqlite3 $db_paths 'select path_key, timestamp from paths where path = \"$jar\";'\n => $sql_res\n";
	($sql_id, $sql_time) = split('\|', $sql_res);
    }

    my $td = "/tmp/slags.$$";
    system("mkdir -p $td");
    chdir $td or die "can not chdir to $td";
    system("unzip $dir/$jar >/dev/null 2>&1");
    open(my $javap_pipe, "-|", "find . -name '*.class' | perl -npe 's/\\.class\$//' | xargs -d \\\\n javap") or 
	die "Can not open find pipe";

    my $class_name;
    while (<$javap_pipe>) {
	chomp;
	next if /^Compiled from.*/;

	if (m/(?:class|interface)\s+($qualified_re)/) {
	    $class_name = $1;
	    $class_name =~ s/\$/./g;
	    last if $class_name =~ m/\.\d+$/; # anonymous classes
	    debug "insert into classes values(\"$class_name\", \"$_\", $sql_id);\n";
	    print $sqlite3_pipe "insert into classes values(\"$class_name\", \"$_\", $sql_id);\n";
	} elsif (m/($qualified_re)\s*\(/) {
	    my $method_name = "$1";
	    debug "insert into methods values(\"$method_name\", \"$class_name\", \"$_\", $sql_id);\n";
	    print $sqlite3_pipe "insert into methods values(\"$method_name\", \"$class_name\", \"$_\", $sql_id);\n";
	} elsif (m/($qualified_re)\s*;/) {
	    my $field_name = "$1";
	    debug "insert into fields values(\"$field_name\", \"$class_name\", \"$_\", $sql_id);\n";
	    print sqlite3_pipe "insert into fields values(\"$field_name\", \"$class_name\", \"$_\", $sql_id);\n";
	} else {
	    debug "slags.pl: unknown line $_";
	}
    }
    close $javap_pipe;
    debug "insert or replace into paths(path_key, path, timestamp) values($sql_id, \"$jar\", $jar_time);";
    system("sqlite3", $db_paths, "insert or replace into paths(path_key, path, timestamp) values($sql_id, \"$jar\", $jar_time);");
    chdir "/";
    system("rm -rf $td");
}
close $sqlite3_pipe;
close $find_pipe;
	
