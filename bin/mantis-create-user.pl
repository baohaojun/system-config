#!/usr/bin/env perl
use strict;
srand;
sub get_coockie()
{
    my $c = '';
    for (my $i=0; $i < 8; $i++) {
        $c .= sprintf "%08x", int(rand(0xffffffff));
    }
    return $c;
}

#mysql -h localhost -p5yrx8JQvY2cv -u mantis mantis -e "insert into mantis_user_table values(6, 'r005', 'r005', 'r005@localhost', '68e109f0f40ca72a15e05cc22786f8e6', '2010-03-22 18:24:59', '', 1, 0, 25, 0, 0, 0, '');"
open my $passwd, "/home/bhj/passwd";

my $id = 1;
while (<$passwd>) {
    chomp;
    next unless m/=/;
    $id++;

    m/(.*?)\s*=\s*(.*?)\s*$/;
    
    my ($user, $passwd) = ($1, $2);

    system("mysql -h localhost -p5yrx8JQvY2cv -u mantis mantis -e \"insert into mantis_user_table values($id, '$user', '$user', '$user\@localhost', '" . 
           `echo -n $passwd|md5sum |awk '{print \$1}'|tr -d '\n'` . 
           "', '" . 
           `date +'%Y-%m-%d %H:%M:%S'|tr -d '\n'` . 
           "', '" . 
           `date +'%Y-%m-%d %H:%M:%S'|tr -d '\n'` . 
           "', 1, 0, 55, 0, 0, 0, '" . 
           get_coockie() . 
           "');\"");
}

