#!/usr/bin/env perl
package jkd;

use Config::GitLike;
use strict;
use v5.10.1; # for say and switch
use autodie qw(:all);
use IPC::System::Simple qw(run runx capture capturex $EXITVAL EXIT_ANY);
use Encode;
use utf8;

use JSON;

my $json = JSON->new->utf8->canonical->pretty;

use String::ShellQuote;

use strict;
use warnings;

use Carp;
use Exporter;
BEGIN { @jkd::ISA = 'Exporter' }

@jkd::EXPORT = qw(

                     name2id name2key key2name name_exists select_args
                     no_spaces_equal no_spaces_convert
                     no_spaces_hash_convert no_spaces_hashget
                     update_names_with_fields getNormalizedName
                     getRealNameFromApi getRealHashKey reWriteHashKeysWithApi
                     get_config_val jkd_jira_to_hash jkd_value_from_json_obj
             );

use feature 'signatures';
no warnings "experimental::signatures";

sub no_spaces_equal($a, $b) {
    my @pair = ($a, $b);
    map {s, | ,,g} @pair;
    return $pair[0] eq $pair[1];
}

sub no_spaces_convert($a) {
    $a =~ s, | ,,g;
    return $a;
}

sub no_spaces_hash_convert($hash) {
    map {
        (my $key = $_) =~ s, | ,,g;
        $hash->{$key} = $hash->{$_};
        $hash->{no_spaces_hash_keys}{$key} = $_;
    } keys %$hash;
}

sub no_spaces_hashget($hash, $key) {
    if ($hash->{$key}) {
        return $hash->{$key};
    }

    if (not defined $hash->{no_spaces_hash_convert_done}) {
        no_spaces_hash_convert $hash;
        $hash->{no_spaces_hash_convert_done} = 'true';
    }

    $hash->{$key} = $hash->{no_spaces_convert $key};
    return $hash->{$key};
}

sub getRealHashKey($hash, $key) {
    if ($hash->{$key}) {
        return $key;
    }

    if (not defined $hash->{no_spaces_hash_convert_done}) {
        no_spaces_hash_convert $hash;
        $hash->{no_spaces_hash_convert_done} = 'true';
    }

    return $hash->{no_spaces_hash_keys}{no_spaces_convert $key};
}

sub getRealNameFromApi($name, $api) {
    my $jsonResult =
        $json->decode(scalar capturex("jkd", "rest", $api));
    my %hash = map {
        ($_->{name} => 1)
    } @$jsonResult;

    return getRealHashKey(\%hash, $name);
}

sub get_config_val($config_name) {
    my ($config_file) = $ENV{scm_secrets_conf};

    if (-e $config_file) {
        my $secret_conf = Config::GitLike->load_file($config_file);
        return $secret_conf->{"${config_name}"};
    }

    die "can't get config";
}

sub reWriteHashKeysWithApi($hash, $api) {
    my $jsonResult =
        $json->decode(scalar capturex("jkd", "rest", $api));
    my %apiHash = map {
        ($_->{name} => 1)
    } @$jsonResult;


    for (keys %$hash) {
        my $realName = getRealHashKey(\%apiHash, $_);
        if ($realName ne $_) {
            $hash->{$realName} = $hash->{$_};
            delete $hash->{$_};
        }
    }
}

sub name2id($jkd_cmds, $name) {
    my $jsonArray =
        $json->decode(scalar capturex("cached-run", "-e", "scm_jira_url", "jkd", @$jkd_cmds));

    map {
        return $_->{id} if $_->{name} eq $name || ($_->{key} && $_->{key} eq $name);
    } @$jsonArray;
    return undef;
}

sub name2key($jkd_cmds, $name) {
    my $jsonArray =
        $json->decode(scalar capturex("cached-run", "-e", "scm_jira_url", "jkd", @$jkd_cmds));

    map {
        return $_->{key} if $_->{name} eq $name || ($_->{key} && $_->{key} eq $name);
    } @$jsonArray;
    return undef;
}

sub key2name($jkd_cmds, $name) {
    my $jsonArray =
        $json->decode(scalar capturex("cached-run", "-e", "scm_jira_url", "jkd", @$jkd_cmds));

    map {
        return $_->{name} if $_->{key} eq $name;
    } @$jsonArray;
    return undef;
}

sub name_exists($jkd_cmds, $name) {
    my $jsonArray =
        $json->decode(scalar capturex("cached-run", "-e", "scm_jira_url", "jkd", @$jkd_cmds));
    map {
        return 1 if $_->{name} eq $name || ($_->{key} && $_->{key} eq $name);
    } @$jsonArray;
    return undef;
}

sub select_args(@) {
    ## start code-generator "^\\s *#\\s *"
    # generate-getopt -s perl -l -P p:prompt O:order-name i:init-input
    ## end code-generator
    ## start generated code
    use Getopt::Long;

    Getopt::Long::Configure("posix_default");

    local @ARGV = @_;

    my $init_input = "";
    my $order_name = "";
    my $prompt = "";

    my $handler_help = sub {
        print ;
        print "\n\n选项和参数：\n";
        printf "%6s", '-i, ';
        printf "%-24s", '--init-input=INIT-INPUT';
        if (length('--init-input=INIT-INPUT') > 24 and length() > 0) {
            print "\n";
            printf "%30s", "";
        }
        printf "%s", ;
        print "\n";
        printf "%6s", '-O, ';
        printf "%-24s", '--order-name=ORDER-NAME';
        if (length('--order-name=ORDER-NAME') > 24 and length() > 0) {
            print "\n";
            printf "%30s", "";
        }
        printf "%s", ;
        print "\n";
        printf "%6s", '-p, ';
        printf "%-24s", '--prompt=PROMPT';
        if (length('--prompt=PROMPT') > 24 and length() > 0) {
            print "\n";
            printf "%30s", "";
        }
        printf "%s", ;
        print "\n";

        exit(0);
    };

    GetOptions (
        'init-input|i=s' => \$init_input,
        'order-name|O=s' => \$order_name,
        'prompt|p=s' => \$prompt,
        'help|h!' => \&$handler_help,
        );


    ## end generated code

    my @command = (
        "select-args", "-p", "$prompt", "-i", "$init_input",
        "-O", "$order_name",
        @ARGV
        );

    my $command = join(" ", shell_quote(@command));
    my $res = decode_utf8 (qx($command));

    return $res;
}

sub getNormalizedName($name) {
    $name =~ s, | ,,g;
    return $name;
}

sub update_names_with_fields($named_obj, $fields_map) {
    my %name_field_map;

    # say STDERR sprintf("update_names_with_fields(%s, %s)",
    #                    decode_utf8($json->encode($named_obj)),
    #                    decode_utf8($json->encode($fields_map))
    #                ) if $ENV{jkd_verbose};

    for (keys %$fields_map) {
        my $name = $fields_map->{$_};

        if (ref $name) {
            $name = $name->{name};
        }

        if ($name) {
            $name = getNormalizedName $name;

            $name_field_map{$name} = $_;
            say STDERR "Created a map: $name: $name_field_map{$name}" if $ENV{jkd_verbose};
        }

    }


    for (keys %$named_obj) {
        my $name = getNormalizedName $_;
        if ($name_field_map{$name} and $name_field_map{$name} ne $_) {
            say STDERR "update $name with $name_field_map{$name}";
            $named_obj->{$name_field_map{$name}} = $named_obj->{$_};
            delete $named_obj->{$_};
        }
    }
}

sub jkd_value_from_json_obj($json_obj) {
    if (not ref $json_obj) {
        return $json_obj;
    } elsif (ref ($json_obj) eq "HASH") {
        return $json_obj->{name} ||
            $json_obj->{key} ||
            $json_obj->{value} ||
            die "Can't get field value from " . decode_utf8($json->encode($json_obj));
    } elsif (ref ($json_obj) eq "ARRAY") {
        my @val_array = map {
            jkd_value_from_json_obj($_)
        } @$json_obj;
        return \@val_array;
    } else {
        die "Can't get field value from " . decode_utf8($json->encode($json_obj));
    }
}

sub jkd_jira_to_hash($jira_key, $jira_fields) {
    my %ret_params;
    my %jira_field_to_var = map { ($jira_fields->{$_}, $_)} keys %$jira_fields;

    my $jira_json =
        $json->decode(scalar capturex(
            "debug-run", "jkd", "print-issue", "-i", "$jira_key",
            map{ ("-f", $jira_fields->{$_}) } keys %$jira_fields
        ));

    for my $var_name (keys %$jira_fields) {
        my $field_name = $jira_fields->{$var_name};

        $ret_params{$var_name} = jkd_value_from_json_obj $jira_json->{$field_name};
    }

    my $args = {};
    %$args = %ret_params;
    $args->{jira_key} = $jira_key;

    return $args;
}

1;
__END__

=head1 NAME

jkd - Perl extension for blah blah blah

=head1 SYNOPSIS

   use jkd;
   blah blah blah

=head1 DESCRIPTION

Stub documentation for jkd,

Blah blah blah.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Bao Haojun, E<lt>baohaojun@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Bao Haojun

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
