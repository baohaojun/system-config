#!/usr/bin/env perl
package Jkdo;

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

use feature 'signatures';
no warnings "experimental::signatures";

sub init($self) {
    $self->{fields} =
        $json->decode(scalar capturex("cached-run", "-e", "scm_jira_url", "jkd", "print-issue", "-i", $self->{key}));
}

sub get_field($self, $field_name) {
    my $field = $self->{fields}{$field_name};
    if (not ref $field) {
        return $field;
    } else {
        return $field->{value} if $field->{value};
        return $field->{name} if $field->{name};
        return $field->{key} if $field->{key};
        die "Don't know how to return field: " . decode_utf8($json->encode($field));
    }
}

sub update_fields($self, $fields) {
    # each field in fields should be: name => value, no matter whether
    # the field is a simple value or a data struct.

    my %fields_to_update;

    for my $field_name (keys %{$fields}) {
        my $field = $self->{fields}{$field_name};
        if (not ref $field) {
            $fields_to_update{$field_name} = $fields->{$field_name};
        } else {
            my $sub_field_name;
            if ($field->{value}) {
                $sub_field_name = "value";
            } elsif ($field->{name}) {
                $sub_field_name = "name";
            } elsif ($field->{key}) {
                $sub_field_name = "key";
            } else {
                die "Don't know how to update field: " . decode_utf8($json->encode($field));
            }
            $fields_to_update{$field_name} = {
                $sub_field_name => $fields->{$field_name}
            };
        }
    }

    runx("debug-run", "jkd", "e", "-i", $self->{key}, "--fields-json", decode_utf8($json->encode(\%fields_to_update)));
    $self->init; # update the fields after update;
}

sub new ($class, $args) {
    my $self = bless {
        key => $args->{key},
    }, $class;
    die "Must specify the jira key" unless $self->{key};

    $self->init;
}

1;
