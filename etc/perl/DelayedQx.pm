use strict;
package DelayedQx;

sub new {
    my $invocant = shift;

    open(my $pipe, "-|", @_) or die "Can not open @_";
    my $output;

    my $delayedQx = sub {
        if ($pipe) {
            $output = join("", <$pipe>);
            close($pipe);
            $pipe = "";
        }
        return $output;
    };
    return bless $delayedQx;
}

*value = sub {
    my $self = shift;
    $self->();
};

1;
