#!perl
# -*- coding: utf-8; -*-
use Test::More tests => 1; # Don't convert to no_plan: besides it not
# being portable to Perl 5.6.1's Test::Harness, we also want to ensure
# that the multiple tests in the synopsis get counted as one (since
# they all run inside the same group).
use Test::Group;
use IO::File;
use File::Spec;
use strict;
use warnings;
use lib "t/lib";
use testlib;

=head1 NAME

30-synopsis.t - Extracts the synopsis code from L<Test::Group>'s POD
documentation and runs it.

=cut

my $source = join("", IO::File->new($INC{"Test/Group.pm"})->getlines);

my %snips = map {
    my $name = $_;
    my ($snip) = ($source =~
m/=for tests "synopsis-$name" begin(.*)=for tests "synopsis-$name" end/s);

    ( $name => $snip )
} (qw(success fail misc));

test "synopsis" => sub {

    # We already have a plan:
    $snips{success} =~ s/(no_plan)/; # $1/;

    # "/tmp/log" is not kosher in win32:
    $snips{misc} =~ s|/tmp/log|File::Spec->devnull|ge;

    ok(eval <<"CODE"); die $@ if $@;
sub I_can_connect { 1 }
sub I_can_make_a_request { 1 }

$snips{success}

begin_reversed_tests();
$snips{fail}
end_reversed_tests();

sub Network::available { 0 } # Curse France Telecom, arrrr!
$snips{misc}

1;
CODE
};
