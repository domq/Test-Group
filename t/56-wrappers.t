use strict;
use warnings;

=head1 NAME

56-wrappers.t - wrapping test(), checking that line numbers in failed
test diagnostics come out right.

=cut

use Test::More;
use Test::Group;
use lib "t/lib";
use testlib;

my @subs = qw(test test2 test3);
plan tests => 3 + @subs*(2*2+1);

my ($ret, $out, $errbits, $linenums) = run_testscript_segments('
use strict;
use warnings;

use Test::More tests => 3;
use Test::Group;

sub test2 ($&) {
    my ($name, $code) = @_;

    local $Test::Group::Level = $Test::Group::Level + 1;
    &test($name, $code);
}

sub test3 ($&) {
    my ($name, $code) = @_;

    local $Test::Group::Level = $Test::Group::Level + 1;
    &test2($name, $code);
}

','

test msg_testouter => sub {
    ok 0, "msg_testinner"; linename("test inner");
}; linename("test outer");

','

test2 msg_test2outer => sub {
    ok 0, "msg_test2inner"; linename("test2 inner");
}; linename("test2 outer");

','

test3 msg_test3outer => sub {
    ok 0, "msg_test3inner"; linename("test3 inner");
}; linename("test3 outer");

', '');

ok $ret >> 8, "test script failed";
is shift @$errbits, '', "preamble no stderr";

is $out, <<EOOUT, "test script stdout";
1..3
not ok 1 - msg_testouter
not ok 2 - msg_test2outer
not ok 3 - msg_test3outer
EOOUT

my $atline_re = '(at \S+|in \S+ at) line ';
foreach my $i (0 .. $#subs) {
    foreach my $io (qw(inner outer)) {
        my $line = $linenums->{"$subs[$i] $io"};
        ok $line, "got $subs[$i] $io linenum";
        like $errbits->[$i],
             qr{Failed test 'msg_$subs[$i]$io'[\s\#]+$atline_re$line\b},
            "$subs[$i] $io fail message";
    }
    my @linewords = $errbits->[$i] =~ /(line)/gi;
    is scalar @linewords, 2, "$subs[$i] no stray line numbers";
}

