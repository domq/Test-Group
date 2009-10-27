use strict;
use warnings;

=head1 NAME

55-predicates.t - defining new test predicates with Test::Group

=cut

use Test::More;
use Test::Group;
use lib "t/lib";
use testlib;

my @preds = qw(foobar_ok foobar_ok_b foobar_ok_bg foobar_ok_bgb);
plan tests => 3 + 4*@preds;

my $preamble = <<'EOSCRIPT';
use strict;
use warnings;

use Test::More tests => 2*4;
use Test::Builder;

EOSCRIPT

# foobar_ok: a Test::Group predicate
$preamble .= get_pod_snippet("foobar_ok");

$preamble .= <<'EOSCRIPT';

# foobar_ok_b: a predicate on top of foobar_ok, using the standard
# Test::Builder predicate-within-predicate mechanism.
sub foobar_ok_b {
    my ($thing, $name) = @_;

    local $Test::Builder::Level = $Test::Builder::Level + 1;
    foobar_ok("woot".$thing, $name);
}

# foobar_ok_bg: a Test::Group predicate on top of foobar_ok_b
sub foobar_ok_bg {
    my ($text, $name) = @_;
    $name ||= "foobar_ok_bg";
    local $Test::Group::InPredicate = 1;
    local $Test::Group::Level = $Test::Group::Level + 1;
    test $name => sub {
        ok "foo", "foo is true";
        foobar_ok_b($text, $name);
        ok "bar", "bar is true";
    };
}

# foobar_ok_bgb: another layer of predicate
sub foobar_ok_bgb {
    my ($thing, $name) = @_;

    local $Test::Builder::Level = $Test::Builder::Level + 1;
    foobar_ok_bg("woot".$thing, $name);
}

# Try each predicate passing
foobar_ok(    "foobar", "test 1");
foobar_ok_b(  "foobar", "test 2");
foobar_ok_bg( "foobar", "test 3");
foobar_ok_bgb("foobar", "test 4");

EOSCRIPT

my ($ret, $out, $errbits, $linenums) = run_testscript_segments(
    $preamble,
    '
    foobar_ok(    "foobaz", "xfoobar_ok");     linename("foobar_ok");
    ','
    foobar_ok_b(  "foobaz", "xfoobar_ok_b");   linename("foobar_ok_b");
    ','
    foobar_ok_bg( "foobaz", "xfoobar_ok_bg");  linename("foobar_ok_bg");
    ','
    foobar_ok_bgb("foobaz", "xfoobar_ok_bgb"); linename("foobar_ok_bgb");
    ', ''
);

ok $ret >> 8, "test script failed";

is $out, <<EOOUT, "test script stdout";
1..8
ok 1 - test 1
ok 2 - test 2
ok 3 - test 3
ok 4 - test 4
not ok 5 - xfoobar_ok
not ok 6 - xfoobar_ok_b
not ok 7 - xfoobar_ok_bg
not ok 8 - xfoobar_ok_bgb
EOOUT

is shift @$errbits, '', "preamble no stderr";

foreach my $i (0 .. $#preds) {
    my $line = $linenums->{$preds[$i]};
    ok $line, "got $preds[$i] linenum";
    like $errbits->[$i],
        qr/Failed test 'x$preds[$i]'/,
        "$preds[$i] fail message";
    like $errbits->[$i],
        qr/\bat \S+ line $line\b/,
        "$preds[$i] fail line number";
    my @linewords = $errbits->[$i] =~ /(line)/gi;
    is scalar @linewords, 1, "$preds[$i] no stray line numbers";
}

