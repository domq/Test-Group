#!/usr/bin/perl -w
# -*- coding: utf-8; -*-

=head1 NAME

60-verbose2.t - Testing Test::Group with C<verbose> set to 2 and above.

=cut

use Test::More;
use lib "t/lib";
use testlib;

use strict;
use warnings;

my @levels = (2, 3, 4, 5, 10, 1000);

plan tests => 1 + @levels*2*3;

ok(my $perl = perl_cmd);

foreach my $level (@levels) {
    foreach my $pass (0, 1) {
        runtest($level);
    }
}

sub runtest {
    my ($level, $pass) = @_;

    my $a = $pass ? 'a' : 'A';
    my $script = <<EOSCRIPT;
use strict;
use warnings;

use Test::More tests => 4;
use Test::Group;

Test::Group->verbose($level);

ok 1, "pre";

test foo => sub {
    ok 1, "foo one";
    ok 1, "foo two";
};

test bar => sub {
    ok 1, "bar one";
    ok 1, "bar two";
    ok_foobarbaz("foobarbaz", "woo woo");
};

ok 1, "post";

sub ok_foobarbaz {
    my (\$thing, \$name) = \@_;
    \$name ||= 'ok_foobarbaz';

    test \$name => sub {
        like \$thing, qr/foo/, "\$name like foo";
        ok_bar(\$thing, "\$name like bar");
        like \$thing, qr/baz/, "\$name like baz";
    };
}

sub ok_bar {
    my (\$thing, \$name) = \@_;
    \$name ||= 'ok_bar';

    test \$name => sub {
        like \$thing, qr/b/, "\$name like b";
        like \$thing, qr/$a/, "\$name like $a";
        like \$thing, qr/r/, "\$name like r";
    };
}
EOSCRIPT

    if ($pass) {
        is $perl->run(stdin => $script) >> 8, 0, "passing verbose $level";
    } else {
        ok $perl->run(stdin => $script) >> 8,    "failing verbose $level";
    }

    my $result = $pass ? 'passing' : 'failing';
    my $not = $pass ? '' : 'not ';

    is scalar($perl->stdout()), <<EOOUT, "$result out at $level";
1..4
ok 1 - pre
ok 2 - foo
${not}ok 3 - bar
ok 4 - post
EOOUT

    my $err = $perl->stderr();
    unless ($pass) {
        $err =~ s/^\s*\n//mg;
        $err =~ s/^\s*\# {2,}.*\n//mg;
    }
    
    my $want_err = <<EOERR;
# Running group of tests - foo
# ok 2.1 foo one
# ok 2.2 foo two
# Running group of tests - bar
# ok 3.1 bar one
# ok 3.2 bar two
# Running group of tests - woo woo
# ok 3.3.1 woo woo like foo
# Running group of tests - woo woo like bar
# ok 3.3.2.1 woo woo like bar like b
# ${not}ok 3.3.2.2 woo woo like bar like A
# ok 3.3.2.3 woo woo like bar like r
# ${not}ok 3.3.2 woo woo like bar
# ok 3.3.3 woo woo like baz
# ${not}ok 3.3 woo woo
EOERR
    $want_err .= "# Looks like you failed 1 test of 4.\n" unless $pass;

    my $too_deep_re = join '\\.', ('\d+') x ($level+1);
    $want_err =~ s/^\# (?:not )?ok $too_deep_re.*\n//mg;
    is $err, $want_err, "$result err at $level";
}

1;
