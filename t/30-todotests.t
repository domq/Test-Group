#!/usr/bin/perl -w
# -*- coding: utf-8; -*-

=head1 NAME

30-todotests.t - Testing correct handling of sub-TODO tests

=cut

use Test::More tests => 13; # Sorry, no_plan not portable for Perl 5.6.1!
use lib "t/lib";
use testlib;

use strict;
use warnings;

ok(my $perl = perl_cmd);

is  $perl->run(stdin => <<'TODO_FAILING') >> 8, 0, "TODO group, failing";
use Test::More tests=>1;
use Test::Group;

test "TODO sub-test (normal i.e. failing)" => sub {
    my $beenthere;
    TODO: {
        local $TODO = "Not quite there yet";
        fail;
        $beenthere++;
    }
    TODO: {
        local $TODO = "Need more budget";
        fail;
        $beenthere++;
    }
    is($beenthere, 2);
};

TODO_FAILING
like $perl->stdout(), qr/not ok 1.*# TODO /,
    "would nonetheless be treated as a success by Test::Harness";
like $perl->stdout(), qr/Not quite there yet.*Need more budget/,
    "all TODO reasons concatenated";

my $script = <<'TODO_SUCCESS';
use Test::More tests=>1;
use Test::Group;

test "TODO sub-test (unexpected success)" => sub {
    pass;
    TODO: {
        local $TODO = "Aha, unexpected TODO success!";
        pass;
    }
};
TODO_SUCCESS

is  $perl->run(stdin => $script) >> 8, 0, "TODO group, unexpected success";
like($perl->stdout(), qr/ok 1.*# TODO /,
     "On the contrary, Test::Harness would report this");

########

$script = <<'TODO_MIXED';
use Test::More tests=>2;
use Test::Group;

test "Mixed TODO tests (overall success)" => sub {
    TODO: {
        local $TODO = "this needs work";
        fail;
    }
    TADA: {
        local $TODO = "this needs less work";
        pass;
    }
};

test "Mixed tests (failure in the supposedly done area)" => sub {
    fail;
    TODO: {
        local $TODO = "this needs work";
        fail;
    }
};

TODO_MIXED

isnt $perl->run(stdin => $script) >> 8, 0, "mixed TODO, shall *fail*"
    or warn $perl->stderr;
like($perl->stdout(), qr/ok 1.*# TODO /, "mixed success modes in sub-TODO");
like($perl->stdout(), qr/not ok 2/, "plain old failure");
unlike($perl->stdout(), qr/not ok 2.*# TODO /, "*not* saved by the bell");

########

$script = <<'TODO_NESTED';
use Test::More tests=>2;
use Test::Group;

test "TODO outer test (nested failure)" => sub {
    test "TODO inner test (nested failure)" => sub {
       local $TODO = "some excuse";
       fail;
    }
};

test "TODO outer test (nested success)" => sub {
    test "TODO inner test (nested success)" => sub {
       local $TODO = "unexpected success";
       fail;
    }
};
TODO_NESTED

is  $perl->run(stdin => $script) >> 8, 0, "nested TODO groups"
    or warn $perl->stderr;
like($perl->stdout(), qr/not ok 1 - TODO/, "nested TODO");
like($perl->stdout(), qr/ok 2 - TODO/, "nested unexpected success");

