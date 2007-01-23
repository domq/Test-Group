use strict;
use warnings;

=head1 NAME

20-internal.t - Testing Test::Group using itself

=cut

use Test::More tests => 6; # Sorry, no_plan does not work with Perl
                           # 5.6.1's Test::Harness

use Test::Group;
use lib "t/lib";
use testlib;

ok(1, "non-wrapped tests still work");

test "success" => sub {
    ok(1);
};


begin_reversed_tests();
test "failure" => sub {
    ok(1);
    ok(1);
    ok(0);
    ok(1);
};

test "exception" => sub {
    die;
};

test "empty (shall fail)" => sub { };

end_reversed_tests();

test "nested tests" => sub {
    pass;
    test "true" => sub { pass };

    begin_reversed_tests();
    test "false" => sub { is("foo", "bar") };
    test "dies" => sub { die };
    end_reversed_tests();
};

1;
