#!perl
# -*- coding: utf-8; -*-

=head1 NAME

testlib.pm - Utility functions for testing L<Test::More>.

=over

=cut

use strict;
use warnings;

=item I<begin_reversed_tests>

A support routine for testing the failure of tests. When
I<begin_reversed_tests> is called, failure means success from now on
and vice versa.  Kids, don't try this at home: this is only for
testing Test::Group itself! In all other situations you should
rewrite your tests so that success actually means success.

=cut

sub begin_reversed_tests {
    $Test::Group::__reversed__ = 1;
    # Temporarily diverts the diagnostics /dev/null, so as not
    # to confuse our own test output with failure diagnostics that
    # actually indicate success (if you know what I mean):
    Test::Builder->new->no_diag(1);
}

=item I<end_reversed_tests>

Cancels the effect of L</begin_reversed_tests>.

=cut

sub end_reversed_tests {
    Test::Builder->new->no_diag(0);
    $Test::Group::__reversed__ = undef;
}

=back

=cut

1;
