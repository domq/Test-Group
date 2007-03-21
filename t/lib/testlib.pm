#!perl
# -*- coding: utf-8; -*-

=head1 NAME

testlib.pm - Utility functions for testing L<Test::More>.

=over

=cut

use strict;
use warnings;
use Test::Cmd;
use Config;

=item I<perl_cmd()>

Returns a newly-constructed L<Test::Cmd> object using the Perl
interpreter currently running as the command, with an @INC
appropriately copied from the one we are currently running under.
This is for testing live test snippets.

=cut

sub perl_cmd {
    return Test::Cmd->new
        (prog => join(' ', $Config{perlpath},
                      (map { ("-I", $_) } @INC), '-'),
         workdir => '');
}

=item I<test_test>

Works like L<Test::Group/test>, except that the result of the test is
not sent upwards to L<Test::Builder> but instead returned as a
reference to an instance of <Test::Group/Test::Group::_Runner internal
class>. E.g. here is how to check that a test group does B<not> pass:

   my $status = test_test "foo" => sub {
      # ...
   };
   ok($status->is_failed);

Also, test diagnostics are suppressed in I<test_test> unless
$ENV{DEBUG} is set.

=cut

sub test_test ($&) {
    my ($name, $sub) = @_;

    # This is just a dummy adapter to protect the test suite
    # against future changes of Test::Group's internals.
    my $subtest = Test::Group::_Runner->new($name, $sub);
    $subtest->mute(1) unless $ENV{DEBUG};
    $subtest->run;
    return $subtest;
}

=item I<get_pod_snippet($name)>

Parses the source code of L<Test::Group> and extracts the snippets of
code that are in the POD therein, identified by C<=for tests> markers.

=cut

use IO::File;
sub get_pod_snippet {
    my ($name) = @_;
    my $source = join("", IO::File->new($INC{"Test/Group.pm"})->getlines);

    my ($snip) = ($source =~
                  m/=for tests "$name" begin(.*)=for tests "$name" end/s);
    die "Did not find snippet $name" if (! defined $snip);
    return $snip;
}

=back

=head2 Mixins to Test::Group::_Runner

I<testlib.pm> defines additional methods for the
L<Test::Group/Test::Group::_Runner internal class>, which are trivial
wrappers around the API that that class already provides and only
exist to make the test suite more easy to read and write.

=over

=head3 prints_OK

=head3 prints_TODO_string

Return respectively the first and second items from the list returned
by L<Test::Group/as_Test_Builder_params>, except if
L<Test::Group/is_skipped> is true in which case they return
respectively 1 and undef.

=cut

sub Test::Group::_Runner::prints_OK {
    my $self = shift;
    return 1 if $self->is_skipped;
    my ($retval, undef) = $self->as_Test_Builder_params;
    return $retval;
}

sub Test::Group::_Runner::prints_TODO_string {
    my $self = shift;
    return if $self->is_skipped;
    my (undef, $retval) = $self->as_Test_Builder_params;
    return $retval;
}


=item I<Test::Group::_Runner::is_failed()>

Returns true iff this test group failed from the point of view of
L<Test::Harness>.  This is computed from the negation of the C<xor> of
L</prints_OK> and L</prints_TODO_string>, just as I<Test::Harness>
would.

=cut

sub Test::Group::_Runner::is_failed {
    my ($self) = @_;
    return ! ($self->prints_TODO_string xor $self->prints_OK);
}

=back

=cut

1;
