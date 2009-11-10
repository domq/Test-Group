#!perl
# -*- coding: utf-8; -*-

=head1 NAME

testlib.pm - Utility functions for testing L<Test::More>.

=cut

use strict;
use warnings;
use Test::Cmd;
use Config;

=head2 I<perl_cmd()>

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

=head2 I<testscript_ok($script_source, $plan, $name)>

Predicate for checking that a test script acts as expected.  Runs
the script and fails if anything unexpected happens.

The expected behavior of the script is defined by calling want_test()
from within the script, just before running each test.

The following functions are available from within the test script
only.

=over

=item I<want_test($pass, $name, @diag)>

Declares that the next test will pass or fail according to C<$pass>
(1 for pass, 0 for fail, 2 for skip), will have name C<$name> (or will
be nameless if C<$name> is undef) and will produce the diagnostic
output lines listed in C<@diag>.

The expected diagnostic lines are treated as strings for an exact
match, unless they have C<-re> prepended, in which case they are
treated as regular expressions.

=item I<fail_diag($test_name [,$from_test_builder], [$line])>

Returns the diagnostic line pattern(s) to match output from a failed
test. C<$test_name> is the name of the test, or undef for a nameless
test.  C<$line> should be defined only if a file and line diagnostic
is expected, and should give the expected line number.

C<$from_test_builder> should be true if L<Test::Builder> rather than
L<Test::Group> is expected to generate the diagnostic.  The expected
text will be adjusted to match the L<Test::Builder> version.

=back

=cut

sub testscript_ok {
    my ($src, $plan, $name) = @_;
    $name ||= 'testscript_ok';

    local $Test::Builder::Level = $Test::Builder::Level + 1;

    my $script = <<'EOSCRIPT';
use strict;
use Test::More tests => __PLAN__;
use Test::Builder;

sub fail_diag {
    my ($test_name, $from_test_builder, $line) = @_;

    my $file = (caller)[1];

    if ($from_test_builder and $Test::Builder::VERSION <= 0.30) {
        my $diag = "    Failed test";
        if (defined $line) {
            $diag .= " ($file at line $line)";
        }
        return $diag;
    } 

    my @diag;
    if (defined $test_name) {
        push @diag, "  Failed test '$test_name'";
    } else {
        push @diag, "  Failed test";
    }
    if (defined $line) {
        my $qm = quotemeta $file;
        push @diag, "-re"."^  (at $qm|in $qm at) line $line\\.?\\s*\$";
    }

    return @diag;
}

sub want_test {
    my ($pass, $name, @diag) = @_;
    my @args = map {defined $_ ? unpack('H*', $_) : 'undef'} @_;
    diag 'XXwant_test_markerXX want_test:' . join ',', @args;
}

EOSCRIPT
    $script =~ s/__PLAN__/$plan/;
    $script .= $src . "\ndiag('XXtestscript_under_test_endXX');\n";

    my $ok = 1;
    my $expect_failed_tests = 0;

    my $perl = perl_cmd or fail("$name perl_cmd failed"), return;
    my $status = $perl->run(stdin => $script);
    my $stdout = $perl->stdout();
    my $stderr = "\n" . $perl->stderr();
    $stderr =~ s/\n[\# ]+XXtestscript_under_test_endXX.*//s;

    my @errbits = split /\n[ \#]+XXwant_test_markerXX/, $stderr;
    my $preamble = shift @errbits;
    if (length $preamble) {
        $ok = 0;
        diag "stderr: $preamble";
    }
    my $rantests = @errbits;
    unless ($rantests == $plan) {
        $ok = 0;
        diag "planned $plan tests, script ran $rantests";
    }

    my $want_out = "1..$plan\n";
    foreach my $i (0 .. $#errbits) {
        my $e = $errbits[$i];
        unless ($e =~ s/^ want_test:([,\w]+)\s*//) {
            $ok = 0;
            diag "missing header in section [$e]";
            next;
        }
        my ($pass, $name, @diag) =
                  map { $_ eq 'undef' ? undef : pack 'H*', $_} split /,/, $1;
        my @orig_diag = @diag;

        my $out = ($pass == 0 ? 'not ' : '') . 'ok ' . ($i+1);
        if ($pass == 2) {
            $out .= " # skip";
            defined $name and $out .= " $name";
        } else {
            defined $name and $out .= " - $name";
        }
        $want_out .= "$out\n";

        $pass or ++$expect_failed_tests;

        $e =~ s/\n$//;
        my @lines = split /\n/, $e, -1;
        my @mismatch;
        foreach my $line (@lines) {
            if (length $line and $line !~ s/^\# //) {
                push @mismatch, "  non-diag stderr [$line]";
            }
            next if @mismatch;
            my $want = shift @diag;
            if (!defined $want) {
                push @mismatch, "  unmatched line '$line'";
            } elsif ($want =~ s/^-re//) {
                unless ($line =~ /$want/) {
                    push @mismatch,
                        "  line '$line'",
                        "  doesn't match '$want'";
                }
            } elsif ($line ne $want) {
                push @mismatch,
                    "  line '$line'",
                    "  isnt '$want'";
            }
        }
        if (@mismatch) {
            $ok = 0;
            diag "STDERR MISMATCH FOR $name...";
            diag " got stderr:";
            foreach my $l (@lines) {
                diag "  [$l]";
            }
            diag " want stderr:";
            foreach my $w (@orig_diag) {
                diag "  [$w]";
            }
            diag " mismatch details:";
            foreach my $m (@mismatch) {
                diag $m;
            }
        }
    }

    if ($stdout ne $want_out) {
       $ok = 0;
       diag "want stdout: $want_out";
       diag "got stdout: $stdout";
    }

    if ($expect_failed_tests and not $status) {
        $ok = 0;
        diag "test script failed to fail";
    } elsif ($status and not $expect_failed_tests) {
        $ok = 0;
        diag "test script unexpectedly failed";
    }

    ok $ok, $name;
}

=head2 I<test_test>

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
    my ($callerpack) = caller(0);
    my $subtest = Test::Group::_Runner->new($name, $callerpack, $sub);
    $subtest->mute(1) unless $ENV{DEBUG};
    $subtest->run;
    return $subtest;
}

=head2 I<get_pod_snippet($name)>

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

=head2 Mixins to Test::Group::_Runner

I<testlib.pm> defines additional methods for the
L<Test::Group/Test::Group::_Runner internal class>, which are trivial
wrappers around the API that that class already provides and only
exist to make the test suite more easy to read and write.

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


=head2 I<Test::Group::_Runner::is_failed()>

Returns true iff this test group failed from the point of view of
L<Test::Harness>.  This is computed from the negation of the C<xor> of
L</prints_OK> and L</prints_TODO_string>, just as I<Test::Harness>
would.

=cut

sub Test::Group::_Runner::is_failed {
    my ($self) = @_;
    return ! ($self->prints_TODO_string xor $self->prints_OK);
}

1;
