#!/usr/bin/perl -w
# -*- coding: utf-8; -*-
#
# (C)-IDEALX

package Test::Group;
use strict;
use warnings;

=head1 NAME

Test::Group - Group together related tests in a test suite

=head1 VERSION

Test::Group version 0.03

=cut

our $VERSION = '0.03';

=head1 SYNOPSIS

=for tests "synopsis-success" begin

    use Test::More no_plan => 1;
    use Test::Group;

    test "hammering the server" => sub {
        ok(I_can_connect);
        for(1..1000) {
           ok(I_can_make_a_request);
        }
    }; # Don't forget the semicolon here!

=for tests "synopsis-success" end

=for tests "synopsis-fail" begin

    test "this test group will fail", sub {
        ok 1, "sub test blah";
        is "foo", "bar"; # Oops!
        ok 1;
        like   "blah blah blah", qr/bla/;
	};

    test "this test will fail but the suite will proceed", sub {
        pass;
        die;
    };

=for tests "synopsis-fail" end

=for tests "synopsis-misc" begin

    # Don't catch exceptions raised in test groups later on
    Test::Group->dont_catch_exceptions;

    # log caught exceptions in /tmp/log
    Test::Group->logfile("/tmp/log");

    # skip the next group of test
	skip_next_test "network not available" if (! Network->available());
    test "bla", sub {
        my $ftp = Net::FTP->new("some.host.name");
        # ...
    };

    begin_skipping_tests "reason";
    # ...
    # all groups of tests here are ignored
    # ...
    end_skipping_tests;

    # from now on, skip all tests whose names do not match /bla/
    test_only qr/bla/;

=for tests "synopsis-misc" end

=head1 DESCRIPTION

Fed up with counting tests to find out what went wrong in your last
test run?  Tired of squinting at your test source to find out where on
earth the faulty test predicate is called, and what it is supposed to
check for?  Then this module is for you!

I<Test::Group> allows for grouping together related tests in a
standard L<Test::Builder> script. This provides a bunch of
maintainability and scalability advantages to large test suites:

=over

=item *

related tests can be grouped and given a name. The intent of the test
author is therefore made explicit with much less effort than would be
needed to name all the individual tests;

=item *

the test output is much shorter and more readable: only failed
subtests show a diagnostic, while test groups with no problems inside
produce a single friendly C<ok> line;

=item *

no more tedious test counting: running an arbitrarily large or
variable number of tests (e.g. in loops) is made easy, without
cluttering the display nor making the test counting painful;

=item *

I<Test::Group> can skip whole groups of tests or even a range of
groups matched by a regex, which helps shortening the debug cycle even
more in test-driven programming.

=back

L<Test::Group> is built atop L<Test::Builder> and plays happily with
L<Test::More> and friends. If you are not already familiar with
L<Test::More>, now would be the time to go take a look.

=head2 Similar modules on CPAN

L<Test::Class> can be used to turn a test suite into a full-fledged
object class of its own, in xUnit style.  It also happens to support a
similar form of test grouping using the C<:Tests> attribute
(introduced in version 0.10).  Switching over to I<Test::Class> will
make a test suite more rugged and provide a number of advantages, but
it will also dilute the "quick-and-dirty" aspect of .t files
somewhat. This may or may not be what you want: for example, the
author of this module enjoys programming most when writing tests,
because the most infamous Perl hacks are par for the course then :-).
Anyway TIMTOWTDI, and I<Test::Group> is a way to reap some of the
benefits of I<Test::Class> (e.g. running only part of the test suite)
without changing one's programming style too much.

=cut

use 5.004;

use Test::Builder;
BEGIN { die "Test::Simple version 0.59 or superior needed, sorry"
            unless Test::Builder->can("create"); }
use IO::File;
use File::Spec;

our $__reversed__;	# Test groups that should fail, will succeed if
                    # set to true, and vice versa. To be used *only*
                    # for self-tests below

my $verbose;
my $skip_counter;
my $skip_reason;
my $test_only_reason;
my $test_only_criteria = sub { 1 };
my $catch_exceptions = 1;
my $logfile;
my $logfd;

=head2 FUNCTIONS

All functions below are intended to be called from the test
script. They are all exported by default.

=cut

use Exporter;
our @ISA    = qw(Exporter);
our @EXPORT = qw(test skip_next_test skip_next_tests
				 begin_skipping_tests end_skipping_tests
				 test_only);

=over

=item I<test($name, $groupsub)>

Executes I<$groupsub>, which must be a reference to a subroutine, in a
controlled environment and groups the results of all
L<Test::Builder>-style tests launched inside into a single call to
L<Test::Builder/ok>, regardless of their number.



outputs a summary of the tests executed
inside.  From the point of view of L<Test::Builder>, a call to
I<test()> will translate to a I<single call> to L<Test::Builder/ok>
regardless of the number of invocations of test assertions that were
made from inside $groupsub; said grouped test succeeds if and only if
all conditions below are met:

=over

=item *

no test assertion in I<$groupsub> failed;

=item *

at least one test assertion in I<$groupsub> passed;

=item *

no exception was thrown.

=back

If any sub-tests failed in I<$groupsub>, diagnostics will be
propagated using L<Test::Builder/diag> as usual. The return value of
I<test> is true if at least one subtest succeeded 

=cut "

sub test($&) {
	my ($name, $code)=@_;

	my $Test = Test::Group::_Hijacker->current();

	$skip_counter and do {
		$skip_counter--;
		$Test->skip($skip_reason);
		undef $skip_reason unless $skip_counter;
		return;
	};

	&$test_only_criteria($name) or do {
		$Test->skip($test_only_reason);
		return;
	};

	$Test->diag("Running group of tests - $name") if ($verbose);

    my ($exn);
    my $subTest = Test::Group::_Hijacker->hijack();
    # WARNING, we are entering a very quirky kind of critical section:
    # there shall be no way of exiting the block below (through normal
    # control flow or exception) without calling $subTest->unhijack()!
    eval { &$code(); 1 } or do {
        if ($catch_exceptions) {
            $exn = (ref($@) || (defined($@) && length($@) > 0)) ? $@ :
                # Factor L<Error> in (TODO: add L<Exception::Class> as
                # well):
                defined($Error::THROWN) ? $Error::THROWN :
                # This can happen when a DESTROY block that runs
                # after the initial exception in turn throws
                # (remedy: use "local $@;" at the beginning of
                # every sub DESTROY):
                "an empty exception";
        } else {
            $subTest->unhijack();
            die $@; # Rethrow
        }
    };
    $subTest->unhijack();
    # Pfew. Critical section is over.

	if (defined $exn) {
		if ($logfd) {
			print $logfd ("Test ``$name'' died with exception".
			  (! defined $exn ? "(undef)" : "\n$exn\n"));
			$name="test ``$name'' died - see log file: ``$logfile''";
		} else {
			# Output diagnostics on one line after the failure message
			my $message = (! defined $exn ? "an undefined exception" :
						   "$exn" ? "$exn" :
						   (UNIVERSAL::can($exn, "stringify") ?
							$exn->stringify() : "an empty exception"));
			chomp $message;
			$name="test ``$name'' died with ``$message''";
		};
	}

	my $ok= (! defined $exn) && $subTest->is_success();
	$ok = !$ok if $__reversed__;
    if (! $ok) {
        $Test->ok(0, $name);
        return 0;
    } elsif (defined(my $todo = $subTest->todo_reasons())) {
        # At least one subtest was a TODO so we are too. If one of the
        # TODO subtests was an unexpected success then so are we.
        no warnings "redefine";
        local *Test::Builder::todo = sub { $todo };
        $Test->ok($subTest->is_todo_unexpected_success(), $name);
        return 1;
    } else {
        # Straight success, no TODO
        $Test->ok(1, $name);
        return 1;
    }
}

=item B<skip_next_tests>

    skip_next_tests 5;
    skip_next_tests 5, "reason";

Skips the 5 following group of tests. Dies if we are currently
skipping tests already.

=item B<skip_next_test>

    skip_next_test;
    skip_next_test "reason";

Equivalent to:

    skip_next_tests 1;
    skip_next_tests 1, "reason";

=item B<begin_skipping_tests>

    begin_skipping_tests
    begin_skipping_tests "reason";

Skips all subsequent groups of tests until blocked by
L</end_skipping_tests>. Dies if we are currently skipping tests
already.

=item B<end_skipping_tests>

Cancels the effect of L</begin_skipping_tests>. Has no effect if we
are not currently skipping tests.

=cut "

sub skip_next_tests {
	my ($counter, $reason) = @_;
	die "ALREADY_SKIPPING" if $skip_counter;
	$skip_counter = $counter;
	$skip_reason  = $reason;
	return 1;
}

sub skip_next_test {
	skip_next_tests 1, @_;
}

sub begin_skipping_tests {
	my ($reason) = @_;
	die "ALREADY_SKIPPING" if $skip_counter;
	$skip_counter = -1;
	$skip_reason = $reason;
	return 1;
}

sub end_skipping_tests {
	$skip_counter = 0;
	return 1;
}

=item I<test_only>

    test_only "bla()", "reason";
    test_only qr/^bla/;
    test_only sub { /bla/ };

Skip all groups of tests whose name does not match the criteria.  The
criteria can be a plain string, a regular expression or a function.

    test_only;

Resets to normal behavior.

=cut

sub test_only (;$$) {
	my ($criteria, $reason) = @_;

	$test_only_reason = $reason;

	if (!defined $criteria) {
		$test_only_criteria = sub { 1 };
	} elsif (!ref $criteria) {
		$test_only_criteria = sub { $_[0] eq $criteria };
	} elsif (ref $criteria eq "Regexp") {
		$test_only_criteria = sub { $_[0] =~ /$criteria/ };
	} elsif (ref $criteria eq "CODE") {
		$test_only_criteria = $criteria;
	}
}

=back

=head1 CLASS METHODS

A handful of class methods are available to tweak the behavior of this
module on a global basis. They are to be invoked like this:

   Test::Group->foo(@args);

=over

=item I<verbose($level)>

Sets verbosity level to $level, where 0 means quietest. For now only 0
and 1 are implemented.

=cut

sub verbose { shift; $verbose = shift }

=item I<catch_exceptions()>

Causes exceptions thrown from within the sub reference passed to
L</test> to be blocked; in this case, the test currently running will
fail but the suite will proceed. This is the default behavior.

Note that I<catch_exceptions> only deals with exceptions arising
inside I<test> blocks; those thrown by surrounding code (if any) still
cause the test script to terminate as usual unless other appropriate
steps are taken.

=item I<dont_catch_exceptions()>

Reverses the effect of L</catch_exceptions>, and causes exceptions
thrown from a L</test> sub reference to be fatal to the whole suite.
This only takes effect for test subs that run after
I<dont_catch_exceptions()> returns; in other words this is B<not> a
whole-script pragma.

=cut

sub catch_exceptions { $catch_exceptions = 1; }
sub dont_catch_exceptions { $catch_exceptions = 0; }

=item B<logfile($logfile)>

Sets the log file for caught exceptions to F<$logfile>.  From this
point on, all exceptions thrown from within a text group (assuming
they are caught, see L</catch_exceptions>) will be written to
F<$logfile> instead of being passed on to L<Test::More/diag>. This is
very convenient with exceptions with a huge text representation (say
an instance of L<Error> containing a stack trace).

=cut

sub logfile {
    my $class = shift;
    $logfile = shift;
	$logfd   = new IO::File("> $logfile") or die "Cannot open $logfile";
}

=back

=begin internals

=head1 INTERNALS

=head2 Test::Group::_Hijacker internal class

This is an internal class which subclasses L<Test::Builder>, and whose
job is to take the place of the real I<Test::Builder> singleton (see
L<Test::Builder/new>) during the time the I<$groupsub> argument to
L</test> is being run.  This involves a fair amount of black magic,
which is performed using the L</Test::Builder::_HijackedByTestGroup
internal class> as an accomplice.

=over

=cut

package Test::Group::_Hijacker;
use base "Test::Builder";

=item I<hijack()>

Object constructor. When called while L</current> is undef, C<<
Test::Builder->new >> is (ahem) temporarily reblessed into the
I<Test::Builder::_HijackedByTestGroup> package, so that any method
calls performed subsequently against it will be routed through
L</Test::Builder::_HijackedByTestGroup internal class> where they can
be tampered with at will.  This works even if third-party code happens
to hold a reference to C<< Test::Builder->new >> created before
I<test_group__hijack> was called.  Returns an object from the
L<Test::Group::_Hijacker> class, which models the state this hijacking
taking place; this object is also available using L</current>
thereafter.

If on the other hand L</current> was already defined before entering
I<hijack>, then a B<nested hijack> is performed: this is to support
nested L</test> group subs.  In this case, the returned object behaves
mostly like the first return value of I<hijack> except that its
L</unhijack> method has no effect.

=cut

sub hijack {
    my ($class) = @_;

    my $self = $class->SUPER::create();
    $self->no_plan();

    my $devnull = new IO::File(File::Spec->devnull, ">");
    $self->output($devnull);
    $self->todo_output($devnull);

    unless ( ($self->{hijacks} = $class->current)->isa($class) ) {
        # Not in case of nested hijack:
        $self->{orig_blessed} = ref($self->{hijacks});
    }

    # Postpone the reblessing to the last moment possible, as the
    # delegating logic (L</AUTOLOAD> below) only works if $current is
    # set. In other words the two following lines of code must be
    # executed atomically:
    bless $self->{hijacks}, "Test::Builder::_HijackedByTestGroup"
        if defined $self->{orig_blessed};
    $class->current($self);

    return $self;
}

=item I<unhijack()>

Unbuggers the C<< Test::Builder->new >> singleton that was reblessed
by L</hijack>, so that it may resume being itself, or pops one item
from the L</current> stack in case of a nested hijack.

=cut

sub unhijack {
    my ($self) = @_;
    if (defined($self->{orig_blessed})) { # Top-level unhijack
        $self->current(undef);
        bless $self->{hijacks}, $self->{orig_blessed};
    } else {
        # Nested unhijack
        $self->current($self->{hijacks});
    }
    1;
}

=item I<current()>

=item I<current($newcurrent)>

Class method, gets or sets the current instance of
I<Test::Group::_Hijacker> w.r.t. the current state of the L</hijack>
/ L</unhijack> call stack.  If the stack is empty, returns C<<
Test::Builder->new >>.

=cut

{
    my $current;

    sub current {
        if (@_ == 1) {
            return defined($current) ? $current :
                Test::Builder->new;  # This is a singleton actually -
            # it should read "Test::Builder->the()" with permission
            # from Michael Schwern :-)
        } else {
            $current = $_[1];
        }
    }
}

=item I<orig_blessed()>

Returns the class in which C<< Test::Builder->new >> was originally
blessed just before it got L</hijack>ed.  Returns undef from inside
L</hijack>.

=cut

sub orig_blessed {
    my $self = shift;
    return $self->{orig_blessed} if defined $self->{orig_blessed};
    return $self->{hijacks}->orig_blessed if defined $self->{hijacks};
    return; # Construction in progress
}

=item I<is_success()>

After the test group is run, and assuming it did not throw an
exception, returns true iff this test group is to be considered a
success.

For now we don't do anything specific with the collected statistics
(number of sub-successes and sub-failures). All we want is to have no
failing tests and at least one passing test.

=cut

sub is_success {
    my ($self) = @_;
    my $successes = scalar grep { $_ }   $self->summary();
    my $failures  = scalar grep { ! $_ } $self->summary();
    return ($successes && ! $failures);
}

=item I<todo_reasons()>

After the test group is run, and assuming it did not throw an
exception, returns a string comprised of any and all the reasons
stipulated in the TODO blocks within (see L<Test::Builder/TODO:
BLOCK>), or undef if no subtest ran inside such a TODO block.

=cut

sub todo_reasons {
    my ($self) = @_;
    my @todoreasons = map 
        { ($_->{type} && $_->{type} eq "todo") ?
              ($_->{reason}) : () } ($self->details);
    return @todoreasons ? join(", ", @todoreasons) : undef;
}

=item i<is_todo_unexpected_success()>

After the test group is run, and assuming it did not throw an
exception, returns true iff at least one of the sub-tests run in a
TODO block was an actual success.  This in turn causes the test group
to itself report an unexpected success.

=cut

sub is_todo_unexpected_success {
    my ($self) = @_;
    return scalar grep
        { $_->{type} && $_->{type} eq "todo" && $_->{actual_ok} }
            ($self->details);
}

=item I<diag>

=item I<todo_output>

=item I<failure_output>

Delegated to the real object (C<< Test::Builder->new >>), so that
diagnostic output can be throttled and redirected at will without
specific knowledge of the hijacking process. (On the other hand, the
hijacking operation always redirects the L<Test::Builder/output> to
C</dev/null> in order not to interfere with the main test suite.)

=cut

foreach my $delegated (qw(diag todo_output failure_output)) {
    my $delegating_stub = sub {
        my ($self) = @_;
        my $origclass = $self->orig_blessed;
        if (! defined $origclass) {
            # Object construction in progress, don't tamper with the call
            # (pass it to the superclass)
            my $sub = Test::Builder->can($delegated); goto $sub;
        } else {
            # Delegate to the grand-master Test::Builder instance.
            $_[0] = Test::Builder->new;
            my $sub = $origclass->can($delegated); goto $sub;
        }
    };
    no strict "refs";
    *{$delegated} = $delegating_stub;
}

=back

=head2 Test::Builder::_HijackedByTestGroup internal class

This is an internal class used as an accomplice by L</hijack> to
hijack the method calls sent to the Test::Builder singleton (see
L<Test::Builder/new>) by the various testing modules from the CPAN,
e.g. L<Test::More>, L<Test::Pod> and friends.  It works almost the
same as the real thing, except for one method call:

=over

=cut

package Test::Builder::_HijackedByTestGroup;

sub AUTOLOAD {
    my ($self) = @_;
	my (undef, $methname) = (our $AUTOLOAD =~ m/^(.*)::(.*?)$/);

    return if ($methname eq "DESTROY"); # Don't mess with this one. Ever.

    my $origpack = Test::Group::_Hijacker->current->orig_blessed;
    if (my $meth = $origpack->can($methname)) {
        goto $meth; # No need to alter @_ for once, we *are* the
                    # delegated object (albeit from a different class)
    } else {
        die sprintf(qq{Can't locate class method "%s" via package "%s"},
                    $methname, $origpack);
    }
}

=item I<ok($bool, $testname)>

Delegates the call to the hijacker object - that is, the return value
of L</hijack> which is also an instance of L<Test::Builder> by virtue
of inheritance.

=cut

sub ok {
    my $self = shift;
    unshift @_, Test::Group::_Hijacker->current;
    my $sub = $_[0]->can("ok"); goto $sub;
}

=back

=end internals

=head1 BUGS

This class uses a somewhat unhealthy dose of black magic to take over
control from L<Test::Builder> when running inside a L</test> group
sub.  While the temporary re-blessing trick used therein is thought to
be very robust, it is not very elegant. Some kind of plugin mechanism
for Test::Builder->new should be designed, implemented and used
instead.

=head1 SEE ALSO

L<Test::Simple>, L<Test::More>, L<Test::Builder>, and friends; the
per-qa mailing list (<>).

=head1 AUTHORS

Dominique Quatravaux <dom@idealx.com>

Nicolas M. Thiéry <nthiery@users.sf.net>

=head1 LICENSE

Copyright (C) 2004 by IDEALX <http://www.idealx.com>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.1 or,
at your option, any later version of Perl 5 you may have available.

=cut

1;
