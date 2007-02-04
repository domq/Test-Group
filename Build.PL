# Build.PL, (C) IDEALX 2005 (See README for license details)

# This script automatically builds a "Build" file in the current
# directory (using an in-house subclass to Module::Build), which in
# turn builds the Test-Group package.
#
# If you don't have Module::Build installed, use the following instead:
#     perl Makefile.PL
#     make
#     make test
#     make install

use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new
    ( module_name         => 'Test::Group',
      license             => 'perl',
      dist_author         => 'Dominique Quatravaux <domq@cpan.org>',
      dist_version_from   => 'lib/Test/Group.pm',
      requires            =>
      {
       'Exporter'     => 0,
       'Test::Simple' => 0.59,  # Test::Builder->create() needed
       'Test::Builder' => 0,
       'Test::More' => 0,
       'File::Spec' => 0,
       'IO::File' => 0,
      },
      build_requires     =>
      {
       # A copy of Test::Cmd is bundled in t/lib to cut down on
       # dependencies and make Test::Group easier to install.
       # Therefore Test::Cmd does not get installed, but it has
       # dependencies of its own:
       'File::Find' => 0,
       'File::Basename' => 0,
      },
    add_to_cleanup      => [ 'Test-Group-*' ],
    create_makefile_pl  => 'traditional', # Makes Module::Build optional
);

$builder->create_build_script();

1;
