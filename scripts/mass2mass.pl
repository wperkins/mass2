#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2mass.pl

# This script reports the total "mass" of a variable in the MASS2 plot
# output file, either for all time steps or for a single time step.

# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September  7, 2000 by William A. Perkins
# Last Change: Tue Jul 24 10:51:00 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use PDL;
use PDL::Char;
use PDL::NetCDF;
use Getopt::Std;

=pod

=head1 NAME

mass2mass.pl - compute total mass of scalar species in the water
column, or bed, from the MASS2 plot file (NetCDF format).

=head1 SYNOPSIS

perl B<mass2mass.pl> [B<-t> I<step> | B<-l>] [B<-o> I<output>] I<file>
I<variable>]

=head1 DESCRIPTION

This script uses the plot output file from MASS2.  It determines the
total mass of a scalar species I<variable> in the simulated domain for
any, or all, timesteps in the plot output file.  This is done in two ways:


The user is responsible to make sure that summing the I<variable>
mass, in either of these manners actually makes sense.  For example,
using the C<vmag> variable would be inappropriate.

In the MASS2 plot output file (NetCDF format), the time-varying
spatial variables may or may not have an C<isbed> attribute.  If this
attribute exists for a variable, it is a bed variable.

=head2 Bed Variables

=head2 Water Column Variables

=head1 OPTIONS

=over

=item B<-o> I<output>

Send data to I<output> instead of standard output.

=back

=head1 AUTHOR

William A. Perkins, Battelle Northwest

=cut


# -------------------------------------------------------------
# CheckPlotFile
# -------------------------------------------------------------
sub CheckPlotFile {
  my $program = $main::program;
  my $ncfile = shift;
  my @checkdims = ("block", "time", "eta", "xi");
  my @checkvars = ("time", "timestamp", "x", "y", "zbot");
  my $dimname;
  my $err = 0;
  my $names = $ncfile->getdimensionnames();

                                # make sure these dimensions exist and
                                # have a length > 0
  foreach $dimname (@checkdims) {
    unless (grep(/^${dimname}$/, @{$names})) {
      printf(STDERR "$program: error: dimension \"%s\" not found in NetCDF file\n", $dimname);
      $err++;
    }
  }
  return ($err == 0);
}

# -------------------------------------------------------------
# VarIsBed
# -------------------------------------------------------------
sub VarIsBed {
  my $ncfile = shift;
  my $variable = shift;
  my $isbed = undef;

  my $atts = $ncfile->getattributenames($variable);
  if (grep(/^isbed$/, @{$atts})) {
    $isbed = $ncfile->getatt('isbed', $variable);
  }
  return $isbed;
}

# -------------------------------------------------------------
# VarConversion
# -------------------------------------------------------------
sub VarConversion {
  my $ncfile = shift;
  my $variable = shift;

  my $atts = $ncfile->getattributenames($variable);
  my $conv = 1.0;

  if (grep(/^Conversion$/, @{$atts})) {
    $conv = $ncfile->getatt('Conversion', $variable)->at(0);
  }

  return $conv;
}

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [-t step|-l] [-o output] file variable ";

my $dotime = 0;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("o:t:l", \%opts));

$dotime = $opts{'t'} if $opts{'t'};
$dotime = -1 if $opts{'l'};

if ($opts{o}) {
  my $name = $opts{o};
  unless (open(OUTPUT, ">$name")) {
    printf(STDERR "$program: error: unable to open file \"%s\" for writing\n", 
           $name);
    die "$usage\n";
  }
} else {
  unless (open(OUTPUT, ">&STDOUT")) {
    die "$program: error: Unable to dup STDOUT\n";
  }
}

my $filename = shift(@ARGV);
my $dovar = shift(@ARGV);

die "$usage\n" unless ($filename && $dovar);

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $ncfile;

unless ($ncfile = PDL::NetCDF->new ("$filename")) {
  die "$program: error: cannot open \"$filename\" (NetCDF file?)\n";
}

unless (CheckPlotFile($ncfile)) {
  die "$program: error: \"$filename\" is not a MASS2 plot file\n";
}

                                # dimensions we need to know

my $times = $ncfile->dimsize("time");
my $blocks = $ncfile->dimsize("block");
my $eta = $ncfile->dimsize("eta");
my $xi = $ncfile->dimsize("xi");
my $etamax = $ncfile->get('etamax');
my $ximax = $ncfile->get('ximax');

                                #  spatial variables we need

my $hp1 = $ncfile->get('hp1');
my $hp2 = $ncfile->get('hp2');

                                # use these for output

my $timestamps = $ncfile->get('timestamp');

my $area;
my $vol;
my $i;
my $j;
my $blk;
my $mass;

                                # compute cell areas

$area = $hp1->zeroes();
for ($blk = 0; $blk < $blocks; $blk++) {
  for ($i = 1; $i < $etamax->at($blk) - 1; $i++) {
    for ($j = 1; $j < $ximax->at($blk) - 1; $j++) {
      $area->set($blk, $i, $j, $hp1->at($blk, $i, $j)*$hp2->at($blk, $i, $j));
    }
  }
}

                                # get(var) and get(var, o, l) assign
                                # different dimensions, so fix it

if ($blocks == 1) {
  my $tmp = $area->slice('(0),:,:');
  $area = $tmp;
}

my $convert = VarConversion($ncfile, $dovar);

my $starttime;

if ($dotime == 0) {
  $starttime = 0;
} elsif ($dotime < 0) {
  $starttime = $times - 1;
} else {
  $starttime = $dotime - 1;
  $times = $dotime;
}

for ($i = $starttime; $i < $times; $i++) {
  my $ts = $timestamps->atstr($i);
  my $var;

  $var = $ncfile->get($dovar, 
                      [$i, 0, 0, 0],
                      [1, $xi, $eta, $blocks]);
  $var *= $area;
  if (! VarIsBed($ncfile, $dovar)) {
    my $depths;
    $depths = $ncfile->get('depth', 
                           [$i, 0, 0, 0],
                           [1, $xi, $eta, $blocks]);
    $var *= $convert;
    $var *= $depths;
  }

  $mass = sum($var);

  printf("%s %15.6g\n", $ts, $mass);
  
}


