#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2flux.pl

# A script to compute water and contaminant fluxes at a MASS2 cross
# section.  Currently, this just does entire cross sections.

# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  7, 2000 by William A. Perkins
# Last Change: Tue Aug  8 09:17:38 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use PDL;
use PDL::Char;
use PDL::NetCDF;
use Getopt::Std;

=pod

=head1 NAME

mass2flux.pl - compute water and/or scalar species flux that crosses a
lateral section of a block or blocks using the MASS2 plot output file.

=head1 SYNOPSIS

perl B<mass2flux.pl> [B<-v> I<variable>] [B<-o> I<output>] I<file>
I<block> I<index> [I<block> I<index> ...]

=head1 DESCRIPTION

=head2 Flow Computation

=head2 Flux Computation

=head1 OPTIONS

=over

=item B<-v> I<variable>

=item B<-c>

Output cumulative values of flow and/or flux rather that rates.

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
my $usage = "usage: $program [-v variable] block index [block index ...]";

my $varname = undef;
my $cumulative = undef;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("o:v:c", \%opts));

$varname = $opts{v} if $opts{v};
$cumulative = 1 if ($opts{'c'});

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
my @todolist = @ARGV;

die "$usage\n" unless ($filename && scalar(@todolist));

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

my $times = $ncfile->dimsize("time");
my $etamax = $ncfile->get('etamax');
my $ximax = $ncfile->get('ximax');
my $timestamps = $ncfile->get('timestamp');
my $x = $ncfile->get('x');
my $y = $ncfile->get('y');

my $starttime = 0;

my $time = $ncfile->get('time');

my $flow;
my $conc;
my $depths;
my $vels;
my $vars;

my $i;
my $j;
my $dy;

                                # we're going to assume a constant
                                # deltat here.  Time is in days in the
                                # plot file; we need to convert it to
                                # seconds

my $deltat;
$deltat = $time->at(1) - $time->at(0);
$deltat *= 86400;

$flow = 0.0;
$conc = 0.0;

my $convert = 1.0;

if ($varname) {
  $convert = VarConversion($ncfile, $varname);
}

for ($i = $starttime; $i < $times; $i++) {
  my $t = $time->at($i);
  my $ts = $timestamps->atstr($i);

  my @lst = @todolist;

  unless ($cumulative) {
    $flow = 0.0;
    $conc = 0.0;
  }

  while (scalar(@lst) > 0) {
    my $block = shift @lst;
    my $index = shift @lst;
    $block --;
    $index --;

    $depths = $ncfile->get('depth', 
                           [$i, 0, $index, $block],
                           [1, $ximax->at($block), 1, 1]);
    $vels = $ncfile->get('uvel', 
                         [$i, 0, $index, $block],
                         [1, $ximax->at($block), 1, 1]);
    if ($varname) {
      $vars = $ncfile->get($varname, 
                           [$i, 0, $index, $block],
                           [1, $ximax->at($block), 1, 1]);
    }

    $dy = 0.0;

    for ($j = 0; $j < $ximax->at($block) - 1; $j++) {

      my ($x1, $y1, $x2, $y2);

      $x1 = $x->at($block, $index, $j);
      $y1 = $y->at($block, $index, $j);

      $x2 = $x->at($block, $index, $j + 1);
      $y2 = $y->at($block, $index, $j + 1);

      $dy = sqrt(($x1 - $x2)**2 + ($y1 - $y2)**2);

      my $f;

      if ($j == 0) {
        $f = $dy*($depths->at($j) + $depths->at($j+1))*
        ($vels->at($j+1))/2.0;
      } elsif ($j == $ximax->at($block) - 2) {
        $f = $dy*($depths->at($j) + $depths->at($j+1))*
          ($vels->at($j))/2.0;
      } else {
        $f = $dy*($depths->at($j) + $depths->at($j+1))*
          ($vels->at($j) + $vels->at($j+1))/4.0;
      }

      $f *= $deltat if ($cumulative);

      $flow += $f;

      if ($varname && $flow > 0.0) {
        $conc += $f*($vars->at($j) + $vars->at($j+1))*$convert/2.0;
      }
    }
  }

  printf(OUTPUT "%s %15.5g", $timestamps->atstr($i), $flow);
  if ($varname) {
    if ($cumulative) {
      printf(OUTPUT " %15.5g", $conc);
    } else {
      if ($flow > 0.0) {
        printf(OUTPUT " %15.5g", $conc/$flow);
      } else {
        printf(OUTPUT " %15.5g", 0.0);
      }
    }
  }
  printf(OUTPUT "\n");
}




