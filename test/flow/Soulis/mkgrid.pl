#! /usr/unsupported/bin/perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: mkgrid.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 23, 2004 by William A. Perkins
# Last Change: Fri Jan 23 11:50:31 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Math::Trig;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program";

my $L = 2.3;
my $W = 0.25;
my $dx = 0.05;
my $dy = 0.025;

my $scale = 1.0/0.3048;

# coordinates of the right bank
my @xo = (0.0, 0.50, 1.15,  1.45,  1.75, 1.80, 2.30);
my @yo = (0.0, 0.00, 0.125, 0.125, 0.01, 0.00, 0.00);

my $slope = 0.02;

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------


my $nx;
my $ny;

$nx = $L/$dx;
$ny = $W/$dy;

my ($xidx, $x, $y);
my ($i, $j);

my $zo = $slope*$L;

printf("%5d%5d\n", $nx+1, $ny+1);

for ($i = 0; $i < $nx + 1; $i++) {
  $xidx = 0;
  $x = $dx*$i;

  my $z = $zo - $x*$slope;

  while ($x > $xo[$xidx] && $xidx < scalar(@xo)) { $xidx++; }

  $y = ($x - $xo[$xidx-1])/($xo[$xidx] - $xo[$xidx-1])*
    ($yo[$xidx] - $yo[$xidx-1]) + $yo[$xidx-1];

  $dy = ($W - $y)/$ny;
  for ($j = 0; $j < $ny + 1; $j++, $y += $dy) {
    printf("%5d%5d%15.6e%15.6e%15.6e\n", $i, $j, 
           $x*$scale, $y*$scale, 0.0*$scale);
  }

}

