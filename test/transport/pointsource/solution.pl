#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: solution.pl

# Solution to a 2-D point source transport problem. From Fischer, et
# al., 1979.

# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August  7, 2000 by William A. Perkins
# Last Change: Wed Apr 23 12:32:07 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Getopt::Std;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program";

my $u = 1.0;                    # mean velocity, feet / second
my $W = 250.0;                  # channel width, feet
my $L = 10000.0;                # channel length, feet
my $d = 6.0;                    # flow depth, feet
my $sigma_t = 0.2;              # transverse dispersivity, feet2/second
my $mdot = 1.0;                 # mass influx, units/sec

my $yo = 125.0;
my $xo = 25.0;

my $infinity = 8;

                                # output calculations interval(s)

my $x;
my $y;
my $deltax = 25;
my $deltay = 5;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("x:y:o:", \%opts));

$x = $opts{'x'} if (defined($opts{'x'}));
$y = $opts{'y'} if (defined($opts{'y'}));


if ($opts{'o'}) {
  my $name = $opts{'o'};
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



# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $pi = atan2(1,1)*4;

my $Co = $mdot/($u*$d*$W);
my $yprimeo = $yo/$W;

my $startx = $xo;
my $endx = $L;
my $starty = 0.0;
my $endy = $W;

if ($x) {
  $startx = $x;
  $endx = $x;
}

if ($y) {
  $starty = $y;
  $endy = $y;
}

for ($x = $startx; $x <= $endx; $x += $deltax) {
  for ($y = $starty; $y <= $endy; $y += $deltay) {
    my $xprime = ($x - $xo)*$sigma_t/($u*$W**2);
    my $yprime = $y/$W;
    my $n;
    my $C = 0;

    $xprime = 0.001 if ($xprime == 0.0);

    for ($n = -$infinity; $n <= $infinity; $n++) {
      $C += 
        (exp(-($yprime - 2*$n - $yprimeo)**2/(4*$xprime)) +
         exp(-($yprime - 2*$n + $yprimeo)**2/(4*$xprime)));
    }
    $C /= sqrt(4*$pi*$xprime);
    $C *= $Co;

    printf(OUTPUT "%.3f %.3f %.5g\n", $x, $y, $C);
  }
}
