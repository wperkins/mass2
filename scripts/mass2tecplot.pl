#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2tecplot.pl
# This script takes a MASS2 "plot" output file (in netcdf) format and
# makes an ASCII tecplot data file
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 11, 1999 by William A. Perkins
# Last Change: Mon Aug 16 13:10:33 1999 by William A. Perkins <perk@tophet.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Getopt::Std;
use NetCDF;

# -------------------------------------------------------------
# CheckPlotFile
# -------------------------------------------------------------
sub CheckPlotFile {
  my $program = shift;
  my $ncid = shift;
  my @checkdims = ("block", "time", "eta", "xi");
  my @checkvars = ("time", "timestamp", "x", "y", "zbot");
  my $dimname;
  my $err = 0;

                                # make sure these dimensions exist and
                                # have a length > 0
  foreach $dimname (@checkdims) {
    my $dimid = NetCDF::dimid($ncid, $dimname);
    my $dimlen = 0;

    if ($dimid < 0) {
      printf(STDERR "$program: error: dimension \"%s\" not found in NetCDF file\n", $dimname);
      $err++;
    } else {
      if (NetCDF::diminq($ncid, $dimid, \$dimname, \$dimlen) != 0) {
        printf(STDERR "$program: error: dimension \"%s\" has no length\n", $dimname);
        $err++;
      }
    }

    if ($err == 0) {
      my $var;
      foreach $var (@checkvars) {
        my $varid = NetCDF::varid($ncid, $var);
        if ($varid <= 0) {
          printf(STDERR "$program: error: variable \"%s\" not found in NetCDF file\n", $var);
          $err++;
        }
      }
    }
  }
  return ($err == 0);
  
}
# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [-l] [-1 time] [-S time] [-E time] [-o output] file";

my $dolist = undef;
my $filename = undef;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("l", \%opts));

$dolist = 1 if ($opts{l});

$filename = shift(@ARGV);

die "$usage\n" unless ($filename);

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $ncdfile;

$ncdfile = NetCDF::open($filename, NetCDF::NOWRITE);
unless ($ncdfile > 0) {
  printf(STDERR "$program: error: unable to open NetCDF File %s\n", $filename);
  die;
}

unless (CheckPlotFile($program, $ncdfile)) {
  printf(STDERR "$program: %s: error: not a MASS2 plot file\n", $filename);
  die;
}

my $junk = "";
my $blocks = 0;
my $blkdimid = NetCDF::dimid($ncdfile, "block");
my $times = 0;
my $timdimid = NetCDF::dimid($ncdfile, "time");
my $tsdimid = NetCDF::dimid($ncdfile, "tslen");
my $tslen = 0;
my $timvarid = NetCDF::varid($ncdfile, "time");
my $tsvarid = NetCDF::varid($ncdfile, "timestamp");
my $etavarid = NetCDF::varid($ncdfile, "etamax");
my $xivarid = NetCDF::varid($ncdfile, "ximax");
my $xvarid = NetCDF::varid($ncdfile, "x");
my $yvarid = NetCDF::varid($ncdfile, "y");
my $zvarid = NetCDF::varid($ncdfile, "zbot");
my $start = 0.0;
my $end = 0.0;
my $startstamp = "";
my @coords = (0);
my @length = (0);
NetCDF::diminq($ncdfile, $blkdimid, \$junk, \$blocks);
NetCDF::diminq($ncdfile, $timdimid, \$junk, \$times);
NetCDF::diminq($ncdfile, $tsdimid, \$junk, \$tslen);
@coords = (0,0);
NetCDF::varget1($ncdfile, $timvarid, \@coords, $start);
@coords = ($times - 1, 0);
NetCDF::varget1($ncdfile, $timvarid, \@coords, $end);
printf(STDERR "$filename has %d blocks and %d time planes\n", $blocks, $times);
printf(STDERR "starting %f and ending %f\n", $start, $end);

my $blk;
my $tn;

printf("variables=\"x\" \"y\" \"zbot\"\n");

for ($tn = 0; $tn < $times; $tn += 1) {
  my @values;
  my $timestamp = " ";

  @coords = ( $tn, 0 );
  @length = ( 1, $tslen );
  NetCDF::varget($ncdfile, $tsvarid, \@coords, \@length, \@values);
  $timestamp = pack("C*", @values);
  chop($timestamp);
  for ($blk = 0; $blk < $blocks; $blk += 1) {
    my $zonename = sprintf("%s block %2d", $timestamp, $blk + 1);
    my $etamax = 0;
    my $ximax = 0;

    @coords = ( $blk );
    NetCDF::varget1($ncdfile, $etavarid, \@coords, $etamax);
    NetCDF::varget1($ncdfile, $xivarid, \@coords, $ximax);
    
    printf("zone t=\"%s\" i=%d, j=%d, f=point\n", $zonename, $etamax + 1, $ximax + 1);

    my $i = 0;
    my $j = 0;

    for ($j = 0; $j <= $ximax; $j++) {
      for ($i = 0; $i <= $etamax; $i++) {
        my $x;
        my $y;
        my $z;

        @coords = ($j, $i, $blk);
        NetCDF::varget1($ncdfile, $xvarid, \@coords, $x);
        NetCDF::varget1($ncdfile, $yvarid, \@coords, $y);
        NetCDF::varget1($ncdfile, $zvarid, \@coords, $z);
        printf("%15.9e %15.9f %15.9f \n", $x, $y, $z);
      }
    }

  }
}

NetCDF::close($ncdfile);
