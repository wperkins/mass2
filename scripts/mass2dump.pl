#! /usr/unsupported/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2dump.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 28, 2000 by William A. Perkins
# Last Change: Tue Dec 19 10:59:32 2000 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use PDL;
use PDL::Char;
use PDL::NetCDF;
use Getopt::Std;

=pod 

=head1 NAME

mass2dump.pl - dumps, in tabular format MASS2 plot output

=head1 SYNOPSIS

perl B<mass2dump.pl> B<-E> I<file>

=head1 DESCRIPTION

=head1 OPTIONS

=over

=item B<-E> 

Do not output the block edges, only the cell centers.

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
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program file";

my $filename;
my $dropedge = undef; 

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("E", \%opts));

$dropedge = 1 if ($opts{'E'});

$filename = shift @ARGV;

die "$usage\n" unless ($filename);

unless (-r $filename) {
  printf(STDERR "$program: error: unable to read file \"%s\"\n", $filename);
  die "$usage\n";
}

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
my $blocks = $ncfile->dimsize("block");
my $etamax = $ncfile->get('etamax');
my $ximax = $ncfile->get('ximax');
my $x = $ncfile->get('x');
my $y = $ncfile->get('y');
my $timestamps = $ncfile->get('timestamp');

my ($blk, $i, $j);

for ($blk = 0; $blk < $blocks; $blk++) {

  my ($istart, $jstart, $iend, $jend) = 
    (0, 0, $etamax->at($blk), $ximax->at($blk));

  ($istart, $jstart, $iend, $jend) = 
    (1, 1, $etamax->at($blk) - 1, $ximax->at($blk) - 1) if ($dropedge);

  for ($i = $istart; $i < $iend; $i++) {
    for ($j = $jstart; $j < $jend; $j++) {
      printf("%5d%5d%5d%15.2f%15.2f\n", $blk + 1, $i + 1, $j + 1, 
             $x->at($blk, $i, $j), $y->at($blk, $i, $j));
    }
  }
}
