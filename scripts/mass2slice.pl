#! /usr/unsupported/gnu/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2slice.pl
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 26, 2000 by William A. Perkins
# Last Change: Thu Jan 29 08:49:05 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use PDL;
use PDL::Char;
use PDL::NetCDF;
use Getopt::Std;

=pod 

=head1 NAME

mass2slice.pl - extract profile and cross section data from MASS2 plot
output files.

=head1 SYNOPSIS

perl B<mass2slice.pl> [B<-d>] [-a] [B<-i>|B<-j>] [B<-t> I<indices>|B<-l>] [B<-o> I<output>]
I<file> I<variable> I<block> I<index> [I<block> I<index> ...]

perl B<mass2slice.pl> B<-p> [B<-t> I<indices>|B<-l>] [B<-o> I<output>]
I<file> I<variable> I<block> I<i> I<j>

=head1 DESCRIPTION

B<mass2slice.pl> is used to extract slices of data from the NetCDF
format MASS2 plot output file.  Slices can be made either
longitudinally (B<-i>) or laterally (B<-j>) across multiple blocks.
By default, all times are output separated by a blank line (useful for
plotting in gnuplot).

=head1 OPTIONS

=over

=item B<-a>

Average the variable across the profile or cross section to produce
one value.

=item B<-d>

Place the date/time in the first two columns of the output.

=item B<-D>

For B<-i> and B<-j> slices, do not output any points where the cell is
dry.  This option is ignored if there is no wet/drying information in I<file>. 

=item B<-i>

Extract a profile.  Distance is computed from upstream to downstream.
The I<index> is the lateral index.

=item B<-j>

Extract a cross section. Distance is computed from the right to left
bank (looking downstream). The I<index> is the longitudinal index.

=item B<-p>

Output a timeseries at a single point rather than a slice.  If this
option is used, both the I<i> and I<j> indices of the point (cell
actually) are expected to be on the command line.

=item B<-t> I<index>

Select a specific time, specified by I<index>, to extract.  By default
all times are extracted, and their time stamp is placed in the output.

=item B<-l>

Extract only the last time in the plot output file.  The B<-t> is
ignored if this option is specified.

=item B<-o> I<output>

Send data to I<output> instead of standard output.

=back

=head1 EXAMPLES

This script is typically used to provide data for profile plotting.  The following gnuplot script will plot a water surface elevation profile from a three block domain:

    plot '<perl mass2slice.pl -t 1 -i plot.nc wsel 1 5 2 10 3 5' \
            using 3:4 with l 1, \
         '<perl mass2slice.pl -l -i plot.nc wsel 1 5 2 10 3 5' \
            using 3:4 with l 3, \
         '<perl mass2slice.pl -i plot.nc zbot 1 5 2 10 3 5' \
            using 3:4 with l 7

Block 2 is either aliged differently or of a different resolution.
The resulting plot will show three curves: the initial conditions
(C<-t 1, wsel>), the final conditions (C<-l, wsel>), and the bottom
elevation (C<zbot>).

=head1 SEE ALSO

gnuplot(1)

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
my $usage = "usage: $program [-i|-j] [-d] [-a] [-t index] file variable block index [block index]";

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("ijo:t:ldpaD", \%opts));

my $dotime = undef;
my @timelist = ();
my $dolong = 1;
my $dolast = undef;
my $dodate = undef;
my $dopoint = undef;
my $doavg = undef;
my $nodry = undef;

$dolong = 0 if ($opts{'j'});
$dolong = 1 if ($opts{'i'});
$dotime = $opts{'t'} if ($opts{'t'});
$dolast = $opts{'l'} if ($opts{'l'});
$dodate = $opts{'d'} if ($opts{'d'});
$dopoint = $opts{'p'} if ($opts{'p'});
$doavg = 1 if ($opts{'a'});
$nodry = 1 if ($opts{'D'});

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
my $variable = shift(@ARGV);
my @todolist = @ARGV;

die "$usage\n" unless ($filename && $variable && scalar(@todolist));

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
my $x = $ncfile->get('x');
my $y = $ncfile->get('y');
my $timestamps = $ncfile->get('timestamp');

my ($i, $j, $block, $index);
my ($dist, $lastx, $lasty, $xc, $yc);

my $notime = undef;
my $starttime = 0;

$dotime = $times if ($dolast);
if ($dotime) {
  unless ($dotime > 0 && $dotime <= $times) {
    die "$program: error: specified time ($dotime) not valid\n$usage\n"
  }
  $starttime = $dotime - 1;
  $times = $dotime;
}

                                # for non-time-dependant variables

if ($variable =~ /^zbot$/) {
  $times = 1;
  $notime = 1;
}

for ($i = $starttime; $i < $times; $i++) {

  $dist = 0.0;
  $lastx = undef;
  $lasty = undef;

  printf(OUTPUT "\n# %s\n", $timestamps->atstr($i)) if (! ($dodate or $dopoint));

  my @lst = @todolist;

  while (scalar(@lst)) {
    $block = shift @lst;
    $index = shift @lst;

    $block--;
    $index--;

    my $v;
    my $j;
    my $isdry = undef;

    if ($dopoint) {
      $j = shift @lst;
      $j--;
      $xc = $x->at($block, $index, $j);
      $yc = $y->at($block, $index, $j);
      if ($notime) {
        $v = $ncfile->get($variable, 
                          [$j, 0, $block], 
                          [1, $etamax->at($block), 1]);
      } else {
        $v = $ncfile->get($variable, 
                          [$i, $j, 0, $block], 
                          [1, 1, $etamax->at($block), 1]);
      }
      printf(OUTPUT "%s ", $timestamps->atstr($i));
      printf(OUTPUT "%12.3f %12.3f %12.5g\n", $xc, $yc, $v->at($index));
      
    } elsif ($dolong) {
      if ($notime) {
        $v = $ncfile->get($variable, 
                          [$index, 0, $block], 
                          [1, $etamax->at($block), 1]);
      } else {
        $v = $ncfile->get($variable, 
                          [$i, $index, 0, $block], 
                          [1, 1, $etamax->at($block), 1]);
        if ($nodry) {
          $isdry = $ncfile->get("isdry", 
                                [$i, $index, 0, $block], 
                                [1, 1, $etamax->at($block), 1]);
        }
      }
      for ($j = 0; $j < $etamax->at($block); $j++) {
        $xc = $x->at($block, $j, $index);
        $yc = $y->at($block, $j, $index);
        if ($j > 0) {
          $dist += sqrt( ($xc - $lastx)**2 + ($yc - $lasty)**2 );
        }
        if ($nodry) {
          if (!$isdry->at($j)) {
            printf(OUTPUT "%s ", $timestamps->atstr($i)) if ($dodate);
            printf(OUTPUT "%12.3f %12.3f %12.3f %12.5g\n", 
                   $xc, $yc, $dist, $v->at($j));
          }
        } else {
          printf(OUTPUT "%s ", $timestamps->atstr($i)) if ($dodate);
          printf(OUTPUT "%12.3f %12.3f %12.3f %12.5g\n", 
                 $xc, $yc, $dist, $v->at($j));
        }
        $lastx = $xc;
        $lasty = $yc;
      }
    } else {
      if ($notime) {
        $v = $ncfile->get($variable, 
                          [0, $index, $block], 
                          [$ximax->at($block), 1, 1]);
      } else {
        $v = $ncfile->get($variable, 
                          [$i, 0, $index, $block], 
                          [1, $ximax->at($block), 1, 1]);
        if ($nodry) {
          $isdry = $ncfile->get("isdry", 
                                [$i, $index, 0, $block], 
                                [1, 1, $etamax->at($block), 1]);
        }
      }
      for ($j = 0; $j < $ximax->at($block); $j++) {
        $xc = $x->at($block, $index, $j);
        $yc = $y->at($block, $index, $j);
        if ($j > 0) {
          $dist += sqrt( ($xc - $lastx)**2 + ($yc - $lasty)**2 );
        }
        if ($nodry) {
          if (!$isdry->at($j)) {
            printf(OUTPUT "%s ", $timestamps->atstr($i)) if ($dodate);
            printf(OUTPUT "%12.3f %12.3f %12.3f %12.5g\n", 
                   $xc, $yc, $dist, $v->at($j));
          }
        } else {
          printf(OUTPUT "%s ", $timestamps->atstr($i)) if ($dodate);
          printf(OUTPUT "%12.3f %12.3f %12.3f %12.5g\n", 
                 $xc, $yc, $dist, $v->at($j));
        }
        # printf(OUTPUT "%s ", $timestamps->atstr($i)) if ($dodate);
        # printf(OUTPUT "%12.3f %12.3f %12.3f %12.5g\n", 
        #        $xc, $yc, $dist, $v->at($j));
        $lastx = $xc;
        $lasty = $yc;
      }
    }


  }

}
