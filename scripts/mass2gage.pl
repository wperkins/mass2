#! /usr/unsupported/gnu/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: mass2gage.pl
# Extract data from the MASS2 (NetCDF format) gage output file
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 17, 1999 by William A. Perkins
# Last Change: Thu Aug  3 13:24:53 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

=pod

=head1 NAME

mass2gage.pl - extract data from a MASS2 gage output file (NetCDF format).

=head1 SYNOPSIS

perl B<mass2gage.pl> B<-l> I<file>

perl B<mass2gage.pl> B<-v> I<var> B<-g> I<gage> [-C|-M] [-1] [B<-o> I<output>] I<file>

=head1 DESCRIPTION

This script is used to extract a data, for one variable and gage, from
a MASS2 gage output file.  It is primarily intended to be used to
check a simulation while in progress.  

=head1 OPTIONS

=over

=item B<-l>

list the gages and time-dependant variables in I<file> and exit.

=item B<-v> I<var>

(required) extract the I<var> time-dependant variable from I<file>;
I<var> may be either an integer or a variable name (as long as the
name does not start with a number), either of which can be obtained
using B<-l>

=item B<-g> I<gage>

(required) extract data from the I<gage> location; I<gage> may be
either an integer or a gage location name (as long as the name does
not start with a number), either of which can be obtained using B<-l>

=item B<-C>

output a cumulative frequency distribution; normally, a time series is
output, this option will cause the data from the specified gage to be
sorted and assigned an exceedance probability 

=item B<-o> I<output>

send extracted data to I<output> (does not work with B<-l>)

=item B<-1> 

add a line at the top of the output (line 1) containing some
information about the extracted data

=item B<-M> 

format as a MASS1/MASS2 boundary condition file (implies -1)

=back

=head1 EXAMPLES

Here is the output from a listing (B<-l>) of a particular gage.nc file: 

    perk@gehenna> perl mass2gage.pl -l gage.nc
    MASS2 Gage Output File:
         "gage.nc"
    3577 time slices:
        starting: 04-05-1996 00:00:00
          ending: 09-01-1996 00:00:03

    Available Gage Locations
    -----------------------------------------------------
    Gage Name                           Block   Eta    Xi
    -----------------------------------------------------
       1 FMS RM106.6 North                  1    25     2
       2 FMS RM106.6 Mid-channel            1    25    12
       3 FMS RM106.6 South                  1    25    23
    -----------------------------------------------------

    Available Time-Dependant Variables:
      8 wsel                 Water Surface Elevation, feet
      9 depth                Depth, feet
     10 vmag                 Velocity Magnitude, feet/second
     11 uvel                 Longitudinal Velocity, feet/second
     12 vvel                 Lateral Velocity, feet/second
     13 temperature          Water Temperature, Centigrade
     14 tdgconc              Total Dissolved Gas Concentration, milligram/liter
     15 tdgpress             Total Dissolved Gas Pressure, millibars
     16 tdgdeltap            Total Dissolved Gas Pressure above Atmospheric, millibars
     17 tdgsat               Total Dissolved Gas Pressure Saturation, percent

Temperature data is extracted from the same file:

    perk@gehenna> perl mass2gage.pl -g 2 -v temperature gage.nc
    04-05-1996 00:00:00           7.152
    04-05-1996 01:00:00           7.165
    ...
    08-31-1996 23:00:03          17.848
    09-01-1996 00:00:03          17.847

The data is formatted as a MASS1/MASS2 boundary condition file:

    perk@gehenna> perl mass2gage.pl -g 19 -v temperature -M gage.nc
    # gage.nc Extraction: FMS RM106.6 Mid-channel: Water Temperature, Centigrade
    04-05-1996 00:00:00           7.152 /
    04-05-1996 01:00:00           7.165 /
    ...
    08-31-1996 23:00:03          17.848 /
    09-01-1996 00:00:03          17.847 /

=head1 AUTHOR(S)

William A. Perkins, Battelle

=head1 SEE ALSO

=cut

use strict;
use Getopt::Std;
use NetCDF;

# -------------------------------------------------------------
# CheckGageFile
# -------------------------------------------------------------
sub CheckGageFile {
  my $program = shift;
  my $ncid = shift;
  my @checkdims = ("gage", "time");
  my @checkvars = ("time", "timestamp", "block", "eta", "xi", "gage_name");
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
  }
  if ($err == 0) {
    my $var;
    foreach $var (@checkvars) {
      my $varid = NetCDF::varid($ncid, $var);
      if ($varid < 0) {
        printf(STDERR "$program: error: variable \"%s\" not found in NetCDF file\n", $var);
        $err++;
      }
    }
  }
  return ($err == 0);
  
}

# -------------------------------------------------------------
# GetGageName
# -------------------------------------------------------------
sub GetGageName {
  my $ncid = shift;
  my $gageid = shift;

  my $idlenid = NetCDF::dimid($ncid, "idlen");
  my $idlen = undef;
  my $name;
  NetCDF::diminq($ncid, $idlenid, \$name, \$idlen);
  $name = undef;

  my $gagevarid = NetCDF::varid($ncid, "gage_name");

  my @coords = ($gageid, 0 );
  my @length = (1, $idlen);
  my @values = ();

  NetCDF::varget($ncid, $gagevarid, \@coords, \@length, \@values);

  $name = pack("C*", @values);
  $name =~ s/\0//g;

  return($name);
}

# -------------------------------------------------------------
# GetGageId
# -------------------------------------------------------------
sub GetGageId {
  my $ncid = shift;
  my $gagename = shift;

  my $gageid = NetCDF::dimid($ncid, "gage");
  my $gages;
  my $name;

  my $i;
  my $found = -1;

  NetCDF::diminq($ncid, $gageid, \$name, \$gages);

  for ($i = 0; $i < $gages; $i++) {
    $name = GetGageName($ncid, $i);
    if ($name eq $gagename) {
      $found = $i;
      last;
    }
  }

  return $found;
}

# -------------------------------------------------------------
# GageOK
# -------------------------------------------------------------
sub GageOK {
  my $ncid = shift;
  my $gageid = shift;

  my $gagedimid = NetCDF::dimid($ncid, "gage");
  my $gages;
  my $name;

  NetCDF::diminq($ncid, $gagedimid, \$name, \$gages);

  return ($gageid >= 0 && $gageid < $gages);
  
}

# -------------------------------------------------------------
# GageList
# -------------------------------------------------------------
sub GageList {
  my $ncid = shift;
  my $gageid = NetCDF::dimid($ncid, "gage");
  my $gages;
  my $block;
  my $eta;
  my $xi;

  my $blkvarid = NetCDF::varid($ncid, "block");
  my $etavarid = NetCDF::varid($ncid, "eta");
  my $xivarid = NetCDF::varid($ncid, "xi");

  my @coords = ();
  my @length = ();
  my @values = ();

  my $name;
  my $i;
  
  NetCDF::diminq($ncid, $gageid, \$name, \$gages);

                                # first list gages and their names

  printf(STDERR "\nAvailable Gage Locations\n");
  printf(STDERR "-----------------------------------------------------\n");
  printf(STDERR "Gage Name                           Block   Eta    Xi\n");
  # printf(STDERR "#### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ##### ##### #####\n");
  printf(STDERR "-----------------------------------------------------\n");
  for ($i = 0; $i < $gages; $i++) {

    $name = GetGageName($ncid, $i);
    @coords = ($i);
    NetCDF::varget1($ncid, $blkvarid, \@coords, $block);
    NetCDF::varget1($ncid, $etavarid, \@coords, $eta);
    NetCDF::varget1($ncid, $xivarid, \@coords, $xi);

    printf(STDERR "%4d %-30.30s %5d %5d %5d\n", 
           $i + 1, $name, $block, $eta, $xi);
    
  }
  printf(STDERR "-----------------------------------------------------\n\n");
}

# -------------------------------------------------------------
# GetVarName
# -------------------------------------------------------------
sub GetVarName {
  my $ncid = shift;
  my $varid = shift;
  my $name;

  my $dims = undef;
  my @dimids = ();
  my $atts = undef;
  my $dtype = undef;

  NetCDF::varinq($ncid, $varid, \$name, \$dtype, \$dims, \@dimids, \$atts);

  return $name;
}

# -------------------------------------------------------------
# GetVarDesc
# -------------------------------------------------------------
sub GetVarDesc {
  my $ncid = shift;
  my $varid = shift;
  my $name;
  my $desc = "";
  my $units = "";
  my $dtype = undef;
  my $length = undef;

  $name = "Description";
  return undef if (NetCDF::attinq($ncid, $varid, $name, \$dtype, \$length) < 0);
  NetCDF::attget($ncid, $varid, $name, \$desc);
  $name = "Units";
  return undef if (NetCDF::attinq($ncid, $varid, $name, \$dtype, \$length) < 0);
  NetCDF::attget($ncid, $varid, $name, \$units);
  $desc .= ", $units";
  $desc =~ s/[\0-\37]//g;
  return $desc;
}

# -------------------------------------------------------------
# GetVarId
# -------------------------------------------------------------
sub GetVarId {
  my $ncid = shift;
  my $varname = shift;

  my $i;
  my @coords = ();
  my @length = ();

  my $found = -1;

  NetCDF::recinq($ncid, \$i, \@coords, \@length);

  foreach $i (@coords) {
    my $name = GetVarName($ncid, $i);
    if ($name eq $varname) {
      $found = $i;
      last;
    }
  }

  return $found;
}

# -------------------------------------------------------------
# VarOK
# -------------------------------------------------------------
sub VarOK {
  my $ncid = shift;
  my $gagedimid = NetCDF::dimid($ncid, "gage");
  my $timedimid = NetCDF::dimid($ncid, "time");
  my $varid = shift;

  my $name = undef;
  my $dims = undef;
  my @dimids = ();
  my $atts = undef;
  my $dtype = undef;

  NetCDF::varinq($ncid, $varid, \$name, \$dtype, \$dims, \@dimids, \$atts);

  return 1 if ($dims == 2 && $dimids[0] == $timedimid && 
               $dimids[1] == $gagedimid );
  return undef;
}

# -------------------------------------------------------------
# VarList
# -------------------------------------------------------------
sub VarList {
  my $ncid = shift;

  my $i;
  my @coords = ();
  my @length = ();

  NetCDF::recinq($ncid, \$i, \@coords, \@length);

  printf(STDERR "Available Time-Dependant Variables:\n");
  foreach $i (sort {$a <=> $b; } @coords) {
    if (VarOK($ncid, $i)) {
      my $name;
      my $desc;
      $name = GetVarName($ncid, $i);
      $desc = GetVarDesc($ncid, $i);
      printf(STDERR "%3d %-20.20s %s\n", $i + 1, $name, $desc);
    }
  }
  printf("\n");

}

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = 
  "usage: $program [-l] file \n" .
  "       $program -v var -g gage [-1] [-C|-M] [-o output] file";

my $dolist = undef;
my $doline1 = undef;
my $doasbc = undef;
my $docdf = undef;
my $var = undef;
my $varisid = undef;
my $gage = undef;
my $gageisid = undef;
my $filename = undef;

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
my %opts = ();
die "$usage\n" unless (getopts("lv:g:1MCo:", \%opts));

$dolist = 1 if ($opts{l});
if ($opts{v}) {
  $var = $opts{v};
  $varisid = 1 if ($var + 0 > 0);
}
if ($opts{g}) {
  $gage = $opts{g};
  $gageisid = 1 if ($gage + 0 > 0);
  $gage-- if ($gageisid);
}

$doline1 = 1 if ($opts{1});

$doasbc = 1 if ($opts{M});
$doline1 = 1 if ($doasbc);

if ($opts{C} && $doasbc) {
  printf(STDERR "$program: warning: -C option ignored in favor of -M option\n");
} else {
  $docdf = 1 if ($opts{C});
}


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

$filename = shift(@ARGV);

die "$usage\n" unless ($filename);

unless ($dolist) {
  die "$usage\n" unless (length($gage) > 0 && length($var) > 0);
}

# -------------------------------------------------------------
# check out the gage file to see if it's OK.
# -------------------------------------------------------------

                                # first just check to see if the file
                                # is kind of like a MASS2 gage file

my $ncid;

$ncid = NetCDF::open($filename, NetCDF::NOWRITE);
unless ($ncid > 0) {
  printf(STDERR "$program: error: unable to open NetCDF File %s\n", $filename);
  die;
}

unless (CheckGageFile($program, $ncid)) {
  printf(STDERR "$program: %s: error: not a MASS2 plot file\n", $filename);
  die;
}

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $junk;
my $i;
                                # dimensions

my $gageid = NetCDF::dimid($ncid, "gage");
my $timedimid = NetCDF::dimid($ncid, "time");
my $tsdimid = NetCDF::dimid($ncid, "tslen");
my $gages = undef;
my $times = undef;
my $tslen = undef;

                                # variables

my $timvarid = NetCDF::varid($ncid, "time");
my $tsvarid = NetCDF::varid($ncid, "timestamp");
my $varid;
my $gagenum;

my @coords;
my @length;
my @values;
my $name;

NetCDF::diminq($ncid, $gageid, \$junk, \$gages);
NetCDF::diminq($ncid, $timedimid, \$junk, \$times);
NetCDF::diminq($ncid, $tsdimid, \$junk, \$tslen);

# -------------------------------------------------------------
# if called for, list the important parts of the gage file
# -------------------------------------------------------------

if ($dolist) {

                                # describe the file

  printf(STDERR "MASS2 Gage Output File:\n\t \"%s\"\n", $filename);
  printf(STDERR "%d time slices:\n", $times);

  @coords = (0, 0 );
  @length = (1, $tslen);
  @values = ();
  NetCDF::varget($ncid, $tsvarid, \@coords, \@length, \@values);
  $name = pack("C*", @values);
  $name =~ s/\0//g;
  printf(STDERR "\tstarting: %s\n", $name);
  @coords = ($times - 1, 0 );
  @values = ();
  NetCDF::varget($ncid, $tsvarid, \@coords, \@length, \@values);
  $name = pack("C*", @values);
  $name =~ s/\0//g;
  printf(STDERR "\t  ending: %s\n", $name);
  
  GageList($ncid);
  VarList($ncid);
  exit(1);
}

                                # check to see if the requested var exists
if ($varisid) {
  $varid = $var;
  unless (VarOK($ncid, $varid)) {
    printf(STDERR "$program: error: specified variable id \"%d\" not found\n", $varid);
    exit(2);
  }
  $var = GetVarName($ncid, $varid);
} else {
  unless (($varid = GetVarId($ncid, $var)) >= 0) {
    printf(STDERR "$program: error: specified variable name \"%s\" not found\n", $var);
    exit(2);
  }
  unless (VarOK($ncid, $varid)) {
    printf(STDERR "$program: error: specified variable name \"%s\" not correct\n", $var);
    exit(2);
  }
}

                                # check to see if the requested gage exists

if ($gageisid) {
  $gagenum = $gage;
  unless (GageOK($ncid, $gagenum)) {
    printf(STDERR "$program: error: unable to find gage id \"%d\"\n", $gagenum);
    exit(2);
  }
  $gage = GetGageName($ncid, $gagenum);
} else {
  unless (($gagenum = GetGageId($ncid, $gage)) >= 0) {
    printf(STDERR "$program: error: unable to find gage name \"%s\"\n", $gage);
    exit(2);
  }
}

if ($doline1) {
  my $f;
  ($f = $filename) =~ s/.*\///;
  printf(OUTPUT "# %s Extraction: %s: %s\n", $f, $gage, GetVarDesc($ncid, $varid));
}

my $end = "";

$end = " /" if $doasbc;

                                # extract data for the gage

my @gagevalues = ();
my $value;

@coords = (0, $gagenum);
@length = ($times, 1);
NetCDF::varget($ncid, $varid, \@coords, \@length, \@gagevalues);

if ($docdf) {
  $i = 0;
  foreach $value (sort { $a <=> $b; } @gagevalues) {
    my $p = ($times - $i++) / $times;
    printf(OUTPUT "%15.5f %15.5g${end}\n", $p, $value);
  }
} else {
  for ($i = 0; $i < $times; $i++) {
    @coords = ($i, 0);
    @length = (1, $tslen);
    @values = ();
    NetCDF::varget($ncid, $tsvarid, \@coords, \@length, \@values);
    my $tstamp = pack("C*", @values);
    chop($tstamp);

    printf(OUTPUT "%s %15.5g${end}\n", $tstamp, $gagevalues[$i]);
  }
}


NetCDF::close($ncid);

