#! /usr/unsupported/gnu/bin/perl
# -*- cperl -*-
# -------------------------------------------------------------
# file: solution.pl
# This script computes the solution to 
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu Jul  6 11:14:20 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Getopt::Std;

# -------------------------------------------------------------
# The following is blatantly plagerized from
# Author:      Peter J. Acklam
# Time-stamp:  2000-01-26 10:43:52
# E-mail:      jacklam@math.uio.no
# URL:         http://www.math.uio.no/~jacklam
# -------------------------------------------------------------
#

########################################################################
## Internal functions.
########################################################################

sub calerf($$) {
    my ($arg, $jint) = @_;
#------------------------------------------------------------------
#
# This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
#   for a real argument  x.  It contains three FUNCTION type
#   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX),
#   and one SUBROUTINE type subprogram, CALERF.  The calling
#   statements for the primary entries are:
#
#                   Y=ERF(X)     (or   Y=DERF(X)),
#
#                   Y=ERFC(X)    (or   Y=DERFC(X)),
#   and
#                   Y=ERFCX(X)   (or   Y=DERFCX(X)).
#
#   The routine  CALERF  is intended for internal packet use only,
#   all computations within the packet being concentrated in this
#   routine.  The function subprograms invoke  CALERF  with the
#   statement
#
#          CALL CALERF(ARG,RESULT,JINT)
#
#   where the parameter usage is as follows
#
#      Function                     Parameters for CALERF
#       call              ARG                  Result          JINT
#
#     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
#     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
#     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2
#
#   The main computation evaluates near-minimax approximations
#   from "Rational Chebyshev approximations for the error function"
#   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
#   transportable program uses rational functions that theoretically
#   approximate  erf(x)  and  erfc(x)  to at least 18 significant
#   decimal digits.  The accuracy achieved depends on the arithmetic
#   system, the compiler, the intrinsic functions, and proper
#   selection of the machine-dependent constants.
#
#*******************************************************************
#*******************************************************************
#
# Explanation of machine-dependent constants
#
#   XMIN   = the smallest positive floating-point number.
#   XINF   = the largest positive finite floating-point number.
#   XNEG   = the largest negative argument acceptable to ERFCX;
#            the negative of the solution to the equation
#            2*exp(x*x) = XINF.
#   XSMALL = argument below which erf(x) may be represented by
#            2*x/sqrt(pi)  and above which  x*x  will not underflow.
#            A conservative value is the largest machine number X
#            such that   1.0 + X = 1.0   to machine precision.
#   XBIG   = largest argument acceptable to ERFC;  solution to
#            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where
#            W(x) = exp(-x*x)/[x*sqrt(pi)].
#   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
#            machine precision.  A conservative value is
#            1/[2*sqrt(XSMALL)]
#   XMAX   = largest acceptable argument to ERFCX; the minimum
#            of XINF and 1/[sqrt(pi)*XMIN].
#
#   Approximate values for some important machines are:
#
#                          XMIN       XINF        XNEG     XSMALL
#
#  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15
#  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15
#  IEEE (IBM/XT,
#    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8
#  IEEE (IBM/XT,
#    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16
#  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17
#  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18
#  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17
#  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16
#
#
#                          XBIG       XHUGE       XMAX
#
#  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293
#  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465
#  IEEE (IBM/XT,
#    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37
#  IEEE (IBM/XT,
#    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307
#  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75
#  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307
#  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38
#  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307
#
#*******************************************************************
#*******************************************************************
#
# Error returns
#
#  The program returns  ERFC = 0      for  ARG .GE. XBIG;
#
#                       ERFCX = XINF  for  ARG .LT. XNEG;
#      and
#                       ERFCX = 0     for  ARG .GE. XMAX.
#
#
#  Author: W. J. Cody
#          Mathematics and Computer Science Division
#          Argonne National Laboratory
#          Argonne, IL 60439
#
#  Latest modification: March 19, 1990
#
# Translation to Perl by Peter J. Acklam, December 3, 1999
#
#------------------------------------------------------------------
    my ($x, $y, $ysq, $xnum, $xden, $result, $del);
#------------------------------------------------------------------
#  Mathematical constants
#------------------------------------------------------------------
    my $sqrpi = 5.6418958354775628695e-1;
    my $thresh = 0.46875;
#------------------------------------------------------------------
#  Machine-dependent constants
#------------------------------------------------------------------
    my $xinf   = 1.79e308;
    my $xneg   = -26.628;
    my $xsmall = 1.11e-16;
    my $xbig   = 26.543;
    my $xhuge  = 6.71e7;
    my $xmax   = 2.53e307;
#------------------------------------------------------------------
#  Coefficients for approximation to  erf  in first interval
#------------------------------------------------------------------
    my @a = (3.16112374387056560e00, 1.13864154151050156e02,
             3.77485237685302021e02, 3.20937758913846947e03,
             1.85777706184603153e-1);
    my @b = (2.36012909523441209e01, 2.44024637934444173e02,
             1.28261652607737228e03, 2.84423683343917062e03);
#------------------------------------------------------------------
#  Coefficients for approximation to  erfc  in second interval
#------------------------------------------------------------------
    my @c=(5.64188496988670089e-1, 8.88314979438837594e00,
           6.61191906371416295e01, 2.98635138197400131e02,
           8.81952221241769090e02, 1.71204761263407058e03,
           2.05107837782607147e03, 1.23033935479799725e03,
           2.15311535474403846e-8);
    my @d=(1.57449261107098347e01, 1.17693950891312499e02,
           5.37181101862009858e02, 1.62138957456669019e03,
           3.29079923573345963e03, 4.36261909014324716e03,
           3.43936767414372164e03, 1.23033935480374942e03);
#------------------------------------------------------------------
#  Coefficients for approximation to  erfc  in third interval
#------------------------------------------------------------------
    my @p=(3.05326634961232344e-1, 3.60344899949804439e-1,
           1.25781726111229246e-1, 1.60837851487422766e-2,
           6.58749161529837803e-4, 1.63153871373020978e-2);
    my @q=(2.56852019228982242e00, 1.87295284992346047e00,
           5.27905102951428412e-1, 6.05183413124413191e-2,
           2.33520497626869185e-3);
#------------------------------------------------------------------
    $x = $arg;
    $y = abs($x);
    if ($y <= $thresh) {
#------------------------------------------------------------------
#  Evaluate  erf  for  |X| <= 0.46875
#------------------------------------------------------------------
        $ysq = 0;
        if ($y >= $xsmall) {$ysq = $y * $y}
        $xnum = $a[4]*$ysq;
        $xden = $ysq;
        foreach my $i (0 .. 2) {
            $xnum = ($xnum + $a[$i]) * $ysq;
            $xden = ($xden + $b[$i]) * $ysq;
        }
        $result = $x * ($xnum + $a[3]) / ($xden + $b[3]);
        if ($jint != 0) {$result = 1 - $result}
        if ($jint == 2) {$result = exp($ysq) * $result}
        return $result;
#------------------------------------------------------------------
#  Evaluate  erfc  for 0.46875 <= |X| <= 4.0
#------------------------------------------------------------------
    } elsif ($y <= 4) {
        $xnum = $c[8]*$y;
        $xden = $y;
        foreach my $i (0 .. 6) {
            $xnum = ($xnum + $c[$i]) * $y;
            $xden = ($xden + $d[$i]) * $y;
        }
        $result = ($xnum + $c[7]) / ($xden + $d[7]);
        if ($jint != 2) {
            $ysq = int($y*16)/16;
            $del = ($y-$ysq)*($y+$ysq);
            $result = exp(-$ysq*$ysq) * exp(-$del) * $result;
        }
#------------------------------------------------------------------
#  Evaluate  erfc  for |X| > 4.0
#------------------------------------------------------------------
    } else {
        $result = 0;
        if ($y >= $xbig) {
            if (($jint != 2) || ($y >= $xmax)) {
                goto FIXUP;
            }
            if ($y >= $xhuge) {
                $result = $sqrpi / $y;
                goto FIXUP;
            }
        }
        $ysq = 1 / ($y * $y);
        $xnum = $p[5]*$ysq;
        $xden = $ysq;
        foreach my $i (0 .. 3) {
            $xnum = ($xnum + $p[$i]) * $ysq;
            $xden = ($xden + $q[$i]) * $ysq;
        }
        $result = $ysq *($xnum + $p[4]) / ($xden + $q[4]);
        $result = ($sqrpi -  $result) / $y;
        if ($jint != 2) {
            $ysq = int($y*16)/16;
            $del = ($y-$ysq)*($y+$ysq);
            $result = exp(-$ysq*$ysq) * exp(-$del) * $result;
        }
    }
#------------------------------------------------------------------
#  Fix up for negative argument, erf, etc.
#------------------------------------------------------------------
  FIXUP: {
        if ($jint == 0) {
            $result = (0.5 - $result) + 0.5;
            if ($x <= 0) {$result = -$result}
        } elsif ($jint == 1) {
            if ($x <= 0) {$result = 2 - $result}
        } else {
            if ($x <= 0) {
                if ($x <= $xneg) {
                    $result = $xinf;
                } else {
                    $ysq = int($x*16)/16;
                    $del = ($x-$ysq)*($x+$ysq);
                    $y = exp($ysq*$ysq) * exp($del);
                    $result = ($y+$y) - $result;
                }
            }
        }
    }
    return $result;
}

#--------------------------------------------------------------------
#
# This subprogram computes approximate values for erf(x).
#   (see comments heading CALERF).
#
#   Author/date: W. J. Cody, January 8, 1985
#
#   Perl translation: Peter J. Acklam
#
#--------------------------------------------------------------------
sub erf ($) {
    my $x = @_ ? shift : $_;
    my $jint = 0;
    calerf($x, $jint);
}

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program [-x X] [-y Y]";

                                # some constants that define the problem

my $W = 100.0;
my $L = 4000.0;
my $sigma_t = 3.0;
my $u = 1.67;
my $Co = 100.0;
my $Cb = 100.0;

my $deltax = 25.0;
my $deltay = 2.5;

my $x = undef;
my $y = undef;

my $inf = 8;
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

my $startx = 0.0;
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
    my $xprime = $x*$sigma_t/($u*$W**2);
    my $yprime = $y/$W;
    my $n;
    my $C = 0;

    $xprime = 0.001 if ($xprime == 0.0);

    for ($n = -$inf; $n <= $inf; $n++) {
      $C += 
        erf( ($yprime + 0.5 + 2*$n)/sqrt(4*$xprime) ) -
        erf( ($yprime - 0.5 + 2*$n)/sqrt(4*$xprime) );
    }
    $C *= $Co/2.0;
    $C += $Cb;

    printf(OUTPUT "%.3f %.3f %.3f\n", $x, $y, $C);
  }
}
