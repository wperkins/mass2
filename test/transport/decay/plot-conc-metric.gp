 # -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Mon Jan 19 14:41:28 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

u = 2.0
K = 0.2
lamda = 6.9315E-05
lamda = 1.386E-04
Ci = 10
a = 4*K*lamda/u**2
Co = Ci*((2/a)*(sqrt(a + 1) - 1))
C(x) = Co*exp(-((u*x)/(2*K))*(sqrt(a+1) - 1))
# C2(x) = Ci*exp(-lamda*x/u)

set samples 2000
set ylabel 'Concentration'
set yrange [4.9:10.1]
set format y '%.1f'
set xlabel 'Longitudinal Distance, feet'
set xrange [0:3100]
set pointsize 0.5
set key

plot '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.nc stuff 1 5' using ($3*0.3048):4 title 'Simulated' with points 3, \
     C(x/0.3048) title 'Analytic Solution' with lines 1