# -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: 2014-06-24 09:05:01 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

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
set format y '%.1f'
set auto y
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set pointsize 1.0
set key below

plot "<awk 'NR > 20 {print $1, $17}' probe.dat" using 1:2 title 'Simulated' with points ls 3, \
     C(x) title 'Analytic Solution' with lines ls 1