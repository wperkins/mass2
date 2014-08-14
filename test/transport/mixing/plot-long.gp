# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: 2014-08-13 15:28:07 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 0.6
set xlabel 'Longitudinal Distance, m'
set xrange [0:1250]
set ylabel 'Concentration'
set key
plot '<perl solution.pl -y 22.5' using ($1*0.3048):3 title 'Analytic (y = 6.9)' with lines ls 1, \
     'probe1y.dat' every ::20 using ($1*0.3048):17 title "Numerical (y = 6.9)" with points ls 1, \
     '<perl solution.pl -y 77.5' using ($1*0.3048):3 title 'Analytic (y = 23.6)' with lines ls 3, \
     'probe2y.dat' every ::20 using ($1*0.3048):17 title "Numerical (y = 23.6)" with points ls 3
