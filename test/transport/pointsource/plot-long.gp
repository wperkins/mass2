# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: 2014-08-13 14:24:16 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 0.6
set xlabel 'Longitudinal Distance, m'
set xrange [0:3100]
set ylabel 'Concentration'
set key
plot '<perl solution.pl -y 75' using ($1*0.3048):3 title 'Analytic (y = 75)' with lines ls 1, \
     '<perl solution.pl -y 125' using ($1*0.3048):3 title 'Analytic (y = 125)' with lines ls 3, \
     '<perl solution.pl -y 155' using ($1*0.3048):3 title 'Analytic (y = 155)' with lines ls 4, \
     'probe1y.dat' every ::20 using ($1*0.3048):17 title "Simulated" with points ls 7, \
     'probe2y.dat' every ::20 using ($1*0.3048):17 notitle with points ls 7, \
     'probe3y.dat' every ::20 using ($1*0.3048):17 notitle with points ls 7

