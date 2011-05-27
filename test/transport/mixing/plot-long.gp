# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu Feb  1 19:03:46 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 0.6
set xlabel 'Longitudinal Distance, m'
set xrange [0:1250]
set ylabel 'Concentration'
set key
plot '<perl solution.pl -y 22.5' using ($1*0.3048):3 title 'Analytic (y = 6.9)' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -i -t 2 plot.nc stuff 1 6' using ($3*0.3048):4 title "Numerical (y = 6.9)" with points ls 1, \
     '<perl solution.pl -y 77.5' using ($1*0.3048):3 title 'Analytic (y = 23.6)' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -i -t 2 plot.nc stuff 1 17' using ($3*0.3048):4 title "Numerical (y = 23.6)" with points ls 3
