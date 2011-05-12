# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu Feb  1 19:03:47 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 24

set pointsize 1.0
set xlabel 'Lateral Distance, m'
set ylabel 'Concentration'
set yrange [140:160]
set xrange [0:31]
set key
plot '<perl solution.pl -x 2020' using ($2*0.3048):3 title 'Analytic (x = 616)' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 1 52' using ($3*0.3048):4 title "Numerical (x = 616)" with points ls 1, \
     '<perl solution.pl -x 3020' using ($2*0.3048):3 title 'Analytic (x = 920)' with lines ls 2, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 1 77' using ($3*0.3048):4 title "Numerical (x = 920)" with points ls 2
