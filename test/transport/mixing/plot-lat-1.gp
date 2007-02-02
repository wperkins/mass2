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
set yrange [100:200]
set xrange [0:31]
set key
plot '<perl solution.pl -x 420' using ($2*0.3048):3 title 'Analytic (x = 128)' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 12' using ($3*0.3048):4 title "Numerical (x = 128)" with points 1, \
     '<perl solution.pl -x 1020' using ($2*0.3048):3 title 'Analytic (x = 311)' with lines 2, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 27' using ($3*0.3048):4 title "Numerical (x = 311)" with points 2
