# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu May  8 12:37:07 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 1.0
set xlabel 'Lateral Distance, feet'
set ylabel 'Concentration'
set yrange [100:200]
set key below
plot '<perl ../mixing/solution.pl -x 430' using 2:3 title 'Analytic (x = 430)' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 1 23' using 3:4 title "Numerical (y = 430)" with points 1, \
     '<perl ../mixing/solution.pl -x 1020' using 2:3 title 'Analytic (y = 1020)' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 2 7' using 3:4 title "Numerical (y = 1020)" with points 3
