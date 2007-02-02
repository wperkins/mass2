# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu Mar 25 13:50:05 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 1.0
set xlabel 'Lateral Distance, feet'
set ylabel 'Concentration'
set yrange [100:200]
set key below
plot '<perl ../mixing/solution.pl -x 420' using 2:3 title 'Analytic (x = 420)' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 12' using 3:4 title "Numerical (y = 420)" with points 1, \
     '<perl ../mixing/solution.pl -x 1020' using 2:3 title 'Analytic (y = 1020)' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 27' using 3:4 title "Numerical (y = 1020)" with points 3
