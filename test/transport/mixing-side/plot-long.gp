# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu Mar 25 13:48:14 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 0.6
set xlabel 'Longitudinal Distance, feet'
set ylabel 'Concentration'
set key below
plot '<perl ../mixing/solution.pl -y 22.5' using 1:3 title 'Analytic (y = 22.5)' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 6' using 3:4 title "Numerical (y = 22.5)" with points ls 1, \
     '<perl ../mixing/solution.pl -y 77.5' using 1:3 title 'Analytic (y = 77.5)' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 17' using 3:4 title "Numerical (y = 77.5)" with points ls 3
