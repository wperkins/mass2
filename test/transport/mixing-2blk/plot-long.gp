# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Thu May  8 12:17:42 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 0.6
set xlabel 'Longitudinal Distance, feet'
set ylabel 'Concentration'
set key below
set arrow from first 800, graph 0.0 to first 800, graph 1.0 nohead lt 0
plot '<perl ../mixing/solution.pl -y 22.5' using 1:3 title 'Analytic (y = 22.5)' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -i -t 2 plot.nc stuff 1 6 2 6' using 3:4 title "Numerical (y = 22.5)" with points ls 1, \
     '<perl ../mixing/solution.pl -y 77.5' using 1:3 title 'Analytic (y = 77.5)' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -i -t 2 plot.nc stuff 1 17 2 17' using 3:4 title "Numerical (y = 77.5)" with points ls 3
