# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Mon Aug  7 21:15:50 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 0.6
set xlabel 'Longitudinal Distance, feet'
set ylabel 'Concentration'
set key below
plot '<perl solution.pl -y 75' using 1:3 title 'Analytic (y = 75)' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 9' using 3:4 title "Numerical (y = 75)" with points 1, \
     '<perl solution.pl -y 125' using 1:3 title 'Analytic (y = 125)' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 14' using 3:4 title "Numerical (y = 125)" with points 3, \
     '<perl solution.pl -y 155' using 1:3 title 'Analytic (y = 155)' with lines 4, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 17' using 3:4 title "Numerical (y = 155)" with points 4


