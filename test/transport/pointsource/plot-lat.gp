# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Mon Aug  7 15:42:14 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 1.0
set xlabel 'Lateral Distance, feet'
set ylabel 'Concentration'
# set yrange [100:200]
set key below
plot '<perl solution.pl -x 525' using 2:3 title 'Analytic (x = 525)' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 12' using 3:4 title "Numerical (y = 525)" with points 1, \
     '<perl solution.pl -x 1025' using 2:3 title 'Analytic (y = 1025)' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 22' using 3:4 title "Numerical (y = 1025)" with points 3, \
     '<perl solution.pl -x 2025' using 2:3 title 'Analytic (y = 2025)' with lines 4, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 42' using 3:4 title "Numerical (y = 2025)" with points 4, \
     '<perl solution.pl -x 5025' using 2:3 title 'Analytic (y = 5025)' with lines 5, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 102' using 3:4 title "Numerical (y = 5025)" with points 5
