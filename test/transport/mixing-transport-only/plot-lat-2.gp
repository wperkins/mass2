# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Wed Jul 26 14:59:34 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set pointsize 1.0
set xlabel 'Lateral Distance, feet'
set ylabel 'Concentration'
set yrange [140:160]
set key below
plot '<perl ../mixing/solution.pl -x 2020' using 2:3 title 'Analytic (x = 2020)' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 1 52' using 3:4 title "Numerical (y = 2020)" with points ls 1, \
     '<perl ../mixing/solution.pl -x 3020' using 2:3 title 'Analytic (y = 3020)' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -j -t 2 plot.nc stuff 1 77' using 3:4 title "Numerical (y = 3020)" with points ls 3
