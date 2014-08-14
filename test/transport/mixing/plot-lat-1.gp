# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: 2014-08-13 15:24:40 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 24

set pointsize 1.0
set xlabel 'Lateral Distance, m'
set ylabel 'Concentration'
set yrange [100:200]
set xrange [0:31]
set key
plot '<perl solution.pl -x 420' using ($2*0.3048):3 title 'Analytic (x = 128)' with lines ls 1, \
     'probe1x.dat' using ($2*0.3048):17 title "Numerical (x = 128)" with points ls 1, \
     '<perl solution.pl -x 1020' using ($2*0.3048):3 title 'Analytic (x = 311)' with lines ls 2, \
     'probe2x.dat' using ($2*0.3048):17 title "Numerical (x = 311)" with points ls 2
