# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: 2014-08-13 15:23:24 d3g096
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
     'probe3x.dat' every ::20 using ($2*0.3048):17 title "Numerical (x = 616)" with points ls 1, \
     '<perl solution.pl -x 3020' using ($2*0.3048):3 title 'Analytic (x = 920)' with lines ls 2, \
     'probe4x.dat' every ::20 using ($2*0.3048):17 title "Numerical (x = 920)" with points ls 2
