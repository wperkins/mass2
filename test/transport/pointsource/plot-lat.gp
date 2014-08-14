# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: 2014-08-13 14:19:14 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 1.0
set xlabel 'Lateral Distance, m'
set ylabel 'Concentration'
set yrange [0:0.008]
set key
plot '<perl solution.pl -x 525' using ($2*0.3048):3 title 'Analytic (x = 160)' with lines ls 1, \
     '<perl solution.pl -x 1025' using ($2*0.3048):3 title 'Analytic (x = 312)' with lines ls 3, \
     '<perl solution.pl -x 2025' using ($2*0.3048):3 title 'Analytic (x = 617)' with lines ls 4, \
     '<perl solution.pl -x 5025' using ($2*0.3048):3 title 'Analytic (x = 1530)' with lines ls 5, \
     'probe1x.dat' every ::20 using ($2*0.3048):17 notitle with points ls 7, \
     'probe2x.dat' every ::20 using ($2*0.3048):17 title "Simulated" with points ls 7, \
     'probe3x.dat' every ::20 using ($2*0.3048):17 notitle with points ls 7, \
     'probe4x.dat' every ::20 using ($2*0.3048):17 notitle with points ls 7
