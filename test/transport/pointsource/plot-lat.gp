# -------------------------------------------------------------
# file: plot-lat-1.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Mon Apr  5 09:56:59 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 1.0
set xlabel 'Lateral Distance, m'
set ylabel 'Concentration'
set yrange [0:0.008]
set key
plot '<perl solution.pl -x 525' using ($2*0.3048):3 title 'Analytic (x = 160)' with lines 1, \
     '<perl solution.pl -x 1025' using ($2*0.3048):3 title 'Analytic (y = 312)' with lines 3, \
     '<perl solution.pl -x 2025' using ($2*0.3048):3 title 'Analytic (y = 617)' with lines 4, \
     '<perl solution.pl -x 5025' using ($2*0.3048):3 title 'Analytic (y = 1530)' with lines 5, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 22' using ($3*0.3048):4 notitle with points 7, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 12' using ($3*0.3048):4 title "Simulated" with points 7, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 42' using ($3*0.3048):4 notitle with points 7, \
     '<perl ../../../scripts/mass2slice.pl -j -l plot.nc stuff 1 102' using ($3*0.3048):4 notitle with points 7
