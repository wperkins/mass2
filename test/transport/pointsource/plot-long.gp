# -------------------------------------------------------------
# file: plot-long.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  6, 2000 by William A. Perkins
# Last Change: Mon Apr  5 09:59:30 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps mono dashed "Helvetica" 24

set pointsize 0.6
set xlabel 'Longitudinal Distance, m'
set xrange [0:3100]
set ylabel 'Concentration'
set key
plot '<perl solution.pl -y 75' using ($1*0.3048):3 title 'Analytic (y = 75)' with lines 1, \
     '<perl solution.pl -y 125' using ($1*0.3048):3 title 'Analytic (y = 125)' with lines 3, \
     '<perl solution.pl -y 155' using ($1*0.3048):3 title 'Analytic (y = 155)' with lines 4, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 9' using ($3*0.3048):4 title "Simulated" with points 7, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 14' using ($3*0.3048):4 notitle with points 7, \
     '<perl ../../../scripts/mass2slice.pl -i -l plot.nc stuff 1 17' using ($3*0.3048):4 notitle with points 7

