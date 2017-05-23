# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2015-04-30 10:11:29 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set xrange [0:1000]
set yrange [0.5:1.5]
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 0.5

set key bottom

plot "<python ../../../scripts/profile.py -J 36 plot000.cgns depth" every 15 using ($5*0.3048):($6*0.3048) title "Simulated" with points ls 3, \
     '<perl mkgrid.pl -s' using ($1*0.3048):($2*0.3048) title "Analytic" with lines ls 7
     
