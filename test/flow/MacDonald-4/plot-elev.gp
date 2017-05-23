# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2015-04-30 10:16:09 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set pointsize 0.75
set xrange [0:1000]
set yrange [0:*]


plot "<python ../../../scripts/profile.py -J 26 plot000.cgns zbot" using ($5*0.3048):($6*0.3048) title "Bottom" with lines ls 7, \
     "<python ../../../scripts/profile.py -J 26 plot000.cgns wsel" every 25 using ($5*0.3048):($6*0.3048) title "Simulated" with points ls 3, \
     '<perl mkgrid.pl -s' using ($1*0.3048):($5*0.3048) title "Analytic" with lines ls 1
     
