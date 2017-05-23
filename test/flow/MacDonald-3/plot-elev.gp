# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2015-04-30 10:04:28 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set pointsize 0.75


plot "<python ../../../scripts/profile.py -J 36 plot000.cgns zbot" using ($5*0.3048):($6*0.3048) title "Bottom" with lines ls 7, \
     "<python ../../../scripts/profile.py -J 36 plot000.cgns wsel" every 30 using ($5*0.3048):($6*0.3048) title "Simulated" with points ls 1, \
     "<python ../../../scripts/profile.py -J 36 plot000.cgns zbot" using ($5*0.3048):($6*0.3048 + (1.125+0.25*sin(pi*$3*0.3048/500))) title "Analytic" with lines ls 3
     
