# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2015-04-30 10:12:23 d3g096
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set yrange [0.8:1.5]
set ylabel "Depth, m"
set format y "%.1f"
set pointsize 0.5

# d(x) = (9./8. + 1./4.*sin(3.141569*x*0.3048/500.))/0.3048
d(x) = (9./8. + 1./4.*sin(3.141569*x/500.))

plot "<python ../../../scripts/profile.py -J 26 plot000.cgns depth" using ($5*0.3048):($6*0.3048) title "Simulated" with points ls 1, \
     d(x) title "Analytic" with lines ls 3
     
