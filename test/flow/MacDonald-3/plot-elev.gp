# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Wed May 11 11:36:01 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, ft"
set ylabel "Elevation, ft"
set pointsize 0.5


plot "<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 36" using ($3*0.3048):($4*0.3048) title "Bottom" with lines ls 7, \
     "<perl ../../../scripts/mass2slice.pl -i -l plot.nc wsel 1 36" using ($3*0.3048):($4*0.3048) title "Simulated" with points ls 1, \
     "<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 36" using ($3*0.3048):($4*0.3048 + (1.125+0.25*sin(pi*$3*0.3048/500))) title "Analytic" with lines ls 3
     
