# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Wed May 11 11:37:20 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps enhanced mono dashed "Helvetica" 22

set xlabel "Longitudinal Distance, m"
set ylabel "Elevation, m"
set pointsize 0.5
set xrange [0:1000]
set yrange [0:*]


plot "<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 26" using ($3*0.3048):($4*0.3048) title "Bottom" with lines ls 7, \
     "<perl ../../../scripts/mass2slice.pl -i -l plot.nc wsel 1 26" using ($3*0.3048):($4*0.3048) title "Simulated" with lines ls 3, \
     '<perl mkgrid.pl -s' using ($1*0.3048):($5*0.3048) title "Analytic" with lines ls 1
     
