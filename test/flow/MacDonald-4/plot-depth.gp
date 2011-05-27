# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: Wed May 11 11:37:11 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
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

plot "<perl ../../../scripts/mass2slice.pl -i -l plot.nc depth 1 26" using ($3*0.3048):($4*0.3048) title "Simulated" with points ls 3, \
     '<perl mkgrid.pl -s' using ($1*0.3048):($2*0.3048) title "Analytic" with lines ls 7
     
