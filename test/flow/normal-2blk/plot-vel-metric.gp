 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:05:08 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$


set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Velocity, m/s'
set format y "%.1f"
set yrange [0:1]
# set auto y
set xrange [0:3100]
set xlabel 'Longitudinal Distance, m'
set pointsize 0.5
set key 

set arrow from first 1524, graph 0.0 to first 1524, graph 1.0 nohead lt 0 

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc vmag 1 5 2 5' using ($3*0.3048):($4*0.3048) title 'Initial Conditions' with lines ls 3, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc vmag 1 5 2 5' using ($3*0.3048):($4*0.3048) title 'Steady State' with linespoints ls 1
