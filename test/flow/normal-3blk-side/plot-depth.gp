 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:19:00 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Depth, feet'
set yrange [2:6]
set format y "%.1f"
set xlabel 'Longitudinal Distance, feet'
# set grid y
# set grid x
set mytics
set pointsize 0.5
set key 

set arrow from first 4000, graph 0 to first 4000, graph 1 nohead lt 7
set arrow from first 6000, graph 0 to first 6000, graph 1 nohead lt 7

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc depth 1 5 2 10 3 5' using 3:4 title 'Initial Conditions' with lines ls 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc depth 1 5 2 10 3 5' using 3:4 title 'Steady State' with linespoints ls 3
