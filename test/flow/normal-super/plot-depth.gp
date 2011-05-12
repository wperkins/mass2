 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:30:55 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Depth, feet'
set yrange [0:6]
set format y "%.1f"
set xlabel 'Longitudinal Distance, feet'
# set grid y
# set grid x
set mytics
set pointsize 0.5
set key left

set label "Critical Depth" at 300, 1.7 font "Helvetica, 18"

set arrow from first 4000, graph 0 to first 4000, graph 1 nohead lt 7
set label "Slope Break" at 3900, 4 center rotate font "Helvetica, 18"


plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc depth 1 5' using 3:4 title 'Initial Conditions' with lines ls 3, \
     1.459 notitle with lines ls 7, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc depth 1 5' using 3:4 title 'Steady State' with linespoints ls 1
