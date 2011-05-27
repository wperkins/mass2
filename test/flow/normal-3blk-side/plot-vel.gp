 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:19:38 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set samples 2000
set ylabel 'Depth, feet'
set yrange [0:3]
# set auto y
set xlabel 'Velocity, feet/second'
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc vmag 1 5 2 10 3 5' using 3:4 title 'Initial Conditions' with linespoints ls 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc vmag 1 5 2 10 3 5' using 3:4 title 'Steady State' with linespoints ls 3
