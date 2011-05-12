# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed May 11 07:14:11 2011 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Elevation, feet'
set auto y
set xlabel 'Longitudinal Distance, feet'
# set grid y
# set grid x
set mytics
set pointsize 0.5
set key at screen 0.8, 0.4

plot '<perl ../../../scripts/mass2slice.pl -t 1 -j plot.nc wsel 1 5' using 3:4 title 'Initial Conditions' with linespoints ls 1, \
     '<perl ../../../scripts/mass2slice.pl -l -j plot.nc wsel 1 5' using 3:4 title 'Steady State' with linespoints ls 3, \
     '<perl ../../../scripts/mass2slice.pl -j plot.nc zbot 1 5' using 3:4 title 'Bottom' with lines ls 7

