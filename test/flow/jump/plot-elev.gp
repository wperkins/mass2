# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Sun Jul 23 21:27:03 2000 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set samples 2000
set ylabel 'Elevation, feet'
set auto y
set xlabel 'Longitudinal Distance, feet'
set xrange [4700:5300]
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc.1 wsel 1 5' using 3:4 title 'Initial Conditions' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc.2 wsel 1 5' using 3:4 title 'Steady State' with linespoints 3, \
     '<perl ../../../scripts/mass2slice.pl -i plot.nc.2 zbot 1 5' using 3:4 title 'Bottom' with linespoints 7