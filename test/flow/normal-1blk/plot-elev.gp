# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Tue Nov 25 08:28:30 2003 by William A. Perkins <perk@leechong.pnl.gov>
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
set key screen 0.8, 0.4

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc wsel 1 5' using 3:4 title 'Initial Conditions' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc wsel 1 5' using 3:4 title 'Steady State' with linespoints 3, \
     '<perl ../../../scripts/mass2slice.pl -i plot.nc zbot 1 5' using 3:4 title 'Bottom' with lines 7

