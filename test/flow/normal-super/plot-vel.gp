 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed Dec 31 09:46:04 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Velocity, feet/second'
# set yrange [0:3]
set xlabel 'Longitudinal Distance, feet'
# set grid y
# set grid x
# set mytics
set pointsize 0.5
set key left

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc vmag 1 5' using 3:4 title 'Initial Conditions' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc vmag 1 5' using 3:4 title 'Steady State' with linespoints 3
