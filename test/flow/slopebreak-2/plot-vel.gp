 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed Jul  5 09:53:18 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set samples 2000
set ylabel 'Velocity, fps'
set format y '%.1f'
# set yrange [3:6]
set auto x
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc vmag 1 5' using 3:4 title 'Initial Conditions' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.nc vmag 1 5' using 3:4 title 'Steady State' with linespoints 3

