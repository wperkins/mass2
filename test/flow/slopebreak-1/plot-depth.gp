 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Thu Feb  1 19:16:56 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Depth, ft'
set format y '%.1f'
# set yrange [3:6]
set auto y
set xlabel 'Longitudinal Distance, ft'
# set grid y
# set grid x
set mytics
set pointsize 0.5
# set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc depth 1 5' using 3:4 title 'Initial Conditions' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.nc depth 1 5' using 3:4 title 'Steady State' with linespoints 1
