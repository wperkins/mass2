# -------------------------------------------------------------
# file: plot-shear.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 30, 2000 by William A. Perkins
# Last Change: Wed Aug 30 08:50:44 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set samples 2000
set ylabel 'Bottom Shear Stress, lb_{f}/ft^{2}'
set ytics nomirror
set format y '%.1e'
set auto y
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc shear 1 5' using 3:4 axes x1y1 title 'Initial Conditions' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc shear 1 5' using 3:4 axes x1y1 title 'Steady State' with linespoints 3
