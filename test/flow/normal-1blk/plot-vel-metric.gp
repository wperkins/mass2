 # -------------------------------------------------------------
# file: plot-depth.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Fri Jan  2 11:26:49 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Velocity, m/s'
set format y "%.1f"
set yrange [0:1]
set xrange [0:3100]
set xlabel 'Longitudinal Distance, m'

set grid y
set grid x
set mytics
set pointsize 0.5
set key

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc vmag 1 5' using ($3*0.3048):($4*0.3048) title 'Initial Conditions' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -t 2 -i plot.nc vmag 1 5' using ($3*0.3048):($4*0.3048) title 'Steady State' with linespoints 1
