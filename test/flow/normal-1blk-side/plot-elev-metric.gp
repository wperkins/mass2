# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Mon Feb 23 13:16:55 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Elevation, m'
set auto y
set xlabel 'Longitudinal Distance, m'
set xrange [0:3100]

# set grid y
# set grid x
set mytics
set pointsize 0.5
set key screen 0.8, 0.4

plot '<perl ../../../scripts/mass2slice.pl -t 1 -j plot.nc wsel 1 5' using ($3*0.3048):($4*0.3048) title 'Initial Conditions' with lines 3, \
     '<perl ../../../scripts/mass2slice.pl -l -j plot.nc wsel 1 5' using ($3*0.3048):($4*0.3048) title 'Steady State' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -j plot.nc zbot 1 5' using ($3*0.3048):($4*0.3048) title 'Bottom' with lines 7

