 # -------------------------------------------------------------
# file: plot-sed.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Thu Aug 31 11:20:38 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set samples 2000
set ylabel 'Steady State Concentration, kg/m^{3}'
set ytics mirror
set format y '%.1f'
set auto y
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set mytics
set pointsize 0.3
set key below

plot '<perl ../../../scripts/mass2slice.pl -l -i plot.nc sediment 1 5' using 3:4 axes x1y1 title 'Sediment' with lines 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc stuff 1 5' using 3:4 axes x1y1 title 'Dissolved' with linespoints 3, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc particulate 1 5' using 3:4 axes x1y1 title 'Particulate' with points 4
