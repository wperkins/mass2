 # -------------------------------------------------------------
# file: plot-sed.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 27, 2000 by William A. Perkins
# Last Change: Wed Aug 30 08:38:35 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set samples 2000
set ylabel 'Sediment Concentration, kg/m^{3}'
set ytics nomirror
set format y '%.1e'
set auto y
set y2label 'Erosion/Deposition Rate, kg/ft^{2}/s'
set y2tics nomirror
set format y2 '%.1e'
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -t 1 -i plot.nc sediment 1 5' using 3:4 axes x1y1 title 'Initial Conditions Concentration' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc sediment 1 5' using 3:4 axes x1y1 title 'Steady State Concentration' with linespoints 3, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc sediment-depos 1 5' using 3:4 axes x1y2 title 'Steady State Deposition' with linespoints 4, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc sediment-erode 1 5' using 3:4 axes x1y2 title 'Steady State Erosion' with linespoints 5 \
