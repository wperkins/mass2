# -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 31, 2000 by William A. Perkins
# Last Change: Thu Aug 31 08:58:53 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set samples 2000
set ylabel 'Concentration, kg/m^{3}'
set ytics nomirror
set format y '%.1e'
set auto y
set xlabel 'Longitudinal Distance, feet'
set grid y
set grid x
set mytics
set pointsize 0.5
set key below

plot '<perl ../../../scripts/mass2slice.pl -l -i plot.nc stuff 1 5' using 3:4 title 'Dissolved' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc particulate 1 5' using 3:4 title 'Particulate' with linespoints 3
