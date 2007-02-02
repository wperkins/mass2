# -------------------------------------------------------------
# file: plot-conc.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 31, 2000 by William A. Perkins
# Last Change: Thu Feb  1 19:04:34 2007 by William A. Perkins <perk@mcperktop.local>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps enhanced mono dashed "Helvetica" 24

set samples 2000
set ylabel 'Concentration, kg/m^{3}'
set ytics nomirror
set format y '%.1e'
set auto y
set xlabel 'Longitudinal Distance, m'
set xrange [0:3100]
set nogrid
set mytics
set pointsize 0.5
set key

plot '<perl ../../../scripts/mass2slice.pl -l -i plot.nc stuff 1 5' using ($3*0.3048):4 title 'Dissolved' with linespoints 1, \
     '<perl ../../../scripts/mass2slice.pl -l -i plot.nc particulate 1 5' using ($3*0.3048):4 title 'Particulate' with linespoints 2
