# -------------------------------------------------------------
# file: plot-froude.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 23, 2000 by William A. Perkins
# Last Change: Sun Jul 23 21:26:51 2000 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------

# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set samples 2000
set ylabel 'Froude Number'
set xlabel 'Longitudinal Distance, feet'
set xrange [4700:5300]
set grid y
set grid x
set mytics
set pointsize 0.5
set nokey

plot '<perl ../../../scripts/mass2slice.pl -l -i plot.nc.2 froude 1 5' using 3:4 title 'Steady State' with linespoints 1
