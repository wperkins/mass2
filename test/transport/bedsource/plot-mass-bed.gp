# -------------------------------------------------------------
# file: plot-mass-bed.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August  4, 2000 by William A. Perkins
# Last Change: Mon Aug  7 08:06:36 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set pointsize 0.5

set xrange ['04-01-1985 03:00:00': '04-01-1985 11:00:00']
set grid x
set ylabel 'Species Mass'
set grid y

set title 'Bed Source'
set key below

plot '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 202' using 1:4 title 'Outflux' with linespoints 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 101' using 1:4 title 'i = 101'  with lines 5, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 10' using 1:4 title 'i = 10' with lines 3, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 5' using 1:4 title 'i = 5' with lines 7, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 3' using 1:4 title 'i = 3' with lines 8, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 2' using 1:4 title 'i = 2' with lines 9, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.bed.nc 1 1' using 1:4 title 'i = 1' with linespoints 4


