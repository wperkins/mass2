# -------------------------------------------------------------
# file: plot-mass-bed.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August  4, 2000 by William A. Perkins
# Last Change: Mon Aug  7 12:53:31 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set pointsize 0.5

set xrange ['04-01-1985 03:00:00': '04-01-1985 11:00:00']
set grid x
set ylabel 'Species Mass'
# set yrange [0:250]
set grid y

set title 'Multiple Bed Sources'
set key below

plot '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 202' using 1:4 title 'Stuff Outflux' with linespoints ls 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 2' using 1:4 title 'Stuff: i = 2' with lines ls 3, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 1' using 1:4 title 'Stuff: i = 1' with linespoints ls 4, \
     '<perl ../../../scripts/mass2flux.pl -c -v morestuff plot.nc 1 202' using 1:4 title 'More Stuff Outflux' with linespoints ls 5, \
     '<perl ../../../scripts/mass2flux.pl -c -v morestuff plot.nc 1 2' using 1:4 title 'More Stuff: i = 2' with lines ls 7, \
     '<perl ../../../scripts/mass2flux.pl -c -v morestuff plot.nc 1 1' using 1:4 title 'More Stuff: i = 1' with linespoints ls 8


