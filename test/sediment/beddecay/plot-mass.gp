# -------------------------------------------------------------
# file: plot-mass.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 14, 2000 by William A. Perkins
# Last Change: Mon Jul 23 14:29:22 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Contaminant Mass'
set yrange [*:250]
set xrange ['04-01-1996 00:00:00' : '04-02-1996 00:00:00' ]

set pointsize 1.0
set key below

plot 'stuff-mass-14.dat' using 1:3 title 'Influx' with lines, \
     '<perl ../../../scripts/mass2mass.pl plot.nc stuff-silt-bed' using 1:3 title 'Particulate Bed Mass' with lines, \
     '<perl ../../../scripts/mass2mass.pl plot.nc stuff-bed' using 1:3 title 'Dissolved Bed Mass' with lines, \
     '<perl ../../../scripts/mass2mass.pl plot.nc stuff' using 1:3 title 'Water Column Mass' with lines, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 101' using 1:4 title 'Outflux' with lines