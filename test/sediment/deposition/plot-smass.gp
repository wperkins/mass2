# -------------------------------------------------------------
# file: plot-mass.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 24, 2000 by William A. Perkins
# Last Change: Fri Jul 20 10:54:04 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# RCS ID: $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Sediment Mass'
set format y '%7.1e'
set key below

plot '< perl ../../../scripts/mass2flux.pl -c -v sediment plot.nc 1 1' using 1:4 title 'Influx' with lines, \
     '< perl ../../../scripts/mass2flux.pl -c -v sediment plot.nc 1 201' using 1:4 title 'Outflux' with lines, \
     '< perl ../../../scripts/mass2mass.pl plot.nc sediment-bed' using 1:3 title 'Bed Mass'with lines, \
     '< perl ../../../scripts/mass2mass.pl plot.nc sediment' using 1:3 title 'Water Column Mass'with lines
