# -------------------------------------------------------------
# file: plot-mass.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 24, 2000 by William A. Perkins
# Last Change: Mon Jul 23 10:18:52 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# RCS ID: $Id$

set terminal postscript eps enhanced color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Contaminant Mass'
set format y '%7.1e'
set key below
set yrange [0:1.6e6]

plot '< perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 1' using 1:4 title 'Influx' with lines, \
     '< perl ../../../scripts/mass2flux.pl -c -v stuff plot.nc 1 101' using 1:4 title 'Outflux' with lines, \
     '< perl ../../../scripts/mass2mass.pl plot.nc stuff-bed' using 1:3 title 'Bed Pore Mass'with lines, \
     '< perl ../../../scripts/mass2mass.pl plot.nc stuff' using 1:3 title 'Water Column Mass'with lines
