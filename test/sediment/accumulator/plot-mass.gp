# -------------------------------------------------------------
# file: plot-mass.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 14, 2000 by William A. Perkins
# Last Change: Wed Nov 15 09:29:29 2000 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------
# $Id$

set term postscript eps color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Sediment Mass'

set pointsize 1.0
set key below

plot '<perl ../../../scripts/mass2flux.pl -c -v silt plot.inst.nc 1 1' using 1:4 title 'Influx' with lines, \
     '<perl ../../../scripts/mass2flux.pl -c -v silt plot.inst.nc 1 101' using 1:4 title 'Outflux' with lines, \
     '<perl ../../../scripts/mass2mass.pl plot.inst.nc silt-bed' using 1:3 title 'Bed Storage' with lines

