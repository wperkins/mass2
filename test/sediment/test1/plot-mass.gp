# -------------------------------------------------------------
# file: plot-mass.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 18, 2001 by William A. Perkins
# Last Change: Wed Jul 18 13:44:57 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

set terminal postscript eps enhanced color dashed "Helvetica" 14

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set ylabel 'Sediment Mass, kg'
set format y2 '%.1e'
set auto y
set pointsize 0.5
set key below


plot '<perl ../../../scripts/mass2mass.pl plot.nc sediment-bed' using 1:3 title "Bed" with lines 1, \
     '<perl ../../../scripts/mass2mass.pl plot.nc sediment' using 1:3 title "Water Column" with lines 3, \
     '<perl ../../../scripts/mass2flux.pl -c -v sediment plot.nc 1 201' using 1:4 title "Outflux" with lines 4
     