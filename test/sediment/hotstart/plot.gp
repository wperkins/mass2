# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 12, 2000 by William A. Perkins
# Last Change: Tue Jul 24 12:15:58 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set pointsize 0.5
set key below


                                # first lets do a stuff mass balance

plot '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.depos.nc 1 1' using 1:4 title 'Dissolved Stuff Influx' with lines 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff-silt plot.depos.nc 1 1' using 1:4 title 'Particulate Stuff Influx' with linespoints 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.depos.nc 1 101' using 1:4 title 'Dissolved Stuff Outflux' with lines 3, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff-silt plot.depos.nc 1 101' using 1:4 title 'Particulate Stuff Outflux' with linespoints 3, \
     '<perl ../../../scripts/mass2mass.pl plot.depos.nc stuff-silt-bed' using 1:3 title 'Particulate Stuff in Bed' with linespoints 4, \
     '<perl ../../../scripts/mass2mass.pl plot.depos.nc stuff-bed' using 1:3 title 'Dissolved Stuff in Bed' with lines 4, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.erode.nc 1 1' using 1:4 notitle with lines 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff-silt plot.erode.nc 1 1' using 1:4 notitle with linespoints 1, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff plot.erode.nc 1 101' using 1:4 notitle with lines 3, \
     '<perl ../../../scripts/mass2flux.pl -c -v stuff-silt plot.erode.nc 1 101' using 1:4 notitle with linespoints 3, \
     '<perl ../../../scripts/mass2mass.pl plot.erode.nc stuff-silt-bed' using 1:3 notitle with linespoints 4, \
     '<perl ../../../scripts/mass2mass.pl plot.erode.nc stuff-bed' using 1:3 notitle with lines 4
     