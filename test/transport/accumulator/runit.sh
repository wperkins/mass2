#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  5, 2000 by William A. Perkins
# Last Change: Wed Nov  8 11:33:01 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

model=${MODEL-../../../mass2_v027}

                                # Warm up

sed -e '15s/05:/03:/' -e '16s/10:/05:/'  mass2_v027.base > mass2_v027.cfg
$model
mv hotstart_01-01-1997_060000.bin hotstart.bin

                                # Run w/o averager

sed -e '15s/05:/06:/' \
    -e '12s/F/T/' \
    mass2_v027.base > mass2_v027.cfg
$model
mv plot.nc plot.inst.nc

                                # Run w/ averager

sed -e '15s/05:/06:/' \
    -e '12s/F/T/' \
    -e '37s/F/T/' \
    mass2_v027.base > mass2_v027.cfg
$model
mv plot.nc plot.avg.nc

gnuplot plot-conc.gp > plot-conc.eps