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
# Last Change: Fri Aug  4 15:44:24 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

model=${MODEL-../../../mass2_v027}

                                # the first simulation does not use a
                                # bed source, just an upstream
                                # concentration

sed -e 's/@FILE@/conc.dat/' scalar_bcspecs.base > scalar_bcspecs.dat
cp scalar_source.base scalar_source.dat
$model
mv -f gage.nc gage.bc.nc
mv -f plot.nc plot.bc.nc
gnuplot plot-mass-bc.gp > plot-mass-bc.eps

#  exit

sed -e 's/@FILE@/background.dat/' scalar_bcspecs.base > scalar_bcspecs.dat
sed -e '2s,/,BEDSOURCE "stuff-list.dat" "stuff-map.dat" /,' scalar_source.base > scalar_source.dat
$model
mv -f gage.nc gage.bed.nc
mv -f plot.nc plot.bed.nc
gnuplot plot-mass-bed.gp > plot-mass-bed.eps
