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
# Last Change: Thu Jul  6 22:16:32 2000 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------
# $Id$

model=${MODEL-../../../mass2_v027}

                                # Courant number = 1.0

cp mass2_v027.base mass2_v027.cfg
$model
gnuplot plot.gp > plot-Cn=1.0.eps
mv plot.nc plot-Cn=1.0.nc

                                # Courant number = 6.0

sed -e '17s/^20.0/120.0/' \
    -e '36s/^36/6 /' \
    -e '37s/^36/6 /' < mass2_v027.base > mass2_v027.cfg
$model
gnuplot plot.gp > plot-Cn=6.0.eps
mv plot.nc plot-Cn=6.0.nc

                                # Courant number = 0.1

sed -e '17s/^20.0/2.0/' \
    -e '36s/^36/360/' \
    -e '37s/^36/360/' < mass2_v027.base > mass2_v027.cfg
$model
gnuplot plot.gp > plot-Cn=0.1.eps
mv plot.nc plot-Cn=0.1.nc

