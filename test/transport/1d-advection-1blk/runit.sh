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
# Last Change: Tue Feb  5 09:03:47 2002 by William A. Perkins <perk@leechong.pnl.gov>
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
    -e '38s/^36/6 /' \
    -e '39s/^36/6 /' < mass2_v027.base > mass2_v027.cfg
$model
gnuplot plot.gp > plot-Cn=6.0.eps
mv plot.nc plot-Cn=6.0.nc

                                # Courant number = 0.1

sed -e '17s/^20.0/2.0/' \
    -e '38s/^36/360/' \
    -e '39s/^36/360/' < mass2_v027.base > mass2_v027.cfg
$model
gnuplot plot.gp > plot-Cn=0.1.eps
mv plot.nc plot-Cn=0.1.nc

