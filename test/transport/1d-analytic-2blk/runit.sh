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
# Last Change: Fri Jul  7 12:24:17 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

set -e

model=${MODEL-../../../mass2_v027}

                                # Courant number = 1.0

cp mass2_v027.base mass2_v027.cfg
$model
sed -e 's/TheTitle/Courant Number = 1.0/' plot.gp | gnuplot > plot-Cn=1.0.eps
mv plot.nc plot-Cn=1.0.nc

                                # Courant number = 6.0

sed -e '18s/^20.0/120.0/' \
    -e '37s/^18/3 /' \
    -e '38s/^18/3 /' < mass2_v027.base > mass2_v027.cfg
$model
sed -e 's/TheTitle/Courant Number = 6.0/' plot.gp | gnuplot > plot-Cn=6.0.eps
mv plot.nc plot-Cn=6.0.nc

                                # Courant number = 0.1

sed -e '18s/^20.0/2.0/' \
    -e '37s/^18/180/' \
    -e '38s/^18/180/' < mass2_v027.base > mass2_v027.cfg
$model
sed -e 's/TheTitle/Courant Number = 0.1/' plot.gp | gnuplot > plot-Cn=0.1.eps
mv plot.nc plot-Cn=0.1.nc

