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
# Last Change: Thu Nov 16 08:57:03 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

                                # The first simulation has no bed

sed -e 's/0\.25/0.00/' mass2_v027.base > mass2_v027.cfg
$model
gnuplot plot-mass.gp > plot-mass-nobed.eps
mv plot.nc plot-nobed.nc

                                # The first simulation has no bed

cp mass2_v027.base mass2_v027.cfg
$model
gnuplot plot-mass.gp > plot-mass-bed.eps
mv plot.nc plot-bed.nc

