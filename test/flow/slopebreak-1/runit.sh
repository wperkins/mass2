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
# Last Change: Wed Sep 13 08:24:17 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

model=${MODEL-../../../mass2_v027}

$model

gnuplot plot-depth.gp > plot-depth.eps
gnuplot plot-elev.gp > plot-elev.eps
gnuplot plot-vel.gp > plot-vel.eps

