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
# Last Change: Wed Jul 18 13:45:15 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

model=${MODEL-../../../mass2_v027}

$model

gnuplot plot-sed.gp > plot-sed.eps
gnuplot plot-shear.gp > plot-shear.eps
gnuplot plot-mass.gp > plot-mass.eps
