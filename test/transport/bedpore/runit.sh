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
# Last Change: Tue Oct 24 10:48:32 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

model=${MODEL-../../../mass2_v027}

$model
gnuplot plot-mass.gp > plot-mass.eps

