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
# Last Change: Fri Jul 20 10:54:52 2001 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

model=${MODEL-../../../mass2_v027}

$model

gnuplot plot-sed.gp > plot-sed.eps
gnuplot plot-conc.gp > plot-conc.eps
gnuplot plot-mass.gp > plot-mass.eps
gnuplot plot-smass.gp > plot-smass.eps

