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
# Last Change: Wed Jul 26 11:13:48 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

$model

gnuplot plot-lat-1.gp > plot-lat-1.eps
gnuplot plot-lat-2.gp > plot-lat-2.eps
gnuplot plot-long.gp > plot-long.eps
