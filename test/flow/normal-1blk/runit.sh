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
# Last Change: Thu Oct  3 08:14:58 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE


model=${MODEL-../../../mass2_v027}

cp mass2_v027.base mass2_v027.cfg
cp bcspecs.base bcspecs.dat

$model

gnuplot plot-depth.gp > plot-depth.eps
gnuplot plot-elev.gp > plot-elev.eps
gnuplot plot-vel.gp > plot-vel.eps

