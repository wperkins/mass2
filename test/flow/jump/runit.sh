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
# Last Change: Tue Aug  1 21:04:11 2000 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------
# $Id$

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE


model=${MODEL-../../../mass2_v027}

cp mass2_v027.cfg.1 mass2_v027.cfg
$model
cp plot.nc plot.nc.1

cp mass2_v027.cfg.2 mass2_v027.cfg
$model
cp plot.nc plot.nc.2

gnuplot plot-depth.gp > plot-depth.eps
gnuplot plot-elev.gp > plot-elev.eps
gnuplot plot-vel.gp > plot-vel.eps
gnuplot plot-froude.gp > plot-froude.eps
gnuplot plot-courant.gp > plot-courant.eps

