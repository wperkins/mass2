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

