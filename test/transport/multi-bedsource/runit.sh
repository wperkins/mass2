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
# Last Change: Mon Aug  7 08:20:30 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------
# $Id$

model=${MODEL-../../../mass2_v027}

$model
gnuplot plot-mass.gp > plot-mass.eps
