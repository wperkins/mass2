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
# Last Change: Tue Sep 26 10:44:59 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

                                # do the initial hydro-only run

cp mass2_v027.warmup mass2_v027.cfg
$model
cp hotstart_04-01-1996_030000.bin hotstart.bin

                                #  deposit some sediment

cp mass2_v027.depos mass2_v027.cfg
$model
cp plot.nc plot.depos.nc
cp hotstart_04-01-1996_180000.bin hotstart.bin

                                #  erode the sediment away

cp mass2_v027.erode mass2_v027.cfg
$model
cp plot.nc plot.erode.nc

