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
# Last Change: Thu Jan 10 10:15:07 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE


model=${MODEL-../../../mass2_v027}

                                # run the channel to normal flow

cp mass2_v027-warmup.cfg mass2_v027.cfg
cp bcspecs-warmup.dat bcspecs.dat
$model

                                # instantaneously turn on the dead zone

cp mass2_v027-dead.cfg mass2_v027.cfg
cp bcspecs-dead.dat bcspecs.dat
cp hotstart_04-01-1996_060000.bin hotstart.bin
$model


