#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created April  5, 1999 by William A. Perkins
# Last Change: Wed Sep 13 06:58:07 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

set -x

                                # do benchmark test -- hydrodynamics
                                # with temperature / dissolved gas
                                # transport

cp mass2_v027.base mass2_v027.cfg
time $model

