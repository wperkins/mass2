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
# Last Change: Wed Jan 30 09:22:29 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE


model=${MODEL-../../../mass2_v027}

$model

tecplot9 -b -y plot.eps dead.lay -p ~/tecplot/macros/export_eps.mcr
