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
# Last Change: Wed May 29 09:41:54 2002 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

                                # Cases: no decay, 24-hr and 12-hr halflife

hlives=" \
    0.0000E-00 \
    2.7379E-03 \
    1.3689E-03 \
"

case=1
for hl in $hlives; do
    sed -e "s/@HL@/$hl/g" scalar_source.base > scalar_source.dat
    $model
    gnuplot plot-mass.gp > plot-mass-$case.eps
    mv plot.nc plot-$case.nc
    case=`expr $case + 1`
done

