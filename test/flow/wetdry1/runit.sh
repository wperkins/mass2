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
# Last Change: Mon Jan 28 09:10:08 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

set -xe

                                # stuff to run on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

                                # warm it up without any drying

cp mass2_v027_warmup.cfg mass2_v027.cfg
time $model

                                # Case 1: hot start all wet and run
                                # through a couple of cycles

cp mass2_v027_case1.cfg mass2_v027.cfg
cp hotstart_04-01-1996_070000.bin hotstart.bin
time $model
mv gage.nc gage_case1.nc
mv plot.nc plot_case1.nc
mv mass_source_monitor.out mass_source_monitor.case1

                                # Case 2: cold start with some dry and
                                # run through a cycle

cp mass2_v027_case2.cfg mass2_v027.cfg
time $model
mv gage.nc gage_case2.nc
mv plot.nc plot_case2.nc
mv mass_source_monitor.out mass_source_monitor.case2

                                # Case 3: run Case 2, but with hot start

cp mass2_v027_case3.cfg mass2_v027.cfg
cp hotstart_04-01-1996_080000.bin hotstart.bin
time $model
mv gage.nc gage_case3.nc
mv plot.nc plot_case3.nc
mv mass_source_monitor.out mass_source_monitor.case3
