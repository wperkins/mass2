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
# Last Change: Tue Oct 24 09:36:38 2000 by William A. Perkins <perk@dora.pnl.gov>
# -------------------------------------------------------------

model=${MODEL-../../../mass2_v027}

                                #  this runs two cases: one with decay
                                #  and one without.  First, without decay.
cp scalar_source.base scalar_source.dat
$model
gnuplot plot-conc.gp > plot-conc-nodecay.eps
cp plot.nc plot.nodecay.nc

                                #  for the decay case we choose a
                                #  half-life such that there will be
                                #  one-half of the incoming
                                #  concentration at the outfall: @ 2
                                #  fps, 5000 s or 1.584E-04 yr

sed -e '9s/0.0/1.584E-04/' scalar_source.base > scalar_source.dat
$model
gnuplot plot-conc.gp > plot-conc-decay.eps
cp plot.nc plot.decay.nc


