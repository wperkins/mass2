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
# Last Change: Tue Mar 19 12:38:25 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model=${MODEL-../../../mass2_v027}

                                # do the initial hydro-only run

cp mass2_v027.warmup mass2_v027.cfg
$model
mv hotstart_04-01-1996_000000.bin hotstart.bin

                                #  run w/o accumulator

cp mass2_v027.run mass2_v027.cfg
$model
mv plot.nc plot.inst.nc
mv gage.nc gage.inst.nc
gnuplot plot-mass.gp > plot-mass.inst.eps

                                #  run w/ accumulator

sed -e '38s/F/T/' mass2_v027.run > mass2_v027.cfg
$model
mv plot.nc plot.avg.nc
mv gage.nc gage.avg.nc
sed -e 's/\.inst\./.avg./' plot-mass.gp | gnuplot > plot-mass.avg.eps

exec < gage_control.dat

num=0
while read blk i j junk; do
    if [ -n $blk ]; then
        num=`expr $num + 1`
        x=`expr $i + 1`
        y=`expr $j + 1`
        sed -e s/@NUM@/"$num"/ -e s/@BLK@/"$blk"/ \
            -e s/@X@/"$x"/ -e s/@Y@/"$y"/ plot-stuff.gp | \
            gnuplot > plot-stuff-$num.eps
        sed -e s/@NUM@/"$num"/ -e s/@BLK@/"$blk"/ \
            -e s/@X@/"$x"/ -e s/@Y@/"$y"/ plot-sediment.gp | \
            gnuplot > plot-sediment-$num.eps
    fi
done
