#! /bin/sh

set -eu


mass2dir="../../.."
mass2slice="perl $mass2dir/scripts/mass2slice.pl -j -t 2 "

sumit() {
    file="$1"
    blk="$2"
    i="$3"

    $mass2slice $file depth $blk $i > upstream_depth.dat
    $mass2slice $file uvel $blk $i > upstream_uvel.dat
    paste -d' ' upstream_depth.dat upstream_uvel.dat | \
        awk -f integrate.awk
}

sumit plot.nc  1  102
sumit plot.nc  2    1


