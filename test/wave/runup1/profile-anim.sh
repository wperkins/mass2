#! /bin/sh
# -------------------------------------------------------------
# file: profile-anim.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  4, 2002 by William A. Perkins
# Last Change: Mon Feb  4 08:55:20 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

mass2slice="perl ../../../scripts/mass2slice.pl"

frames=450
fstep=1

f=1

cat <<EOF
set term png small color
set nokey
set yrange [-25:25]
set xrange [0:150]
EOF

while [ "$f" -le "$frames" ]; do
    out=`echo $f | awk '{printf("pics/frame%05d.png", $1);}' `
    cat <<EOF
set output "$out"
plot '<$mass2slice plot.nc zbot 1 5' using 1:4 with lines 1, \
     '<$mass2slice -t $f plot.nc wsel 1 5' using 1:4 with lines 3
EOF
    f=`expr $f + $fstep`
done