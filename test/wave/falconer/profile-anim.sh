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
# Last Change: Thu Dec 12 15:00:40 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

mass2slice="perl ../../../scripts/mass2slice.pl"

case=3
plotfile="plot.case$case.nc"
frames=85
fstep=1

f=1

cat <<EOF
set term png small color
load 'plot-one.gp'
EOF

while [ "$f" -le "$frames" ]; do
    out=`echo $case $f | awk '{printf("pics/case%1d.frame%05d.png", $1, $2);}' `
    cat <<EOF
set output "$out"
plot '<perl ../../../scripts/mass2slice.pl $plotfile zbot 1 3' using (\$1/3.28083989501):(\$4/3.28083989501) notitle with lines 7, \
     '<perl ../../../scripts/mass2slice.pl -t $f $plotfile wsel 1 3' using (\$1/3.28083989501):(\$4/3.28083989501) notitle with lines 1
EOF
    f=`expr $f + $fstep`
done
