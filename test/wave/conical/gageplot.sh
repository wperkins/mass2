#! /bin/sh
# -------------------------------------------------------------
# file: gageplot.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 20, 2002 by William A. Perkins
# Last Change: Wed Mar 20 07:13:43 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

out="$1"

case=`expr "$@" : '.*case\([abc]\)'`
gage=`expr "$@" : '.*gage\([0-9][0-9]*\)'`


obsfile="data/ts2${case}.txt"

case $gage in
    1|2|3|4) g=$gage; l="Incident Wave" ;;
    6) g=5; l="Island Toe" ;;
    9) g=6 ; l="Island Front Shore";;
    16) g=7; l="Island Side Shore";;
    22) g=8; l="Island Back Shore";;
esac
fld=`expr $g + 1`

sed -e "s/@GAGE@/$gage/g" \
    -e "s/@CASE@/$case/g" \
    -e "s/@LOCATION@/$l/g" \
    -e "s/@OBSFLD@/$fld/g" \
    -e "s/@GAGEID@/$g/g" \
    -e "s,@OBS@,$obsfile,g" gage.gp | gnuplot > $out