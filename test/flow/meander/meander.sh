#! /bin/sh

# Set AWK if not set
AWK=${AWK:-awk}

vars="clstuff rbstuff lbstuff uvel"
sects="126 142 164 187 209 231"
plotscale="-Jx0.75/0.75"

for v in $vars; do

    psbasemap -P -Bf1 -Bm5 -R12/32/-6/6/ $plotscale -K > junk.ps

    case $v in
        clstuff)
            scale="1.5"; factor="1.0"; units="C/Co";;
        *bstuff)
            scale="3.0"; factor="1.0"; units="C/Co";;
        *vel)
            scale="1.5"; factor="0.8333"; units="ft/s";;
        *)
            scale="1.0"; factor="1.0"; units="";;
    esac

    for i in $sects; do
        close="-K"
        if [ $i = "231" ]; then
            close="-Sx20/0/${scale}${units} -K "
        fi
        perl ../../../scripts/mass2slice.pl -j -l plot.nc $v 1 $i | \
            awk 'NF == 4 { print $1*0.3048, $2*0.3048, $4*'${factor}' } { next; }' | \
            pswiggle -R $plotscale -O -T2 -W -Z$scale -A90 $close >> junk.ps
    done

    $AWK 'NF == 1 { print ">"; next;} { print; }' meander_rails.grd | \
        psxy -O -R  $plotscale -M -N  >> junk.ps

    ps2epsi junk.ps $v.eps

done
