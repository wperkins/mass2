#! /bin/sh

perl ~/gas-transport/utilities/gridgen2arc.pl -T -r -x 3.2808399 meander1.grd | \
    awk '{ z = -$4*0.000350; printf("%5d %5d %15.3f %15.3f %15.5e\n", $2, $3, $4, $5, z);}' > meander1.dat
