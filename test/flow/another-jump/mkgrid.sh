#! /bin/sh
# -------------------------------------------------------------
# file: mkgrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 23, 2004 by William A. Perkins
# Last Change: Fri Jan 23 14:29:57 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

# dimensions in meters

W="0.45"
L="13.9"

ncellx=238
ncelly=5

nvertx=`expr $ncellx + 1`
nverty=`expr $ncelly + 1`

dx=`echo scale=6\; $L/$ncellx | bc`
dy=`echo scale=6\; $W/$ncelly | bc`

# make grid.out in metric units

../../../util/cart_grid/cartgrid <<EOF
$dx    ! longitudinal spacing
$dy    ! lateral spacing
$nvertx    ! downstream nodes
$nverty    ! cross stream nodes
0.0    ! starting x coordinate
0.0    ! starting y coordinate

0.0    ! slope
0.0    ! downstream elevation
EOF


# convert the grid to english units

awk -f - grid.out > grid.dat <<EOF
NF == 5 {
  x = \$3/0.3048;
  y = \$4/0.3048;
  z = \$5/0.3048;
  printf("%5d %5d %12.6g %12.6g %12.6g\n", \$1, \$2, x, y, z);
  next;
}
{ print; }
EOF
