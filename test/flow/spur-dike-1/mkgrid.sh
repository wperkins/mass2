#! /bin/sh
# -------------------------------------------------------------
# file: mkgrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  8, 2003 by William A. Perkins
# Last Change: Mon Jan 19 10:11:29 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

# dimensions in meters

L=5.4
W=0.9
nx=217
ny=37
dx=`echo scale=6\;$L/\($nx - 1\) | bc`
dy=`echo scale=6\;$W/\($ny - 1\) | bc`

../../../util/cart_grid/cartgrid <<EOF
$dx    ! longitudinal spacing
$dy    ! lateral spacing
$nx    ! downstream nodes
$ny    ! cross stream nodes
0.0    ! starting x coordinate 
0.0    ! starting y coordinate

5.9014e-05 ! slope
0.0    ! downstream elevation
EOF

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