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
# Last Change: Thu Oct  9 09:25:24 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

../../../util/cart_grid/cartgrid <<EOF
0.01   ! longitudinal spacing
0.01   ! lateral spacing
541    ! downstream nodes
91     ! cross stream nodes
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