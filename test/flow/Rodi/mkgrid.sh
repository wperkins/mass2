#! /bin/sh
# -------------------------------------------------------------
# file: mkgrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 20, 2004 by William A. Perkins
# Last Change: Mon Feb 23 08:23:54 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

../../../util/cart_grid/cartgrid <<EOF
0.0455
0.0455
161
41
-1.820
0

0
0
EOF

exit

# grid.out must be edited by hand to put a extra line at x = 0.0225m

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
