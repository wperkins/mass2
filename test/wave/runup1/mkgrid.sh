#! /bin/sh
# -------------------------------------------------------------
# file: mkgrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  1, 2002 by William A. Perkins
# Last Change: Fri Feb  1 13:46:49 2002 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

../../../util/cart_grid/cartgrid <<EOF
0.1                 ! dx - longitudinal spacing
0.25                ! dy - lateral spacing
1501                ! longitudinal points
11                  ! lateral points
0.0                 ! downstream x-coord
0.0                 ! downstream y-coord

0.5                 ! slope
-50.0               ! downstream elevation
EOF
