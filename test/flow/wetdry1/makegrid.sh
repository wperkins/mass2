#! /bin/sh
# -------------------------------------------------------------
# file: makegrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 10, 2001 by William A. Perkins
# Last Change: Mon Dec 10 15:25:53 2001 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------


../../../util/cart_grid/cartgrid <<EOF
10          ! delta x
10          ! delta y
101         ! downsteam nodes
51          ! lateral nodes
0.0         ! x origin
0.0         ! y origin

0.0         ! slope
0.0         ! bottom elevation
EOF

