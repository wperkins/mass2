#! /bin/sh
# -------------------------------------------------------------
# file: mkgrid.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 25, 2003 by William A. Perkins
# Last Change: Thu Jan  8 11:07:29 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$


# This is the supercritical part of the channel.  It is 1000 feet long.

../../../util/cart_grid/cartgrid <<EOF
5
50
101
11
4000
0

0.0676
0.0
EOF
mv grid.out grid2.dat

# This is the sub critical part of the channel.  It is like the normal
# subcrital flow tests.  It is 4000 feet long with 10x400 cells.

../../../util/cart_grid/cartgrid <<EOF
100
50
41
11
0
0

1.425e-04
33.8
EOF
mv grid.out grid1.dat


# combine them

echo  141 11 > grid.dat
tail +2 grid1.dat >> grid.dat
tail +13 grid2.dat >> grid.dat
