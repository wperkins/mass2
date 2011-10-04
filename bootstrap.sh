#! /bin/sh
# -------------------------------------------------------------
# file: bootstrap.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: Mon Oct  3 10:15:01 2011 by William A. Perkins <d3g096@flophouse>
# -------------------------------------------------------------
# $Id$

aclocal -I time_series/m4 -I m4 && automake --foreign --add-missing --copy && autoconf

(cd time_series; sh bootstrap.sh)