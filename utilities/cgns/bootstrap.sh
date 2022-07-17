#!/bin/sh
# -------------------------------------------------------------
# file: bootstrap.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 17, 2006 by William A. Perkins
# Last Change: 2018-10-05 06:40:28 d3g096
# -------------------------------------------------------------
# $Id$

dirs="\
    /net/flophouse/files0/perksoft/linux64/share/aclocal \
    /home/d3g096/stuff/share/aclocal \
    /Users/d3g096/stuff/share/aclocal \
    /usr/local/share/aclocal \
"

aclocalopts="-I ../m4"

for d in $dirs; do
    if [ -d $d ]; then
        aclocalopts="$aclocalopts -I $d"
    fi
done

aclocal $aclocalopts && automake --foreign --add-missing --copy && autoconf  

