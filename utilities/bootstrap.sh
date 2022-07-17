#!/bin/sh
aclocal -I m4 && automake --foreign --add-missing --copy && autoconf  
( cd cgns; sh ./bootstrap.sh )
