#! /bin/csh -f
# -------------------------------------------------------------
# file: env.csh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 16, 1999 by William A. Perkins
# Last Change: Thu Sep  9 08:40:30 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
# $Id$

if( ! $?TEC75HOME ) then
    setenv TEC75HOME /usr/software/tecplot
endif
setenv TECADDONDEVDIR $PWD
set path = ($path $TEC75HOME/util/addons/bin)

set os = `uname -s`

switch( $os )
    case Linux*:
        setenv TECADDONDEVPLATFORM linux-s.203
        breaksw
    case IRIX64*:
        setenv TECADDONDEVPLATFORM sgix.62
        breaksw
    case IRIX:
        setenv TECADDONDEVPLATFORM sgix.52
        breaksw
    case SunOS*:
        setenv TECADDONDEVPLATFORM sun4.55
        breaksw
    case OSF1*:
        setenv TECADDONDEVPLATFORM decalpha.32
        breaksw
    default:
endsw
