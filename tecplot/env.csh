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
# Last Change: Tue Dec 11 09:14:39 2001 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

if( ! $?TEC90HOME ) then
    setenv TEC90HOME /usr/software/tecplot9
endif
setenv TECADDONDEVDIR $PWD
set path = ($path $TEC90HOME/adk/bin)

set os = `uname -s`

switch( $os )
    case Linux*:
        setenv TECADDONDEVPLATFORM linux.22
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
