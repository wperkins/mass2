
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
# Last Change: Wed Apr 22 10:53:54 2009 by William A. Perkins <d3g096@bearflag.pnl.gov>
# -------------------------------------------------------------
# $Id$

if( ! $?TECHOME ) then
    setenv TECHOME /usr/software/tecplot10
endif
setenv TEC100HOME $TECHOME
setenv TEC90HOME $TECHOME
setenv TEC360HOME $TECHOME
setenv TECADDONDEVDIR $PWD
set path = ($path $TECHOME/adk/bin)

set os = `uname -s`

switch( $os )
    case Linux*:
        setenv TECADDONDEVPLATFORM linux64.26
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
    case Darwin*:
        setenv TECADDONDEVPLATFORM macx.101
    default:
endsw
