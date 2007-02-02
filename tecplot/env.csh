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
# Last Change: Thu Jul 15 07:35:08 2004 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------
# $Id$

if( ! $?TECHOME ) then
    setenv TECHOME /usr/software/tecplot10
endif
setenv TEC100HOME $TECHOME
setenv TEC90HOME $TECHOME
setenv TECADDONDEVDIR $PWD
set path = ($path $TECHOME/adk/bin)

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
    case Darwin*:
        setenv TECADDONDEVPLATFORM macx.101
    default:
endsw
