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
# Last Change: Tue Apr  4 14:31:54 2000 by William A. Perkins <perk@gehenna.pnl.gov>
# -------------------------------------------------------------
# $Id$

if( ! $?TEC80HOME ) then
    setenv TEC80HOME /usr/software/tecplot8
endif
setenv TECADDONDEVDIR $PWD
set path = ($path $TEC80HOME/adk/bin)

set os = `uname -s`

switch( $os )
    case Linux*:
        setenv TECADDONDEVPLATFORM linux.203
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
