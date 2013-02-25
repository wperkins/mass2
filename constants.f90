! ----------------------------------------------------------------
! file: constants.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August 19, 2009 by William A. Perkins
! Last Change: Wed Aug 19 10:57:26 2009 by William A. Perkins <d3g096@bearflag.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE constants
! ----------------------------------------------------------------
MODULE constants

  IMPLICIT NONE
  
  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  DOUBLE PRECISION, PARAMETER :: grav = 32.2 ! ft/s/s
  DOUBLE PRECISION, PARAMETER :: tiny = 1.0D-100
  DOUBLE PRECISION, PARAMETER :: density = 1.94 ! slug/ft^3
  DOUBLE PRECISION, PARAMETER :: density_air = 0.00237  ! 60 degrees F
  DOUBLE PRECISION, PARAMETER :: bigfactor = 1.0d80
  DOUBLE PRECISION, PARAMETER :: vonkarmon = 0.4
  DOUBLE PRECISION, PARAMETER :: viscosity_water = 1.22e-05 ! ft^2/s @ 60F

END MODULE constants
