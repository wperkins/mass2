! ----------------------------------------------------------------
! file: distance.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May  5, 2003 by William A. Perkins
! Last Change: Mon May  5 12:10:44 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! REAL FUNCTION DISTANCE
! This function computes the distance between two points: (x1, y1) and
!  (x2, y2)
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION distance(x1, y1, x2, y2)

  IMPLICIT NONE
  DOUBLE PRECISION x1, y1, x2, y2

  distance = ABS(x1 - x2)**2.0 + ABS(y1 - y2)**2.0
  distance = SQRT(distance)
END FUNCTION distance


