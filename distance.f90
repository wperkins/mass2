! ----------------------------------------------------------------
! file: distance.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May  5, 2003 by William A. Perkins
! Last Change: Tue May  6 14:37:32 2003 by William A. Perkins <perk@leechong.pnl.gov>
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


! ----------------------------------------------------------------
! SUBROUTINE interpolate_point
! This routine locates a point the specified fraction of the distance
! between (x1, y1) and (x2,y2). fract is assumed to be between 0.0 and 1.0
! ----------------------------------------------------------------
SUBROUTINE interpolate_point(fract, x1, y1, z1, x2, y2, z2, x3, y3, z3)

  IMPLICIT NONE
  
  DOUBLE PRECISION, INTENT(IN) :: fract, x1, y1, z1, x2, y2, z2
  DOUBLE PRECISION, INTENT(OUT) :: x3, y3, z3

  DOUBLE PRECISION :: dx, dy, dz

  dx = x2 - x1
  dy = y2 - y1
  dz = z2 - z1

  x3 = x1 + dx*fract
  y3 = y1 + dy*fract
  z3 = z1 + dz*fract

END SUBROUTINE interpolate_point
