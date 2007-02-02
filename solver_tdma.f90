! ----------------------------------------------------------------
! file: solver_tmda.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 16, 2002 by William A. Perkins
! Last Change: Thu Jul 17 11:39:11 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL
! ----------------------------------------------------------------
! MODULE solver
! ----------------------------------------------------------------
MODULE solver_module

  USE solver_common

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_initialize
  ! Returns 0 if all is well
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_initialize(blocks, xmax, ymax, do_flow, do_transport)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blocks, xmax(blocks), ymax(blocks)
    LOGICAL, INTENT(IN) :: do_flow, do_transport
    solver_initialize = 0
  END FUNCTION solver_initialize

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver(blk, ieq, x_start, x_end, y_start, y_end, its, &
       &ap, aw, ae, as, an, bp, x)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blk, ieq, x_start, x_end, y_start, y_end, its
    DOUBLE PRECISION, INTENT(IN), DIMENSION(x_start:x_end,y_start:y_end) :: ap, aw, ae, as, an, bp
    DOUBLE PRECISION, INTENT(INOUT), DIMENSION(x_start:x_end,y_start:y_end) :: x

    CALL solve_tdma(its, x_start, x_end, y_start, y_end, ap, aw, ae, as, an, bp, x)

    solver = 0
  END FUNCTION solver

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_finalize
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_finalize()

    IMPLICIT NONE
    solver_finalize = 0

  END FUNCTION solver_finalize



END MODULE solver_module
