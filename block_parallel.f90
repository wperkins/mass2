! ----------------------------------------------------------------
! file: block_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: Wed Dec 22 07:32:46 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE block_parallel
! ----------------------------------------------------------------
MODULE block_module

  USE utility
  USE block_variable

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! ----------------------------------------------------------------
  ! TYPE block_struct
  ! ----------------------------------------------------------------
  TYPE block_struct
     INTEGER :: index
     INTEGER :: xmax, ymax
     TYPE (block_var_base), POINTER :: varbase
     TYPE (block_var), POINTER :: x_grid, y_grid, zbot_grid
  END TYPE block_struct

  TYPE(block_struct), PUBLIC, ALLOCATABLE :: block(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_allocate_size
  !
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE block_allocate_size(blk, idx, xmax, ymax)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: idx, xmax, ymax
    INTEGER :: imin, imax, jmin, jmax

    blk%index = idx
    blk%xmax = xmax
    blk%ymax = ymax

    ! make a base for all (parallel) variables

    blk%varbase => block_var_base_allocate(blk%xmax, blk%ymax)

    blk%x_grid => block_var_allocate("x_grid", blk%varbase, const=.TRUE.)
    blk%y_grid => block_var_allocate("y_grid", blk%varbase, const=.TRUE.)
    blk%zbot_grid => block_var_allocate("zbot_grid", blk%varbase, const=.TRUE.)

  END SUBROUTINE block_allocate_size

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns(blk, i, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i, j
    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_owned
    jmin = blk%varbase%jmin_owned
    imax = blk%varbase%imax_owned
    jmax = blk%varbase%jmax_owned

    block_owns = (&
         &(imin .LE. i) .AND. (i .LE. imax) .AND. &
         &(jmin .LE. j) .AND. (j .LE. jmax))
  END FUNCTION block_owns

  

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_distribution_report
  ! ----------------------------------------------------------------
  SUBROUTINE block_distribution_report()

    IMPLICIT NONE

    INTEGER :: b, nblk(1)
    INTEGER :: p, nproc, me
    INTEGER :: ij(4)
    INTEGER :: lo(ndim), hi(ndim)

    me = ga_nodeid()
    nproc = ga_nnodes()

    nblk = UBOUND(block)
    DO b = 1, nblk(1)
       IF (me == 0) THEN
          WRITE(*, 95) b, block(b)%varbase%imin_global, &
               &block(b)%varbase%imax_global, &
               &block(b)%varbase%jmin_global, &
               &block(b)%varbase%jmax_global
       END IF
       DO p = 0, nproc-1
          ij = 0
          lo = 0
          hi = 0
          IF (p == me) THEN
             ij(1) = block(b)%varbase%imin_owned
             ij(2) = block(b)%varbase%imax_owned
             ij(3) = block(b)%varbase%jmin_owned
             ij(4) = block(b)%varbase%jmax_owned
             CALL nga_distribution(block(b)%varbase%ga_handle, me, lo, hi)
          END IF
          call ga_igop(MT_INT, ij, 4, '+')
          call ga_igop(MT_INT, lo, ndim, '+')
          call ga_igop(MT_INT, hi, ndim, '+')

          IF (me == 0) THEN
             WRITE(*,100) b, p, ij(1), ij(2), ij(3), ij(4)
             WRITE(*,101) b, p, lo(1), hi(1), lo(2), hi(2)
          END IF
       END DO
    END DO
95  FORMAT('Block ', I2, ':      Global MASS2: ', I3, ': ', I3, ': ', I3, ': ', I3)
100 FORMAT('Block ', I2, ': Process ', I2, ': MASS2: ', I3, ': ', I3, ': ', I3, ': ', I3)
101 FORMAT('Block ', I2, ': Process ', I2, ': GA:    ', I3, ': ', I3, ': ', I3, ': ', I3)

  END SUBROUTINE block_distribution_report


END MODULE block_module
