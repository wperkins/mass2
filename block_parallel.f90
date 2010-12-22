! ----------------------------------------------------------------
! file: block_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: Wed Dec 22 15:02:13 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
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
     TYPE (block_var), POINTER :: x, y, zbot
     TYPE (block_var), POINTER :: x_out, y_out, zbot_out
     TYPE (block_var), POINTER :: x_xsi, y_xsi, x_eta, y_eta
     TYPE (block_var), POINTER :: hp1, hp2, hv1, hv2, hu1, hu2, gp12
     TYPE (block_var), POINTER :: eddy, kx_diff, ky_diff, chezy
     TYPE (block_var), POINTER :: uvel, vvel, depth, wsel
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

    ! grid coordinates, in their various forms

    blk%x_grid => block_var_allocate("x_grid", blk%varbase, const=.TRUE.)
    blk%y_grid => block_var_allocate("y_grid", blk%varbase, const=.TRUE.)
    blk%zbot_grid => block_var_allocate("zbot_grid", blk%varbase, const=.TRUE.)
    blk%x => block_var_allocate("x", blk%varbase, const=.TRUE.)
    blk%y => block_var_allocate("y", blk%varbase, const=.TRUE.)
    blk%zbot => block_var_allocate("zbot", blk%varbase, const=.TRUE.)
    blk%x_out => block_var_allocate("x_out", blk%varbase, const=.TRUE.)
    blk%y_out => block_var_allocate("y_out", blk%varbase, const=.TRUE.)
    blk%zbot_out => block_var_allocate("zbot_out", blk%varbase, const=.TRUE.)

    ! grid metrics

    blk%x_xsi => block_var_allocate("x_xsi", blk%varbase, const=.TRUE.)
    blk%y_xsi => block_var_allocate("y_xsi", blk%varbase, const=.TRUE.)
    blk%x_eta => block_var_allocate("x_eta", blk%varbase, const=.TRUE.)
    blk%y_eta => block_var_allocate("y_eta", blk%varbase, const=.TRUE.)
    blk%hp1 => block_var_allocate("hp1", blk%varbase, const=.TRUE.)
    blk%hp2 => block_var_allocate("hp2", blk%varbase, const=.TRUE.)
    blk%hu1 => block_var_allocate("hu1", blk%varbase, const=.TRUE.)
    blk%hu2 => block_var_allocate("hu2", blk%varbase, const=.TRUE.)
    blk%hv1 => block_var_allocate("hv1", blk%varbase, const=.TRUE.)
    blk%hv2 => block_var_allocate("hv2", blk%varbase, const=.TRUE.)
    blk%gp12 => block_var_allocate("gp12", blk%varbase, const=.TRUE.)

    ! simulation coefficients that can vary spatially

    blk%chezy => block_var_allocate("chezy", blk%varbase, const=.TRUE.)
    blk%kx_diff => block_var_allocate("kx_diff", blk%varbase, const=.TRUE.)
    blk%ky_diff => block_var_allocate("ky_diff", blk%varbase, const=.TRUE.)
    blk%eddy => block_var_allocate("eddy", blk%varbase, const=.TRUE.)

    ! hydrodynamic variables

    blk%uvel => block_var_allocate("uvel", blk%varbase, const=.FALSE.)
    blk%vvel => block_var_allocate("vvel", blk%varbase, const=.FALSE.)
    blk%depth => block_var_allocate("depth", blk%varbase, const=.FALSE.)
    blk%wsel => block_var_allocate("wsel", blk%varbase, const=.TRUE.)
    
    

  END SUBROUTINE block_allocate_size

  ! ----------------------------------------------------------------
  ! FUNCTION block_var_buffer
  ! ----------------------------------------------------------------
  FUNCTION block_buffer(blk)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(IN) :: blk
    DOUBLE PRECISION, POINTER :: block_buffer(:, :)

    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_global
    imax = blk%varbase%imax_global
    jmin = blk%varbase%jmin_global
    jmax = blk%varbase%jmax_global

    ALLOCATE(block_buffer(imin:imax, jmin:jmax))

    RETURN

  END FUNCTION block_buffer

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_i(blk, i)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i
    INTEGER :: imin, imax

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned

    block_owns_i = ((imin .LE. i) .AND. (i .LE. imax))
  END FUNCTION block_owns_i
  
  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_j(blk, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: j
    INTEGER :: jmin, jmax

    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

    block_owns_j = ((jmin .LE. j) .AND. (j .LE. jmax))
  END FUNCTION block_owns_j

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns(blk, i, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i, j
    block_owns = (block_owns_i(blk, i) .AND. block_owns_j(blk, j))
  END FUNCTION block_owns


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_owns_window
  ! ----------------------------------------------------------------
  SUBROUTINE block_owns_window(blk, imin, jmin, imax, jmax)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(OUT) :: imin, jmin, imax, jmax

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned
    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

  END SUBROUTINE block_owns_window
  


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
