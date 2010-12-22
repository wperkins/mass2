! ----------------------------------------------------------------
! file: block_grid.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 21, 2010 by William A. Perkins
! Last Change: Wed Dec 22 08:05:40 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE block_parallel_grid
!
! Block grid manipulation routines
! ----------------------------------------------------------------
MODULE block_parallel_grid

  USE utility
  USE block_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"
  INTEGER, PARAMETER, PRIVATE :: grid_iounit = 15

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_read_grid
  ! ----------------------------------------------------------------
  SUBROUTINE block_read_grid(blk, idx, fname)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: idx
    CHARACTER (LEN=*), INTENT(IN) :: fname

    INTEGER :: nx, ny
    INTEGER :: i, j, junk
    DOUBLE PRECISION, ALLOCATABLE :: x(:, :), y(:, :), z(:, :)
    INTEGER :: imin, imax, jmin, jmax

    CALL open_existing(fname, grid_iounit)
    READ(grid_iounit,*) nx, ny

    ALLOCATE(x(1:nx, 1:ny), y(1:nx, 1:ny), z(1:nx, 1:ny))

    DO i = 1, nx
       DO j = 1, ny
          READ(grid_iounit,*) junk, junk, x(i, j), y(i, j), z(i, j)
       END DO
    END DO

    CLOSE(grid_iounit)

    CALL block_allocate_size(blk, idx, nx, ny)

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned
    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

    imin = MAX(imin, 1)
    jmin = MAX(jmin, 1)
    imax = MIN(imax, nx)
    jmax = MIN(jmax, ny)
    

    blk%x_grid%current(imin:imax, jmin:jmax) = x(imin:imax, jmin:jmax)
    blk%y_grid%current(imin:imax, jmin:jmax) = y(imin:imax, jmin:jmax)
    blk%zbot_grid%current(imin:imax, jmin:jmax) = z(imin:imax, jmin:jmax)
    
    CALL block_var_put(blk%x_grid, BLK_VAR_CURRENT)
    CALL block_var_put(blk%y_grid, BLK_VAR_CURRENT)
    CALL block_var_put(blk%zbot_grid, BLK_VAR_CURRENT)
    CALL ga_sync()
    CALL block_var_get(blk%x_grid, BLK_VAR_CURRENT)
    CALL block_var_get(blk%y_grid, BLK_VAR_CURRENT)
    CALL block_var_get(blk%zbot_grid, BLK_VAR_CURRENT)

    ! CALL ga_print(blk%y_grid%ga_handle)

  END SUBROUTINE block_read_grid

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_build_ghost
  ! ----------------------------------------------------------------
  SUBROUTINE block_build_ghost(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk

    CALL block_extrap_ghost(blk)

  END SUBROUTINE block_build_ghost


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_extrap_ghost
  !
  ! This routine extrapolates the internal grid into the ghost cells.
  ! It is assumed that the process owning the edge of the internal
  ! grid will own the ghost cells too.
  ! ----------------------------------------------------------------
  SUBROUTINE block_extrap_ghost(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER :: i, j, ig
    DOUBLE PRECISION :: x, y, z
    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_owned
    jmin = MAX(blk%varbase%jmin_owned, 1)
    imax = blk%varbase%imax_owned
    jmax = MIN(blk%varbase%jmax_owned, blk%ymax)

    DO ig = 1, nghost
       
       i = 1 - ig
       IF (imin .LE. i .AND. i .LE. imax) THEN
          DO j = jmin, jmax
             x = blk%x_grid%current(i+1,j) - &
                  &(blk%x_grid%current(i+2,j) - blk%x_grid%current(i+1,j))
             y = blk%y_grid%current(i+1,j) - &
                  &(blk%y_grid%current(i+2,j) - blk%y_grid%current(i+1,j))
             z = blk%zbot_grid%current(i+1,j) - &
                  &(blk%zbot_grid%current(i+2,j) - blk%zbot_grid%current(i+1,j))
             blk%x_grid%current(i,j) = x
             blk%y_grid%current(i,j) = y
             blk%zbot_grid%current(i,j) = z
          END DO
       END IF
       i = blk%xmax + ig
       IF (imin .LE. i .AND. i .LE. imax) THEN
          DO j = jmin, jmax
             x = blk%x_grid%current(i-1,j) - &
                  &(blk%x_grid%current(i-2,j) - blk%x_grid%current(i-1,j))
             y = blk%y_grid%current(i-1,j) - &
                  &(blk%y_grid%current(i-2,j) - blk%y_grid%current(i-1,j))
             z = blk%zbot_grid%current(i-1,j) - &
                  &(blk%zbot_grid%current(i-2,j) - blk%zbot_grid%current(i-1,j))
             blk%x_grid%current(i,j) = x
             blk%y_grid%current(i,j) = y
             blk%zbot_grid%current(i,j) = z
          END DO
       END IF
    END DO

    imin = MAX(blk%varbase%imin_owned, 1)
    jmin = blk%varbase%jmin_owned
    imax = MIN(blk%varbase%imax_owned, blk%xmax)
    jmax = blk%varbase%jmax_owned

    DO ig = 1, nghost
       j = 1 - ig
       IF (jmin .LE. j .AND. j .LE. jmax) THEN
          DO i = imin, imax
             x = blk%x_grid%current(i,j+1) - &
                  &(blk%x_grid%current(i,j+2) - blk%x_grid%current(i,j+1))
             y = blk%y_grid%current(i,j+1) - &
                  &(blk%y_grid%current(i,j+2) - blk%y_grid%current(i,j+1))
             z = blk%zbot_grid%current(i,j+1) - &
                  &(blk%zbot_grid%current(i,j+2) - blk%zbot_grid%current(i,j+1))
             blk%x_grid%current(i,j) = x
             blk%y_grid%current(i,j) = y
             blk%zbot_grid%current(i,j) = z
          END DO
       END IF
       j = blk%ymax + ig
       IF (jmin .LE. j .AND. j .LE. jmax) THEN
          DO i = imin, imax
             x = blk%x_grid%current(i,j-1) - &
                  &(blk%x_grid%current(i,j-2) - blk%x_grid%current(i,j-1))
             y = blk%y_grid%current(i,j-1) - &
                  &(blk%y_grid%current(i,j-2) - blk%y_grid%current(i,j-1))
             z = blk%zbot_grid%current(i,j-1) - &
                  &(blk%zbot_grid%current(i,j-2) - blk%zbot_grid%current(i,j-1))
             blk%x_grid%current(i,j) = x
             blk%y_grid%current(i,j) = y
             blk%zbot_grid%current(i,j) = z
          END DO
       END IF
    END DO

    ! update global arrays and make local copies current on all processors

    CALL block_var_put(blk%x_grid)
    CALL block_var_put(blk%y_grid)
    CALL block_var_put(blk%zbot_grid)
    CALL ga_sync()
    CALL block_var_get(blk%x_grid)
    CALL block_var_get(blk%y_grid)
    CALL block_var_get(blk%zbot_grid)
    ! CALL ga_print(blk%y_grid%ga_handle)

    CALL block_extrap_ghost_corners(blk)

    CALL block_var_put(blk%x_grid)
    CALL block_var_put(blk%y_grid)
    CALL block_var_put(blk%zbot_grid)
    CALL ga_sync()
    CALL block_var_get(blk%x_grid)
    CALL block_var_get(blk%y_grid)
    CALL block_var_get(blk%zbot_grid)
    ! CALL ga_print(blk%y_grid%ga_handle)

  END SUBROUTINE block_extrap_ghost

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_extrap_ghost_corners
  ! ----------------------------------------------------------------
  SUBROUTINE block_extrap_ghost_corners(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER :: i, j, ig, jg

    DO ig = 1, nghost
       DO jg = 1, nghost
          CALL block_extrap_1_corner(blk, 1 - ig, 1 - jg, 1, 1)
          CALL block_extrap_1_corner(blk, 1 - ig, blk%ymax + jg, 1, -1)
          CALL block_extrap_1_corner(blk, blk%xmax + ig, 1 - jg, -1, 1)
          CALL block_extrap_1_corner(blk, blk%xmax + ig, blk%ymax + jg, -1, -1)
       END DO
    END DO
  END SUBROUTINE block_extrap_ghost_corners

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_extrap_1_corner
  ! ----------------------------------------------------------------
  SUBROUTINE block_extrap_1_corner(blk, i, j, ioff, joff)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: i, j, ioff, joff
    DOUBLE PRECISION :: mi, mj, bi, bj, zi, zj
    INTEGER :: i1, i2, j1, j2

    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_owned
    jmin = blk%varbase%jmin_owned
    imax = blk%varbase%imax_owned
    jmax = blk%varbase%jmax_owned

    IF (.NOT. block_owns(blk, i, j)) RETURN

    i1 = i + ioff
    i2 = i + 2*ioff
    j1 = j + joff
    j2 = j + 2*joff

    IF (blk%x_grid%current(i, j2) .NE. blk%x_grid%current(i, j1)) THEN
       mi = (blk%y_grid%current(i, j2) - blk%y_grid%current(i, j1))/&
            &(blk%x_grid%current(i, j2) - blk%x_grid%current(i, j1))
       bi = blk%y_grid%current(i, j2) - mi*blk%x_grid%current(i, j2)
    END IF

    IF (blk%x_grid%current(i2, j) .NE. blk%x_grid%current(i1, j)) THEN
       mj = (blk%y_grid%current(i2, j) - blk%y_grid%current(i1, j))/&
            &(blk%x_grid%current(i2, j) - blk%x_grid%current(i1, j))
       bj = blk%y_grid%current(i2, j) - mj*blk%x_grid%current(i2, j)
    END IF

    IF (blk%x_grid%current(i, j2) .EQ. blk%x_grid%current(i, j1)) THEN
       blk%x_grid%current(i, j) = blk%x_grid%current(i, j2)
       blk%y_grid%current(i, j) = mj*blk%x_grid%current(i, j) + bj
    ELSE IF (blk%x_grid%current(i2, j) .EQ. blk%x_grid%current(i1, j)) THEN
       blk%x_grid%current(i, j) = blk%x_grid%current(i2, j)
       blk%y_grid%current(i, j) = mi*blk%x_grid%current(i, j) + bi
    ELSE 
       blk%y_grid%current(i, j) = (bi - mi/mj*bj)*(mj/(mj - mi))
       blk%x_grid%current(i, j) = (blk%y_grid%current(i, j) - bj)/mj
    END IF

    zi = blk%zbot_grid%current(i,j1) - (blk%zbot_grid%current(i,j2) - blk%zbot_grid%current(i,j1))
    zj = blk%zbot_grid%current(i1,j) - (blk%zbot_grid%current(i2,j) - blk%zbot_grid%current(i1,j))

    blk%zbot_grid%current(i,j) = 0.5*(zi+zj)

  END SUBROUTINE block_extrap_1_corner


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_gridplot
  ! ----------------------------------------------------------------
  SUBROUTINE block_gridplot(fname, doghost)

    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: fname
    LOGICAL, INTENT(IN), OPTIONAL :: doghost

    LOGICAL :: mydoghost = .FALSE.
    INTEGER :: imin, imax, jmin, jmax, ni, nj, iblk
    INTEGER :: nblk(1)
    INTEGER :: junk
    INTEGER :: lo(ndim), hi(ndim)
    DOUBLE PRECISION, ALLOCATABLE :: tmp(:, :)

    IF (PRESENT(doghost)) mydoghost = doghost

    CALL open_new(fname, grid_iounit)

    nblk = UBOUND(block)

    WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
    WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""

    DO iblk= 1, nblk(1)
       imin = block(iblk)%varbase%imin_global
       imax = block(iblk)%varbase%imax_global
       jmin = block(iblk)%varbase%jmin_global
       jmax = block(iblk)%varbase%jmax_global

       ALLOCATE(tmp(imin:imax, jmin:jmax))

       lo = 1
       CALL nga_inquire(block(iblk)%varbase%ga_handle, junk, junk, hi)

       IF (mydoghost) THEN
          imin = 1 - i_ghost
          imax = block(iblk)%xmax + i_ghost
          jmin = 1 - j_ghost
          jmax = block(iblk)%ymax + j_ghost
       ELSE
          imin = 1
          imax = block(iblk)%xmax
          jmin = 1
          jmax = block(iblk)%ymax
       END IF

       ni = imax - imin + 1
       nj = jmax - jmin + 1

       WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblk,""""," i=", ni, " j= ",nj
       CALL nga_get(block(iblk)%x_grid%ga_handle, lo, hi, tmp, hi)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)
       CALL nga_get(block(iblk)%y_grid%ga_handle, lo, hi, tmp, hi)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)
       CALL nga_get(block(iblk)%zbot_grid%ga_handle, lo, hi, tmp, hi)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)
    END DO

    CLOSE(grid_iounit)

  END SUBROUTINE block_gridplot


END MODULE block_parallel_grid
