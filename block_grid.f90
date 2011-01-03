! ----------------------------------------------------------------
! file: block_grid.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 21, 2010 by William A. Perkins
! Last Change: Mon Jan  3 06:22:26 2011 by William A. Perkins <d3g096@PE10588.pnl.gov>
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

    DEALLOCATE(x, y, z)

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
    DOUBLE PRECISION, POINTER :: tmp(:, :)

    IF (PRESENT(doghost)) mydoghost = doghost

    CALL open_new(fname, grid_iounit)

    nblk = UBOUND(block)

    WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
    WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""

    DO iblk= 1, nblk(1)

       tmp => block_buffer(block(iblk))

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
       CALL block_var_all(block(iblk)%x_grid, tmp)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)
       CALL block_var_all(block(iblk)%y_grid, tmp)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)
       CALL block_var_all(block(iblk)%zbot_grid, tmp)
       WRITE(grid_iounit,'(8G16.8)')tmp(imin:imax,jmin:jmax)

       DEALLOCATE(tmp)
    END DO

    CLOSE(grid_iounit)

  END SUBROUTINE block_gridplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_interp_grid
  ! interpolate x_grid,y_grid,zbot_grid onto the c.v. points by simple
  ! averaging
  ! ----------------------------------------------------------------
  SUBROUTINE block_interp_grid(blk)

    IMPLICIT NONE

    TYPE(block_struct), INTENT(INOUT) :: blk
    INTEGER :: i, j, ivec(4),jvec(4),ivec2(4),jvec2(4), idx
    INTEGER :: imin, imax, jmin, jmax

    imin = MAX(blk%varbase%imin_owned, i_index_min + 1)
    imax = MIN(blk%varbase%imax_owned, blk%xmax + i_index_extra - 1)
    jmin = MAX(blk%varbase%jmin_owned, j_index_min + 1)
    jmax = MIN(blk%varbase%jmax_owned, blk%ymax + j_index_extra - 1)

    DO i = imin, imax
       DO j = jmin, jmax
          blk%x%current(i,j) = 0.25*(blk%x_grid%current(i,j)+blk%x_grid%current(i-1,j)+&
               & blk%x_grid%current(i,j-1)+blk%x_grid%current(i-1,j-1))
          blk%y%current(i,j) = 0.25*(blk%y_grid%current(i,j)+blk%y_grid%current(i-1,j)+&
               & blk%y_grid%current(i,j-1)+blk%y_grid%current(i-1,j-1))
          blk%zbot%current(i,j) = 0.25*(blk%zbot_grid%current(i,j)+blk%zbot_grid%current(i-1,j)+&
               & blk%zbot_grid%current(i,j-1)+blk%zbot_grid%current(i-1,j-1))
       END DO
    END DO

    ! now take care of the edges of the grid
    ! remember that the c.v.'s have an extra i,j line than the grid

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned
    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

    i=i_index_min
    IF (block_owns_i(blk, i)) THEN
       DO j= jmin, jmax
          blk%x%current(i,j) = 0.5*(blk%x_grid%current(i,j)+blk%x_grid%current(i,j-1))
          blk%y%current(i,j) = 0.5*(blk%y_grid%current(i,j)+blk%y_grid%current(i,j-1))
          blk%zbot%current(i,j) = 0.5*(blk%zbot_grid%current(i,j)+blk%zbot_grid%current(i,j-1))
       END DO
    END IF

    i= blk%xmax+i_index_extra
    IF (block_owns_i(blk, i)) THEN
       DO j= jmin, jmax
          blk%x%current(i,j) = 0.5*(blk%x_grid%current(i-1,j)+blk%x_grid%current(i-1,j-1))
          blk%y%current(i,j) = 0.5*(blk%y_grid%current(i-1,j)+blk%y_grid%current(i-1,j-1))
          blk%zbot%current(i,j) = 0.5*(blk%zbot_grid%current(i-1,j)+blk%zbot_grid%current(i-1,j-1))
       END DO
    END IF

    j=j_index_min
    IF (block_owns_j(blk, j)) THEN
       DO i = imin, imax
          blk%x%current(i,j) = 0.5*(blk%x_grid%current(i,j)+blk%x_grid%current(i-1,j))
          blk%y%current(i,j) = 0.5*(blk%y_grid%current(i,j)+blk%y_grid%current(i-1,j))
          blk%zbot%current(i,j) = 0.5*(blk%zbot_grid%current(i,j)+blk%zbot_grid%current(i-1,j))
       END DO
    END IF

    j= blk%ymax+j_index_extra
    IF (block_owns_j(blk, j)) THEN
       DO i = imin, imax
          blk%x%current(i,j) = 0.5*(blk%x_grid%current(i,j-1)+blk%x_grid%current(i-1,j-1))
          blk%y%current(i,j) = 0.5*(blk%y_grid%current(i,j-1)+blk%y_grid%current(i-1,j-1))
          blk%zbot%current(i,j) = 0.5*(blk%zbot_grid%current(i,j-1)+blk%zbot_grid%current(i-1,j-1))
       END DO
    END IF

    ivec = (/i_index_min, i_index_min,&
         &blk%xmax + i_index_extra, blk%xmax+i_index_extra/)
    jvec = (/j_index_min, blk%ymax+j_index_extra,&
         &j_index_min,blk%ymax+j_index_extra/)
    ivec2 = (/i_index_min,i_index_min,&
         &blk%xmax+i_index_extra-1,blk%xmax+i_index_extra-1/)
    jvec2 = (/j_index_min,blk%ymax+j_index_extra-1,&
         &j_index_min,blk%ymax+j_index_extra-1/)

    DO idx = 1, 4
       i = ivec(idx)
       j = jvec(idx)
       IF (block_owns(blk, i, j)) THEN
          blk%x%current(i,j) = blk%x_grid%current(ivec2(idx),jvec2(idx))
          blk%y%current(i,j) = blk%y_grid%current(ivec2(idx),jvec2(idx))
          blk%zbot%current(i,j) = blk%zbot_grid%current(ivec2(idx),jvec2(idx))
       END IF
    END DO

    CALL block_var_put(blk%x)
    CALL block_var_put(blk%y)
    CALL block_var_put(blk%zbot)
    CALL ga_sync()
    CALL block_var_get(blk%x)
    CALL block_var_get(blk%y)
    CALL block_var_get(blk%zbot)
    
  END SUBROUTINE block_interp_grid


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_metrics
  ! Compute the metric coefficients for the block. Depending on the
  ! metric coeff. and location use either the grid (x,y) or the node
  ! (x,y)
  ! ----------------------------------------------------------------
  SUBROUTINE block_metrics(blk)

    IMPLICIT NONE

    TYPE (block_struct) :: blk

    INTEGER :: imin, imax, jmin, jmax, i, j

    imin = i_index_min
    imax = blk%xmax+i_index_extra
    jmin = j_index_min
    jmax = blk%ymax+j_index_extra

    ! metric coeff. 2 on the u face of the c.v.

    blk%hu2%current = 1.0e-20            ! bogus nonzero value
    DO i=imin, imax-1
       DO j=jmin+1, jmax-1
          IF (block_owns(blk, i, j)) THEN
             blk%hu2%current(i,j) = & 
                  SQRT((blk%x_grid%current(i,j) - blk%x_grid%current(i,j-1))**2 + &
                  (blk%y_grid%current(i,j) - blk%y_grid%current(i,j-1))**2)
          END IF
       END DO
    END DO

    ! metric coeff 1 on the u face of the c.v.

    blk%hu1%current = 1.0e-20            ! bogus nonzero value
    DO i=imin+1, imax-i_index_extra
       DO j=jmin, jmax
          IF (block_owns(blk, i, j)) THEN
             blk%hu1%current(i,j) = &
                  SQRT((blk%x%current(i+1,j) - blk%x%current(i,j))**2 + &
                  (blk%y%current(i+1,j) - blk%y%current(i,j))**2)
          END IF
       END DO
    END DO

    ! on the edge it's only a half-distance

    i=imin
    DO j=jmin, jmax
       IF (block_owns(blk, i, j)) THEN
          blk%hu1%current(i,j) = &
               SQRT(((blk%x%current(i+1,j) - blk%x%current(i,j)))**2 + &
               ((blk%y%current(i+1,j) - blk%y%current(i,j)))**2)
       END IF
    END DO
    i=imax-1
    DO j=jmin, jmax
       IF (block_owns(blk, i, j)) THEN
          blk%hu1%current(i,j) = &
               SQRT(((blk%x%current(i+1,j) - blk%x%current(i,j)))**2 + &
               ((blk%y%current(i+1,j) - blk%y%current(i,j)))**2)
       END IF
    END DO

    ! metric coeff. 1 on the v face of the c.v.

    DO i=imin+1, imax-1
       DO j=jmin, jmax - 1
          IF (block_owns(blk, i, j)) THEN
             blk%hv1%current(i,j) = &
                  SQRT((blk%x_grid%current(i,j) - blk%x_grid%current(i-1,j))**2 + &
                  (blk%y_grid%current(i,j) - blk%y_grid%current(i-1,j))**2) 
          END IF
       END DO
    END DO

    ! metric coeff. 2 on the v face of the c.v.

    DO i=imin, imax
       DO j=jmin+1, jmax - 1
          IF (block_owns(blk, i, j)) THEN
             blk%hv2%current(i,j) = &
                  SQRT((blk%x%current(i,j+1) - blk%x%current(i,j))**2 + &
                  (blk%y%current(i,j+1) - blk%y%current(i,j))**2)
          END IF
       END DO
    END DO

    ! on the edge it's only a half-distance

    j = jmin
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%hv2%current(i,j) = &
               SQRT(((blk%x%current(i,j+1) - blk%x%current(i,j)))**2 + &
               ((blk%y%current(i,j+1) - blk%y%current(i,j)))**2)
       END IF
    END DO
    j = jmax - 1
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%hv2%current(i,j) = &
               SQRT(((blk%x%current(i,j+1) - blk%x%current(i,j)))**2 + &
               ((blk%y%current(i,j+1) - blk%y%current(i,j)))**2)
       END IF
    END DO

    ! compute metric tensor and derivatives at the nodal points hp1, hp2

    DO i = imin+1, imax-1
       DO j=jmin+1, jmax-1
          IF (block_owns(blk, i, j)) THEN
             blk%x_eta%current(i,j) = 0.5*(blk%x_grid%current(i,j) + blk%x_grid%current(i-1,j) & 
                  - blk%x_grid%current(i,j-1) - blk%x_grid%current(i-1,j-1))
             blk%y_eta%current(i,j) = 0.5*(blk%y_grid%current(i,j) + blk%y_grid%current(i-1,j) & 
                  - blk%y_grid%current(i,j-1) - blk%y_grid%current(i-1,j-1))
             blk%x_xsi%current(i,j) = 0.5*(blk%x_grid%current(i,j) + blk%x_grid%current(i,j-1) & 
                  - blk%x_grid%current(i-1,j) - blk%x_grid%current(i-1,j-1))
             blk%y_xsi%current(i,j) = 0.5*(blk%y_grid%current(i,j) + blk%y_grid%current(i,j-1) & 
                  - blk%y_grid%current(i-1,j) - blk%y_grid%current(i-1,j-1))
          END IF
       END DO
    END DO
    i=imin
    DO j=jmin+1, jmax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_eta%current(i,j) = blk%x_grid%current(i,j) - blk%x_grid%current(i,j-1)
          blk%y_eta%current(i,j) = blk%y_grid%current(i,j) - blk%y_grid%current(i,j-1)
       END IF
    END DO
    i=imax-1
    DO j=jmin+1, jmax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_eta%current(i+1,j) = blk%x_grid%current(i,j) - blk%x_grid%current(i,j-1)
          blk%y_eta%current(i+1,j) = blk%y_grid%current(i,j) - blk%y_grid%current(i,j-1)
       END IF
    END DO
    j = jmin
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_xsi%current(i,j) = blk%x_grid%current(i,j) - blk%x_grid%current(i-1,j)
          blk%y_xsi%current(i,j) = blk%y_grid%current(i,j) - blk%y_grid%current(i-1,j)
       END IF
    END DO
    j=jmax-1
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_xsi%current(i,j+1) = blk%x_grid%current(i,j) - blk%x_grid%current(i-1,j)
          blk%y_xsi%current(i,j+1) = blk%y_grid%current(i,j) - blk%y_grid%current(i-1,j)
       END IF
    END DO

    IF (block_owns_i(blk, imin)) THEN
       blk%x_xsi%current(imin,:) = blk%x_xsi%current(imin+1,:)
       blk%y_xsi%current(imin,:) = blk%y_xsi%current(imin+1,:)
    END IF
    IF (block_owns_i(blk, imax)) THEN 
       blk%x_xsi%current(imax,:) = blk%x_xsi%current(imax-1,:)
       blk%y_xsi%current(imax,:) = blk%y_xsi%current(imax-1,:)
    END IF
    IF (block_owns_j(blk, jmin)) THEN
       blk%x_eta%current(:,jmin) = blk%x_eta%current(:,jmin+1)
       blk%y_eta%current(:,jmin) = blk%y_eta%current(:,jmin+1)
    END IF
    IF (block_owns_j(blk, jmax)) THEN
       blk%x_eta%current(:,jmax) = blk%x_eta%current(:,jmax-1)
       blk%y_eta%current(:,jmax) = blk%y_eta%current(:,jmax-1)
    END IF

    blk%hp1%current = SQRT(blk%x_xsi%current**2 + blk%y_xsi%current**2)
    blk%hp2%current = SQRT(blk%x_eta%current**2 + blk%y_eta%current**2)

    ! compute nonorthogonal part of the metric tensor as a check on grid quality

    blk%gp12%current = blk%x_xsi%current*blk%x_eta%current + &
         &blk%y_xsi%current*blk%y_eta%current

    CALL block_var_put(blk%hp1)
    CALL block_var_put(blk%hp2)
    CALL block_var_put(blk%hu1)
    CALL block_var_put(blk%hv2)
    CALL block_var_put(blk%gp12)
    CALL block_var_put(blk%x_xsi)
    CALL block_var_put(blk%y_xsi)
    CALL block_var_put(blk%x_eta)
    CALL block_var_put(blk%y_eta)
    CALL ga_sync()
    CALL block_var_get(blk%hp1)
    CALL block_var_get(blk%hp2)
    CALL block_var_get(blk%hu1)
    CALL block_var_get(blk%hv2)
    CALL block_var_get(blk%gp12)
    CALL block_var_get(blk%x_xsi)
    CALL block_var_get(blk%y_xsi)
    CALL block_var_get(blk%x_eta)
    CALL block_var_get(blk%y_eta)


  END SUBROUTINE block_metrics

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_geometry
  ! This routine is used to compute the geometry of the output.  It is
  ! intended to be the ONLY place where the coordinates of output data
  ! are computed.  This routine exists so that output grids can remain
  ! understandable even when the computation grid is complicated with
  ! ghost cells, half cells, etc.
  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_geometry()

  !   USE globals
  !   USE misc_vars, ONLY: i_ghost, j_ghost
  !   IMPLICIT NONE

  !   INTEGER :: iblock, i, j

  !   DO iblock = 1, max_blocks

  !                               ! without ghost cells, the output
  !                               ! locations are the same as the
  !                               ! computation locations

  !      block(iblock)%x_out = block(iblock)%x
  !      block(iblock)%y_out = block(iblock)%y
  !      block(iblock)%zbot_out = block(iblock)%zbot

  !                               ! with ghost cells at the up/down
  !                               ! stream end, the output locations
  !                               ! need to be interpolated at the block
  !                               ! ends. The number of ghost cells in
  !                               ! irrevalent, as long as there are
  !                               ! ghost cells

  !      IF (i_ghost .GT. 0) THEN
  !         i = 1
  !         block(iblock)%x_out(i,1) = block(iblock)%x_grid(i,1)
  !         block(iblock)%y_out(i,1) = block(iblock)%y_grid(i,1)
  !         block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i,1)
  !         DO j = 2, block(iblock)%ymax
  !            block(iblock)%x_out(i,j) = &
  !                 &0.5*(block(iblock)%x_grid(i,j-1) + block(iblock)%x_grid(i,j))
  !            block(iblock)%y_out(i,j) = &
  !                 &0.5*(block(iblock)%y_grid(i,j-1) + block(iblock)%y_grid(i,j))
  !            block(iblock)%zbot_out(i,j) = &
  !                 &0.5*(block(iblock)%zbot_grid(i,j-1) + block(iblock)%zbot_grid(i,j))
  !         END DO
  !         j = block(iblock)%ymax + 1
  !         block(iblock)%x_out(i,j) = block(iblock)%x_grid(i,j-1)
  !         block(iblock)%y_out(i,j) = block(iblock)%y_grid(i,j-1)
  !         block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i,j-1)
          
  !         i =  block(iblock)%xmax + 1
  !         block(iblock)%x_out(i,1) = block(iblock)%x_grid(i-1,1)
  !         block(iblock)%y_out(i,1) = block(iblock)%y_grid(i-1,1)
  !         block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i-1,1)
  !         DO j = 2, block(iblock)%ymax
  !            block(iblock)%x_out(i,j) = &
  !                 &0.5*(block(iblock)%x_grid(i-1,j-1) + block(iblock)%x_grid(i-1,j))
  !            block(iblock)%y_out(i,j) = &
  !                 &0.5*(block(iblock)%y_grid(i-1,j-1) + block(iblock)%y_grid(i-1,j))
  !            block(iblock)%zbot_out(i,j) = &
  !                 &0.5*(block(iblock)%zbot_grid(i-1,j-1) + block(iblock)%zbot_grid(i-1,j))
  !         END DO
  !         j = block(iblock)%ymax + 1
  !         block(iblock)%x_out(i,j) = block(iblock)%x_grid(i-1,j-1)
  !         block(iblock)%y_out(i,j) = block(iblock)%y_grid(i-1,j-1)
  !         block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i-1,j-1)
  !      END IF
       
  !                               ! ditto for lateral ghost cells

  !      IF (j_ghost .GT. 0) THEN
  !         j = 1
  !         DO i = 2, block(iblock)%xmax
  !            block(iblock)%x_out(i,j) = &
  !                 &0.5*(block(iblock)%x_grid(i,j) + block(iblock)%x_grid(i-1,j))
  !            block(iblock)%y_out(i,j) = &
  !                 &0.5*(block(iblock)%y_grid(i,j) + block(iblock)%y_grid(i-1,j))
  !            block(iblock)%zbot_out(i,j) = &
  !                 &0.5*(block(iblock)%zbot_grid(i,j) + block(iblock)%zbot_grid(i-1,j))
  !         END DO
  !         j = block(iblock)%ymax + 1
  !         DO i = 2, block(iblock)%xmax
  !            block(iblock)%x_out(i,j) = &
  !                 &0.5*(block(iblock)%x_grid(i,j-1) + block(iblock)%x_grid(i-1,j-1))
  !            block(iblock)%y_out(i,j) = &
  !                 &0.5*(block(iblock)%y_grid(i,j-1) + block(iblock)%y_grid(i-1,j-1))
  !            block(iblock)%zbot_out(i,j) = &
  !                 &0.5*(block(iblock)%zbot_grid(i,j-1) + block(iblock)%zbot_grid(i-1,j-1))
  !         END DO
  !      END IF

  !   END DO
  ! END SUBROUTINE plot_geometry


END MODULE block_parallel_grid
