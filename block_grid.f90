! ----------------------------------------------------------------
! file: block_grid.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 21, 2010 by William A. Perkins
! Last Change: Tue Oct 18 08:34:21 2011 by William A. Perkins <d3g096@flophouse>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE block_grid
!
! Block grid manipulation routines
! ----------------------------------------------------------------
MODULE block_grid

  USE utility
  USE config, ONLY: max_blocks, grid_file_name
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
    

    blk%x_grid(imin:imax, jmin:jmax) = x(imin:imax, jmin:jmax)
    blk%y_grid(imin:imax, jmin:jmax) = y(imin:imax, jmin:jmax)
    blk%zbot_grid(imin:imax, jmin:jmax) = z(imin:imax, jmin:jmax)
    
    CALL block_var_put(blk%bv_x_grid, BLK_VAR_CURRENT)
    CALL block_var_put(blk%bv_y_grid, BLK_VAR_CURRENT)
    CALL block_var_put(blk%bv_zbot_grid, BLK_VAR_CURRENT)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_x_grid, BLK_VAR_CURRENT)
    CALL block_var_get(blk%bv_y_grid, BLK_VAR_CURRENT)
    CALL block_var_get(blk%bv_zbot_grid, BLK_VAR_CURRENT)

    ! CALL ga_print(blk%y_grid%ga_handle)

    DEALLOCATE(x, y, z)

  END SUBROUTINE block_read_grid

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
             x = blk%x_grid(i+1,j) - &
                  &(blk%x_grid(i+2,j) - blk%x_grid(i+1,j))
             y = blk%y_grid(i+1,j) - &
                  &(blk%y_grid(i+2,j) - blk%y_grid(i+1,j))
             z = blk%zbot_grid(i+1,j) - &
                  &(blk%zbot_grid(i+2,j) - blk%zbot_grid(i+1,j))
             blk%x_grid(i,j) = x
             blk%y_grid(i,j) = y
             blk%zbot_grid(i,j) = z
          END DO
       END IF
       i = blk%xmax + ig
       IF (imin .LE. i .AND. i .LE. imax) THEN
          DO j = jmin, jmax
             x = blk%x_grid(i-1,j) - &
                  &(blk%x_grid(i-2,j) - blk%x_grid(i-1,j))
             y = blk%y_grid(i-1,j) - &
                  &(blk%y_grid(i-2,j) - blk%y_grid(i-1,j))
             z = blk%zbot_grid(i-1,j) - &
                  &(blk%zbot_grid(i-2,j) - blk%zbot_grid(i-1,j))
             blk%x_grid(i,j) = x
             blk%y_grid(i,j) = y
             blk%zbot_grid(i,j) = z
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
             x = blk%x_grid(i,j+1) - &
                  &(blk%x_grid(i,j+2) - blk%x_grid(i,j+1))
             y = blk%y_grid(i,j+1) - &
                  &(blk%y_grid(i,j+2) - blk%y_grid(i,j+1))
             z = blk%zbot_grid(i,j+1) - &
                  &(blk%zbot_grid(i,j+2) - blk%zbot_grid(i,j+1))
             blk%x_grid(i,j) = x
             blk%y_grid(i,j) = y
             blk%zbot_grid(i,j) = z
          END DO
       END IF
       j = blk%ymax + ig
       IF (jmin .LE. j .AND. j .LE. jmax) THEN
          DO i = imin, imax
             x = blk%x_grid(i,j-1) - &
                  &(blk%x_grid(i,j-2) - blk%x_grid(i,j-1))
             y = blk%y_grid(i,j-1) - &
                  &(blk%y_grid(i,j-2) - blk%y_grid(i,j-1))
             z = blk%zbot_grid(i,j-1) - &
                  &(blk%zbot_grid(i,j-2) - blk%zbot_grid(i,j-1))
             blk%x_grid(i,j) = x
             blk%y_grid(i,j) = y
             blk%zbot_grid(i,j) = z
          END DO
       END IF
    END DO

    ! update global arrays and make local copies current on all processors

    CALL block_var_put(blk%bv_x_grid)
    CALL block_var_put(blk%bv_y_grid)
    CALL block_var_put(blk%bv_zbot_grid)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_x_grid)
    CALL block_var_get(blk%bv_y_grid)
    CALL block_var_get(blk%bv_zbot_grid)
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

    CALL block_var_put(blk%bv_x_grid)
    CALL block_var_put(blk%bv_y_grid)
    CALL block_var_put(blk%bv_zbot_grid)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_x_grid)
    CALL block_var_get(blk%bv_y_grid)
    CALL block_var_get(blk%bv_zbot_grid)
    ! CALL ga_print(blk%y_grid%ga_handle)

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

    LOGICAL realequal
    EXTERNAL realequal

    IF (.NOT. block_owns(blk, i, j)) RETURN

    i1 = i + ioff
    i2 = i + 2*ioff
    j1 = j + joff
    j2 = j + 2*joff

    mi = 0.0
    bi = 0.0
    mj = 0.0
    bj = 0.0

    IF (.NOT. realequal(blk%x_grid(i, j2), blk%x_grid(i, j1))) THEN
       mi = (blk%y_grid(i, j2) - blk%y_grid(i, j1))/&
            &(blk%x_grid(i, j2) - blk%x_grid(i, j1))
       bi = blk%y_grid(i, j2) - mi*blk%x_grid(i, j2)
    END IF

    IF (.NOT. realequal(blk%x_grid(i2, j), blk%x_grid(i1, j))) THEN
       mj = (blk%y_grid(i2, j) - blk%y_grid(i1, j))/&
            &(blk%x_grid(i2, j) - blk%x_grid(i1, j))
       bj = blk%y_grid(i2, j) - mj*blk%x_grid(i2, j)
    END IF

    IF (realequal(blk%x_grid(i, j2), blk%x_grid(i, j1))) THEN
       blk%x_grid(i, j) = blk%x_grid(i, j2)
       blk%y_grid(i, j) = mj*blk%x_grid(i, j) + bj
    ELSE IF (realequal(blk%x_grid(i2, j), blk%x_grid(i1, j))) THEN
       blk%x_grid(i, j) = blk%x_grid(i2, j)
       blk%y_grid(i, j) = mi*blk%x_grid(i, j) + bi
    ELSE IF (.NOT. (realequal(mi, 0.0d00) .OR. realequal(mj, 0.0d00))) THEN
       blk%y_grid(i, j) = (bi - mi/mj*bj)*(mj/(mj - mi))
       blk%x_grid(i, j) = (blk%y_grid(i, j) - bj)/mj
    ELSE IF (.NOT. realequal(mi, 0.0d00)) THEN
       blk%y_grid(i, j) = blk%y_grid(i1, j)
       blk%x_grid(i, j) = (blk%y_grid(i, j) - bi)/mi
    ELSE IF (.NOT. realequal(mj, 0.0d00)) THEN
       blk%y_grid(i, j) = blk%y_grid(i, j1)
       blk%x_grid(i, j) = (blk%y_grid(i, j) - bj)/mj
    ELSE 
       CALL error_message("serious mesh problem", fatal=.TRUE.)
    END IF

    zi = blk%zbot_grid(i,j1) - (blk%zbot_grid(i,j2) - blk%zbot_grid(i,j1))
    zj = blk%zbot_grid(i1,j) - (blk%zbot_grid(i2,j) - blk%zbot_grid(i1,j))

    blk%zbot_grid(i,j) = 0.5*(zi+zj)

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

    IF (PRESENT(doghost)) mydoghost = doghost

    CALL open_new(fname, grid_iounit)

    nblk = UBOUND(block)

    WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
    WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""

    DO iblk= 1, nblk(1)

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
       CALL block_var_get_all(block(iblk)%bv_x_grid, block(iblk)%buffer)
       WRITE(grid_iounit,'(8G16.8)')block(iblk)%buffer(imin:imax,jmin:jmax)
       CALL block_var_get_all(block(iblk)%bv_y_grid, block(iblk)%buffer)
       WRITE(grid_iounit,'(8G16.8)')block(iblk)%buffer(imin:imax,jmin:jmax)
       CALL block_var_get_all(block(iblk)%bv_zbot_grid, block(iblk)%buffer)
       WRITE(grid_iounit,'(8G16.8)')block(iblk)%buffer(imin:imax,jmin:jmax)

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
          blk%x(i,j) = 0.25*(blk%x_grid(i,j)+blk%x_grid(i-1,j)+&
               & blk%x_grid(i,j-1)+blk%x_grid(i-1,j-1))
          blk%y(i,j) = 0.25*(blk%y_grid(i,j)+blk%y_grid(i-1,j)+&
               & blk%y_grid(i,j-1)+blk%y_grid(i-1,j-1))
          blk%zbot(i,j) = 0.25*(blk%zbot_grid(i,j)+blk%zbot_grid(i-1,j)+&
               & blk%zbot_grid(i,j-1)+blk%zbot_grid(i-1,j-1))
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
          IF (j .GT. j_index_min) THEN 
             blk%x(i,j) = 0.5*(blk%x_grid(i,j)+blk%x_grid(i,j-1))
             blk%y(i,j) = 0.5*(blk%y_grid(i,j)+blk%y_grid(i,j-1))
             blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j)+blk%zbot_grid(i,j-1))
          ELSE 
             blk%x(i,j) = blk%x_grid(i,j)
             blk%y(i,j) = blk%y_grid(i,j)
             blk%zbot(i,j) = blk%zbot_grid(i,j)
          END IF
       END DO
    END IF

    i= blk%xmax+i_index_extra
    IF (block_owns_i(blk, i)) THEN
       DO j= jmin, jmax
          IF (j .GT. j_index_min) THEN 
             blk%x(i,j) = 0.5*(blk%x_grid(i-1,j)+blk%x_grid(i-1,j-1))
             blk%y(i,j) = 0.5*(blk%y_grid(i-1,j)+blk%y_grid(i-1,j-1))
             blk%zbot(i,j) = 0.5*(blk%zbot_grid(i-1,j)+blk%zbot_grid(i-1,j-1))
          ELSE 
             blk%x(i,j) = blk%x_grid(i-1,j)
             blk%y(i,j) = blk%y_grid(i-1,j)
             blk%zbot(i,j) = blk%zbot_grid(i-1,j)
          END IF
       END DO
    END IF

    j=j_index_min
    IF (block_owns_j(blk, j)) THEN
       DO i = imin, imax
          IF (i .GT. i_index_min) THEN
             blk%x(i,j) = 0.5*(blk%x_grid(i,j)+blk%x_grid(i-1,j))
             blk%y(i,j) = 0.5*(blk%y_grid(i,j)+blk%y_grid(i-1,j))
             blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j)+blk%zbot_grid(i-1,j))
          ELSE 
             blk%x(i,j) = blk%x_grid(i,j)
             blk%y(i,j) = blk%y_grid(i,j)
             blk%zbot(i,j) = blk%zbot_grid(i,j)
          END IF
       END DO
    END IF

    j= blk%ymax+j_index_extra
    IF (block_owns_j(blk, j)) THEN
       DO i = imin, imax
          IF (i .GT. i_index_min) THEN
             blk%x(i,j) = 0.5*(blk%x_grid(i,j-1)+blk%x_grid(i-1,j-1))
             blk%y(i,j) = 0.5*(blk%y_grid(i,j-1)+blk%y_grid(i-1,j-1))
             blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j-1)+blk%zbot_grid(i-1,j-1))
          ELSE 
             blk%x(i,j) = blk%x_grid(i,j-1)
             blk%y(i,j) = blk%y_grid(i,j-1)
             blk%zbot(i,j) = blk%zbot_grid(i,j-1)
          END IF
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
          blk%x(i,j) = blk%x_grid(ivec2(idx),jvec2(idx))
          blk%y(i,j) = blk%y_grid(ivec2(idx),jvec2(idx))
          blk%zbot(i,j) = blk%zbot_grid(ivec2(idx),jvec2(idx))
       END IF
    END DO

    CALL block_var_put(blk%bv_x)
    CALL block_var_put(blk%bv_y)
    CALL block_var_put(blk%bv_zbot)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_x)
    CALL block_var_get(blk%bv_y)
    CALL block_var_get(blk%bv_zbot)
    
  END SUBROUTINE block_interp_grid


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_metrics
  ! Compute the metric coefficients for the block. Depending on the
  ! metric coeff. and location use either the grid (x,y) or the node
  ! (x,y)
  ! ----------------------------------------------------------------
  SUBROUTINE block_metrics(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk

    INTEGER :: imin, imax, jmin, jmax, i, j
    DOUBLE PRECISION :: a, b, c, d, e, f, g, h, ii, di, dj, dzdi, dzdj

    imin = blk%varbase%imin_global
    imax = blk%varbase%imax_global
    jmin = blk%varbase%jmin_global
    jmax = blk%varbase%jmax_global

    ! metric coeff. 2 on the u face of the c.v.

    blk%hu2 = 1.0e-20            ! bogus nonzero value
    DO i=imin, imax-1
       DO j=jmin+1, jmax-1
          IF (block_owns(blk, i, j)) THEN
             blk%hu2(i,j) = & 
                  SQRT((blk%x_grid(i,j) - blk%x_grid(i,j-1))**2 + &
                  (blk%y_grid(i,j) - blk%y_grid(i,j-1))**2)
          END IF
       END DO
    END DO

    ! metric coeff 1 on the u face of the c.v.

    blk%hu1 = 1.0e-20            ! bogus nonzero value
    DO i=imin+1, imax-i_index_extra
       DO j=jmin, jmax
          IF (block_owns(blk, i, j)) THEN
             blk%hu1(i,j) = &
                  SQRT((blk%x(i+1,j) - blk%x(i,j))**2 + &
                  (blk%y(i+1,j) - blk%y(i,j))**2)
          END IF
       END DO
    END DO

    ! on the edge it's only a half-distance

    i=imin
    DO j=jmin, jmax
       IF (block_owns(blk, i, j)) THEN
          blk%hu1(i,j) = &
               SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
               ((blk%y(i+1,j) - blk%y(i,j)))**2)
       END IF
    END DO
    i=imax-1
    DO j=jmin, jmax
       IF (block_owns(blk, i, j)) THEN
          blk%hu1(i,j) = &
               SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
               ((blk%y(i+1,j) - blk%y(i,j)))**2)
       END IF
    END DO

    ! metric coeff. 1 on the v face of the c.v.

    DO i=imin+1, imax-1
       DO j=jmin, jmax - 1
          IF (block_owns(blk, i, j)) THEN
             blk%hv1(i,j) = &
                  SQRT((blk%x_grid(i,j) - blk%x_grid(i-1,j))**2 + &
                  (blk%y_grid(i,j) - blk%y_grid(i-1,j))**2) 
          END IF
       END DO
    END DO

    ! metric coeff. 2 on the v face of the c.v.

    DO i=imin, imax
       DO j=jmin+1, jmax - 1
          IF (block_owns(blk, i, j)) THEN
             blk%hv2(i,j) = &
                  SQRT((blk%x(i,j+1) - blk%x(i,j))**2 + &
                  (blk%y(i,j+1) - blk%y(i,j))**2)
          END IF
       END DO
    END DO

    ! on the edge it's only a half-distance

    j = jmin
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%hv2(i,j) = &
               SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
               ((blk%y(i,j+1) - blk%y(i,j)))**2)
       END IF
    END DO
    j = jmax - 1
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%hv2(i,j) = &
               SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
               ((blk%y(i,j+1) - blk%y(i,j)))**2)
       END IF
    END DO

    ! compute metric tensor and derivatives at the nodal points hp1, hp2

    DO i = imin+1, imax-1
       DO j=jmin+1, jmax-1
          IF (block_owns(blk, i, j)) THEN
             blk%x_eta(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i-1,j) & 
                  - blk%x_grid(i,j-1) - blk%x_grid(i-1,j-1))
             blk%y_eta(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i-1,j) & 
                  - blk%y_grid(i,j-1) - blk%y_grid(i-1,j-1))
             blk%x_xsi(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i,j-1) & 
                  - blk%x_grid(i-1,j) - blk%x_grid(i-1,j-1))
             blk%y_xsi(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i,j-1) & 
                  - blk%y_grid(i-1,j) - blk%y_grid(i-1,j-1))
          END IF
       END DO
    END DO
    i=imin
    DO j=jmin+1, jmax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_eta(i,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
          blk%y_eta(i,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
       END IF
    END DO
    i=imax-1
    DO j=jmin+1, jmax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_eta(i+1,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
          blk%y_eta(i+1,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
       END IF
    END DO
    j = jmin
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_xsi(i,j) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
          blk%y_xsi(i,j) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
       END IF
    END DO
    j=jmax-1
    DO i=imin+1, imax-1
       IF (block_owns(blk, i, j)) THEN
          blk%x_xsi(i,j+1) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
          blk%y_xsi(i,j+1) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
       END IF
    END DO

    IF (block_owns_i(blk, imin)) THEN
       blk%x_xsi(imin,:) = blk%x_xsi(imin+1,:)
       blk%y_xsi(imin,:) = blk%y_xsi(imin+1,:)
    END IF
    IF (block_owns_i(blk, imax)) THEN 
       blk%x_xsi(imax,:) = blk%x_xsi(imax-1,:)
       blk%y_xsi(imax,:) = blk%y_xsi(imax-1,:)
    END IF
    IF (block_owns_j(blk, jmin)) THEN
       blk%x_eta(:,jmin) = blk%x_eta(:,jmin+1)
       blk%y_eta(:,jmin) = blk%y_eta(:,jmin+1)
    END IF
    IF (block_owns_j(blk, jmax)) THEN
       blk%x_eta(:,jmax) = blk%x_eta(:,jmax-1)
       blk%y_eta(:,jmax) = blk%y_eta(:,jmax-1)
    END IF

    blk%hp1 = SQRT(blk%x_xsi**2 + blk%y_xsi**2)
    blk%hp2 = SQRT(blk%x_eta**2 + blk%y_eta**2)

    ! compute nonorthogonal part of the metric tensor as a check on grid quality

    blk%gp12 = blk%x_xsi*blk%x_eta + &
         &blk%y_xsi*blk%y_eta

    ! compute a slope (like Arc/Info, using an 8 point scheme) for each cell
    
    imin = MAX(blk%varbase%imin_owned, 2)
    imax = MIN(blk%varbase%imax_owned, blk%xmax)
    jmin = MAX(blk%varbase%jmin_owned, 2)
    jmax = MIN(blk%varbase%jmax_owned, blk%ymax)
    DO i = imin, imax
       DO j = jmin, jmax
          a = blk%zbot(i-1, j-1)
          b = blk%zbot(i  , j-1)
          c = blk%zbot(i+1, j-1)
          d = blk%zbot(i-1, j  )
          e = blk%zbot(i  , j  )
          f = blk%zbot(i+1, j  )
          g = blk%zbot(i-1, j+1)
          h = blk%zbot(i  , j+1)
          ii = blk%zbot(i+1, j+1)

          di = blk%hp1(i, j)
          dj = blk%hp2(i, j)
          
          dzdi = ((a + 2.0*d + g) - (c + 2.0*f + ii)) / (8.0 * di)
          dzdj = ((a + 2.0*b + c) - (g + 2.0*h + ii)) / (8.0 * dj)
          blk%slope(i,j) = SQRT(dzdi*dzdi + dzdj*dzdj)
       END DO
    END DO

    CALL block_var_put(blk%bv_hp1)
    CALL block_var_put(blk%bv_hp2)
    CALL block_var_put(blk%bv_hu1)
    CALL block_var_put(blk%bv_hu2)
    CALL block_var_put(blk%bv_hv1)
    CALL block_var_put(blk%bv_hv2)
    CALL block_var_put(blk%bv_gp12)
    CALL block_var_put(blk%bv_x_xsi)
    CALL block_var_put(blk%bv_y_xsi)
    CALL block_var_put(blk%bv_x_eta)
    CALL block_var_put(blk%bv_y_eta)
    CALL block_var_put(blk%bv_slope)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_hp1)
    CALL block_var_get(blk%bv_hp2)
    CALL block_var_get(blk%bv_hu1)
    CALL block_var_get(blk%bv_hu2)
    CALL block_var_get(blk%bv_hv1)
    CALL block_var_get(blk%bv_hv2)
    CALL block_var_get(blk%bv_gp12)
    CALL block_var_get(blk%bv_x_xsi)
    CALL block_var_get(blk%bv_y_xsi)
    CALL block_var_get(blk%bv_x_eta)
    CALL block_var_get(blk%bv_y_eta)


  END SUBROUTINE block_metrics

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_plot_geometry
  ! This routine is used to compute the geometry of the output.  It is
  ! intended to be the ONLY place where the coordinates of output data
  ! are computed.  This routine exists so that output grids can remain
  ! understandable even when the computation grid is complicated with
  ! ghost cells, half cells, etc.
  ! ----------------------------------------------------------------
  SUBROUTINE block_plot_geometry(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER :: i, j

    ! without ghost cells, the output locations are the same as the
    ! computation locations

    blk%x_out = blk%x
    blk%y_out = blk%y
    blk%zbot_out = blk%zbot

    ! with ghost cells at the up/down stream end, the output
    ! locations need to be interpolated at the block ends. The
    ! number of ghost cells in irrevalent, as long as there are
    ! ghost cells

    IF (i_ghost .GT. 0) THEN
       i = 1
       IF (block_owns(blk, i, 1)) THEN
          blk%x_out(i,1) = blk%x_grid(i,1)
          blk%y_out(i,1) = blk%y_grid(i,1)
          blk%zbot_out(i,1) = blk%zbot_grid(i,1)
       END IF
       DO j = 2, blk%ymax
          IF (block_owns(blk, i, j)) THEN
             blk%x_out(i,j) = &
                  &0.5*(blk%x_grid(i,j-1) + blk%x_grid(i,j))
             blk%y_out(i,j) = &
                  &0.5*(blk%y_grid(i,j-1) + blk%y_grid(i,j))
             blk%zbot_out(i,j) = &
                  &0.5*(blk%zbot_grid(i,j-1) + blk%zbot_grid(i,j))
          END IF
       END DO
       j = blk%ymax + 1
       IF (block_owns(blk, i, j)) THEN
          blk%x_out(i,j) = blk%x_grid(i,j-1)
          blk%y_out(i,j) = blk%y_grid(i,j-1)
          blk%zbot_out(i,j) = blk%zbot_grid(i,j-1)
       END IF

       i =  blk%xmax + 1
       IF (block_owns(blk, i, 1)) THEN
          blk%x_out(i,1) = blk%x_grid(i-1,1)
          blk%y_out(i,1) = blk%y_grid(i-1,1)
          blk%zbot_out(i,1) = blk%zbot_grid(i-1,1)
       END IF
       DO j = 2, blk%ymax
          IF (block_owns(blk, i, j)) THEN
             blk%x_out(i,j) = &
                  &0.5*(blk%x_grid(i-1,j-1) + blk%x_grid(i-1,j))
             blk%y_out(i,j) = &
                  &0.5*(blk%y_grid(i-1,j-1) + blk%y_grid(i-1,j))
             blk%zbot_out(i,j) = &
                  &0.5*(blk%zbot_grid(i-1,j-1) + blk%zbot_grid(i-1,j))
          END IF
       END DO
       j = blk%ymax + 1
       IF (block_owns(blk, i, j)) THEN
          blk%x_out(i,j) = blk%x_grid(i-1,j-1)
          blk%y_out(i,j) = blk%y_grid(i-1,j-1)
          blk%zbot_out(i,j) = blk%zbot_grid(i-1,j-1)
       END IF
    END IF

    ! ditto for lateral ghost cells

    IF (j_ghost .GT. 0) THEN
       j = 1
       DO i = 2, blk%xmax
          IF (block_owns(blk, i, j)) THEN 
             blk%x_out(i,j) = &
                  &0.5*(blk%x_grid(i,j) + blk%x_grid(i-1,j))
             blk%y_out(i,j) = &
                  &0.5*(blk%y_grid(i,j) + blk%y_grid(i-1,j))
             blk%zbot_out(i,j) = &
                  &0.5*(blk%zbot_grid(i,j) + blk%zbot_grid(i-1,j))
          END IF
       END DO
       j = blk%ymax + 1
       DO i = 2, blk%xmax
          IF (block_owns(blk, i, j)) THEN 
             blk%x_out(i,j) = &
                  &0.5*(blk%x_grid(i,j-1) + blk%x_grid(i-1,j-1))
             blk%y_out(i,j) = &
                  &0.5*(blk%y_grid(i,j-1) + blk%y_grid(i-1,j-1))
             blk%zbot_out(i,j) = &
                  &0.5*(blk%zbot_grid(i,j-1) + blk%zbot_grid(i-1,j-1))
          END IF
       END DO
    END IF

    CALL block_var_put(blk%bv_x_out)
    CALL block_var_put(blk%bv_y_out)
    CALL block_var_put(blk%bv_zbot_out)
    CALL block_var_sync()
    CALL block_var_get(blk%bv_x_out)
    CALL block_var_get(blk%bv_y_out)
    CALL block_var_get(blk%bv_zbot_out)

  END SUBROUTINE block_plot_geometry

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_geometry
  ! ----------------------------------------------------------------
  SUBROUTINE plot_geometry()

    IMPLICIT NONE

    INTEGER :: b

    DO b = 1, max_blocks
       CALL block_plot_geometry(block(b))
    END DO

  END SUBROUTINE plot_geometry




END MODULE block_grid
