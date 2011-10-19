! ----------------------------------------------------------------
! file: profile_init.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November 19, 1998 by William A. Perkins
! Last Change: Tue Oct 18 08:49:42 2011 by William A. Perkins <d3g096@flophouse>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE profile_init
! ----------------------------------------------------------------
MODULE profile_init

  USE config
  USE block_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! REAL FUNCTION u_slope
  ! This routine computes the longitudinal water surface slope at the i,
  ! j c.v. *face*.  Assumes wsel has been computed.
  !
  ! LOCAL: i, j ownership must be checked by caller
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION u_slope(iblock, i, j)
    IMPLICIT NONE

    INTEGER iblock, i, j
    DOUBLE PRECISION dz, dx

    EXTERNAL distance
    DOUBLE PRECISION distance

    IF (i .GE. block(iblock)%xmax + 1) THEN
       dx = distance(block(iblock)%x(i - 1,j), block(iblock)%y(i - 1,j),&
            & block(iblock)%x(i,j), block(iblock)%y(i,j))
       dz = block(iblock)%wsel(i - 1,j)- block(iblock)%wsel(i,j)
    ELSE
       dx = distance(block(iblock)%x(i,j), block(iblock)%y(i,j),&
            & block(iblock)%x(i + 1,j), block(iblock)%y(i + 1,j))
       dz = block(iblock)%wsel(i,j)- block(iblock)%wsel(i + 1,j)
    END IF
    u_slope = dz/dx
    ! WRITE(*,'("In u_slope: ", 3I5, 3F10.4)') iblock, i, j, dx, dz, dz/dx
  END FUNCTION u_slope

  ! ----------------------------------------------------------------
  ! REAL FUNCTION u_slope_avg
  ! This function computes the average (arithmatic mean) of longitudinal
  ! slope along a lateral c.v. face.
  ! 
  ! COLLECTIVE
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION u_slope_avg(iblk, i)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    INTEGER :: iblk, i, j
    DOUBLE PRECISION :: s, stot(1)
    INTEGER :: imin, imax, jmin, jmax

    CALL block_owned_window(block(iblk), imin, imax, jmin, jmax)

    stot(1) = 0.0
    IF (block_owns_i(block(iblk), i)) THEN
       DO j = jmin, jmax
          s = u_slope(iblk, i, j)
          stot(1) = stot(1) + s
       END DO
    END IF

    CALL ga_dgop(MT_F_DBL, stot, 1, '+');
    
    u_slope_avg = stot(1)/DBLE(block(iblk)%ymax + 1)
    ! WRITE(*,'("In u_slope_avg: ", 2I5, 2F10.6)') iblk, i, stot, u_slope_avg
  END FUNCTION u_slope_avg


  ! ----------------------------------------------------------------
  ! SUBROUTINE stage_init_file
  ! This will read the open file designated by iounit and initial the
  ! stage for the specified block iblk
  ! ----------------------------------------------------------------
  SUBROUTINE stage_init_file(given_initial_wsel, iblk, iounit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk, iounit
    LOGICAL, INTENT(IN) :: given_initial_wsel
    INTEGER :: i, j, junk
    DOUBLE PRECISION :: xjunk, zjunk
    REAL, ALLOCATABLE :: z(:,:)

    INTEGER :: imin, imax, jmin, jmax

    CALL block_used_window(block(iblk), imin, imax, jmin, jmax)

    ALLOCATE(z(imin:imax, jmin:jmax))

    READ(iounit,*)junk, junk 

    ! read the block's elevations into the
    ! temporary array

    DO i = 1, block(iblk)%xmax
       DO j = 1, block(iblk)%ymax
          READ(iounit,*)junk,junk,xjunk,xjunk,zjunk
          IF (block_uses(block(iblk), i, j)) THEN
             z(i,j) = zjunk
          END IF
       END DO
    END DO

    DO i = 2, block(iblk)%xmax
       DO j = 2, block(iblk)%ymax
          IF (block_owns(block(iblk), i, j)) THEN
             block(iblk)%depth(i,j) = 0.25*(z(i,j) + z(i-1,j) + z(i,j-1) + z(i-1,j-1))
          END IF
       END DO
    END DO

    i = 1 
    DO j= 2, block(iblk)%ymax
       IF (block_owns(block(iblk), i, j)) THEN
          block(iblk)%depth(i,j) = 0.5*(z(i,j) + z(i,j-1))
       END IF
    END DO

    i= block(iblk)%xmax+1
    DO j= 2, block(iblk)%ymax
       IF (block_owns(block(iblk), i, j)) THEN
          block(iblk)%depth(i,j) = 0.5*(z(i-1,j) + z(i-1,j-1))
       END IF
    END DO

    j=1
    DO i=2, block(iblk)%xmax
       IF (block_owns(block(iblk), i, j)) THEN
          block(iblk)%depth(i,j) = 0.5*(z(i,j) + z(i-1,j))
       END IF
    END DO

    j= block(iblk)%ymax+1
    DO i=2, block(iblk)%xmax
       IF (block_owns(block(iblk), i, j)) THEN
          block(iblk)%depth(i,j) = 0.5*(z(i,j-1) + z(i-1,j-1))
       END IF
    END DO

    ! do the corners

    IF (block_owns(block(iblk), 1, 1)) block(iblk)%depth(1, 1) = z(1, 1)
    IF (block_owns(block(iblk), 1, block(iblk)%ymax+1)) &
         &block(iblk)%depth(1, block(iblk)%ymax+1) = z(1, block(iblk)%ymax)
    IF (block_owns(block(iblk), block(iblk)%xmax+1, 1)) &
         &block(iblk)%depth(block(iblk)%xmax+1, 1) = z(block(iblk)%xmax, 1)
    IF (block_owns(block(iblk), block(iblk)%xmax+1, block(iblk)%ymax+1)) &
         &block(iblk)%depth(block(iblk)%xmax+1,block(iblk)%ymax+1 ) = &
         &z(block(iblk)%xmax, block(iblk)%ymax)

    IF (given_initial_wsel) &
         & block(iblk)%depth = block(iblk)%depth - block(iblk)%zbot

    WHERE ( block(iblk)%depth < dry_zero_depth ) 
       block(iblk)%depth = dry_zero_depth
    END WHERE

    ! initialize wsel

    block(iblk)%wsel = block(iblk)%depth + block(iblk)%zbot

    block(iblk)%depthold = block(iblk)%depth
    block(iblk)%depthstar = block(iblk)%depth

    CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_wsel, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_STAR)
    CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_OLD)
    CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_OLDOLD)
    CALL block_var_sync()
    CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_wsel, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_STAR)

    DEALLOCATE(z)

  END SUBROUTINE stage_init_file



  ! ----------------------------------------------------------------
  ! SUBROUTINE vel_init
  ! This routine estimates the initial velocity in the specified block
  ! assuming discharge is downstream and given an initial water surface
  ! elevation
  ! ----------------------------------------------------------------
  SUBROUTINE vel_init(iblk, manning, mann_con)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk
    LOGICAL, INTENT(IN) :: manning
    DOUBLE PRECISION, INTENT(IN) :: mann_con
    INTEGER :: i,j 
    DOUBLE PRECISION :: s, d, w, r, sign
    INTEGER :: imin, imax, jmin, jmax

    EXTERNAL distance
    DOUBLE PRECISION distance

    CALL block_owned_window(block(iblk), imin, imax, jmin, jmax)

    block(iblk)%vvel = 0.0
    block(iblk)%vvelold = 0.0
    block(iblk)%vvelstar = 0.0
    DO i = 2, block(iblk)%xmax
       s = u_slope_avg(iblk, i)
       if (s < 0.0) THEN 
          sign = -1.0
       ELSE
          sign = 1.0
       END if
       s = abs(s)
       ! s = abs(u_slope_avg(iblk, i)) ! assume only downstream flow
       ! block(iblk)%uvel(i, 1) = 0.0
       DO j = 2, block(iblk)%ymax
          IF (block_owns(block(iblk), i, j)) THEN
             IF (i .LE. 1) THEN
                d = block(iblk)%depth(i, j)
                w = distance(block(iblk)%x_grid(i, j - 1), block(iblk)%y_grid(i, j - 1),&
                     & block(iblk)%x_grid(i, j), block(iblk)%y_grid(i, j))
             ELSE IF (i .GE. block(iblk)%xmax + 1) THEN
                d = block(iblk)%depth(i, j)
                w = distance(block(iblk)%x_grid(i, j - 1), block(iblk)%y_grid(i, j - 1),&
                     & block(iblk)%x_grid(i - 1, j), block(iblk)%y_grid(i - 1, j))
             ELSE
                d = (block(iblk)%depth(i, j) + block(iblk)%depth(i + 1, j))/2.0
                w = distance(block(iblk)%x_grid(i + 1, j - 1), block(iblk)%y_grid(i + 1, j - 1),&
                     & block(iblk)%x_grid(i + 1, j), block(iblk)%y_grid(i + 1, j))
             END IF
             r = d
             ! r = (d*w)/(2.0*d + w)
             IF (manning) THEN
                block(iblk)%uvel(i, j) = &
                     & (mann_con/block(iblk)%chezy(i,j))*(r**2.0)**(0.33333)*SQRT(abs(s))
             ELSE
                block(iblk)%uvel(i, j) = &
                     & block(iblk)%chezy(i,j)*SQRT(r*abs(s))
             END IF
             
             ! to b safe, limit Froude number to 0.5
             
             IF (block(iblk)%uvel(i, j) > 0.5*SQRT(grav*d)) THEN
                block(iblk)%uvel(i, j) = 0.5*SQRT(grav*d)
             END IF
             
             block(iblk)%uvel(i, j) = block(iblk)%uvel(i, j)*sign

             ! WRITE(*,'("In profile_init: ", 3I5, 3F10.6)') iblk, i, j, d, s, block(iblk)%uvel(i, j)
          END IF

       END DO
       ! block(iblk)%uvel(i, block(iblk)%ymax + 1) = 0.0
    END DO

    ! assumes ghosts are owned by the same processor as the first cell

    IF (block_owns_i(block(iblk), 2)) &
         & block(iblk)%uvel(1, :) = block(iblk)%uvel(2, :)
    IF (block_owns_i(block(iblk), block(iblk)%xmax + 1))&
         & block(iblk)%uvel(block(iblk)%xmax + 1, :) =&
         & block(iblk)%uvel(block(iblk)%xmax, :)
    block(iblk)%uvelold = block(iblk)%uvel
    block(iblk)%uvelstar = block(iblk)%uvel

    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_STAR)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLD)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLDOLD)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_STAR)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLD)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLDOLD)
    CALL block_var_sync()
    CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_STAR)
    CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_STAR)
    

  END SUBROUTINE vel_init


  ! ----------------------------------------------------------------
  ! SUBROUTINE vel_init_file
  ! ----------------------------------------------------------------
  SUBROUTINE vel_init_file(iblk, xunit, yunit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk, xunit, yunit
    INTEGER :: i, j, junk
    DOUBLE PRECISION :: xjunk, jacob, ujunk, vjunk

    REAL, ALLOCATABLE :: vx(:,:), vy(:,:)
    INTEGER :: imin, imax, jmin, jmax

    CALL block_used_window(block(iblk), imin, imax, jmin, jmax)

    ALLOCATE(vx(imin:imax,jmin:jmax), vy(imin:imax,jmin:jmax))

    READ(xunit,*) junk, junk
    READ(yunit,*) junk, junk
    DO i = 1, block(iblk)%xmax
       DO j = 1, block(iblk)%ymax
          READ(xunit,*)junk,junk,xjunk,xjunk,ujunk
          READ(yunit,*)junk,junk,xjunk,xjunk,vjunk
          IF (block_uses(block(iblk), i, j)) THEN
             vx(i,j) = ujunk
             vy(i,j) = vjunk
          END IF
       END DO
    END DO

    block(iblk)%u_cart = 0.0
    block(iblk)%v_cart = 0.0

    DO i = 2, block(iblk)%xmax
       DO j = 2, block(iblk)%ymax

          IF (.NOT. block_owns(block(iblk), i, j)) CYCLE

          ! determine the cell-center velocity components

          block(iblk)%u_cart(i,j) = 0.25*(vx(i,j) + vx(i-1,j) + vx(i,j-1) + vx(i-1,j-1))
          block(iblk)%v_cart(i,j) = 0.25*(vy(i,j) + vy(i-1,j) + vy(i,j-1) + vy(i-1,j-1))

          ! compute velocity at the cell center in computational coordinates

          jacob = block(iblk)%hp1(i,j)**2*block(iblk)%hp2(i,j)**2 - block(iblk)%gp12(i,j)**2

          block(iblk)%uvel_p(i,j) = &
               &(block(iblk)%u_cart(i,j)*block(iblk)%y_eta(i,j) -&
               &  block(iblk)%v_cart(i,j)*block(iblk)%x_eta(i,j))*&
               &block(iblk)%hp1(i,j)/jacob

          block(iblk)%vvel_p(i,j) = &
               &(block(iblk)%u_cart(i,j)*block(iblk)%y_xsi(i,j) -&
               &  block(iblk)%v_cart(i,j)*block(iblk)%x_xsi(i,j))*&
               &block(iblk)%hp2(i,j)/jacob
       END DO
    END DO

    IF (block_uses_i(block(iblk), 1)) block(iblk)%vvel(1,:) = 0.0
    IF (block_uses_i(block(iblk), block(iblk)%ymax+1)) &
         &block(iblk)%vvel(block(iblk)%ymax+1,:) = 0.0

    IF (block_uses_i(block(iblk), 1)) block(iblk)%uvel(1,:) = block(iblk)%uvel_p(2,:)
    IF (block_uses_i(block(iblk), block(iblk)%xmax+1)) &
         &block(iblk)%uvel(block(iblk)%xmax+1,:) = block(iblk)%uvel_p(block(iblk)%xmax,:)

    DO i = 2, block(iblk)%xmax
       DO j = 2, block(iblk)%ymax
          IF (block_owns(block(iblk), i, j)) THEN
             block(iblk)%uvel(i,j) = 0.5*(block(iblk)%uvel_p(i,j) + block(iblk)%uvel_p(i+1,j))
             block(iblk)%vvel(i,j) = 0.5*(block(iblk)%vvel_p(i,j) + block(iblk)%vvel_p(i+1,j))
          END IF
       END DO
    END DO

    block(iblk)%uvelold = block(iblk)%uvel
    block(iblk)%uveloldold = block(iblk)%uvel
    block(iblk)%uvelstar = block(iblk)%uvel
    block(iblk)%vvelold = block(iblk)%vvel
    block(iblk)%vveloldold = block(iblk)%vvel
    block(iblk)%vvelstar = block(iblk)%vvel

    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_STAR)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLD)
    CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLDOLD)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_STAR)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLD)
    CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLDOLD)
    CALL block_var_sync()
    CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_STAR)
    CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
    CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_STAR)

    DEALLOCATE(vx, vy)

  END SUBROUTINE vel_init_file


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_initialize
  ! This routine is used to read a list of files and load initial water
  ! surface elevations (or depths) from the files.
  ! ----------------------------------------------------------------
  SUBROUTINE profile_initialize(given_initial_wsel, manning, mann_con)

    USE utility

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: given_initial_wsel
    LOGICAL, INTENT(IN) :: manning
    DOUBLE PRECISION, INTENT(IN) :: mann_con
    INTEGER :: iblk
    INTEGER :: list_iounit = 25, unit1=15, unit2=16
    CHARACTER (LEN=80) :: list_file_name = "initial_specs.dat"
    CHARACTER (LEN=1024) :: profile_file_name, uvel_file_name, vvel_file_name
    CHARACTER (LEN=1024) :: buf

    CALL open_existing(list_file_name, list_iounit)

    DO iblk = 1, max_blocks

       uvel_file_name = "NONE"
       vvel_file_name = "NONE"

       READ(list_iounit, *) profile_file_name, uvel_file_name, vvel_file_name

       CALL open_existing(profile_file_name, unit1)

       IF (given_initial_wsel) THEN
          WRITE(buf,*)'reading initial ws elevations for block n = ',iblk,&
               & ' from ', TRIM(profile_file_name)
       ELSE 
          WRITE(buf,*)'reading initial depths for block n = ',iblk,&
               & ' from ', TRIM(profile_file_name)
       END IF
       CALL status_message(buf)

       CALL stage_init_file(given_initial_wsel, iblk, unit1)

       CLOSE (unit1)

       IF (uvel_file_name .EQ. "NONE") THEN
          WRITE(buf,*)'computing initial longitudinal velocity (U) for block n = ', &
               &iblk, ' from initial water surface elevations'
          CALL status_message(buf)
          CALL vel_init(iblk, manning, mann_con)
       ELSE 
          CALL open_existing(uvel_file_name, unit1)
          WRITE(buf,*)'reading initial longitudinal velocity (U) for block n = ',&
               &iblk, ' from ', TRIM(uvel_file_name)
          CALL status_message(buf)

          IF (vvel_file_name .EQ. "NONE") THEN
             WRITE(buf, *) TRIM(list_file_name), &
                  &': northward (y) velocity file missing'
             CALL error_message(buf, FATAL=.TRUE.)
          END IF

          CALL open_existing(vvel_file_name, unit2)
          WRITE(buf,*)'reading initial lateral velocity (V) for block n = ',&
               &iblk, ' from ', TRIM(vvel_file_name)
          CALL status_message(buf)

          CALL vel_init_file(iblk, unit1, unit2)

          CLOSE(unit1)
          CLOSE(unit2)

       END IF
    END DO

    CLOSE(list_iounit)

  END SUBROUTINE profile_initialize

END MODULE profile_init


