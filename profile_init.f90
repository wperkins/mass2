! ----------------------------------------------------------------
! file: profile_init.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November 19, 1998 by William A. Perkins
! Last Change: Thu Jan 29 16:17:38 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! REAL FUNCTION u_slope
! This routine computes the longitudinal water surface slope at the i,
! j c.v. *face*.  Assumes wsel has been computed.
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION u_slope(iblock, i, j)
  USE globals
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
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION u_slope_avg(iblock, i)
  USE globals
  IMPLICIT NONE

  INTEGER :: iblock, i, j
  DOUBLE PRECISION :: s, stot
  EXTERNAL u_slope
  DOUBLE PRECISION u_slope
  
  stot = 0.0
  DO j = 1, block(iblock)%ymax + 1
     s = u_slope(iblock, i, j)
     stot = stot + s
  END DO 
  u_slope_avg = stot/DBLE(block(iblock)%ymax + 1)
  ! WRITE(*,'("In u_slope_avg: ", 2I5, 2F10.6)') iblock, i, stot, u_slope_avg
END FUNCTION u_slope_avg


! ----------------------------------------------------------------
! SUBROUTINE stage_init_file
! This will read the open file designated by iounit and initial the
! stage for the specified block iblk
! ----------------------------------------------------------------
SUBROUTINE stage_init_file(given_initial_wsel, iblk, iounit)

  USE globals
  USE misc_vars, ONLY: do_wetdry, dry_zero_depth

  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: iblk, iounit
  LOGICAL, INTENT(IN) :: given_initial_wsel
  INTEGER :: i, j, junk
  DOUBLE PRECISION :: xjunk
  REAL, ALLOCATABLE :: z(:,:)


  ALLOCATE(z(block(iblk)%xmax,block(iblk)%ymax))

  READ(iounit,*)junk, junk 
     
                                ! read the block's elevations into the
                                ! temporary array
	 
  DO i = 1, block(iblk)%xmax
     DO j = 1, block(iblk)%ymax
        READ(iounit,*)junk,junk,xjunk,xjunk,z(i,j)
     END DO
  END DO

  DO i = 2, block(iblk)%xmax
     DO j = 2, block(iblk)%ymax
        block(iblk)%depth(i,j) = 0.25*(z(i,j) + z(i-1,j) + z(i,j-1) + z(i-1,j-1))
     END DO
  END DO

  i = 1 
  DO j= 2, block(iblk)%ymax
     block(iblk)%depth(i,j) = 0.5*(z(i,j) + z(i,j-1))
  END DO

  i= block(iblk)%xmax+1
  DO j= 2, block(iblk)%ymax
     block(iblk)%depth(i,j) = 0.5*(z(i-1,j) + z(i-1,j-1))
  END DO
  
  j=1
  DO i=2, block(iblk)%xmax
     block(iblk)%depth(i,j) = 0.5*(z(i,j) + z(i-1,j))
  END DO

  j= block(iblk)%ymax+1
  DO i=2, block(iblk)%xmax
     block(iblk)%depth(i,j) = 0.5*(z(i,j-1) + z(i-1,j-1))
  END DO

                                ! do the corners

  block(iblk)%depth(&
       & (/1,1,block(iblk)%xmax+1,block(iblk)%xmax+1/),&
       & (/1,block(iblk)%ymax+1,1,block(iblk)%ymax+1/)) =&
       & z((/1,1,block(iblk)%xmax,block(iblk)%xmax/),&
       & (/1,block(iblk)%ymax,1,block(iblk)%ymax/))

  IF (given_initial_wsel)&
       & block(iblk)%depth = block(iblk)%depth - block(iblk)%zbot

  IF (do_wetdry) THEN
     WHERE (block(iblk)%depth .LT. dry_zero_depth)
        block(iblk)%depth = dry_zero_depth
     END WHERE
  END IF

                                ! initialize wsel

  block(iblk)%wsel = block(iblk)%depth + block(iblk)%zbot

  block(iblk)%depthold = block(iblk)%depth
  block(iblk)%dstar = block(iblk)%depth


  DEALLOCATE(z)

END SUBROUTINE stage_init_file



! ----------------------------------------------------------------
! SUBROUTINE vel_init
! This routine estimates the initial velocity in the specified block
! assuming discharge is downstream and given an initial water surface
! elevation
! ----------------------------------------------------------------
SUBROUTINE vel_init(iblk, manning, mann_con)

  USE globals

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblk
  LOGICAL, INTENT(IN) :: manning
  DOUBLE PRECISION, INTENT(IN) :: mann_con
  INTEGER :: i,j 
  DOUBLE PRECISION :: s, d, w, r, sign

  EXTERNAL u_slope_avg, distance
  DOUBLE PRECISION u_slope_avg, distance

  block(iblk)%vvel = 0.0
  block(iblk)%vold = 0.0
  block(iblk)%vstar = 0.0
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
        
        
     END DO
     ! block(iblk)%uvel(i, block(iblk)%ymax + 1) = 0.0
  END DO
  block(iblk)%uvel(1, :) = block(iblk)%uvel(2, :)
  block(iblk)%uvel(block(iblk)%xmax + 1, :) =&
       & block(iblk)%uvel(block(iblk)%xmax, :)
  block(iblk)%uold = block(iblk)%uvel
  block(iblk)%ustar = block(iblk)%uvel

END SUBROUTINE vel_init


! ----------------------------------------------------------------
! SUBROUTINE vel_init_file
! ----------------------------------------------------------------
SUBROUTINE vel_init_file(iblk, xunit, yunit)

  USE globals

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblk, xunit, yunit
  INTEGER :: i, j, junk
  DOUBLE PRECISION :: xjunk, jacob

  REAL, ALLOCATABLE :: vx(:,:), vy(:,:)


  ALLOCATE(vx(block(iblk)%xmax,block(iblk)%ymax), &
       &vy(block(iblk)%xmax,block(iblk)%ymax))

  READ(xunit,*) junk, junk
  READ(yunit,*) junk, junk
  DO i = 1, block(iblk)%xmax
     DO j = 1, block(iblk)%ymax
        READ(xunit,*)junk,junk,xjunk,xjunk,vx(i,j)
        READ(yunit,*)junk,junk,xjunk,xjunk,vy(i,j)
     END DO
  END DO

  block(iblk)%u_cart(i,j) = 0.0
  block(iblk)%v_cart(i,j) = 0.0

  DO i = 2, block(iblk)%xmax
     DO j = 2, block(iblk)%ymax
        
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

  block(iblk)%vvel(1,:) = 0.0
  block(iblk)%vvel(block(iblk)%ymax+1,:) = 0.0

  block(iblk)%uvel(1,:) = block(iblk)%uvel_p(2,:)
  block(iblk)%uvel(block(iblk)%xmax+1,:) = block(iblk)%uvel_p(block(iblk)%xmax-1,:)

  DO i = 2, block(iblk)%xmax
     DO j = 2, block(iblk)%ymax
        block(iblk)%uvel(i,j) = 0.5*(block(iblk)%uvel_p(i,j) + block(iblk)%uvel_p(i+1,j))
        block(iblk)%vvel(i,j) = 0.5*(block(iblk)%vvel_p(i,j) + block(iblk)%vvel_p(i+1,j))
     END DO
  END DO

  DEALLOCATE(vx, vy)

END SUBROUTINE vel_init_file


! ----------------------------------------------------------------
! SUBROUTINE profile_init
! This routine is used to read a list of files and load initial water
! surface elevations (or depths) from the files.
! ----------------------------------------------------------------
SUBROUTINE profile_init(given_initial_wsel, manning, mann_con)

  USE globals
  USE misc_vars, ONLY: do_wetdry, dry_zero_depth
  USE utility

  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: given_initial_wsel
  LOGICAL, INTENT(IN) :: manning
  DOUBLE PRECISION, INTENT(IN) :: mann_con
  INTEGER :: iblock
  INTEGER :: list_iounit = 25, unit1=15, unit2=16
  CHARACTER (LEN=80) :: list_file_name = "initial_specs.dat"
  CHARACTER (LEN=1024) :: profile_file_name, uvel_file_name, vvel_file_name
  CHARACTER (LEN=1024) :: buf

  CALL open_existing(list_file_name, list_iounit)

  DO iblock = 1, max_blocks

     uvel_file_name = "NONE"
     vvel_file_name = "NONE"

     READ(list_iounit, *) profile_file_name, uvel_file_name, vvel_file_name

     CALL open_existing(profile_file_name, unit1)

     IF (given_initial_wsel) THEN
        WRITE(buf,*)'reading initial ws elevations for block n = ',iblock,&
             & ' from ', TRIM(profile_file_name)
     ELSE 
        WRITE(buf,*)'reading initial depths for block n = ',iblock,&
             & ' from ', TRIM(profile_file_name)
     END IF
     CALL status_message(buf)

     CALL stage_init_file(given_initial_wsel, iblock, unit1)

     CLOSE (unit1)

     IF (uvel_file_name .EQ. "NONE") THEN
        WRITE(buf,*)'computing initial longitudinal velocity (U) for block n = ', &
             &iblock, ' from initial water surface elevations'
        CALL status_message(buf)
        CALL vel_init(iblock, manning, mann_con)
     ELSE 
        CALL open_existing(uvel_file_name, unit1)
        WRITE(buf,*)'reading initial longitudinal velocity (U) for block n = ',&
             &iblock, ' from ', TRIM(uvel_file_name)
        CALL status_message(buf)

        IF (vvel_file_name .EQ. "NONE") THEN
           WRITE(buf, *) TRIM(list_file_name), &
                &': northward (y) velocity file missing'
           CALL error_message(buf, FATAL=.TRUE.)
        END IF

        CALL open_existing(vvel_file_name, unit2)
        WRITE(buf,*)'reading initial lateral velocity (V) for block n = ',&
             &iblock, ' from ', TRIM(vvel_file_name)
        CALL status_message(buf)

        CALL vel_init_file(iblock, unit1, unit2)

        CLOSE(unit1)
        CLOSE(unit2)
           
     END IF
  END DO

  CLOSE(list_iounit)

END SUBROUTINE profile_init
