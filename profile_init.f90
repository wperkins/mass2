! ----------------------------------------------------------------
! file: profile_init.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November 19, 1998 by William A. Perkins
! Last Change: Thu May  2 08:31:23 2002 by William A. Perkins <perk@leechong.pnl.gov>
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
! SUBROUTINE profile_init
! This routine is used to read a list of files and load initial water
! surface elevations (or depths) from the files.
! ----------------------------------------------------------------
SUBROUTINE profile_init(given_initial_wsel, manning, mann_con, status_iounit)

  USE globals
  USE misc_vars, ONLY: do_wetdry, dry_zero_depth

  IMPLICIT NONE

  LOGICAL :: given_initial_wsel
  LOGICAL :: manning
  DOUBLE PRECISION :: mann_con
  INTEGER :: status_iounit
  INTEGER :: iblock, imax, jmax
  INTEGER :: list_iounit = 25, grid_iounit=15, i, j, junk
  DOUBLE PRECISION :: xjunk, s, d, w, r
  REAL, ALLOCATABLE :: z(:,:)
  CHARACTER (LEN=80) :: list_file_name = "initial_specs.dat"
  CHARACTER (LEN=80) :: profile_file_name 

  EXTERNAL u_slope_avg, distance
  DOUBLE PRECISION u_slope_avg, distance

  imax = MAXVAL(block(:)%xmax)
  jmax = MAXVAL(block(:)%ymax)

  ALLOCATE(z(imax,jmax))

  OPEN(list_iounit,file=list_file_name)
  DO iblock = 1, max_blocks
     READ(list_iounit, *) profile_file_name
     OPEN(grid_iounit,file=profile_file_name)	

                                ! first line ignored

     READ(grid_iounit,*)junk, junk 
     
     IF (given_initial_wsel) THEN
        WRITE(status_iounit,*)'reading initial ws elevations for block n = ',iblock,&
             & ' from ', profile_file_name
     ELSE 
        WRITE(status_iounit,*)'reading initial depths for block n = ',iblock,&
             & ' from ', profile_file_name
     END IF

                                ! read the block's elevations into the
                                ! temporary array
	 
     DO i = 1, block(iblock)%xmax
        DO j = 1, block(iblock)%ymax
           READ(grid_iounit,*)junk,junk,xjunk,xjunk,z(i,j)
        END DO
     END DO

     CLOSE (grid_iounit)

     DO i = 2, block(iblock)%xmax
        DO j = 2, block(iblock)%ymax
           block(iblock)%depth(i,j) = 0.25*(z(i,j) + z(i-1,j) + z(i,j-1) + z(i-1,j-1))
        END DO
     END DO

     i = 1 
     DO j= 2, block(iblock)%ymax
        block(iblock)%depth(i,j) = 0.5*(z(i,j) + z(i,j-1))
     END DO

     i= block(iblock)%xmax+1
     DO j= 2, block(iblock)%ymax
        block(iblock)%depth(i,j) = 0.5*(z(i-1,j) + z(i-1,j-1))
     END DO

     j=1
     DO i=2, block(iblock)%xmax
        block(iblock)%depth(i,j) = 0.5*(z(i,j) + z(i-1,j))
     END DO

     j= block(iblock)%ymax+1
     DO i=2, block(iblock)%xmax
        block(iblock)%depth(i,j) = 0.5*(z(i,j-1) + z(i-1,j-1))
     END DO

                                ! do the corners

     block(iblock)%depth(&
          & (/1,1,block(iblock)%xmax+1,block(iblock)%xmax+1/),&
          & (/1,block(iblock)%ymax+1,1,block(iblock)%ymax+1/)) =&
          & z((/1,1,block(iblock)%xmax,block(iblock)%xmax/),&
          & (/1,block(iblock)%ymax,1,block(iblock)%ymax/))

     IF (given_initial_wsel)&
          & block(iblock)%depth = block(iblock)%depth - block(iblock)%zbot

     IF (do_wetdry) THEN
        WHERE (block(iblock)%depth .LT. dry_zero_depth)
           block(iblock)%depth = dry_zero_depth
        END WHERE
     END IF

                                ! initialize wsel

     block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot

  END DO

  CLOSE(list_iounit)

  DEALLOCATE(z)


                                ! initialize longitudinal velocities
                                ! with something reasonable

  DO iblock = 1, max_blocks
     block(iblock)%vvel = 0.0
     block(iblock)%vold = 0.0
     block(iblock)%vstar = 0.0
     DO i = 2, block(iblock)%xmax
        s = abs(u_slope_avg(iblock, i)) ! assume only downstream flow
        ! block(iblock)%uvel(i, 1) = 0.0
        DO j = 2, block(iblock)%ymax
           IF (i .LE. 1) THEN
              d = block(iblock)%depth(i, j)
              w = distance(block(iblock)%x_grid(i, j - 1), block(iblock)%y_grid(i, j - 1),&
                   & block(iblock)%x_grid(i, j), block(iblock)%y_grid(i, j))
           ELSE IF (i .GE. block(iblock)%xmax + 1) THEN
              d = block(iblock)%depth(i, j)
              w = distance(block(iblock)%x_grid(i, j - 1), block(iblock)%y_grid(i, j - 1),&
                   & block(iblock)%x_grid(i - 1, j), block(iblock)%y_grid(i - 1, j))
           ELSE
              d = (block(iblock)%depth(i, j) + block(iblock)%depth(i + 1, j))/2.0
              w = distance(block(iblock)%x_grid(i + 1, j - 1), block(iblock)%y_grid(i + 1, j - 1),&
                   & block(iblock)%x_grid(i + 1, j), block(iblock)%y_grid(i + 1, j))
           END IF
           r = d
           ! r = (d*w)/(2.0*d + w)
           IF (manning) THEN
              block(iblock)%uvel(i, j) =&
                   & (mann_con/block(iblock)%chezy(i,j))*(r**2.0)**(0.33333)*SQRT(s)
           ELSE
              block(iblock)%uvel(i, j) =&
                   & block(iblock)%chezy(i,j)*SQRT(r*s)
           END IF

                                ! to b safe, limit Froude number to 0.5

           IF (block(iblock)%uvel(i, j) > 0.5*SQRT(grav*d)) THEN
              block(iblock)%uvel(i, j) = 0.5*SQRT(grav*d)
           END IF

           ! WRITE(*,'("In profile_init: ", 3I5, 3F10.6)') iblock, i, j, d, s, block(iblock)%uvel(i, j)


        END DO
        ! block(iblock)%uvel(i, block(iblock)%ymax + 1) = 0.0
     END DO
     block(iblock)%uvel(1, :) = block(iblock)%uvel(2, :)
     block(iblock)%uvel(block(iblock)%xmax + 1, :) =&
          & block(iblock)%uvel(block(iblock)%xmax, :)
     block(iblock)%uold = block(iblock)%uvel
     block(iblock)%ustar = block(iblock)%uvel
  END DO

END SUBROUTINE profile_init
