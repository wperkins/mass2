PROGRAM cartgrid

USE elevtbl

IMPLICIT NONE

INTEGER :: gridfile =10, plotfile=11
INTEGER i,j,k
REAL :: delta_x,delta_y,slope,ystart,xstart,zmin
INTEGER :: xmax=150, ymax=15
REAL, ALLOCATABLE, DIMENSION(:,:) :: x,y,zbot

                                ! if true, read elevations from a file
                                ! and interpolate y-direction
                                ! elevations
LOGICAL :: dotable
CHARACTER (LEN=1024) :: tblfile

slope = 0.001
delta_x = 100.0
delta_y = 50.0
write(6,*)"This program create rectangular grids."
write(6,*)"No checks are made on data input so think before you type!"
write(6,*) "Input longitudinal spacing (delta x) desired"
read(5,*) delta_x
write(6,*) "Input lateral spacing (delta y) desired"
read(5,*) delta_y
write(6,*) "Input number of downstream nodes desired"
read(5,*)xmax
write(6,*) "Input number of cross stream nodes desired"
read(5,*)ymax
write(6,*) "Input downstream starting x coordinate"
read(5,*)xstart
write(6,*) "Input starting y coordinate"
read(5,*)ystart

tblfile = ''
write(6,*) "Enter name of bottom elevation file (RETURN for none)"
read(5,'(256A)') tblfile
dotable = (LEN_TRIM(tblfile) .GT. 0)

IF (dotable) THEN
   CALL elevread(tblfile)
ELSE
   write(6,*) "Input slope"
   read(5,*)slope
   write(6,*) "Input downstream bottom elevation"
   read(5,*)zmin
END IF

write(6,*)"Thanks! Output file is grid.out"
!write(6,*)"xstart=",xstart, "ystart=",ystart,"zmin=",zmin,"slope=",slope,xmax,ymax
ALLOCATE(x(xmax,ymax),y(xmax,ymax),zbot(xmax,ymax))
do j=1,ymax
 x(1,j)=xstart
end do
y(1,1)=ystart
  	DO i=2,xmax
    	x(i,1) = x(i-1,1) + delta_x
  	END DO
  	DO j=2,ymax
    	y(1,j) = y(1,j-1) + delta_y
  	END DO
  	DO i=2,xmax
    	DO j=1,ymax
    	x(i,j) = x(i,1)
    	y(i,j) = y(1,j)
    	END DO
  	END DO

IF (dotable) THEN
   DO i = 1, xmax
      zbot(i,:) = elevinterp(x(i,1))
   END DO
ELSE
   do k=1,ymax
      zbot(xmax,k) = zmin
   end do
   DO i=xmax-1,1,-1
      zbot(i,1) = zbot(i+1,1) + delta_x*slope
      DO j=2,ymax
         zbot(i,j) = zbot(i,1)
      END DO
  END DO
END IF

! write in grid x,y
1001 FORMAT(i5,2x,i5)
1000 FORMAT(i5,2x,i5,2x,g12.6,2x,g12.6,2x,g12.6)
!write(6,*)"did format "
OPEN(gridfile,file='grid.out')
!OPEN(10)
!write(6,*)'opened output'
  write(gridfile,1001)xmax, ymax
    DO i=1,xmax
      DO j=1,ymax
        WRITE(gridfile,1000)i,j,x(i,j),y(i,j),zbot(i,j)
        !write(6,*)"printint",i,j
      END DO
    END DO
CLOSE(gridfile)
END PROGRAM cartgrid

