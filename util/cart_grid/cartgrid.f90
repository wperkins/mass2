PROGRAM cartgrid

IMPLICIT NONE

INTEGER :: gridfile =10, plotfile=11
INTEGER i,j,k
REAL :: delta_x,delta_y,slope,ystart,xstart,zmin
INTEGER :: xmax=150, ymax=15
REAL, ALLOCATABLE, DIMENSION(:,:) :: x,y,zbot

x = 0.0
y = 0.0
zbot = 0.0
slope = 0.001
write(6,*)"This program create rectangular grids that have"
write(6,*)"a cell size of 100 by 50 ft"
write(6,*)"No checks are made on data input so think before you type!"
write(6,*) "Input number of downstream node desired"
read(5,*)xmax
write(6,*) "Input number of cross stream nodes desired"
read(5,*)ymax
write(6,*) "Input downstream starting x coordinate"
read(5,*)xstart
write(6,*) "Input starting y coordinate"
read(5,*)ystart
write(6,*) "Input slope"
read(5,*)slope
write(6,*) "Input downstream bottom elevation"
read(5,*)zmin
write(6,*)"Thanks! Output file is grid.out"
!write(6,*)"xstart=",xstart, "ystart=",ystart,"zmin=",zmin,"slope=",slope,xmax,ymax
ALLOCATE(x(xmax,ymax),y(xmax,ymax),zbot(xmax,ymax))
do j=1,ymax
 x(1,j)=xstart
end do
y(1,1)=ystart
delta_x = 100.0
delta_y = 50.0
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
do k=1,ymax
  zbot(xmax,k) = zmin
end do
  DO i=xmax-1,1,-1
    zbot(i,1) = zbot(i+1,1) + delta_x*slope
    DO j=2,ymax
      zbot(i,j) = zbot(i,1)
    END DO

  END DO

! write in grid x,y
1001 FORMAT(i5,2x,i5)
1000 FORMAT(i5,2x,i5,2x,f12.4,2x,f12.4,2x,f12.4)
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

