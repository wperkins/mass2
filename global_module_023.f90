
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 module file
!
! VERSION and DATE: 0.23 4-24-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!
!
!***************************************************************
!
MODULE globals
IMPLICIT NONE

INTEGER  :: max_blocks

INTEGER :: status

DOUBLE PRECISION, SAVE :: grav = 32.2, tiny = 1.0D-100
DOUBLE PRECISION, SAVE :: density = 1.94
DOUBLE PRECISION, SAVE :: density_air = 0.00237  ! 60 degrees F

TYPE block_struct
	INTEGER :: xmax,ymax
	DOUBLE PRECISION, POINTER :: x(:,:)		! x location at control volume nodes
  DOUBLE PRECISION, POINTER :: y(:,:)		! y location at control volume nodes
	DOUBLE PRECISION, POINTER :: x_grid(:,:)		! x location on grid (c.v. corner points)
  DOUBLE PRECISION, POINTER :: y_grid(:,:)		! y location on grid (c.v. corner points)
  DOUBLE PRECISION, POINTER :: x_xsi(:,:)	! x derivative wrt xsi (wrt = with respect to)
  DOUBLE PRECISION, POINTER :: y_xsi(:,:)	! y derivative wrt xsi
  DOUBLE PRECISION, POINTER :: x_eta(:,:)	! x derivative wrt eta
  DOUBLE PRECISION, POINTER :: y_eta(:,:)	! y derivative wrt eta
  DOUBLE PRECISION, POINTER :: hp1(:,:)		! metric coeff. 1 in xsi direction : P loc
  DOUBLE PRECISION, POINTER :: hp2(:,:)		! metric coeff. 2 in eta direction : P loc
  DOUBLE PRECISION, POINTER :: gp12(:,:)	! nonorthogonal part of the metric tensor
  DOUBLE PRECISION, POINTER :: hv1(:,:)		! metric coeff. 1 in xsi direction : v vel loc
  DOUBLE PRECISION, POINTER :: hv2(:,:)		! metric coeff. 2 in eta direction : v vel loc
  DOUBLE PRECISION, POINTER :: hu1(:,:)		! metric coeff. 1 in xsi direction : u vel loc
  DOUBLE PRECISION, POINTER :: hu2(:,:)		! metric coeff. 2 in eta direction : u vel loc
  DOUBLE PRECISION, POINTER :: uvel_p(:,:)		! u vel at c.v. node
  DOUBLE PRECISION, POINTER :: vvel_p(:,:)		! v vel at c.v. node
  DOUBLE PRECISION, POINTER :: u_cart(:,:)	! u cartesian velocity component x-dir
  DOUBLE PRECISION, POINTER :: v_cart(:,:)	! v cartesian velocity component x-dir
  DOUBLE PRECISION, POINTER :: uvel(:,:) 		! u depth-ave velocity
  DOUBLE PRECISION, POINTER :: vvel(:,:)		! v depth-ave velocity
  
  DOUBLE PRECISION, POINTER :: depth(:,:)		! water DEPTH (NOT WS ELEVATION)
  DOUBLE PRECISION, POINTER :: wsel(:,:)		! Water Surface ELEVATION
  DOUBLE PRECISION, POINTER :: eddy(:,:)		! eddy viscosity depth-ave
  DOUBLE PRECISION, POINTER :: kx_diff(:,:)		! scalar turb diffusivity xsi direction
  DOUBLE PRECISION, POINTER :: ky_diff(:,:)		! scalar turb diffusivity eta direction
  DOUBLE PRECISION, POINTER :: uold(:,:) 		! old time u depth-ave velocity
  DOUBLE PRECISION, POINTER :: vold(:,:)		! old time v depth-ave velocity
  
  DOUBLE PRECISION, POINTER :: depthold(:,:)		! old time water depth (NOT WS ELEVATION)
  DOUBLE PRECISION, POINTER :: zbot(:,:)				! bottom elevation at control volume nodes
  DOUBLE PRECISION, POINTER :: zbot_grid(:,:)		! bottom elevation at grid points (c.v. corners)
  DOUBLE PRECISION, POINTER :: ustar(:,:)   ! u* velocity field
  DOUBLE PRECISION, POINTER :: vstar(:,:) 	! v* velocity field
  DOUBLE PRECISION, POINTER :: dstar(:,:)		! d* depth field
  DOUBLE PRECISION, POINTER :: dp(:,:)			! d' depth correction field
  DOUBLE PRECISION, POINTER :: bedshear1(:,:)		! bed shear stress in xsi direction
  DOUBLE PRECISION, POINTER :: bedshear2(:,:)		! bed shear stress in eta direction
  DOUBLE PRECISION, POINTER :: windshear1(:,:)	! wind shear stress in xsi direction
  DOUBLE PRECISION, POINTER :: windshear2(:,:)	! wind shear stress in eta direction
  DOUBLE PRECISION, POINTER :: chezy(:,:)				! chezy bed shear stress coefficient
  DOUBLE PRECISION, POINTER :: mass_source(:,:)		!  mass source term or residual

  DOUBLE PRECISION, POINTER :: TDG_stuff(:,:)	! work array for output of TDG delP, %Sat
  DOUBLE PRECISION, POINTER :: work(:,:)        ! general work array
  DOUBLE PRECISION, POINTER :: froude_num(:,:)  ! Froude number based on local depth - velocity
  DOUBLE PRECISION, POINTER :: courant_num(:,:) ! Courant number

END TYPE block_struct

! structure for disposable variables that can be overwritten for each block
! need to only allocate one of these for the single largest (xmax, ymax)

TYPE coeff_struct

! NOTE: loc P,E, etc. are RELATIVE to staggered grid variables U,V,C,D
    DOUBLE PRECISION, POINTER :: ap(:,:)		! coeff at loc P in discretization
    DOUBLE PRECISION, POINTER :: ae(:,:)		! coeff at loc E in discretization
    DOUBLE PRECISION, POINTER :: aw(:,:)		! coeff at loc W in discretization
    DOUBLE PRECISION, POINTER :: an(:,:)		! coeff at loc N in discretization
    DOUBLE PRECISION, POINTER :: as(:,:)		! coeff at loc S in discretization
    DOUBLE PRECISION, POINTER :: bp(:,:)		! coeff at loc P in discretization
    
    DOUBLE PRECISION, POINTER :: cp(:,:)		! coeff at loc P in discretization  d'
    DOUBLE PRECISION, POINTER :: ce(:,:)		! coeff at loc E in discretization  d'
    DOUBLE PRECISION, POINTER :: cw(:,:)		! coeff at loc W in discretization  d'
    DOUBLE PRECISION, POINTER :: cn(:,:)		! coeff at loc N in discretization  d'
    DOUBLE PRECISION, POINTER :: cs(:,:)		! coeff at loc S in discretization  d'
    DOUBLE PRECISION, POINTER :: dp(:,:)		! coeff at loc P in discretization  d'
    DOUBLE PRECISION, POINTER :: lud(:,:)		! part of p' coeff that has U vel stuff
    DOUBLE PRECISION, POINTER :: lvd(:,:)		! part of p' coeff that has V vel stuff
    DOUBLE PRECISION, POINTER :: source(:,:)	! source term
    

END TYPE coeff_struct


TYPE(block_struct), ALLOCATABLE :: block(:)
TYPE(coeff_struct) :: coeff

CONTAINS
!#########################################################################
SUBROUTINE allocate_blocks(error_iounit,status_iounit)

	IMPLICIT NONE
	INTEGER :: alloc_stat,error_iounit,status_iounit

	ALLOCATE(block(max_blocks), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of blocks'
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of blocks'
	ENDIF

END SUBROUTINE allocate_blocks


!#########################################################################
SUBROUTINE allocate_block_components(n, imax, jmax, status_iounit)

! this routine allocates each component in the array of blocks
! allows minimal memory use for each block
IMPLICIT NONE
INTEGER :: n, imax, jmax, status_iounit	! block number, max i elements, max j elements

WRITE(status_iounit,*)'starting component allocation for block number - ',n
WRITE(status_iounit,*)'         maximum number of i elements = ', imax
WRITE(status_iounit,*)'         maximum number of j elements = ', jmax

	ALLOCATE(block(n)%x(imax,jmax))
	ALLOCATE(block(n)%y(imax,jmax))
	ALLOCATE(block(n)%x_grid(imax,jmax))
	ALLOCATE(block(n)%y_grid(imax,jmax))
  ALLOCATE(block(n)%x_xsi(imax,jmax))	! x derivative wrt xsi (wrt = with respect to)
  ALLOCATE(block(n)%y_xsi(imax,jmax))	! y derivative wrt xsi
  ALLOCATE(block(n)%x_eta(imax,jmax))	! x derivative wrt eta
  ALLOCATE(block(n)%y_eta(imax,jmax))	! y derivative wrt eta
  ALLOCATE(block(n)%hp1(imax,jmax))		! metric coeff. 1 in xsi direction : P loc
  ALLOCATE(block(n)%hp2(imax,jmax))		! metric coeff. 2 in eta direction : P loc
	ALLOCATE(block(n)%gp12(imax,jmax))	! nonorthogonal part of the metric tensor
  ALLOCATE(block(n)%hv1(imax,jmax))		! metric coeff. 1 in xsi direction : v loc
  ALLOCATE(block(n)%hv2(imax,jmax))		! metric coeff. 2 in eta direction : v loc
  ALLOCATE(block(n)%hu1(imax,jmax))		! metric coeff. 1 in xsi direction : u loc
  ALLOCATE(block(n)%hu2(imax,jmax))		! metric coeff. 2 in eta direction : u loc
  ALLOCATE(block(n)%uvel_p(imax,jmax))		! u vel at c.v. node
  ALLOCATE(block(n)%vvel_p(imax,jmax))		! v vel at c.v. node
  ALLOCATE(block(n)%u_cart(imax,jmax))	! u cartesian velocity component x-dir
  ALLOCATE(block(n)%v_cart(imax,jmax))	! v cartesian velocity component y-dir
  ALLOCATE(block(n)%uvel(imax,jmax)) 		! u depth-ave velocity
  ALLOCATE(block(n)%vvel(imax,jmax))		! v depth-ave velocity
  
  ALLOCATE(block(n)%depth(imax,jmax))		! water DEPTH (NOT WS ELEVATION)
	ALLOCATE(block(n)%wsel(imax,jmax))		! Water Surface ELEVATION
  ALLOCATE(block(n)%eddy(imax,jmax))		! eddy viscosity depth-ave
	ALLOCATE(block(n)%kx_diff(imax,jmax))		! scalar turb diffusivity xsi direction
	ALLOCATE(block(n)%ky_diff(imax,jmax))		! scalar turb diffusivity eta direction
  ALLOCATE(block(n)%uold(imax,jmax)) 		! old time u depth-ave velocity
  ALLOCATE(block(n)%vold(imax,jmax))		! old time v depth-ave velocity
  
  ALLOCATE(block(n)%depthold(imax,jmax))	! old time water depth (NOT WS ELEVATION)
  ALLOCATE(block(n)%zbot(imax,jmax))			! bottom elevation at control volume nodes
	ALLOCATE(block(n)%zbot_grid(imax,jmax))	! bottom elevation at grid nodes
  ALLOCATE(block(n)%ustar(imax,jmax))			! u* velocity field
  ALLOCATE(block(n)%vstar(imax,jmax)) 		! v* velocity field
  ALLOCATE(block(n)%dstar(imax,jmax))			! d* depth field
  ALLOCATE(block(n)%dp(imax,jmax))				! d' depth correction field
  ALLOCATE(block(n)%bedshear1(imax,jmax)) ! bed shear stress in xsi direction
  ALLOCATE(block(n)%bedshear2(imax,jmax)) ! bed shear stress in eta direction
	ALLOCATE(block(n)%windshear1(imax,jmax))	! wind shear stress in xsi direction
  ALLOCATE(block(n)%windshear2(imax,jmax))	! wind shear stress in eta direction
  ALLOCATE(block(n)%chezy(imax,jmax))				! chezy bed shear stress coefficient
  ALLOCATE(block(n)%mass_source(imax,jmax))	! mass source
  ALLOCATE(block(n)%TDG_stuff(imax,jmax))		! TDG work array
  ALLOCATE(block(n)%work(imax,jmax))         ! general work array
  ALLOCATE(block(n)%froude_num(imax,jmax))   ! froude number
  ALLOCATE(block(n)%courant_num(imax,jmax))  ! courant number


WRITE(status_iounit,*)'completed component allocation for block number - ',n

END SUBROUTINE allocate_block_components


!################################################################################
SUBROUTINE allocate_coeff_components(imax, jmax, status_iounit)

! this routine allocates each component in the array of blocks
! allows minimal memory use for each block
IMPLICIT NONE
INTEGER :: imax, jmax, status_iounit	! block number, max i elements, max j elements


WRITE(status_iounit,*)'starting component allocation for coeff'
WRITE(status_iounit,*)'         maximum number of i elements = ', imax
WRITE(status_iounit,*)'         maximum number of j elements = ', jmax

ALLOCATE(coeff%ap(imax,jmax))
ALLOCATE(coeff%ae(imax,jmax))		! coeff at loc E in discretization
ALLOCATE(coeff%aw(imax,jmax))		! coeff at loc W in discretization
ALLOCATE(coeff%an(imax,jmax))		! coeff at loc N in discretization
ALLOCATE(coeff%as(imax,jmax))		! coeff at loc S in discretization
ALLOCATE(coeff%bp(imax,jmax))		! coeff at loc P in discretization
    
ALLOCATE(coeff%cp(imax,jmax))		! coeff at loc P in discretization  d'
ALLOCATE(coeff%ce(imax,jmax))		! coeff at loc E in discretization  d'
ALLOCATE(coeff%cw(imax,jmax))		! coeff at loc W in discretization  d'
ALLOCATE(coeff%cn(imax,jmax))		! coeff at loc N in discretization  d'
ALLOCATE(coeff%cs(imax,jmax))		! coeff at loc S in discretization  d'
ALLOCATE(coeff%dp(imax,jmax))		! coeff at loc P in discretization  d'
ALLOCATE(coeff%lud(imax,jmax))		! part of p' coeff that has U vel stuff
ALLOCATE(coeff%lvd(imax,jmax))		! part of p' coeff that has V vel stuff
ALLOCATE(coeff%source(imax,jmax))	! source term




WRITE(status_iounit,*)'completed component allocation for coeff'

END SUBROUTINE allocate_coeff_components


!#########################################################################
SUBROUTINE deallocate_globals

IMPLICIT NONE


END SUBROUTINE deallocate_globals


END MODULE globals
