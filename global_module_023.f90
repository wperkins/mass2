
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

USE utility

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

INTEGER  :: max_blocks

INTEGER :: status

DOUBLE PRECISION, PARAMETER :: grav = 32.2, tiny = 1.0D-100
DOUBLE PRECISION, PARAMETER :: density = 1.94
DOUBLE PRECISION, PARAMETER :: density_air = 0.00237  ! 60 degrees F
DOUBLE PRECISION, PARAMETER :: bigfactor = 1.0d80
DOUBLE PRECISION, PARAMETER :: vonkarmon = 0.4
DOUBLE PRECISION, PARAMETER :: viscosity_water = 1.22e-05 ! ft^2/s @ 60F

                                ! a list of cell types

INTEGER, PUBLIC, PARAMETER :: &
     &CELL_NORMAL_TYPE = 1, &
     &CELL_BOUNDARY_TYPE = 3

                                ! a list of flow boundary condition
                                ! types

INTEGER, PUBLIC, PARAMETER :: &
     &FLOWBC_NONE = 0, &
     &FLOWBC_VEL = 1, &
     &FLOWBC_FLOW = 2, &
     &FLOWBC_ELEV = 3, &
     &FLOWBC_BLOCK = 4, &
     &FLOWBC_ZEROG = 5, &
     &FLOWBC_BOTH = 6

TYPE cell_type_struct
   INTEGER :: xtype, ytype
   ! the rest applies only for BOUNDARY type cells
   INTEGER :: xbctype, ybctype
END TYPE cell_type_struct


TYPE isdead_struct
   LOGICAL :: u, v, p, transient
END TYPE isdead_struct

TYPE block_struct
   INTEGER :: xmax,ymax
   DOUBLE PRECISION, POINTER :: x(:,:)		! x location at control volume nodes
   DOUBLE PRECISION, POINTER :: y(:,:)		! y location at control volume nodes
   DOUBLE PRECISION, POINTER :: x_grid(:,:)		! x location on grid (c.v. corner points)
   DOUBLE PRECISION, POINTER :: y_grid(:,:)		! y location on grid (c.v. corner points)
   DOUBLE PRECISION, POINTER :: x_out(:,:) ! x locations for output
   DOUBLE PRECISION, POINTER :: y_out(:,:) ! y locations for output
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
   DOUBLE PRECISION, POINTER :: uflux(:,:)      ! east face flux for c.v.
   DOUBLE PRECISION, POINTER :: vflux(:,:)      ! north face flux for c.v.
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

   DOUBLE PRECISION, POINTER :: uoldold(:,:) ! previous old time u depth-ave velocity
   DOUBLE PRECISION, POINTER :: voldold(:,:) ! previous old time v depth-ave velocity
   DOUBLE PRECISION, POINTER :: deptholdold(:,:) ! previous old time water depth (NOT WS ELEVATION)

   DOUBLE PRECISION, POINTER :: zbot(:,:)				! bottom elevation at control volume nodes
   DOUBLE PRECISION, POINTER :: zbot_grid(:,:)		! bottom elevation at grid points (c.v. corners)
   DOUBLE PRECISION, POINTER :: zbot_out(:,:)
   DOUBLE PRECISION, POINTER :: ustar(:,:)   ! u* velocity field
   DOUBLE PRECISION, POINTER :: vstar(:,:) 	! v* velocity field
   DOUBLE PRECISION, POINTER :: dstar(:,:)		! d* depth field
   DOUBLE PRECISION, POINTER :: dp(:,:)			! d' depth correction field
   DOUBLE PRECISION, POINTER :: bedshear1(:,:)		! bed shear stress in xsi direction
   DOUBLE PRECISION, POINTER :: bedshear2(:,:)		! bed shear stress in eta direction
   DOUBLE PRECISION, POINTER :: windshear1(:,:)	! wind shear stress in xsi direction
   DOUBLE PRECISION, POINTER :: windshear2(:,:)	! wind shear stress in eta direction
   DOUBLE PRECISION, POINTER :: shear(:,:) ! bed shear stress magnitude @ velocity locations
   DOUBLE PRECISION, POINTER :: chezy(:,:)				! chezy bed shear stress coefficient
   DOUBLE PRECISION, POINTER :: mass_source(:,:)		!  mass source term or residual
   
   DOUBLE PRECISION, POINTER :: TDG_stuff(:,:)	! work array for output of TDG delP, %Sat
   DOUBLE PRECISION, POINTER :: work(:,:)        ! general work array
   DOUBLE PRECISION, POINTER :: froude_num(:,:)  ! Froude number based on local depth - velocity
   DOUBLE PRECISION, POINTER :: courant_num(:,:) ! Courant number

                                ! These variables are used for
                                ! transport; they hold hydrdynamic
                                ! values that are calculated before
                                ! the transport calculations begin

  ! DOUBLE PRECISION, POINTER :: inlet_area(:) 
  DOUBLE PRECISION, POINTER :: k_e(:,:), k_w(:,:), k_n(:,:), k_s(:,:)
  DOUBLE PRECISION, POINTER :: depth_e(:,:), depth_w(:,:), depth_n(:,:), depth_s(:,:)
  DOUBLE PRECISION, POINTER :: flux_e(:,:), flux_w(:,:), flux_n(:,:), flux_s(:,:)
  DOUBLE PRECISION, POINTER :: diffu_e(:,:), diffu_w(:,:), diffu_n(:,:), diffu_s(:,:)
  DOUBLE PRECISION, POINTER :: pec_e(:,:), pec_w(:,:), pec_n(:,:), pec_s(:,:)
  DOUBLE PRECISION, POINTER :: apo(:,:)
  DOUBLE PRECISION, POINTER :: lud(:,:) ! part of p' coeff that has U vel stuff
  DOUBLE PRECISION, POINTER :: lvd(:,:) ! part of p' coeff that has V vel stuff
  
  TYPE (isdead_struct), POINTER :: isdead(:,:)
  TYPE (cell_type_struct), POINTER :: cell(:,:)

  LOGICAL, POINTER :: isdry(:,:)

  DOUBLE PRECISION, POINTER :: xsource(:,:)

END TYPE block_struct

! structure for disposable variables that can be overwritten for each block
! need to only allocate one of these for the single largest (xmax, ymax)


TYPE(block_struct), ALLOCATABLE :: block(:)

CONTAINS
!#########################################################################
SUBROUTINE allocate_blocks()

	IMPLICIT NONE
	INTEGER :: alloc_stat

	ALLOCATE(block(max_blocks), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the array of blocks', fatal=.TRUE.)
	ELSE
       CALL status_message('allocation successful for array of blocks')
	ENDIF

END SUBROUTINE allocate_blocks


!#########################################################################
SUBROUTINE allocate_block_components(n, status_iounit)

  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra

  ! this routine allocates each component in the array of blocks
  ! allows minimal memory use for each block
  IMPLICIT NONE
  INTEGER :: n, status_iounit	! block number

  INTEGER :: imin, imax, jmin, jmax ! index limits

  CHARACTER (LEN=1024) :: msg

  imin = i_index_min
  imax = block(n)%xmax + i_index_extra
  jmin = j_index_min
  jmax = block(n)%ymax + j_index_extra

  WRITE(msg,*)'starting component allocation for block number - ',n
  CALL status_message(msg)
  WRITE(msg,*)'         maximum number of i elements = ', imax
  CALL status_message(msg)
  WRITE(msg,*)'         maximum number of j elements = ', jmax
  CALL status_message(msg)

  ALLOCATE(block(n)%x(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%y(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%x_grid(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%y_grid(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%x_out(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%y_out(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%x_xsi(imin:imax,jmin:jmax))	! x derivative wrt xsi (wrt = with respect to)
  ALLOCATE(block(n)%y_xsi(imin:imax,jmin:jmax))	! y derivative wrt xsi
  ALLOCATE(block(n)%x_eta(imin:imax,jmin:jmax))	! x derivative wrt eta
  ALLOCATE(block(n)%y_eta(imin:imax,jmin:jmax))	! y derivative wrt eta
  ALLOCATE(block(n)%hp1(imin:imax,jmin:jmax))		! metric coeff. 1 in xsi direction : P loc
  block(n)%hp1 = 1.0
  ALLOCATE(block(n)%hp2(imin:imax,jmin:jmax))		! metric coeff. 2 in eta direction : P loc
  block(n)%hp2 = 1.0
  ALLOCATE(block(n)%gp12(imin:imax,jmin:jmax))	! nonorthogonal part of the metric tensor
  ALLOCATE(block(n)%hv1(imin:imax,jmin:jmax))		! metric coeff. 1 in xsi direction : v loc
  ALLOCATE(block(n)%hv2(imin:imax,jmin:jmax))		! metric coeff. 2 in eta direction : v loc
  ALLOCATE(block(n)%hu1(imin:imax,jmin:jmax))		! metric coeff. 1 in xsi direction : u loc
  ALLOCATE(block(n)%hu2(imin:imax,jmin:jmax))		! metric coeff. 2 in eta direction : u loc
  ALLOCATE(block(n)%uvel_p(imin:imax,jmin:jmax))		! u vel at c.v. node
  block(n)%uvel_p = 0.0
  ALLOCATE(block(n)%vvel_p(imin:imax,jmin:jmax))		! v vel at c.v. node
  block(n)%vvel_p = 0.0
  ALLOCATE(block(n)%u_cart(imin:imax,jmin:jmax))	! u cartesian velocity component x-dir
  block(n)%u_cart = 0.0
  ALLOCATE(block(n)%v_cart(imin:imax,jmin:jmax))	! v cartesian velocity component y-dir
  block(n)%v_cart = 0.0
  ALLOCATE(block(n)%uvel(imin:imax,jmin:jmax)) 		! u depth-ave velocity
  block(n)%uvel = 0.0
  ALLOCATE(block(n)%vvel(imin:imax,jmin:jmax))		! v depth-ave velocity
  block(n)%vvel = 0.0
  ALLOCATE(block(n)%uflux(imin:imax,jmin:jmax))		! east face flux in c.v.
  block(n)%uflux = 0.0
  ALLOCATE(block(n)%vflux(imin:imax,jmin:jmax))		! north face flux in c.v.
  block(n)%vflux = 0.0
  
  ALLOCATE(block(n)%depth(imin:imax,jmin:jmax))		! water DEPTH (NOT WS ELEVATION)
  ALLOCATE(block(n)%wsel(imin:imax,jmin:jmax))		! Water Surface ELEVATION
  ALLOCATE(block(n)%eddy(imin:imax,jmin:jmax))		! eddy viscosity depth-ave
  ALLOCATE(block(n)%kx_diff(imin:imax,jmin:jmax))		! scalar turb diffusivity xsi direction
  ALLOCATE(block(n)%ky_diff(imin:imax,jmin:jmax))		! scalar turb diffusivity eta direction

  ALLOCATE(block(n)%uold(imin:imax,jmin:jmax)) 		! old time u depth-ave velocity
  ALLOCATE(block(n)%vold(imin:imax,jmin:jmax))		! old time v depth-ave velocity
  ALLOCATE(block(n)%depthold(imin:imax,jmin:jmax))	! old time water depth (NOT WS ELEVATION)

  ALLOCATE(block(n)%uoldold(imin:imax,jmin:jmax)) 		! old time u depth-ave velocity
  ALLOCATE(block(n)%voldold(imin:imax,jmin:jmax))		! old time v depth-ave velocity
  ALLOCATE(block(n)%deptholdold(imin:imax,jmin:jmax))	! old time water depth (NOT WS ELEVATION)

  ALLOCATE(block(n)%zbot(imin:imax,jmin:jmax))			! bottom elevation at control volume nodes
  ALLOCATE(block(n)%zbot_grid(imin:imax,jmin:jmax))	! bottom elevation at grid nodes
  ALLOCATE(block(n)%zbot_out(imin:imax,jmin:jmax))	! bottom elevation at grid nodes
  ALLOCATE(block(n)%ustar(imin:imax,jmin:jmax))			! u* velocity field
  ALLOCATE(block(n)%vstar(imin:imax,jmin:jmax)) 		! v* velocity field
  ALLOCATE(block(n)%dstar(imin:imax,jmin:jmax))			! d* depth field
  ALLOCATE(block(n)%dp(imin:imax,jmin:jmax))				! d' depth correction field
  ALLOCATE(block(n)%bedshear1(imin:imax,jmin:jmax)) ! bed shear stress in xsi direction
  ALLOCATE(block(n)%bedshear2(imin:imax,jmin:jmax)) ! bed shear stress in eta direction
  ALLOCATE(block(n)%shear(imin:imax,jmin:jmax)) ! bed shear stress magnitude @ velocity locations
  block(n)%shear = 0.0
  ALLOCATE(block(n)%windshear1(imin:imax,jmin:jmax))	! wind shear stress in xsi direction
  ALLOCATE(block(n)%windshear2(imin:imax,jmin:jmax))	! wind shear stress in eta direction
  ALLOCATE(block(n)%chezy(imin:imax,jmin:jmax))				! chezy bed shear stress coefficient
  ALLOCATE(block(n)%mass_source(imin:imax,jmin:jmax))	! mass source
  block(n)%mass_source = 0.0
  ALLOCATE(block(n)%TDG_stuff(imin:imax,jmin:jmax))		! TDG work array
  ALLOCATE(block(n)%work(imin:imax,jmin:jmax))         ! general work array
  ALLOCATE(block(n)%froude_num(imin:imax,jmin:jmax))   ! froude number
  block(n)%froude_num = 0.0
  ALLOCATE(block(n)%courant_num(imin:imax,jmin:jmax))  ! courant number
  block(n)%courant_num = 0.0

                                ! precalculated values for transport 

  ! ALLOCATE(block(n)%inlet_area(jmin:jmax))
  ALLOCATE(block(n)%k_e(imin:imax,jmin:jmax),block(n)%k_w(imin:imax,jmin:jmax),&
       &block(n)%k_n(imin:imax,jmin:jmax),block(n)%k_s(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%depth_e(imin:imax,jmin:jmax),block(n)%depth_w(imin:imax,jmin:jmax),&
       &block(n)%depth_n(imin:imax,jmin:jmax),block(n)%depth_s(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%flux_e(imin:imax,jmin:jmax),block(n)%flux_w(imin:imax,jmin:jmax),&
       &block(n)%flux_n(imin:imax,jmin:jmax),block(n)%flux_s(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%diffu_e(imin:imax,jmin:jmax),block(n)%diffu_w(imin:imax,jmin:jmax),&
       &block(n)%diffu_n(imin:imax,jmin:jmax),block(n)%diffu_s(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%pec_e(imin:imax,jmin:jmax),block(n)%pec_w(imin:imax,jmin:jmax),&
       &block(n)%pec_n(imin:imax,jmin:jmax),block(n)%pec_s(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%apo(imin:imax,jmin:jmax))
  ALLOCATE(block(n)%lud(imin:imax,jmin:jmax))		! part of p' coeff that has U vel stuff
  ALLOCATE(block(n)%lvd(imin:imax,jmin:jmax))		! part of p' coeff that has V vel stuff

  ALLOCATE(block(n)%isdead(imin:imax, jmin:jmax))

  block(n)%isdead(:,:)%u = .FALSE.
  block(n)%isdead(:,:)%v = .FALSE.
  block(n)%isdead(:,:)%p = .FALSE.
  block(n)%isdead(:,:)%transient = .TRUE.

  ALLOCATE(block(n)%cell(imin:imax, jmin:jmax))
  block(n)%cell(:,:)%xtype = CELL_NORMAL_TYPE
  block(n)%cell(:,:)%xbctype = FLOWBC_NONE
  block(n)%cell(:,:)%ytype = CELL_NORMAL_TYPE
  block(n)%cell(:,:)%ybctype = FLOWBC_NONE

  ALLOCATE(block(n)%isdry(imin:imax, jmin:jmax))

  block(n)%isdry = .FALSE.

  ALLOCATE(block(n)%xsource(imin:imax, jmin:jmax))

  block(n)%xsource = 0.0

  WRITE(msg,*)'completed component allocation for block number - ',n
  CALL status_message(msg)

END SUBROUTINE allocate_block_components


!################################################################################

!#########################################################################
SUBROUTINE deallocate_globals

IMPLICIT NONE


END SUBROUTINE deallocate_globals

! ----------------------------------------------------------------
! SUBROUTINE velocity_shift
! This routine computes velocity components at the cell center
! ----------------------------------------------------------------
SUBROUTINE velocity_shift()

  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra

  IMPLICIT NONE

  INTEGER :: iblk, i, j
  DOUBLE PRECISION :: flux1, flux2, fluxsum

  DO iblk = 1, max_blocks

                                ! cell center velocity components 
                                ! with fluxes !
     block(iblk)%uflux = 0.0
     block(iblk)%vflux = 0.0
     DO i = i_index_min+1, block(iblk)%xmax+i_index_extra
        fluxsum = 0.0
        DO j=2, block(iblk)%ymax 
           flux1 = uflux(block(iblk), i-1, j, j)
           flux2 = uflux(block(iblk), i, j, j)
           fluxsum = fluxsum + flux2
           block(iblk)%uflux(i,j) = flux2
           block(iblk)%uvel_p(i,j) = 0.5*(flux1+flux2)/block(iblk)%hp2(i,j)/block(iblk)%depth(i,j)
        END DO
        block(iblk)%uflux(i,1) = block(iblk)%uflux(i,2)
        block(iblk)%uflux(i,block(iblk)%ymax+1) = block(iblk)%uflux(i,block(iblk)%ymax)
        ! DO j=2, block(iblk)%ymax 
        !    block(iblk)%uflux(i,j) = fluxsum
        ! END DO
     END DO

     DO j= j_index_min+1, block(iblk)%ymax + j_index_extra
        fluxsum = 0.0
        DO i = 2, block(iblk)%xmax
           flux1 = vflux(block(iblk), i, i, j-1)
           flux2 = vflux(block(iblk), i, i, j)
           fluxsum = fluxsum + flux2
           block(iblk)%vflux(i,j) = flux2
           block(iblk)%vvel_p(i,j) = 0.5*(flux1+flux2)/block(iblk)%hp1(i,j)/block(iblk)%depth(i,j)
        END DO
        block(iblk)%vflux(1,j) = block(iblk)%vflux(2,j)
        block(iblk)%vflux(block(iblk)%xmax+1,j) = block(iblk)%vflux(block(iblk)%xmax,j)
        ! DO i = 2, block(iblk)%xmax
        !    block(iblk)%vflux(i,j) = fluxsum
        ! END DO
     END DO


!      DO i= 1, block(iblk)%xmax + 2
!         DO j=1, block(iblk)%ymax + 2
!            block(iblk)%uvel_p(i,j) = &
!                 &0.5*(block(iblk)%uvel(i,j)+block(iblk)%uvel(i-1,j))
!            block(iblk)%vvel_p(i,j) = &
!                 &0.5*(block(iblk)%vvel(i,j)+block(iblk)%vvel(i,j-1))
!         END DO
!      END DO
     i = 1
     DO j = 2, block(iblk)%ymax
        block(iblk)%uvel_p(i,j) = block(iblk)%uvel(i+1,j)
     END DO
     j = 1
     DO i = 2, block(iblk)%xmax
        block(iblk)%vvel_p(i,j) = block(iblk)%vvel(i,j+1)
     END DO
     
!!$     i=block(iblk)%xmax+1
!!$     DO j=2, block(iblk)%ymax
!!$        block(iblk)%uvel_p(i,j) = block(iblk)%uvel(i-1,j)
!!$        block(iblk)%vvel_p(i,j) = &
!!$             &0.25*(block(iblk)%vvel(i-1,j)+block(iblk)%vvel(i-1,j-1) +&
!!$             &block(iblk)%vvel(i,j)+block(iblk)%vvel(i,j-1))
!!$     END DO

                                ! eastward & northward velocity @ cell center

     WHERE (block(iblk)%hp1 .NE. 0.0 .AND. block(iblk)%hp2 .NE. 0.0)
        block(iblk)%u_cart = &
             &(block(iblk)%uvel_p/block(iblk)%hp1)*block(iblk)%x_xsi + &
             &(block(iblk)%vvel_p/block(iblk)%hp2)*block(iblk)%x_eta
        block(iblk)%v_cart = &
             &(block(iblk)%uvel_p/block(iblk)%hp1)*block(iblk)%y_xsi + &
             &(block(iblk)%vvel_p/block(iblk)%hp2)*block(iblk)%y_eta
     ELSEWHERE
        block(iblk)%u_cart = 0.0
        block(iblk)%v_cart = 0.0
     END WHERE
  END DO
END SUBROUTINE velocity_shift

! ----------------------------------------------------------------
! SUBROUTINE metrics
! Compute the metric coefficients for the block. Depending on the
! metric coeff. and location use either the grid (x,y) or the node
! (x,y)
! ----------------------------------------------------------------
SUBROUTINE metrics(blk)

  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra

  IMPLICIT NONE

  TYPE (block_struct) :: blk

  INTEGER :: imin, imax, jmin, jmax, i, j

  imin = i_index_min
  imax = blk%xmax+i_index_extra
  jmin = j_index_min
  jmax = blk%ymax+j_index_extra

   ! metric coeff. 2 on the u face of the c.v.

   blk%hu2 = 1.0e-20            ! bogus nonzero value
   DO i=imin, imax-1
      DO j=jmin+1, jmax-1
         blk%hu2(i,j) = & 
              SQRT((blk%x_grid(i,j) - blk%x_grid(i,j-1))**2 + &
              (blk%y_grid(i,j) - blk%y_grid(i,j-1))**2)
      END DO
   END DO

   ! metric coeff 1 on the u face of the c.v.

   blk%hu1 = 1.0e-20            ! bogus nonzero value
   DO i=imin+1, imax-i_index_extra
      DO j=jmin, jmax
         blk%hu1(i,j) = &
              SQRT((blk%x(i+1,j) - blk%x(i,j))**2 + &
              (blk%y(i+1,j) - blk%y(i,j))**2)
      END DO
   END DO

   ! on the edge it's only a half-distance

   i=imin
   DO j=jmin, jmax
      blk%hu1(i,j) = &
           SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
           ((blk%y(i+1,j) - blk%y(i,j)))**2)
   END DO
   i=imax-1
   DO j=jmin, jmax
      blk%hu1(i,j) = &
           SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
           ((blk%y(i+1,j) - blk%y(i,j)))**2)
   END DO
   
   ! metric coeff. 1 on the v face of the c.v.

   DO i=imin+1, imax-1
      DO j=jmin, jmax - 1
         blk%hv1(i,j) = &
              SQRT((blk%x_grid(i,j) - blk%x_grid(i-1,j))**2 + &
              (blk%y_grid(i,j) - blk%y_grid(i-1,j))**2) 
      END DO
   END DO

   ! metric coeff. 2 on the v face of the c.v.

   DO i=imin, imax
      DO j=jmin+1, jmax - 1
         blk%hv2(i,j) = &
              SQRT((blk%x(i,j+1) - blk%x(i,j))**2 + &
              (blk%y(i,j+1) - blk%y(i,j))**2)
      END DO
   END DO

   ! on the edge it's only a half-distance

   j = jmin
   DO i=imin+1, imax-1
      blk%hv2(i,j) = &
           SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
           ((blk%y(i,j+1) - blk%y(i,j)))**2)
   END DO
   j = jmax - 1
   DO i=imin+1, imax-1
      blk%hv2(i,j) = &
           SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
           ((blk%y(i,j+1) - blk%y(i,j)))**2)
   END DO

   ! compute metric tensor and derivatives at the nodal points hp1, hp2

   DO i = imin+1, imax-1
      DO j=jmin+1, jmax-1
         blk%x_eta(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i-1,j) & 
              - blk%x_grid(i,j-1) - blk%x_grid(i-1,j-1))
         blk%y_eta(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i-1,j) & 
              - blk%y_grid(i,j-1) - blk%y_grid(i-1,j-1))
         blk%x_xsi(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i,j-1) & 
              - blk%x_grid(i-1,j) - blk%x_grid(i-1,j-1))
         blk%y_xsi(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i,j-1) & 
              - blk%y_grid(i-1,j) - blk%y_grid(i-1,j-1))
      END DO
   END DO
   i=imin
   DO j=jmin+1, jmax-1
      blk%x_eta(i,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
      blk%y_eta(i,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
   END DO
   i=imax-1
   DO j=jmin+1, jmax-1
      blk%x_eta(i+1,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
      blk%y_eta(i+1,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
   END DO
   j = jmin
   DO i=imin+1, imax-1
      blk%x_xsi(i,j) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
      blk%y_xsi(i,j) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
   END DO
   j=jmax-1
   DO i=imin+1, imax-1
      blk%x_xsi(i,j+1) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
      blk%y_xsi(i,j+1) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
   END DO

   blk%x_xsi(imin,:) = blk%x_xsi(imin+1,:)
   blk%x_xsi(imax,:) = blk%x_xsi(imax-1,:)

   blk%y_xsi(imin,:) = blk%y_xsi(imin+1,:)
   blk%y_xsi(imax,:) = blk%y_xsi(imax-1,:)

   blk%x_eta(:,jmin) = blk%x_eta(:,jmin+1)
   blk%x_eta(:,jmax) = blk%x_eta(:,jmax-1)

   blk%y_eta(:,jmin) = blk%y_eta(:,jmin+1)
   blk%y_eta(:,jmax) = blk%y_eta(:,jmax-1)


   blk%hp1 = SQRT(blk%x_xsi**2 + blk%y_xsi**2)
   blk%hp2 = SQRT(blk%x_eta**2 + blk%y_eta**2)
  
   ! compute nonorthogonal part of the metric tensor as a check on grid quality

   blk%gp12 = blk%x_xsi*blk%x_eta + blk%y_xsi*blk%y_eta
	


END SUBROUTINE metrics


! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION uarea
! computes the flow area for an arbitrary u location, i is a u
! location, jbeg and jend are cell locations
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION uarea(blk, i, jbeg, jend)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(IN) :: blk
  INTEGER, INTENT(IN) :: i, jbeg, jend
  
  INTEGER :: j
  DOUBLE PRECISION :: d, a

  uarea = 0.0

  DO j = jbeg, jend
     IF (i .GT. blk%xmax) THEN
        d = blk%depth(i-1, j)
     ELSE IF (i .LT. 2) THEN
        d = blk%depth(i, j)
     ELSE
        d = 0.5*(blk%depth(i+1, j) + blk%depth(i, j))
     END IF
     a = d*blk%hu2(i,j)
     uarea = uarea + a
  END DO

END FUNCTION uarea

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION uflux
! computes the flux through an arbitrary u location, i is a u
! location, jbeg and jend are cell locations
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION uflux(blk, i, jbeg, jend)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(IN) :: blk
  INTEGER, INTENT(IN) :: i, jbeg, jend
  
  INTEGER :: j
  DOUBLE PRECISION :: a, q

  uflux = 0.0

  DO j = jbeg, jend
     a = uarea(blk, i, j, j)
     q = a*blk%uvel(i,j)
     uflux = uflux + q
  END DO
END FUNCTION uflux

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION varea
! computes the flow area for an arbitrary v location, j is a v
! location, ibeg and iend are cell locations
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION varea(blk, ibeg, iend, j)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(IN) :: blk
  INTEGER, INTENT(IN) :: ibeg, iend, j
  
  INTEGER :: i
  DOUBLE PRECISION :: d, a

  varea = 0.0

  DO i = ibeg, iend
     IF (j .GT. blk%ymax) THEN
        d = blk%depth(i, j-1)
     ELSE IF (j .LT. 2) THEN
        d = blk%depth(i, j)
     ELSE
        d = 0.5*(blk%depth(i, j+1) + blk%depth(i, j))
     END IF
     a = d*blk%hv1(i,j)
     varea = varea + a
  END DO

END FUNCTION varea

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION vflux
! computes the flux through an arbitrary v location, i is a u
! location, jbeg and jend are cell locations
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION vflux(blk, ibeg, iend, j)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(IN) :: blk
  INTEGER, INTENT(IN) :: ibeg, iend, j 
  
  INTEGER :: i
  DOUBLE PRECISION :: a, q

  vflux = 0.0

  DO i = ibeg, iend
     a = varea(blk, i, i, j)
     q = a*blk%vvel(i,j)
     vflux = vflux + q
  END DO
END FUNCTION vflux

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION dinterp
! Interpolate depth at the location x, y which should be close to the
! centroid of cell i, j.  The method used is inverse distance
! weighting.  The cell and its 4 immediate neighbors (if they exist)
! are used.
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION dinterp(blk, x, y, ihint, jhint)

  IMPLICIT NONE

  DOUBLE PRECISION, EXTERNAL :: distance

  TYPE (block_struct), INTENT(IN) :: blk
  DOUBLE PRECISION, INTENT(IN) :: x, y
  INTEGER, INTENT(IN) :: ihint, jhint

  INTEGER :: i, j, ibeg, iend, jbeg, jend
  DOUBLE PRECISION :: d, wtotal

  jbeg = jhint - 1
  jend = jhint + 1
  IF (jbeg .LT. 2) jbeg = 2
  IF (jend .GT. blk%ymax) jend = blk%ymax
  
  ibeg = ihint - 1
  iend = ihint + 1
  IF (ibeg .LT. 2) ibeg = 2
  IF (iend .GT. blk%xmax) iend = blk%xmax

  wtotal = 0.0
  dinterp = 0.0

  j = jhint

  DO i = ibeg, iend
     d = distance(x, y, blk%x(i,j), blk%y(i,j))
     IF (d .LT. 1.0d-10) THEN
        dinterp = blk%depth(i,j)
        RETURN
     END IF
     wtotal = wtotal + 1.0/d
     dinterp = dinterp + blk%depth(i,j)/d
  END DO

  i = ihint
  DO j = jbeg, jend
     IF (j .NE. jhint) THEN
        d = distance(x, y, blk%x(i,j), blk%y(i,j))
        IF (d .LT. 1.0d-10) THEN
           dinterp = blk%depth(i,j)
           RETURN
        END IF
        wtotal = wtotal + 1.0/d
        dinterp = dinterp + blk%depth(i,j)/d
     END IF
  END DO

  dinterp = dinterp / wtotal
END FUNCTION dinterp

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION wsinterp
! Interpolate wsel at the location x, y which should be close to the
! centroid of cell i, j.  The method used is inverse distance
! weighting.  The cell and its 4 immediate neighbors (if they exist)
! are used.
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION wsinterp(blk, x, y, ihint, jhint)

  IMPLICIT NONE

  DOUBLE PRECISION, EXTERNAL :: distance

  TYPE (block_struct), INTENT(IN) :: blk
  DOUBLE PRECISION, INTENT(IN) :: x, y
  INTEGER, INTENT(IN) :: ihint, jhint

  INTEGER :: i, j, ibeg, iend, jbeg, jend
  DOUBLE PRECISION :: d, wtotal

  jbeg = MAX(jhint - 1, 2)
  jend = MIN(jhint + 1, blk%ymax)
  
  ibeg = MAX(ihint - 1, 2)
  iend = MIN(ihint + 1, blk%xmax)

  wtotal = 0.0
  wsinterp = 0.0

  j = jhint

  DO i = ibeg, iend
     d = distance(x, y, blk%x(i,j), blk%y(i,j))
     IF (d .LT. 1.0d-10) THEN
        wsinterp = blk%wsel(i,j)
        RETURN
     END IF
     IF (.NOT. blk%isdry(i,j)) THEN
        wtotal = wtotal + 1.0/d
        wsinterp = wsinterp + blk%wsel(i,j)/d
     END IF
  END DO

  i = ihint
  DO j = jbeg, jend
     IF (j .NE. jhint) THEN     ! cell at (ihint, jhint) already included
        d = distance(x, y, blk%x(i,j), blk%y(i,j))
        IF (d .LT. 1.0d-10) THEN
           wsinterp = blk%wsel(i,j)
           RETURN
        END IF
        IF (.NOT. blk%isdry(i,j)) THEN
           wtotal = wtotal + 1.0/d
           wsinterp = wsinterp + blk%wsel(i,j)/d
        END IF
     END IF
  END DO

  IF (wtotal .GT. 0.0) THEN
     wsinterp = wsinterp / wtotal
  ELSE
     wsinterp = blk%wsel(ihint,jhint)
  END IF

END FUNCTION wsinterp

END MODULE globals
