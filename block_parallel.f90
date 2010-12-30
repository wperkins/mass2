! ----------------------------------------------------------------
! file: block_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: Thu Dec 30 12:21:16 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE block_parallel
! ----------------------------------------------------------------
MODULE block_module

  USE utility
  USE block_variable

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

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

  ! ----------------------------------------------------------------
  ! TYPE cell_type_struct
  ! ----------------------------------------------------------------
  TYPE cell_type_struct
     INTEGER :: xtype, ytype
     ! the rest applies only for BOUNDARY type cells
     INTEGER :: xbctype, ybctype
  END TYPE cell_type_struct

  ! ----------------------------------------------------------------
  ! TYPE isdead_struct
  ! ----------------------------------------------------------------
  TYPE isdead_struct
     LOGICAL :: u, v, p, transient
  END TYPE isdead_struct


  ! ----------------------------------------------------------------
  ! TYPE block_struct
  ! ----------------------------------------------------------------
  TYPE block_struct
     INTEGER :: index
     INTEGER :: xmax, ymax
     TYPE (block_var_base), POINTER :: varbase
     TYPE (block_var), POINTER :: x_grid, y_grid, zbot_grid
     TYPE (block_var), POINTER :: x, y, zbot
     TYPE (block_var), POINTER :: x_out, y_out, zbot_out
     TYPE (block_var), POINTER :: x_xsi, y_xsi, x_eta, y_eta
     TYPE (block_var), POINTER :: hp1, hp2, hv1, hv2, hu1, hu2, gp12
     TYPE (block_var), POINTER :: eddy, kx_diff, ky_diff, chezy
     TYPE (block_var), POINTER :: uvel, vvel, depth, wsel
     TYPE (block_var), POINTER :: uvel_p, vvel_p, u_cart, v_cart
     TYPE (block_var), POINTER :: froude_num, courant_num
     TYPE (block_var), POINTER :: mass_source

     DOUBLE PRECISION, POINTER :: uflux(:,:)      ! east face flux for c.v.
     DOUBLE PRECISION, POINTER :: vflux(:,:)      ! north face flux for c.v.
     DOUBLE PRECISION, POINTER :: dp(:,:)			! d' depth correction field
     DOUBLE PRECISION, POINTER :: bedshear1(:,:)		! bed shear stress in xsi direction
     DOUBLE PRECISION, POINTER :: bedshear2(:,:)		! bed shear stress in eta direction
     DOUBLE PRECISION, POINTER :: windshear1(:,:)	! wind shear stress in xsi direction
     DOUBLE PRECISION, POINTER :: windshear2(:,:)	! wind shear stress in eta direction
     DOUBLE PRECISION, POINTER :: shear(:,:) ! bed shear stress magnitude @ velocity locations
     DOUBLE PRECISION, POINTER :: apo(:,:)
     DOUBLE PRECISION, POINTER :: lud(:,:) ! part of p' coeff that has U vel stuff
     DOUBLE PRECISION, POINTER :: lvd(:,:) ! part of p' coeff that has V vel stuff

     DOUBLE PRECISION, POINTER :: xsource(:,:)


     ! These variables are used for transport; they hold hydrdynamic
     ! values that are calculated before the transport calculations
     ! begin

     DOUBLE PRECISION, POINTER :: k_e(:,:), k_w(:,:), k_n(:,:), k_s(:,:)
     DOUBLE PRECISION, POINTER :: depth_e(:,:), depth_w(:,:), depth_n(:,:), depth_s(:,:)
     DOUBLE PRECISION, POINTER :: flux_e(:,:), flux_w(:,:), flux_n(:,:), flux_s(:,:)
     DOUBLE PRECISION, POINTER :: diffu_e(:,:), diffu_w(:,:), diffu_n(:,:), diffu_s(:,:)
     DOUBLE PRECISION, POINTER :: pec_e(:,:), pec_w(:,:), pec_n(:,:), pec_s(:,:)
     DOUBLE PRECISION, POINTER :: TDG_stuff(:,:)	! work array for output of TDG delP, %Sat
  
     TYPE (isdead_struct), POINTER :: isdead(:,:)
     TYPE (cell_type_struct), POINTER :: cell(:,:)

     LOGICAL, POINTER :: isdry(:,:)

  END TYPE block_struct

  TYPE(block_struct), PUBLIC, ALLOCATABLE :: block(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_allocate_size
  !
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE block_allocate_size(blk, idx, xmax, ymax)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: idx, xmax, ymax
    INTEGER :: imin, imax, jmin, jmax

    blk%index = idx
    blk%xmax = xmax
    blk%ymax = ymax

    ! make a base for all (parallel) variables

    blk%varbase => block_var_base_allocate(blk%xmax, blk%ymax)

    ! grid coordinates, in their various forms

    blk%x_grid => block_var_allocate("x_grid", blk%varbase, const=.TRUE.)
    blk%y_grid => block_var_allocate("y_grid", blk%varbase, const=.TRUE.)
    blk%zbot_grid => block_var_allocate("zbot_grid", blk%varbase, const=.TRUE.)
    blk%x => block_var_allocate("x", blk%varbase, const=.TRUE.)
    blk%y => block_var_allocate("y", blk%varbase, const=.TRUE.)
    blk%zbot => block_var_allocate("zbot", blk%varbase, const=.TRUE.)
    blk%x_out => block_var_allocate("x_out", blk%varbase, const=.TRUE.)
    blk%y_out => block_var_allocate("y_out", blk%varbase, const=.TRUE.)
    blk%zbot_out => block_var_allocate("zbot_out", blk%varbase, const=.TRUE.)

    ! grid metrics

    blk%x_xsi => block_var_allocate("x_xsi", blk%varbase, const=.TRUE.)
    blk%y_xsi => block_var_allocate("y_xsi", blk%varbase, const=.TRUE.)
    blk%x_eta => block_var_allocate("x_eta", blk%varbase, const=.TRUE.)
    blk%y_eta => block_var_allocate("y_eta", blk%varbase, const=.TRUE.)
    blk%hp1 => block_var_allocate("hp1", blk%varbase, const=.TRUE.)
    blk%hp2 => block_var_allocate("hp2", blk%varbase, const=.TRUE.)
    blk%hu1 => block_var_allocate("hu1", blk%varbase, const=.TRUE.)
    blk%hu2 => block_var_allocate("hu2", blk%varbase, const=.TRUE.)
    blk%hv1 => block_var_allocate("hv1", blk%varbase, const=.TRUE.)
    blk%hv2 => block_var_allocate("hv2", blk%varbase, const=.TRUE.)
    blk%gp12 => block_var_allocate("gp12", blk%varbase, const=.TRUE.)

    ! simulation coefficients that can vary spatially

    blk%chezy => block_var_allocate("chezy", blk%varbase, const=.TRUE.)
    blk%kx_diff => block_var_allocate("kx_diff", blk%varbase, const=.TRUE.)
    blk%ky_diff => block_var_allocate("ky_diff", blk%varbase, const=.TRUE.)
    blk%eddy => block_var_allocate("eddy", blk%varbase, const=.TRUE.)

    ! hydrodynamic variables

    blk%uvel => block_var_allocate("uvel", blk%varbase, const=.FALSE.)
    blk%vvel => block_var_allocate("vvel", blk%varbase, const=.FALSE.)
    blk%depth => block_var_allocate("depth", blk%varbase, const=.FALSE.)
    blk%wsel => block_var_allocate("wsel", blk%varbase, const=.TRUE.)

    blk%u_cart => block_var_allocate("u_cart", blk%varbase, const=.TRUE.)
    blk%v_cart => block_var_allocate("v_cart", blk%varbase, const=.TRUE.)
    blk%u_cart => block_var_allocate("u_cart", blk%varbase, const=.TRUE.)
    blk%v_cart => block_var_allocate("v_cart", blk%varbase, const=.TRUE.)

    ! local variables that do not need to be shared with other processors

    CALL block_owned_window(blk, imin, imax, jmin, jmax)

    ALLOCATE(blk%uflux(imin:imax, jmin:jmax))
    ALLOCATE(blk%vflux(imin:imax, jmin:jmax))
    ALLOCATE(blk%dp(imin:imax, jmin:jmax))
    ALLOCATE(blk%bedshear1(imin:imax, jmin:jmax))
    ALLOCATE(blk%bedshear2(imin:imax, jmin:jmax))
    ALLOCATE(blk%shear(imin:imax, jmin:jmax))
    ALLOCATE(blk%windshear1(imin:imax, jmin:jmax))
    ALLOCATE(blk%windshear2(imin:imax, jmin:jmax))
    ALLOCATE(blk%apo(imin:imax, jmin:jmax))
    ALLOCATE(blk%lud(imin:imax, jmin:jmax))
    ALLOCATE(blk%lvd(imin:imax, jmin:jmax))
    ALLOCATE(blk%xsource(imin:imax, jmin:jmax)) 

  END SUBROUTINE block_allocate_size

  ! ----------------------------------------------------------------
  ! FUNCTION block_buffer
  ! ----------------------------------------------------------------
  FUNCTION block_buffer(blk)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(IN) :: blk
    DOUBLE PRECISION, POINTER :: block_buffer(:, :)

    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_global
    imax = blk%varbase%imax_global
    jmin = blk%varbase%jmin_global
    jmax = blk%varbase%jmax_global

    ALLOCATE(block_buffer(imin:imax, jmin:jmax))

    RETURN

  END FUNCTION block_buffer

  ! ----------------------------------------------------------------
  ! FUNCTION block_array_owned(blk)
  ! ----------------------------------------------------------------
  FUNCTION block_array_owned(blk)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(IN) :: blk
    DOUBLE PRECISION, POINTER :: block_array_owned(:, :)

    INTEGER :: imin, imax, jmin, jmax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)

    ALLOCATE(block_array_owned(imin:imax, jmin:jmax))

    RETURN

  END FUNCTION block_array_owned

  ! ----------------------------------------------------------------
  ! FUNCTION block_array_used(blk)
  ! ----------------------------------------------------------------
  FUNCTION block_array_used(blk)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(IN) :: blk
    DOUBLE PRECISION, POINTER :: block_array_used(:, :)

    INTEGER :: imin, imax, jmin, jmax

    CALL block_used_window(blk, imin, imax, jmin, jmax)

    ALLOCATE(block_array_used(imin:imax, jmin:jmax))

    RETURN

  END FUNCTION block_array_used

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_i(blk, i)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i
    INTEGER :: imin, imax

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned

    block_owns_i = ((imin .LE. i) .AND. (i .LE. imax))
  END FUNCTION block_owns_i
  
  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_j(blk, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: j
    INTEGER :: jmin, jmax

    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

    block_owns_j = ((jmin .LE. j) .AND. (j .LE. jmax))
  END FUNCTION block_owns_j

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns(blk, i, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i, j
    block_owns = (block_owns_i(blk, i) .AND. block_owns_j(blk, j))
  END FUNCTION block_owns


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_owned_window
  ! ----------------------------------------------------------------
  SUBROUTINE block_owned_window(blk, imin, jmin, imax, jmax)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(OUT) :: imin, jmin, imax, jmax

    imin = blk%varbase%imin_owned
    imax = blk%varbase%imax_owned
    jmin = blk%varbase%jmin_owned
    jmax = blk%varbase%jmax_owned

  END SUBROUTINE block_owned_window
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_used_window
  ! ----------------------------------------------------------------
  SUBROUTINE block_used_window(blk, imin, jmin, imax, jmax)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(OUT) :: imin, jmin, imax, jmax

    imin = blk%varbase%imin_used
    imax = blk%varbase%imax_used
    jmin = blk%varbase%jmin_used
    jmax = blk%varbase%jmax_used

  END SUBROUTINE block_used_window
  


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_distribution_report
  ! ----------------------------------------------------------------
  SUBROUTINE block_distribution_report()

    IMPLICIT NONE

    INTEGER :: b, nblk(1)
    INTEGER :: p, nproc, me
    INTEGER :: ij(4)
    INTEGER :: lo(ndim), hi(ndim)

    me = ga_nodeid()
    nproc = ga_nnodes()

    nblk = UBOUND(block)
    DO b = 1, nblk(1)
       IF (me == 0) THEN
          WRITE(*, 95) b, block(b)%varbase%imin_global, &
               &block(b)%varbase%imax_global, &
               &block(b)%varbase%jmin_global, &
               &block(b)%varbase%jmax_global
       END IF
       DO p = 0, nproc-1
          ij = 0
          lo = 0
          hi = 0
          IF (p == me) THEN
             ij(1) = block(b)%varbase%imin_owned
             ij(2) = block(b)%varbase%imax_owned
             ij(3) = block(b)%varbase%jmin_owned
             ij(4) = block(b)%varbase%jmax_owned
             CALL nga_distribution(block(b)%varbase%ga_handle, me, lo, hi)
          END IF
          call ga_igop(MT_INT, ij, 4, '+')
          call ga_igop(MT_INT, lo, ndim, '+')
          call ga_igop(MT_INT, hi, ndim, '+')

          IF (me == 0) THEN
             WRITE(*,100) b, p, ij(1), ij(2), ij(3), ij(4)
             WRITE(*,101) b, p, lo(1), hi(1), lo(2), hi(2)
          END IF
       END DO
    END DO
95  FORMAT('Block ', I2, ':      Global MASS2: ', I3, ': ', I3, ': ', I3, ': ', I3)
100 FORMAT('Block ', I2, ': Process ', I2, ': MASS2: ', I3, ': ', I3, ': ', I3, ': ', I3)
101 FORMAT('Block ', I2, ': Process ', I2, ': GA:    ', I3, ': ', I3, ': ', I3, ': ', I3)

  END SUBROUTINE block_distribution_report


END MODULE block_module
