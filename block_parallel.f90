! ----------------------------------------------------------------
! file: block_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: 2014-04-22 07:50:50 d3g096
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

     ! this defines the distribution of all parallel variables for
     ! this block

     TYPE (block_var_base), POINTER :: varbase

     DOUBLE PRECISION, POINTER :: x_grid(:,:)
     DOUBLE PRECISION, POINTER :: y_grid(:,:)
     DOUBLE PRECISION, POINTER :: zbot_grid(:,:)
     DOUBLE PRECISION, POINTER :: x(:,:)
     DOUBLE PRECISION, POINTER :: y(:,:)
     DOUBLE PRECISION, POINTER :: zbot(:,:)
     DOUBLE PRECISION, POINTER :: x_out(:,:)
     DOUBLE PRECISION, POINTER :: y_out(:,:)
     DOUBLE PRECISION, POINTER :: zbot_out(:,:)
     DOUBLE PRECISION, POINTER :: x_xsi(:,:)
     DOUBLE PRECISION, POINTER :: y_xsi(:,:)
     DOUBLE PRECISION, POINTER :: x_eta(:,:)
     DOUBLE PRECISION, POINTER :: y_eta(:,:)
     DOUBLE PRECISION, POINTER :: hp1(:,:)
     DOUBLE PRECISION, POINTER :: hp2(:,:)
     DOUBLE PRECISION, POINTER :: hv1(:,:)
     DOUBLE PRECISION, POINTER :: hv2(:,:)
     DOUBLE PRECISION, POINTER :: hu1(:,:)
     DOUBLE PRECISION, POINTER :: hu2(:,:)
     DOUBLE PRECISION, POINTER :: gp12(:,:)
     DOUBLE PRECISION, POINTER :: slope(:,:)
     DOUBLE PRECISION, POINTER :: eddy(:,:)
     DOUBLE PRECISION, POINTER :: kx_diff(:,:)
     DOUBLE PRECISION, POINTER :: ky_diff(:,:)
     DOUBLE PRECISION, POINTER :: chezy(:,:)


     DOUBLE PRECISION, POINTER :: uvel(:,:)
     DOUBLE PRECISION, POINTER :: uvelstar(:,:)
     DOUBLE PRECISION, POINTER :: uvelold(:,:)
     DOUBLE PRECISION, POINTER :: uveloldold(:,:)
     DOUBLE PRECISION, POINTER :: vvel(:,:)
     DOUBLE PRECISION, POINTER :: vvelstar(:,:)
     DOUBLE PRECISION, POINTER :: vvelold(:,:)
     DOUBLE PRECISION, POINTER :: vveloldold(:,:)
     DOUBLE PRECISION, POINTER :: depth(:,:)
     DOUBLE PRECISION, POINTER :: depthstar(:,:)
     DOUBLE PRECISION, POINTER :: depthold(:,:)
     DOUBLE PRECISION, POINTER :: deptholdold(:,:)
     DOUBLE PRECISION, POINTER :: wsel(:,:)
     DOUBLE PRECISION, POINTER :: dp(:,:) ! d' depth correction field
     DOUBLE PRECISION, POINTER :: uvel_p(:,:)
     DOUBLE PRECISION, POINTER :: vvel_p(:,:)
     DOUBLE PRECISION, POINTER :: u_cart(:,:)
     DOUBLE PRECISION, POINTER :: v_cart(:,:)
     DOUBLE PRECISION, POINTER :: vmag(:,:)
     DOUBLE PRECISION, POINTER :: shear(:,:)
     DOUBLE PRECISION, POINTER :: froude_num(:,:)
     DOUBLE PRECISION, POINTER :: courant_num(:,:)
     DOUBLE PRECISION, POINTER :: mass_source(:,:)
     DOUBLE PRECISION, POINTER :: uflux(:,:)
     DOUBLE PRECISION, POINTER :: vflux(:,:)
     DOUBLE PRECISION, POINTER :: lud(:,:) ! part of p' coeff that has U vel stuff
     DOUBLE PRECISION, POINTER :: lvd(:,:) ! part of p' coeff that has V vel stuff

     ! These are managers for the above variables having values that
     ! need to be known over multiple processors, either for
     ! computation or input/output

     TYPE (block_var), POINTER :: bv_x_grid
     TYPE (block_var), POINTER :: bv_y_grid
     TYPE (block_var), POINTER :: bv_zbot_grid
     TYPE (block_var), POINTER :: bv_x
     TYPE (block_var), POINTER :: bv_y
     TYPE (block_var), POINTER :: bv_zbot
     TYPE (block_var), POINTER :: bv_x_out
     TYPE (block_var), POINTER :: bv_y_out
     TYPE (block_var), POINTER :: bv_zbot_out
     TYPE (block_var), POINTER :: bv_x_xsi
     TYPE (block_var), POINTER :: bv_y_xsi
     TYPE (block_var), POINTER :: bv_x_eta
     TYPE (block_var), POINTER :: bv_y_eta
     TYPE (block_var), POINTER :: bv_hp1
     TYPE (block_var), POINTER :: bv_hp2
     TYPE (block_var), POINTER :: bv_hv1
     TYPE (block_var), POINTER :: bv_hv2
     TYPE (block_var), POINTER :: bv_hu1
     TYPE (block_var), POINTER :: bv_hu2
     TYPE (block_var), POINTER :: bv_gp12
     TYPE (block_var), POINTER :: bv_slope
     TYPE (block_var), POINTER :: bv_eddy
     TYPE (block_var), POINTER :: bv_kx_diff
     TYPE (block_var), POINTER :: bv_ky_diff
     TYPE (block_var), POINTER :: bv_chezy
     TYPE (block_var), POINTER :: bv_uvel
     TYPE (block_var), POINTER :: bv_vvel
     TYPE (block_var), POINTER :: bv_depth
     TYPE (block_var), POINTER :: bv_wsel
     TYPE (block_var), POINTER :: bv_dp
     TYPE (block_var), POINTER :: bv_uvel_p
     TYPE (block_var), POINTER :: bv_vvel_p
     TYPE (block_var), POINTER :: bv_u_cart
     TYPE (block_var), POINTER :: bv_v_cart
     TYPE (block_var), POINTER :: bv_vmag
     TYPE (block_var), POINTER :: bv_shear
     TYPE (block_var), POINTER :: bv_froude_num
     TYPE (block_var), POINTER :: bv_courant_num
     TYPE (block_var), POINTER :: bv_mass_source
     TYPE (block_var), POINTER :: bv_uflux
     TYPE (block_var), POINTER :: bv_vflux
     TYPE (block_var), POINTER :: bv_isdry
     TYPE (block_var), POINTER :: bv_dead
     TYPE (block_var), POINTER :: bv_lud
     TYPE (block_var), POINTER :: bv_lvd

     ! these values are only needed locally

     DOUBLE PRECISION, POINTER :: bedshear1(:,:) ! bed shear stress in xsi direction
     DOUBLE PRECISION, POINTER :: bedshear2(:,:) ! bed shear stress in eta direction
     DOUBLE PRECISION, POINTER :: windshear1(:,:) ! wind shear stress in xsi direction
     DOUBLE PRECISION, POINTER :: windshear2(:,:) ! wind shear stress in eta direction
     DOUBLE PRECISION, POINTER :: apo(:,:)

     DOUBLE PRECISION, POINTER :: xsource(:,:)


     ! These local variables are used for transport; they hold
     ! hydrdynamic values that are calculated before the transport
     ! calculations begin

     DOUBLE PRECISION, POINTER :: k_e(:,:), k_w(:,:), k_n(:,:), k_s(:,:)
     DOUBLE PRECISION, POINTER :: depth_e(:,:), depth_w(:,:), depth_n(:,:), depth_s(:,:)
     DOUBLE PRECISION, POINTER :: flux_e(:,:), flux_w(:,:), flux_n(:,:), flux_s(:,:)
     DOUBLE PRECISION, POINTER :: diffu_e(:,:), diffu_w(:,:), diffu_n(:,:), diffu_s(:,:)
     DOUBLE PRECISION, POINTER :: pec_e(:,:), pec_w(:,:), pec_n(:,:), pec_s(:,:)
  
     TYPE (isdead_struct), POINTER :: isdead(:,:)
     TYPE (cell_type_struct), POINTER :: cell(:,:)

     LOGICAL, POINTER :: isdry(:,:)
     LOGICAL, POINTER :: isdrystar(:,:)

     DOUBLE PRECISION :: mass_source_sum(1)

     ! This is allocated only by the root process.  It is used to
     ! collect the various block_var's to the root process for output

     DOUBLE PRECISION, POINTER :: buffer(:,:)

  END TYPE block_struct

  ! where the all the blocks are stored
  TYPE(block_struct), PUBLIC, ALLOCATABLE :: block(:)

  ! some work arrays used when applying boundary conditions
  DOUBLE PRECISION, ALLOCATABLE, PUBLIC :: inlet_area(:), table_input(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_allocate_size
  !
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE block_allocate_size(blk, idx, xmax, ymax)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: idx, xmax, ymax
    INTEGER :: imin, imax, jmin, jmax
    INTEGER :: pgrp, dims(1)
    LOGICAL :: ok

    blk%index = idx
    blk%xmax = xmax
    blk%ymax = ymax

    ! make a base for all (parallel) variables

    blk%varbase => block_var_base_allocate(blk%xmax, blk%ymax, nslice)

    ! grid coordinates, in their various forms

    blk%bv_x_grid => block_var_allocate("x_grid", blk%varbase, const=.TRUE.)
    blk%bv_y_grid => block_var_allocate("y_grid", blk%varbase, const=.TRUE.)
    blk%bv_zbot_grid => block_var_allocate("zbot_grid", blk%varbase, const=.TRUE.)
    blk%bv_x => block_var_allocate("x", blk%varbase, const=.TRUE.)
    blk%bv_y => block_var_allocate("y", blk%varbase, const=.TRUE.)
    blk%bv_zbot => block_var_allocate("zbot", blk%varbase, const=.TRUE.)
    blk%bv_x_out => block_var_allocate("x_out", blk%varbase, const=.TRUE.)
    blk%bv_y_out => block_var_allocate("y_out", blk%varbase, const=.TRUE.)
    blk%bv_zbot_out => block_var_allocate("zbot_out", blk%varbase, const=.TRUE.)

    ! grid metrics

    blk%bv_x_xsi => block_var_allocate("x_xsi", blk%varbase, const=.TRUE.)
    blk%bv_y_xsi => block_var_allocate("y_xsi", blk%varbase, const=.TRUE.)
    blk%bv_x_eta => block_var_allocate("x_eta", blk%varbase, const=.TRUE.)
    blk%bv_y_eta => block_var_allocate("y_eta", blk%varbase, const=.TRUE.)
    blk%bv_hp1 => block_var_allocate("hp1", blk%varbase, const=.TRUE.)
    blk%bv_hp2 => block_var_allocate("hp2", blk%varbase, const=.TRUE.)
    blk%bv_hu1 => block_var_allocate("hu1", blk%varbase, const=.TRUE.)
    blk%bv_hu2 => block_var_allocate("hu2", blk%varbase, const=.TRUE.)
    blk%bv_hv1 => block_var_allocate("hv1", blk%varbase, const=.TRUE.)
    blk%bv_hv2 => block_var_allocate("hv2", blk%varbase, const=.TRUE.)
    blk%bv_gp12 => block_var_allocate("gp12", blk%varbase, const=.TRUE.)
    blk%bv_slope => block_var_allocate("slope", blk%varbase, const=.TRUE.)

    ! simulation coefficients that can vary spatially

    blk%bv_chezy => block_var_allocate("chezy", blk%varbase, const=.TRUE.)
    blk%bv_kx_diff => block_var_allocate("kx_diff", blk%varbase, const=.TRUE.)
    blk%bv_ky_diff => block_var_allocate("ky_diff", blk%varbase, const=.TRUE.)
    blk%bv_eddy => block_var_allocate("eddy", blk%varbase, const=.TRUE.)

    ! hydrodynamic variables

    blk%bv_uvel => block_var_allocate("uvel", blk%varbase, const=.FALSE.)
    blk%bv_vvel => block_var_allocate("vvel", blk%varbase, const=.FALSE.)
    blk%bv_depth => block_var_allocate("depth", blk%varbase, const=.FALSE.)
    blk%bv_wsel => block_var_allocate("wsel", blk%varbase, const=.TRUE.)
    blk%bv_dp => block_var_allocate("dp", blk%varbase, const=.TRUE.)

    blk%bv_uvel_p => block_var_allocate("uvel_p", blk%varbase, const=.TRUE.)
    blk%bv_vvel_p => block_var_allocate("vvel_p", blk%varbase, const=.TRUE.)
    blk%bv_u_cart => block_var_allocate("u_cart", blk%varbase, const=.TRUE.)
    blk%bv_v_cart => block_var_allocate("v_cart", blk%varbase, const=.TRUE.)

    blk%bv_vmag => block_var_allocate("vmag", blk%varbase, const=.TRUE.)
    blk%bv_shear => block_var_allocate("shear", blk%varbase, const=.TRUE.)
    blk%bv_froude_num => block_var_allocate("froude_num", blk%varbase, const=.TRUE.)
    blk%bv_courant_num => block_var_allocate("courant_num", blk%varbase, const=.TRUE.)
    blk%bv_mass_source => block_var_allocate("mass_source", blk%varbase, const=.TRUE.)

    blk%bv_uflux => block_var_allocate("u_flux", blk%varbase, const=.TRUE.)
    blk%bv_vflux => block_var_allocate("v_flux", blk%varbase, const=.TRUE.)

    blk%bv_isdry => block_var_allocate("isdry", blk%varbase, const=.TRUE.)
    blk%bv_dead => block_var_allocate("dead", blk%varbase, const=.TRUE.)

    blk%bv_lud => block_var_allocate("lud", blk%varbase, const=.TRUE.)
    blk%bv_lvd => block_var_allocate("lvd", blk%varbase, const=.TRUE.)

    ! local variables that need to be shared with other processors
    ! point at the arrays allocated as part of a block_var

    blk%x_grid => blk%bv_x_grid%current
    blk%y_grid => blk%bv_y_grid%current
    blk%zbot_grid => blk%bv_zbot_grid%current
    blk%x => blk%bv_x%current
    blk%y => blk%bv_y%current
    blk%zbot => blk%bv_zbot%current
    blk%x_out => blk%bv_x_out%current
    blk%y_out => blk%bv_y_out%current
    blk%zbot_out => blk%bv_zbot_out%current
    blk%x_xsi => blk%bv_x_xsi%current
    blk%y_xsi => blk%bv_y_xsi%current
    blk%x_eta => blk%bv_x_eta%current
    blk%y_eta => blk%bv_y_eta%current
    blk%hp1 => blk%bv_hp1%current
    blk%hp2 => blk%bv_hp2%current
    blk%hv1 => blk%bv_hv1%current
    blk%hv2 => blk%bv_hv2%current
    blk%hu1 => blk%bv_hu1%current
    blk%hu2 => blk%bv_hu2%current
    blk%gp12 => blk%bv_gp12%current
    blk%slope => blk%bv_slope%current
    blk%eddy => blk%bv_eddy%current
    blk%kx_diff => blk%bv_kx_diff%current
    blk%ky_diff => blk%bv_ky_diff%current
    blk%chezy => blk%bv_chezy%current

    blk%uvel => blk%bv_uvel%current
    blk%uvelstar => blk%bv_uvel%star
    blk%uvelold => blk%bv_uvel%old
    blk%uveloldold => blk%bv_uvel%oldold
    blk%vvel => blk%bv_vvel%current
    blk%vvelstar => blk%bv_vvel%star
    blk%vvelold => blk%bv_vvel%old
    blk%vveloldold => blk%bv_vvel%oldold
    blk%depth => blk%bv_depth%current
    blk%depthstar => blk%bv_depth%star
    blk%depthold => blk%bv_depth%old
    blk%deptholdold => blk%bv_depth%oldold
    blk%wsel => blk%bv_wsel%current
    blk%dp => blk%bv_dp%current
    blk%uvel_p => blk%bv_uvel_p%current
    blk%vvel_p => blk%bv_vvel_p%current
    blk%u_cart => blk%bv_u_cart%current
    blk%v_cart => blk%bv_v_cart%current
    blk%vmag => blk%bv_vmag%current
    blk%shear => blk%bv_shear%current
    blk%froude_num => blk%bv_froude_num%current
    blk%courant_num => blk%bv_courant_num%current
    blk%mass_source => blk%bv_mass_source%current
    blk%uflux => blk%bv_uflux%current
    blk%vflux => blk%bv_vflux%current
    blk%lud => blk%bv_lud%current
    blk%lvd => blk%bv_lvd%current

    ! these are not shared with other processors, so the "owned"
    ! window will do

    blk%bedshear1 => block_array_owned(blk)
    blk%bedshear2 => block_array_owned(blk)
    blk%windshear1 => block_array_owned(blk)
    blk%windshear2 => block_array_owned(blk)
    blk%apo => block_array_owned(blk)
    blk%xsource => block_array_owned(blk)
    blk%k_e => block_array_owned(blk)
    blk%k_w => block_array_owned(blk)
    blk%k_n => block_array_owned(blk)
    blk%k_s => block_array_owned(blk)
    blk%depth_e => block_array_owned(blk)
    blk%depth_w => block_array_owned(blk)
    blk%depth_n => block_array_owned(blk)
    blk%depth_s => block_array_owned(blk)
    blk%flux_e => block_array_owned(blk)
    blk%flux_w => block_array_owned(blk)
    blk%flux_n => block_array_owned(blk)
    blk%flux_s => block_array_owned(blk)
    blk%diffu_e => block_array_owned(blk)
    blk%diffu_w => block_array_owned(blk)
    blk%diffu_n => block_array_owned(blk)
    blk%diffu_s => block_array_owned(blk)
    blk%pec_e => block_array_owned(blk)
    blk%pec_w => block_array_owned(blk)
    blk%pec_n => block_array_owned(blk)
    blk%pec_s => block_array_owned(blk)

    CALL block_used_window(blk, imin, imax, jmin, jmax)
    ALLOCATE(blk%isdead(imin:imax, jmin:jmax))
    blk%isdead%u = .FALSE.
    blk%isdead%v = .FALSE.
    blk%isdead%p = .FALSE.
    blk%isdead%transient = .FALSE.
    ALLOCATE(blk%cell(imin:imax, jmin:jmax))
    ALLOCATE(blk%isdry(imin:imax, jmin:jmax))
    blk%isdry = .FALSE.
    ALLOCATE(blk%isdrystar(imin:imax, jmin:jmax))
    blk%isdrystar = .FALSE.
    
    blk%buffer => block_buffer(blk)

    blk%mass_source_sum = 0.0

  END SUBROUTINE block_allocate_size

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_deallocate
  ! ----------------------------------------------------------------
  SUBROUTINE block_deallocate(blk)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_struct), INTENT(INOUT) :: blk
    LOGICAL :: ok

    CALL block_var_deallocate(blk%bv_x_grid)
    CALL block_var_deallocate(blk%bv_y_grid)
    CALL block_var_deallocate(blk%bv_zbot_grid)
    CALL block_var_deallocate(blk%bv_x)
    CALL block_var_deallocate(blk%bv_y)
    CALL block_var_deallocate(blk%bv_zbot)
    CALL block_var_deallocate(blk%bv_x_out)
    CALL block_var_deallocate(blk%bv_y_out)
    CALL block_var_deallocate(blk%bv_zbot_out)

    CALL block_var_deallocate(blk%bv_x_xsi)
    CALL block_var_deallocate(blk%bv_y_xsi)
    CALL block_var_deallocate(blk%bv_x_eta)
    CALL block_var_deallocate(blk%bv_y_eta)
    CALL block_var_deallocate(blk%bv_hp1)
    CALL block_var_deallocate(blk%bv_hp2)
    CALL block_var_deallocate(blk%bv_hu1)
    CALL block_var_deallocate(blk%bv_hu2)
    CALL block_var_deallocate(blk%bv_hv1)
    CALL block_var_deallocate(blk%bv_hv2)
    CALL block_var_deallocate(blk%bv_gp12)

    CALL block_var_deallocate(blk%bv_chezy)
    CALL block_var_deallocate(blk%bv_kx_diff)
    CALL block_var_deallocate(blk%bv_ky_diff)
    CALL block_var_deallocate(blk%bv_eddy)

    CALL block_var_deallocate(blk%bv_uvel)
    CALL block_var_deallocate(blk%bv_vvel)
    CALL block_var_deallocate(blk%bv_depth)
    CALL block_var_deallocate(blk%bv_wsel)
    CALL block_var_deallocate(blk%bv_dp)

    CALL block_var_deallocate(blk%bv_uvel_p)
    CALL block_var_deallocate(blk%bv_vvel_p)
    CALL block_var_deallocate(blk%bv_u_cart)
    CALL block_var_deallocate(blk%bv_v_cart)

    CALL block_var_deallocate(blk%bv_vmag)
    CALL block_var_deallocate(blk%bv_shear)
    CALL block_var_deallocate(blk%bv_froude_num)
    CALL block_var_deallocate(blk%bv_courant_num)
    CALL block_var_deallocate(blk%bv_mass_source)

    CALL block_var_deallocate(blk%bv_uflux)
    CALL block_var_deallocate(blk%bv_vflux)

    CALL block_var_deallocate(blk%bv_isdry)
    CALL block_var_deallocate(blk%bv_dead)

    CALL block_var_deallocate(blk%bv_lud)
    CALL block_var_deallocate(blk%bv_lvd)

    DEALLOCATE(blk%bedshear1)
    DEALLOCATE(blk%bedshear2)
    DEALLOCATE(blk%windshear1)
    DEALLOCATE(blk%windshear2)
    DEALLOCATE(blk%apo)
    DEALLOCATE(blk%xsource)
    DEALLOCATE(blk%k_e)
    DEALLOCATE(blk%k_w)
    DEALLOCATE(blk%k_n)
    DEALLOCATE(blk%k_s)
    DEALLOCATE(blk%depth_e)
    DEALLOCATE(blk%depth_w)
    DEALLOCATE(blk%depth_n)
    DEALLOCATE(blk%depth_s)
    DEALLOCATE(blk%flux_e)
    DEALLOCATE(blk%flux_w)
    DEALLOCATE(blk%flux_n)
    DEALLOCATE(blk%flux_s)
    DEALLOCATE(blk%diffu_e)
    DEALLOCATE(blk%diffu_w)
    DEALLOCATE(blk%diffu_n)
    DEALLOCATE(blk%diffu_s)
    DEALLOCATE(blk%pec_e)
    DEALLOCATE(blk%pec_w)
    DEALLOCATE(blk%pec_n)
    DEALLOCATE(blk%pec_s)

    DEALLOCATE(blk%isdead)
    DEALLOCATE(blk%cell)

    DEALLOCATE(blk%isdry)
    DEALLOCATE(blk%isdrystar)

    DEALLOCATE(blk%buffer)


  END SUBROUTINE block_deallocate


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
  ! FUNCTION block_logical_buffer
  ! ----------------------------------------------------------------
  FUNCTION block_logical_buffer(blk)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(IN) :: blk
    LOGICAL, POINTER :: block_logical_buffer(:, :)

    INTEGER :: imin, imax, jmin, jmax

    imin = blk%varbase%imin_global
    imax = blk%varbase%imax_global
    jmin = blk%varbase%jmin_global
    jmax = blk%varbase%jmax_global

    ALLOCATE(block_logical_buffer(imin:imax, jmin:jmax))

    RETURN

  END FUNCTION block_logical_buffer

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
    block_array_owned = 0.0

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
    block_array_used = 0.0

    RETURN

  END FUNCTION block_array_used

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_i(blk, i)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i

    block_owns_i = block_var_base_owns_i(blk%varbase, i)
  END FUNCTION block_owns_i
  
  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_owns_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_owns_j(blk, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: j

    block_owns_j = block_var_base_owns_j(blk%varbase, j)
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
  ! LOGICAL FUNCTION block_uses_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_uses_i(blk, i)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i

    block_uses_i = block_var_base_uses_i(blk%varbase, i)
  END FUNCTION block_uses_i
  
  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_uses_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_uses_j(blk, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: j

    block_uses_j = block_var_base_uses_j(blk%varbase, j)
  END FUNCTION block_uses_j

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_uses
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_uses(blk, i, j)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: i, j
    block_uses = (block_uses_i(blk, i) .AND. block_uses_j(blk, j))
  END FUNCTION block_uses

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_owned_window
  ! ----------------------------------------------------------------
  SUBROUTINE block_owned_window(blk, imin, imax, jmin, jmax)

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
  SUBROUTINE block_used_window(blk, imin, imax, jmin, jmax)

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

#include "mafdecls.fh"
#include "global.fh"
 
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
          call ga_igop(MT_F_INT, ij, 4, '+')
          call ga_igop(MT_F_INT, lo, ndim, '+')
          call ga_igop(MT_F_INT, hi, ndim, '+')

          IF (me == 0) THEN
             WRITE(*,100) b, p, ij(1), ij(2), ij(3), ij(4)
             WRITE(*,101) b, p, lo(1), hi(1), lo(2), hi(2)
          END IF
       END DO
    END DO
95  FORMAT('Block ', I2, ':      Global MASS2: ', I5, ': ', I5, ': ', I5, ': ', I5)
100 FORMAT('Block ', I2, ': Process ', I5, ': MASS2: ', I5, ': ', I5, ': ', I5, ': ', I5)
101 FORMAT('Block ', I2, ': Process ', I5, ': GA:    ', I5, ': ', I5, ': ', I5, ': ', I5)

  END SUBROUTINE block_distribution_report

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_collect
  ! ----------------------------------------------------------------
  SUBROUTINE block_collect(blk, var)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (block_var), INTENT(IN) :: var

    CALL block_var_get_all(var, blk%buffer)

  END SUBROUTINE block_collect

END MODULE block_module
