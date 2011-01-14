! ----------------------------------------------------------------
! file: hydro_solve.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 23, 2002 by William A. Perkins
! Last Change: Thu Jan 13 14:53:37 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL


! ----------------------------------------------------------------
! MODULE hydro_solve
! ----------------------------------------------------------------
MODULE hydro_solve

  USE config
  USE block_hydro
  USE block_hydro_bc
  USE differencing
  USE solver_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  !#################################################################################
  !---------------------------------------------------------------------------------
  !
  ! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
  !
  !---------------------------------------------------------------------------------
  SUBROUTINE hydro()

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    INTEGER :: x_beg, y_beg, x_end, y_end, num_bc, i, j
    LOGICAL :: alldry
    DOUBLE PRECISION :: maxx_mass
    LOGICAL :: ds_flux_given
    INTEGER :: iblock
    INTEGER :: iteration

    ! Assign U,V,D BCs for this time
    ! set U boundary conditions for this time

    !----------------------------------------------------------------------------
    ! iteration loop at a fixed time using prescribed U (or Discharge),V,D BCs

    DO iteration = 1,number_hydro_iterations

       !**** BLOCK LOOP HERE *****
       DO iblock = 1,max_blocks 

          ds_flux_given = .FALSE. ! ignore special velocity/flux processing if not needed

          CALL default_hydro_bc(block(iblock))

          ! loop over the total number of bc specifications
          CALL block_compute_bc_flux(block(iblock), block_bc(iblock))
          DO num_bc = 1, block_bc(iblock)%num_bc
             CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
                  &.FALSE., ds_flux_given)
          END DO

          CALL block_var_put(block(iblock)%bv_uvel, BLK_VAR_CURRENT)
          CALL block_var_put(block(iblock)%bv_uvel, BLK_VAR_STAR)
          CALL block_var_put(block(iblock)%bv_vvel, BLK_VAR_CURRENT)
          CALL block_var_put(block(iblock)%bv_vvel, BLK_VAR_STAR)
          CALL block_var_put(block(iblock)%bv_depth, BLK_VAR_CURRENT)
          CALL block_var_put(block(iblock)%bv_depth, BLK_VAR_STAR)
          CALL ga_sync()
          CALL block_var_get(block(iblock)%bv_uvel, BLK_VAR_CURRENT)
          CALL block_var_get(block(iblock)%bv_uvel, BLK_VAR_STAR)
          CALL block_var_get(block(iblock)%bv_vvel, BLK_VAR_CURRENT)
          CALL block_var_get(block(iblock)%bv_vvel, BLK_VAR_STAR)
          CALL block_var_get(block(iblock)%bv_depth, BLK_VAR_CURRENT)
          CALL block_var_get(block(iblock)%bv_depth, BLK_VAR_STAR)
          
          !-------------------------------------------------------------------------

          x_beg = 2
          y_beg = 2
          x_end = block(iblock)%xmax
          y_end = block(iblock)%ymax

          ! Turn off cells that are dry, and
          ! check to see if the entire block is
          ! dry.  If it is, there is really no
          ! point in doing these calculations.

          alldry = .FALSE.
          IF (do_wetdry) THEN
             alldry = .TRUE.
             DO i=1,x_end+1
                DO j=1,y_end+1
                   IF (block_uses(block(iblock), i, j)) THEN
                      IF (block(iblock)%isdry(i,j)) THEN
                         block(iblock)%isdead(i  , j  )%p = .TRUE.
                         IF (block_uses(block(iblock), i-1, j)) &
                              &block(iblock)%isdead(i-1, j  )%u = .TRUE.
                         block(iblock)%isdead(i  , j  )%u = .TRUE.
                         IF (block_uses(block(iblock), i, j-1)) &
                              &block(iblock)%isdead(i  , j-1)%v = .TRUE.
                         block(iblock)%isdead(i  , j  )%v = .TRUE.
                      ELSE 
                         alldry = .FALSE.
                      END IF
                   END IF
                END DO
             END DO
          END IF

          CALL uvel_solve(block(iblock), delta_t)

          CALL vvel_solve(block(iblock), delta_t)

          CALL depth_solve(block(iblock), delta_t)

!!$          IF(debug)THEN
!!$             WRITE(output_iounit,*)"U* Velocity (before depth correction)"
!!$             DO i=1,block(iblock)%xmax
!!$                WRITE(output_iounit,1000)block(iblock)%ustar(i,:)
!!$                WRITE(output_iounit,*)
!!$             END DO
!!$
!!$             WRITE(output_iounit,*)" V* Velocity (before depth correction)"
!!$             DO i=1,block(iblock)%xmax
!!$                WRITE(output_iounit,1000)block(iblock)%vstar(i,:)
!!$                WRITE(output_iounit,*)
!!$             END DO
!!$          ENDIF
!!$1000      FORMAT(50(f12.4,2x))

          CALL correct_velocity(block(iblock))

          CALL depth_check(block(iblock), current_time%date_string, current_time%time_string)

          IF (do_wetdry .AND. iterate_wetdry)&
               &CALL check_wetdry(block(iblock))

          ! Calculate bed shear stress here if it is needed for eddy
          ! viscosity calculation
          CALL bedshear(block(iblock))

          ! If specified, compute eddy viscosity
          IF (do_calc_eddy) THEN
             CALL calc_eddy_viscosity(block(iblock))
          END IF

       END DO		! block loop end

       !-----------------------------------------------------------------------------------
       ! check to see if mass source has been reduced below tolerance and break out of loop

       maxx_mass = 0

       DO iblock=1,max_blocks
          CALL block_mass_source_sum(block(iblock))
          IF (block(iblock)%mass_source_sum(1) >= maxx_mass) &
               &maxx_mass = block(iblock)%mass_source_sum(1)
       END DO

       hydro_iteration = iteration

       IF(maxx_mass < max_mass_source_sum) EXIT ! break out of internal iteration loop

       !------------------------------------------------------------------------

    END DO    ! internal time iteration loop for momentum, depth correction equations
    

    IF (do_wetdry .AND. .NOT. iterate_wetdry) THEN
       DO iblock = 1, max_blocks
          CALL check_wetdry(block(iblock))
       END DO
    END IF

  END SUBROUTINE hydro

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION harmonic
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION harmonic(x1, x2)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: x1, x2

    IF (x1 + x2 .EQ. 0.0) THEN
       harmonic = 0.0
    ELSE 
       harmonic = 2.0*x1*x2/(x1 + x2)
    END IF

  END FUNCTION harmonic

  ! ----------------------------------------------------------------
  ! SUBROUTINE uvel_solve
  ! (Collective)
  ! ----------------------------------------------------------------
  SUBROUTINE uvel_solve(blk, delta_t)

    IMPLICIT NONE

    TYPE(block_struct), INTENT(INOUT) :: blk
    DOUBLE PRECISION, INTENT(IN) :: delta_t


    INTEGER :: blkidx

    DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
    DOUBLE PRECISION :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
    DOUBLE PRECISION :: zbot_e, zbot_w
    DOUBLE PRECISION :: flux_e,flux_w,flux_n,flux_s					! fluxes
    DOUBLE PRECISION :: diffu_e,diffu_w,diffu_n,diffu_s			! diffusion
    DOUBLE PRECISION :: aw2, ae2, as2, an2, ap2   ! second order flux coefficients
    DOUBLE PRECISION :: aww, aee, ass, ann        ! second order flux coefficients
    DOUBLE PRECISION :: apo                       ! coefficients in discretization eqns
    DOUBLE PRECISION :: u_p, u_s, u_n	! u velocities at P and on staggered grid
    DOUBLE PRECISION :: v_p, v_n, v_s, v_e, v_w	! v velocities at P and on staggered grid

    INTEGER :: x_beg, y_beg, i, j, junk

    DOUBLE PRECISION :: sc, sp
    DOUBLE PRECISION :: h1_eta_p, h2_xsi_p						! derivatives of metric coeff
    DOUBLE PRECISION :: h1_eta_e, h1_eta_w, h1_eta_n, h1_eta_s	! e.g., h1_eta_p is the partial deriv
    DOUBLE PRECISION :: h2_xsi_e, h2_xsi_w, h2_xsi_n, h2_xsi_s	! of h1 in eta direction at point p
    DOUBLE PRECISION :: curve_1,curve_2,curve_3,curve_4,curve_5,curve_6,curve_7	! curvature terms
    DOUBLE PRECISION :: k_p,k_e,k_w,k_n,k_s 
    DOUBLE PRECISION :: cross_term				! eddy viscosity cross term in momement equations
    DOUBLE PRECISION, EXTERNAL :: afunc

    INTEGER :: x_end, y_end
    INTEGER :: imin, imax, jmin, jmax

    LOGICAL :: slip

    DOUBLE PRECISION :: &
         &ap(2:blk%xmax, 2:blk%ymax), &
         &ae(2:blk%xmax, 2:blk%ymax), &
         &aw(2:blk%xmax, 2:blk%ymax), &
         &an(2:blk%xmax, 2:blk%ymax), &
         &as(2:blk%xmax, 2:blk%ymax), &
         &bp(2:blk%xmax, 2:blk%ymax), &
         &source(2:blk%xmax, 2:blk%ymax)

    blkidx = blk%index

    x_beg = 2
    y_beg = 2
    x_end = blk%xmax
    y_end = blk%ymax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, x_beg)
    imax = MIN(imax, x_end)
    jmin = MAX(jmin, y_beg)
    jmax = MIN(jmax, y_end)

    DO i=imin, imax
       DO j=jmin, jmax

          hp1 = blk%hu1(i,j) 
          hp2 = blk%hu2(i,j)
          he1 = blk%hp1(i+1,j)
          he2 = blk%hp2(i+1,j)
          hw1 = blk%hp1(i,j)
          hw2 = blk%hp2(i,j)
          hs1 = 0.50*(blk%hv1(i,j-1) + blk%hv1(i+1,j-1))
          hs2 = 0.50*(blk%hv2(i,j-1) + blk%hv2(i+1,j-1))
          hn1 = 0.50*(blk%hv1(i,j) + blk%hv1(i+1,j))
          hn2 = 0.50*(blk%hv2(i,j) + blk%hv2(i+1,j))

          ! WRITE(*,*) i, j, hs1, hs2, hn1, hn2

          v_p = 0.25*(blk%vvel(i,j) + blk%vvel(i+1,j) &
               + blk%vvel(i,j-1) + blk%vvel(i+1,j-1))

          ! Use harmonic averages for eddy viscosity

          k_e = blk%eddy(i,j+1)   ! replace with geometric weighted k's
          k_w = blk%eddy(i,j)
          k_p = harmonic(k_w, k_e)
          k_n = harmonic(k_p, harmonic(blk%eddy(i,j+1), blk%eddy(i+1,j+1)))
          k_s = harmonic(k_p, harmonic(blk%eddy(i,j-1), blk%eddy(i+1,j-1)))

          ! WRITE(*,*) i, j, k_e, k_w, k_p, k_n, k_s

          depth_e = blk%depth(i+1,j)
          zbot_e = blk%zbot(i+1,j)
          depth_w = blk%depth(i,j)
          zbot_w = blk%zbot(i,j)
          depth_p = 0.5*(depth_e + depth_w)
          depth_n = 0.25*(blk%depth(i,j)+blk%depth(i,j+1) &
               + blk%depth(i+1,j)+blk%depth(i+1,j+1))
          depth_s = 0.25*(blk%depth(i,j)+blk%depth(i,j-1) &
               + blk%depth(i+1,j)+blk%depth(i+1,j-1))

          IF(j == y_beg) depth_s = 0.5*(blk%depth(i,j-1)+blk%depth(i+1,j-1))
          IF(j == y_end) depth_n = 0.5*(blk%depth(i,j+1)+blk%depth(i+1,j+1))

          flux_e = he2*0.5*(blk%uvel(i,j)+ blk%uvel(i+1,j))*depth_e
          flux_w = hw2*0.5*(blk%uvel(i,j)+ blk%uvel(i-1,j))*depth_w
          flux_n = hn1*0.5*(blk%vvel(i,j)+ blk%vvel(i+1,j))*depth_n
          flux_s = hs1*0.5*(blk%vvel(i,j)+ blk%vvel(i-1,j))*depth_s
          diffu_e =  2.0*k_e*depth_e*he2/he1
          diffu_w =  2.0*k_w*depth_w*hw2/hw1
          diffu_n =  k_n*depth_n*hn1/hn2
          diffu_s =  k_s*depth_s*hs1/hs2
          ! pec_e = flux_e/diffu_e
          ! pec_w = flux_w/diffu_w
          ! pec_n = flux_n/diffu_n
          ! pec_s = flux_s/diffu_s
          ! ae(i,j) = diffu_e*afunc(pec_e) + max(-flux_e,0.0d0)
          ! aw(i,j) = diffu_w*afunc(pec_w) + max(flux_w,0.0d0)
          ! an(i,j) = diffu_n*afunc(pec_n) + max(-flux_n,0.0d0)
          ! as(i,j) = diffu_s*afunc(pec_s) + max(flux_s,0.0d0)
          CALL differ2(diff_uv, flux_w, diffu_w, flux_e, diffu_e, &
               &blk%uvel(i-2,j), blk%uvel(i-1,j), blk%uvel(i,j), blk%uvel(i+1,j), blk%uvel(i+2,j), &
               &aw(i,j), aw2, aww, ae(i,j), ae2, aee)
          CALL differ2(diff_uv, flux_s, diffu_s, flux_n, diffu_n, &
               &blk%uvel(i,j-2), blk%uvel(i,j-1), blk%uvel(i,j), blk%uvel(i,j+1), blk%uvel(i,j+2), &
               &as(i,j), as2, ass, an(i,j), an2, ann)

          apo = hp1*hp2*0.5*(blk%depthold(i,j)+blk%depthold(i+1,j))/delta_t

          source(i,j) = 0.0

          !** U source term wind stress ***
          wind_speed = sqrt(uvel_wind**2 + vvel_wind**2)
          wind_drag_coeff = (0.8 + 0.065*wind_speed)*0.001 ! Wu(1982)
          blk%windshear1(i,j) = density_air*wind_drag_coeff*uvel_wind*wind_speed
          source(i,j) = source(i,j) + hp1*hp2*blk%windshear1(i,j)/density

          ! compute the cross term from bousinesq eddy viscosity this term appears
          !	even in cartesian grids
          cross_term = (depth_n*k_n)*(blk%vvel(i+1,j) - blk%vvel(i,j)) &
               - (depth_s*k_s)*(blk%vvel(i+1,j-1) - blk%vvel(i,j-1))

          source(i,j) =  source(i,j) + cross_term

          ! compute all the stuff for the curvature terms in a cartesian grid all
          !	these terms should be zero because the gradients of the metric coeff
          !	will be zero
          ! compute derivatives of metric coeff
          h1_eta_p = 0.5*(blk%hu1(i,j+1) - blk%hu1(i,j-1))

          h2_xsi_p = blk%hp2(i+1,j) - blk%hp2(i,j) 

          h1_eta_e = (blk%hv1(i+1,j) - blk%hv1(i+1,j-1))
          h1_eta_w = (blk%hv1(i,j) - blk%hv1(i,j-1))

          h1_eta_n = blk%hu1(i,j+1) - blk%hu1(i,j)
          h1_eta_s = blk%hu1(i,j) - blk%hu1(i,j-1)
          h2_xsi_n = blk%hv2(i+1,j) - blk%hv2(i,j)
          h2_xsi_s = blk%hv2(i+1,j-1) - blk%hv2(i,j-1)

          u_p = blk%uvel(i,j)
          u_n = 0.5*(blk%uvel(i,j)+blk%uvel(i,j+1))
          u_s = 0.5*(blk%uvel(i,j)+blk%uvel(i,j-1))
          v_e = 0.5*(blk%vvel(i+1,j)+blk%vvel(i+1,j-1))
          v_w = 0.5*(blk%vvel(i,j)+blk%vvel(i,j-1))
          v_n = 0.5*(blk%vvel(i,j)+blk%vvel(i+1,j))
          v_s = 0.5*(blk%vvel(i,j-1)+blk%vvel(i+1,j-1))

          ! WRITE(*,*) i, j, u_p, u_s, u_n
          ! WRITE(*,*) i, j, v_w, v_e, v_s, v_n
          IF(j == y_end)THEN
             h2_xsi_p = 2.0*h2_xsi_p
             h1_eta_e = h1_eta_p
          END IF

          ! compute each part of the U curvature terms
          curve_1 = -depth_p * u_p * v_p * h1_eta_p
          curve_2 = depth_p * v_p * v_p* h2_xsi_p
          curve_3 = (2.0 * k_e * depth_e * v_e/he1) * h1_eta_e &
               - (2.0 * k_w * depth_w * v_w/hw1) * h1_eta_w
          curve_4 = -(depth_n * k_n * v_n/hn2) * h2_xsi_n &
               + (depth_s * k_s * v_s/hs2) * h2_xsi_s
          curve_5 = -(depth_n * k_n * u_n/hn2) * h1_eta_n &
               + (depth_s * k_s * u_s/hs2) * h1_eta_s
          curve_6 = depth_p*k_p*h1_eta_p*((v_e - v_w)/hp1 - (v_p/(hp1*hp2))*(h2_xsi_p) &
               + (u_n - u_s)/hp2 - (u_p/(hp1*hp2))*(h1_eta_p))
          curve_7 = -2.0*depth_p*k_p*h2_xsi_p*((v_n - v_s)/hp2 + (u_p/(hp1*hp2))*h2_xsi_p)

          source(i,j) =  source(i,j) + curve_1 + curve_2 + curve_3 &
               + curve_4 + curve_5 + curve_6 + curve_7
          ! end of U curvature terms ---------------------------------------------

          ap(i,j) = ae(i,j)+aw(i,j)+an(i,j)+as(i,j)
          ap2 = ae2 + aw2 + as2 + an2 + aww + aee + ass + ann


          source(i,j) = source(i,j) + blend_uv*(-ae(i,j) + ae2)*blk%uvel(i+1,j)
          source(i,j) = source(i,j) + blend_uv*(           aee)*blk%uvel(i+2,j)
          source(i,j) = source(i,j) + blend_uv*(-aw(i,j) + aw2)*blk%uvel(i-1,j)
          source(i,j) = source(i,j) + blend_uv*(           aww)*blk%uvel(i-2,j)
          source(i,j) = source(i,j) + blend_uv*(-an(i,j) + an2)*blk%uvel(i,j+1)
          source(i,j) = source(i,j) + blend_uv*(           ann)*blk%uvel(i,j+2)
          source(i,j) = source(i,j) + blend_uv*(-as(i,j) + as2)*blk%uvel(i,j-1)
          source(i,j) = source(i,j) + blend_uv*(           ass)*blk%uvel(i,j-2)
          source(i,j) = source(i,j) + blend_uv*( ap(i,j) - ap2)*blk%uvel(i,j)

          ! blended 3-time level time discretization

          ap(i,j) = ap(i,j) + apo
          source(i,j) = source(i,j) + apo*blk%uvelold(i,j) + apo*blend_time*( &
               &-0.5*blk%uvel(i,j) + blk%uvelold(i,j) - 0.5*blk%uveloldold(i,j))

          !** Bed Shear Stress Linearization **

          sc = 0.0
          sp = 0.0
          CALL linear_friction(blk%chezy(i,j), depth_p,&
               &blk%uvel(i,j), v_p, hp1*hp2, sc, sp)

          source(i,j) = source(i,j) + sc
          ap(i,j) = ap(i,j) + sp

          IF (blk%isdead(i,j)%u) THEN

             ! force zero velocity when specified

             source(i,j) = source(i,j) + bigfactor*0.0
             ap(i,j) = ap(i,j) + bigfactor
          ELSE 
             IF (i .EQ. x_beg) THEN
                ! upstream (west)
                SELECT CASE (blk%cell(i,j)%xtype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                      ap(i,j) = ap(i,j) - aw(i,j)
                      aw(i,j) = 0.0
                   CASE DEFAULT
                      source(i,j) = source(i,j) + aw(i,j)*blk%uvel(i-1,j)
                      aw(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   source(i,j) = source(i,j) + aw(i,j)*blk%uvel(i-1,j)
                   aw(i,j) = 0.0
                END SELECT
             ELSE IF (i .EQ. x_end) THEN
                ! downstream (east)
                SELECT CASE (blk%cell(i,j)%xtype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                      ap(i,j) = ap(i,j) - ae(i,j)
                      ae(i,j) = 0.0
                   CASE DEFAULT
                      source(i,j) = source(i,j) + bigfactor*blk%uvel(i+1,j)
                      ap(i,j) = ap(i,j) + bigfactor
                   END SELECT
                CASE DEFAULT
                   source(i,j) = source(i,j) + ae(i,j)*blk%uvel(i+1,j)
                   ae(i,j) = 0.0
                END SELECT
             END IF

             IF (j .EQ. y_beg) THEN 
                ! right bank (south)
                IF (blk%cell(i,j)%ytype .EQ. CELL_BOUNDARY_TYPE .OR.&
                     &blk%cell(i+1,j)%ytype .EQ. CELL_BOUNDARY_TYPE) THEN
                   slip = .TRUE.
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                   CASE DEFAULT
                      slip = slip .AND. (blk%vvel(i,j-1) .LE. 0.0)
                      slip = slip .AND. (blk%vvel(i+1,j-1) .LE. 0.0)
                   END SELECT
                   IF (slip) THEN
                      ap(i,j) = ap(i,j) - as(i,j)
                   ELSE 
                      ap(i,j) = ap(i,j) + as(i,j)
                      source(i,j) = source(i,j) + 2*as(i,j)*blk%uvel(i,j-1)
                   END IF
                   as(i,j) = 0.0
                ELSE 
                   source(i,j) = source(i,j) + as(i,j)*blk%uvel(i,j-1)
                   as(i,j) = 0.0
                END IF
             ELSE IF (j .EQ. y_end) THEN
                ! left bank (north)
                IF (blk%cell(i,j)%ytype .EQ. CELL_BOUNDARY_TYPE .OR.&
                     &blk%cell(i+1,j)%ytype .EQ. CELL_BOUNDARY_TYPE) THEN
                   slip = .TRUE.
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                   CASE DEFAULT
                      slip = slip .AND. (blk%vvel(i,j) .GE. 0.0)
                      slip = slip .AND. (blk%vvel(i+1,j) .GE. 0.0)
                   END SELECT
                   IF (slip) THEN
                      ap(i,j) = ap(i,j) - an(i,j)
                   ELSE 
                      ap(i,j) = ap(i,j) + an(i,j)
                      source(i,j) = source(i,j) + 2*an(i,j)*blk%uvel(i,j+1)
                   END IF
                   an(i,j) = 0.0
                ELSE
                   source(i,j) = source(i,j) + an(i,j)*blk%uvel(i,j+1)
                   an(i,j) = 0.0
                END IF
             END IF

             ! IF there is a wall blocking lateral flow, we need to
             ! disconnect from the u cell on the other side (with a
             ! slip condition)
             IF ((blk%isdead(i,j)%v .OR. &
                  &blk%isdead(i+1,j)%v) .AND. .NOT. &
                  &blk%isdead(i,j+1)%u) THEN 
                ap(i,j) = ap(i,j) - an(i,j)
                an(i,j) = 0.0
             END IF
             IF ((blk%isdead(i,j-1)%v .OR. &
                  &blk%isdead(i+1,j-1)%v) .AND. .NOT. &
                  &blk%isdead(i,j-1)%u) THEN
                ap(i,j) = ap(i,j) - as(i,j)
                as(i,j) = 0.0
             END IF

          END IF

          bp(i,j) = source(i,j) &
               - 0.5*grav*hp2*(depth_e**2 - depth_w**2) &
               - grav*hp2*depth_p*(blk%zbot(i+1,j) - blk%zbot(i,j))

          ! implicit underrelaxation

          bp(i,j) = bp(i,j) + (1 - relax_uv)/relax_uv*ap(i,j)*blk%uvel(i,j)
          ap(i,j) = ap(i,j)/relax_uv

          ! compute and store for use in pressure correction equation

          blk%lud(i,j) = 0.5*grav*hp2*(depth_e+depth_w)/ap(i,j)

          ! WRITE (*, *) i, j, apo, as(i,j), aw(i,j), ap(i, j), ae(i,j), an(i,j)
       END DO
    END DO

    junk =  solver(blkidx, SOLVE_U, imin, imax, jmin, jmax, scalar_sweep, &
         &ap(imin:imax, jmin:jmax), aw(imin:imax, jmin:jmax), &
         &ae(imin:imax, jmin:jmax), as(imin:imax, jmin:jmax), &
         &an(imin:imax, jmin:jmax), bp(imin:imax, jmin:jmax), &
         &blk%uvelstar(imin:imax,jmin:jmax))

    CALL block_var_put(blk%bv_uvel, BLK_VAR_STAR)
    CALL block_var_put(blk%bv_lud)
    CALL ga_sync()
    CALL block_var_get(blk%bv_uvel, BLK_VAR_STAR)
    CALL block_var_get(blk%bv_lud)

  END SUBROUTINE uvel_solve

  ! ----------------------------------------------------------------
  ! SUBROUTINE vvel_solve
  ! ----------------------------------------------------------------
  SUBROUTINE vvel_solve(blk, delta_t)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    DOUBLE PRECISION, INTENT(IN) :: delta_t

    INTEGER :: blkidx

    DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
    DOUBLE PRECISION :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
    DOUBLE PRECISION :: zbot_e, zbot_w, zbot_n, zbot_s, dwsdx
    DOUBLE PRECISION :: flux_e,flux_w,flux_n,flux_s					! fluxes
    DOUBLE PRECISION :: diffu_e,diffu_w,diffu_n,diffu_s			! diffusion
    DOUBLE PRECISION :: pec_e,pec_w,pec_n,pec_s	! peclet numbers
    DOUBLE PRECISION :: aw2, ae2, as2, an2, ap2   ! second order flux coefficients
    DOUBLE PRECISION :: aww, aee, ass, ann        ! second order flux coefficients
    DOUBLE PRECISION :: apo
    DOUBLE PRECISION :: u_p, u_e, u_w, u_s, u_n	! u velocities at P and on staggered grid
    DOUBLE PRECISION :: v_p, v_n, v_s, v_e, v_w	! v velocities at P and on staggered grid

    INTEGER :: k, x_beg, y_beg, num_bc, i, j, junk

    DOUBLE PRECISION :: sc, sp
    DOUBLE PRECISION :: h1_eta_p, h2_xsi_p						! derivatives of metric coeff
    DOUBLE PRECISION :: h1_eta_e, h1_eta_w, h1_eta_n, h1_eta_s	! e.g., h1_eta_p is the partial deriv
    DOUBLE PRECISION :: h2_xsi_e, h2_xsi_w, h2_xsi_n, h2_xsi_s	! of h1 in eta direction at point p
    DOUBLE PRECISION :: curve_1,curve_2,curve_3,curve_4,curve_5,curve_6,curve_7	! curvature terms
    DOUBLE PRECISION :: k_p,k_e,k_w,k_n,k_s 
    DOUBLE PRECISION :: cross_term				! eddy viscosity cross term in momement equations
    DOUBLE PRECISION, EXTERNAL :: afunc

    DOUBLE PRECISION :: &
         &ap(2:blk%xmax, 2:blk%ymax), &
         &ae(2:blk%xmax, 2:blk%ymax), &
         &aw(2:blk%xmax, 2:blk%ymax), &
         &an(2:blk%xmax, 2:blk%ymax), &
         &as(2:blk%xmax, 2:blk%ymax), &
         &bp(2:blk%xmax, 2:blk%ymax), &
         &source(2:blk%xmax, 2:blk%ymax)

    INTEGER :: x_end, y_end
    INTEGER :: imin, imax, jmin, jmax

    LOGICAL :: slip

    blkidx = blk%index

    x_beg = 2
    y_beg = 2
    x_end = blk%xmax
    y_end = blk%ymax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, x_beg)
    imax = MIN(imax, x_end)
    jmin = MAX(jmin, y_beg)
    jmax = MIN(jmax, y_end)

    DO i=imin, imax
       DO j=jmin, jmax
          hp1 = blk%hv1(i,j) 
          hp2 = blk%hv2(i,j)
          he1 = 0.50*(blk%hu1(i,j) + blk%hu1(i,j+1))
          he2 = 0.50*(blk%hu2(i,j) + blk%hu2(i,j+1))
          hw1 = 0.50*(blk%hu1(i-1,j) + blk%hu1(i-1,j+1))
          hw2 = 0.50*(blk%hu2(i-1,j) + blk%hu2(i-1,j+1))
          hs1 = blk%hp1(i,j)
          hs2 = blk%hp2(i,j)
          hn1 = blk%hp1(i,j+1)
          hn2 = blk%hp2(i,j+1)

          u_p = 0.25*(blk%uvel(i,j) + blk%uvel(i-1,j) &
               + blk%uvel(i,j+1) + blk%uvel(i-1,j+1))

          k_s = blk%eddy(i,j)
          k_n = blk%eddy(i,j+1)
          k_p = harmonic(k_s, k_n)
          k_e = harmonic(k_p, harmonic(blk%eddy(i+1,j), blk%eddy(i+1,j+1)))
          k_w = harmonic(k_p, harmonic(blk%eddy(i-1,j), blk%eddy(i-1,j+1)))

          depth_n = blk%depth(i,j+1)
          zbot_n = blk%zbot(i,j+1)
          depth_s = blk%depth(i,j)
          zbot_s = blk%zbot(i,j)
          depth_p = 0.5*(depth_n + depth_s)
          depth_e = 0.25*(blk%depth(i,j)+blk%depth(i,j+1) &
               + blk%depth(i+1,j)+blk%depth(i+1,j+1))
          depth_w = 0.25*(blk%depth(i,j)+blk%depth(i-1,j) &
               + blk%depth(i-1,j)+blk%depth(i-1,j+1))

          IF(i == x_beg) depth_w = 0.5*(blk%depth(i-1,j)+blk%depth(i-1,j+1))
          IF(i == x_end) depth_e = 0.5*(blk%depth(i+1,j)+blk%depth(i+1,j+1))

          flux_e = he2*0.5*(blk%uvel(i,j)+ blk%uvel(i,j+1))*depth_e
          flux_w = hw2*0.5*(blk%uvel(i-1,j)+ blk%uvel(i-1,j+1))*depth_w
          flux_n = hn1*0.5*(blk%vvel(i,j)+ blk%vvel(i,j+1))*depth_n
          flux_s = hs1*0.5*(blk%vvel(i,j)+ blk%vvel(i,j-1))*depth_s
          diffu_e =  2.0*k_e*depth_e*he2/he1
          diffu_w =  2.0*k_w*depth_w*hw2/hw1
          diffu_n =  k_n*depth_n*hn1/hn2
          diffu_s =  k_s*depth_s*hs1/hs2

          CALL differ2(diff_uv, flux_w, diffu_w, flux_e, diffu_e, &
               &blk%vvel(i-2,j), blk%vvel(i-1,j), blk%vvel(i,j), blk%vvel(i+1,j), blk%vvel(i+2,j), &
               &aw(i,j), aw2, aww, ae(i,j), ae2, aee)
          CALL differ2(diff_uv, flux_s, diffu_s, flux_n, diffu_n, &
               &blk%vvel(i,j-2), blk%vvel(i,j-1), blk%vvel(i,j), blk%vvel(i,j+1), blk%vvel(i,j+2), &
               &as(i,j), as2, ass, an(i,j), an2, ann)

          apo = hp1*hp2*0.5*(blk%depthold(i,j)+blk%depthold(i,j+1))/delta_t

          source(i,j) = 0.0

          !** note V source term  wind stress ***
          wind_speed = sqrt(uvel_wind**2 + vvel_wind**2)
          wind_drag_coeff = (0.8 + 0.065*wind_speed)*0.001 ! Wu(1982)
          blk%windshear2(i,j) = density_air*wind_drag_coeff*vvel_wind*wind_speed
          source(i,j) = source(i,j) + hp1*hp2*blk%windshear2(i,j)/density


          cross_term = (depth_e*k_e)*(blk%uvel(i,j+1) - blk%uvel(i,j)) &
               - (depth_w*k_w)*(blk%uvel(i-1,j+1) - blk%uvel(i-1,j))

          source(i,j) =  source(i,j) + cross_term

          ! compute all the stuff for the curvature terms; in a cartesian grid all
          !	these terms should be zero because the gradients of the metric coeff
          !	will be zero
          ! compute derivatives of metric coeff
          h1_eta_p = blk%hp1(i,j+1) - blk%hp1(i,j)
          h2_xsi_p = 0.5*(blk%hv2(i+1,j) - blk%hv2(i-1,j))

          h2_xsi_n = blk%hu2(i,j+1) - blk%hu2(i-1,j+1)
          h2_xsi_s = blk%hu2(i,j) - blk%hu2(i-1,j)
          h2_xsi_e = blk%hv2(i+1,j) - blk%hv2(i,j)
          h2_xsi_w = blk%hv2(i,j) - blk%hv2(i-1,j)
          h1_eta_e = blk%hu1(i,j+1) - blk%hu1(i,j)
          h1_eta_w = blk%hu1(i-1,j+1) - blk%hu1(i-1,j)

          v_p = blk%vvel(i,j)
          u_n = 0.5*(blk%uvel(i-1,j+1)+blk%uvel(i,j+1))
          u_s = 0.5*(blk%uvel(i,j)+blk%uvel(i-1,j))
          u_e = 0.5*(blk%uvel(i,j)+blk%uvel(i,j+1))
          u_w = 0.5*(blk%uvel(i-1,j)+blk%uvel(i-1,j+1))
          v_e = 0.5*(blk%vvel(i+1,j)+blk%vvel(i,j))
          v_w = 0.5*(blk%vvel(i,j)+blk%vvel(i-1,j))
          v_n = 0.5*(blk%vvel(i,j)+blk%vvel(i,j+1))
          v_s = 0.5*(blk%vvel(i,j)+blk%vvel(i,j-1))

          ! compute each part of the curvature terms
          curve_1 = -depth_p * u_p * v_p * h2_xsi_p
          curve_2 = depth_p * u_p * u_p* h1_eta_p
          curve_3 = (2.0 * k_n * depth_n * u_n/hn2) * h2_xsi_n &
               - (2.0 * k_s * depth_s * u_s/hs1) * h2_xsi_s
          curve_4 = -(depth_e * k_e * v_e/he1) * h2_xsi_e &
               + (depth_w * k_w * v_w/hw1) * h2_xsi_w
          curve_5 = -(depth_e * k_e * u_e/he1) * h1_eta_e &
               + (depth_w * k_w * u_w/hw1) * h1_eta_w
          curve_6 = depth_p*k_p*h2_xsi_p*((v_e - v_w)/hp1 - (v_p/(hp1*hp2))*(h2_xsi_p) &
               + (u_n - u_s)/hp2 - (u_p/(hp1*hp2))*(h1_eta_p))
          curve_7 = -2.0*depth_p*k_p*h1_eta_p*((u_e - u_w)/hp1 + (v_p/(hp1*hp2))*h1_eta_p)

          source(i,j) =  source(i,j) + curve_1 + curve_2 + curve_3 &
               + curve_4 + curve_5 + curve_6 + curve_7

          ! end of V curvature terms ---------------------------------------------

          ap(i,j) = ae(i,j)+aw(i,j)+an(i,j)+as(i,j)
          ap2 = ae2 + aw2 + as2 + an2 + aww + aee + ass + ann

          source(i,j) = source(i,j) + blend_uv*(-ae(i,j) + ae2)*blk%vvel(i+1,j)
          source(i,j) = source(i,j) + blend_uv*(           aee)*blk%vvel(i+2,j)
          source(i,j) = source(i,j) + blend_uv*(-aw(i,j) + aw2)*blk%vvel(i-1,j)
          source(i,j) = source(i,j) + blend_uv*(           aww)*blk%vvel(i-2,j)
          source(i,j) = source(i,j) + blend_uv*(-an(i,j) + an2)*blk%vvel(i,j+1)
          source(i,j) = source(i,j) + blend_uv*(           ann)*blk%vvel(i,j+2)
          source(i,j) = source(i,j) + blend_uv*(-as(i,j) + as2)*blk%vvel(i,j-1)
          source(i,j) = source(i,j) + blend_uv*(           ass)*blk%vvel(i,j-2)
          source(i,j) = source(i,j) + blend_uv*( ap(i,j) - ap2)*blk%vvel(i,j)

          ! blended 3-time level time discretization

          ap(i,j) = ap(i,j) + apo
          source(i,j) = source(i,j) + apo*blk%vvelold(i,j) + &
               &apo*blend_time*( -0.5*blk%vvel(i,j) + &
               &blk%vvelold(i,j) - 0.5*blk%vveloldold(i,j))

          !** Bed Shear Stress Linearization **

          sc = 0.0
          sp = 0.0
          CALL linear_friction(blk%chezy(i,j), depth_p, &
               &u_p, blk%vvel(i,j), hp1*hp2, sc, sp)

          ! IF (do_wetdry .AND. depth_p .LE. dry_rewet_depth*2.0) THEN
          !    dwsdx = ABS(((depth_n + zbot_n) - (depth_s + zbot_s))/hp2)
          !    CALL shallow_v_nudge(blk%chezy(i,j), depth_p,&
          !         &blk%vvel(i,j), u_p, dwsdx, sc, sp)
          ! END IF

          source(i,j) = source(i,j) + sc
          ap(i,j) = ap(i,j) + sp

          ! force zero velocity when specified

          IF (blk%isdead(i,j)%v) THEN
             source(i,j) = source(i,j) + bigfactor*0.0
             ap(i,j) = ap(i,j) + bigfactor
          ELSE 
             IF (i .EQ. x_beg) THEN
                ! upstream (west)
                IF (blk%cell(i,j)%xtype .EQ. CELL_BOUNDARY_TYPE .OR.&
                     &blk%cell(i,j+1)%xtype .EQ. CELL_BOUNDARY_TYPE) THEN
                   slip = .TRUE.
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                   CASE DEFAULT
                      slip = slip .AND. (blk%uvel(i,j) .LE. 0.0)
                      slip = slip .AND. (blk%uvel(i,j+1) .LE. 0.0)
                   END SELECT
                   IF (slip) THEN
                      ap(i,j) = ap(i,j) - aw(i,j)
                   ELSE
                      ap(i,j) = ap(i,j) + aw(i,j)
                      source(i,j) = source(i,j) + 2.0*aw(i,j)*blk%vvel(i-1,j)
                   END IF
                   aw(i,j) = 0.0
                ELSE 
                   source(i,j) = source(i,j) + aw(i,j)*blk%vvel(i-1,j)
                   aw(i,j) = 0.0
                END IF
             ELSE IF (i .EQ. x_end) THEN
                ! downstream (east)
                IF (blk%cell(i,j)%xtype .EQ. CELL_BOUNDARY_TYPE .OR.&
                     &blk%cell(i,j+1)%xtype .EQ. CELL_BOUNDARY_TYPE) THEN
                   slip = .TRUE.
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                   CASE DEFAULT
                      slip = slip .AND. (blk%uvel(i,j) .GE. 0.0)
                      slip = slip .AND. (blk%uvel(i,j+1) .GE. 0.0)
                   END SELECT
                   IF (slip) THEN
                      ap(i,j) = ap(i,j) - ae(i,j)
                   ELSE
                      ap(i,j) = ap(i,j) + ae(i,j)
                      source(i,j) = source(i,j) + 2.0*ae(i,j)*blk%vvel(i+1,j)
                   END IF
                   ae(i,j) = 0.0
                ELSE
                   source(i,j) = source(i,j) + ae(i,j)*blk%vvel(i+1,j)
                   ae(i,j) = 0.0
                END IF
             END IF

             IF (j .EQ. y_beg) THEN
                SELECT CASE (blk%cell(i,j)%ytype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV)
                      ap(i,j) = ap(i,j) - as(i,j)
                      as(i,j) = 0.0
                   CASE DEFAULT
                      source(i,j) = source(i,j) + as(i,j)*blk%vvel(i,j-1)
                      as(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   source(i,j) = source(i,j) + as(i,j)*blk%vvel(i,j-1)
                   as(i,j) = 0.0
                END SELECT
             ELSE IF (j .EQ. y_end) THEN
                SELECT CASE (blk%cell(i,j)%ytype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV, FLOWBC_ZEROG)
                      ap(i,j) = ap(i,j) - an(i,j)
                      an(i,j) = 0.0
                   CASE DEFAULT
                      source(i,j) = source(i,j) + bigfactor*blk%vvel(i,j+1)
                      ap(i,j) = ap(i,j) + bigfactor
                   END SELECT
                CASE DEFAULT
                   source(i,j) = source(i,j) + an(i,j)*blk%vvel(i,j+1)
                   an(i,j) = 0.0
                END SELECT
             END IF

             ! IF there is a wall blocking longitudinal flow, we need
             ! to disconnect from the v cell on the other side (with
             ! a slip condition)
             IF ((blk%isdead(i-1,j)%u .OR. &
                  &blk%isdead(i-1,j+1)%u) .AND. .NOT. &
                  &blk%isdead(i-1,j)%v) THEN
                ap(i,j) = ap(i,j) - aw(i,j)
                aw(i,j) = 0.0
             END IF
             IF ((blk%isdead(i,j)%u .OR. &
                  &blk%isdead(i,j+1)%u) .AND. .NOT. &
                  &blk%isdead(i+1,j)%v) THEN
                ap(i,j) = ap(i,j) - ae(i,j)
                ae(i,j) = 0.0
             END IF

          END IF

          bp(i,j) = source(i,j) &
               - 0.5*grav*hp1*(depth_n**2 - depth_s**2) &
               - grav*hp1*depth_p*(blk%zbot(i,j+1) - blk%zbot(i,j))

          ! implicit underrelaxation

          bp(i,j) = bp(i,j) + (1 - relax_uv)/relax_uv*ap(i,j)*blk%vvel(i,j)
          ap(i,j) = ap(i,j)/relax_uv

          ! compute and store for use in pressure correction equation

          blk%lvd(i,j) = 0.5*grav*hp1*(depth_n+depth_s)/ap(i,j)
       END DO
    END DO

    ! apply zero flow conditions on sides

! FIXME
    junk = solver(blkidx, SOLVE_V, imin, imax, jmin, jmax, scalar_sweep, &
         &ap(imin:imax, jmin:jmax), aw(imin:imax, jmin:jmax), &
         &ae(imin:imax, jmin:jmax), as(imin:imax, jmin:jmax), &
         &an(imin:imax, jmin:jmax), bp(imin:imax, jmin:jmax), &
         &blk%vvelstar(imin:imax,jmin:jmax))

    CALL block_var_put(blk%bv_vvel, BLK_VAR_STAR)
    CALL block_var_put(blk%bv_lvd)
    CALL ga_sync()
    CALL block_var_get(blk%bv_vvel, BLK_VAR_STAR)
    CALL block_var_get(blk%bv_lvd)
    

  END SUBROUTINE vvel_solve


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_friction 
  !
  ! Linearization of the friction term.  ustar is the (previous)
  ! velocity in the direction we are interested in; vstar is the
  ! (previous) velocity in the other direction
  !
  ! WARNING: values of sc and sp are intentionally incremented.
  ! ----------------------------------------------------------------
  SUBROUTINE linear_friction(c, depth, ustar, vstar, harea, sc, sp)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: c, depth, ustar, vstar, harea
    DOUBLE PRECISION, INTENT(INOUT) :: sc, sp

    DOUBLE PRECISION :: roughness, vmagstar, cterm, pterm

    ! Also uses: grav, density

    IF(manning)THEN
       IF (do_wetdry) THEN
          roughness = (grav*c**2)/(mann_con*MAX(depth, dry_depth)**0.3333333)
       ELSE
          roughness = (grav*c**2)/(mann_con*depth**0.3333333)
       END IF
    ELSE
       roughness = c
    END IF
    vmagstar = sqrt(ustar*ustar + vstar*vstar)

    ! Alternative: linearize by making the source term all constant:

    ! cterm = - harea*roughness*ustar*vmagstar
    ! pterm = 0.0

    ! Alternative: linearize by using current ustar and previous
    ! estimate of velocity magnitude

    cterm = 0.0
    pterm = - harea*roughness*vmagstar

    ! Alternative: Taylor Series expansion -- only good for vmag > 0.0,

    ! IF (vmagstar > 1.0d-20) THEN
    !    cterm = harea*roughness*(ustar**3.0)/vmagstar
    !    pterm = - harea*roughness*(ustar*ustar/vmagstar + vmagstar)
    ! ELSE 
    !    cterm = 0.0
    !    pterm = harea*roughness*vmagstar
    ! END IF

    sc = sc + cterm
    sp = sp - pterm

  END SUBROUTINE linear_friction

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION afunc
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION afunc(peclet_num)

    IMPLICIT NONE

    DOUBLE PRECISION :: peclet_num

    peclet_num = abs(peclet_num)

    afunc = max(0.0d0,(1.0-0.1*peclet_num)**5)  !power-law
    !afunc = 1.0                              !upwind-difference

  END FUNCTION afunc

  ! ----------------------------------------------------------------
  ! SUBROUTINE shallow_v_nudge
  ! ----------------------------------------------------------------
  SUBROUTINE shallow_v_nudge(c, depth, ustar, vstar, dwsdx, sc, sp)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: c, depth, ustar, vstar, dwsdx
    DOUBLE PRECISION, INTENT(INOUT) :: sc, sp

    DOUBLE PRECISION :: vmagstar, sf, utilde, cterm, pterm, sstar

    DOUBLE PRECISION, PARAMETER :: mfactor = 2.0

    sstar = sp*ustar + sc
    vmagstar = sqrt(ustar*ustar + vstar*vstar)

    IF (manning) THEN
       sf = c**2.0/mann_con*(ustar*vmagstar/(depth**(4/3)))
    ELSE
       sf = (ustar*vmagstar)/depth/c**2.0
    END IF

    sf = ABS(sf)

    IF (sf .GE. mfactor*ABS(dwsdx) .AND. sf .GT. 0.0) THEN
       utilde = ustar/vmagstar*SQRT(ABS(dwsdx))
       ! utilde = ustar/ABS(ustar)*SQRT(ABS(dwsdx))
       IF (manning) THEN
          utilde = utilde * c*depth**(2/3)/SQRT(mann_con)
       ELSE
          utilde = utilde * c*SQRT(depth)
       END IF

       IF (ABS(utilde - ustar) .GT. 0.0) THEN 
          cterm = (sstar*utilde)/(utilde - ustar)
          pterm = sstar/(utilde - ustar)

          sc = cterm
          sp = -pterm
       END IF
    END IF

  END SUBROUTINE shallow_v_nudge


  ! ----------------------------------------------------------------
  ! SUBROUTINE depth_solve
  ! ----------------------------------------------------------------
  SUBROUTINE depth_solve(blk, delta_t)

    IMPLICIT NONE

    TYPE(block_struct), INTENT(INOUT) :: blk
    DOUBLE PRECISION, INTENT(IN) :: delta_t
    INTEGER :: x_beg, y_beg, x_end, y_end, i, j, junk

    INTEGER :: blkidx

    DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
    DOUBLE PRECISION :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
    DOUBLE PRECISION :: flux_e,flux_w,flux_n,flux_s					! fluxes
    DOUBLE PRECISION :: cpo								! coefficients in discretization eqns
    DOUBLE PRECISION :: djunk

    INTEGER :: imin, imax, jmin, jmax

    DOUBLE PRECISION :: &
         &cp(2:blk%xmax, 2:blk%ymax), &
         &ce(2:blk%xmax, 2:blk%ymax), &
         &cw(2:blk%xmax, 2:blk%ymax), &
         &cn(2:blk%xmax, 2:blk%ymax), &
         &cs(2:blk%xmax, 2:blk%ymax), &
         &bp(2:blk%xmax, 2:blk%ymax)

    blkidx = blk%index
    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, x_beg)
    jmin = MAX(jmin, y_beg)
    imax = MIN(imax, x_end)
    jmax = MIN(jmax, y_end)

    DO i=imin, imax
       DO j=jmin, jmax
          hp1 = blk%hp1(i,j)
          hp2 = blk%hp2(i,j)
          he1 = blk%hu1(i,j)
          he2 = blk%hu2(i,j)
          hw1 = blk%hu1(i-1,j)
          hw2 = blk%hu2(i-1,j)
          hs1 = blk%hv1(i,j-1)
          hs2 = blk%hv2(i,j-1)
          hn1 = blk%hv1(i,j)
          hn2 = blk%hv2(i,j)

          depth_e = 0.5*(blk%depth(i,j)+blk%depth(i+1,j))
          depth_w = 0.5*(blk%depth(i,j)+blk%depth(i-1,j))
          depth_n = 0.5*(blk%depth(i,j)+blk%depth(i,j+1))
          depth_s = 0.5*(blk%depth(i,j)+blk%depth(i,j-1))

          ! IF(i == 2)			depth_w = blk%depth(i-1,j)
          ! IF(i == x_end)	depth_e = blk%depth(i+1,j)
          ! IF(j == 2)			depth_s = blk%depth(i,j-1)
          ! IF(j == y_end)	depth_n = blk%depth(i,j+1)

          flux_e = he2*blk%uvelstar(i,j)*depth_e
          flux_w = hw2*blk%uvelstar(i-1,j)*depth_w
          flux_n = hn1*blk%vvelstar(i,j)*depth_n
          flux_s = hs1*blk%vvelstar(i,j-1)*depth_s

          cpo = hp1*hp2/delta_t

          ! blended 3-time level time discretization

          djunk = &
               &cpo*(blk%depthold(i,j) - blk%depth(i,j)) + &
               &cpo*blend_time*(-0.5*blk%depth(i,j) + blk%depthold(i,j) - &
               &0.5*blk%deptholdold(i,j)) + &
               &flux_w - flux_e + flux_s - flux_n + &
               &blk%xsource(i,j)*hp1*hp2
          blk%mass_source(i,j) = djunk


          ce(i,j) = he2*depth_e*blk%lud(i,j)
          cw(i,j) = hw2*depth_w*blk%lud(i-1,j)
          cn(i,j) = hn1*depth_n*blk%lvd(i,j)
          cs(i,j) = hs1*depth_s*blk%lvd(i,j-1)
          cp(i,j) = ce(i,j) + cw(i,j) + cn(i,j) + cs(i,j) &
               + cpo

          ! bp(i,j) = blk%mass_source(i,j) + &
          !     &blk%xsource(i,j)*hp1*hp2
          ! blk%mass_source(i,j) = blk%mass_source(i,j) + &
          !      &blk%xsource(i,j)*hp1*hp2
          bp(i,j) = blk%mass_source(i,j)

          IF (blk%isdead(i,j)%p) THEN
             bp(i,j) = bp(i,j) + bigfactor*0.0
             cp(i,j) = cp(i,j) + bigfactor
          ELSE 
             IF (i .EQ. x_beg) THEN

                SELECT CASE (blk%cell(i,j)%xtype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_BOTH)
                      cp(i,j) = cp(i,j) + cw(i,j)
                      cw(i,j) = 0.0
                   CASE DEFAULT
                      cp(i,j) = cp(i,j) - cw(i,j)
                      cw(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   bp(i,j) = bp(i,j) + cw(i,j)*blk%dp(i-1,j)
                   cw(i,j) = 0.0
                END SELECT

             ELSE IF (i .EQ. x_end) THEN

                SELECT CASE (blk%cell(i,j)%xtype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%xbctype)
                   CASE (FLOWBC_ELEV, FLOWBC_BOTH)
                      cp(i,j) = cp(i,j) + ce(i,j)
                      ce(i,j) = 0.0
                   CASE DEFAULT
                      cp(i,j) = cp(i,j) - ce(i,j)
                      ce(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   bp(i,j) = bp(i,j) + ce(i,j)*blk%dp(i+1,j)
                   ce(i,j) = 0.0
                END SELECT
             END IF

             IF (j .EQ. y_beg) THEN
                SELECT CASE (blk%cell(i,j)%ytype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV, FLOWBC_BOTH)
                      cp(i,j) = cp(i,j) + cs(i,j)
                      cs(i,j) = 0.0
                   CASE DEFAULT
                      cp(i,j) = cp(i,j) - cs(i,j)
                      cs(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   bp(i,j) = bp(i,j) + cs(i,j)*blk%dp(i,j-1)
                   cs(i,j) = 0.0
                END SELECT
             ELSE IF (j .EQ. y_end) THEN
                SELECT CASE (blk%cell(i,j)%ytype)
                CASE (CELL_BOUNDARY_TYPE)
                   SELECT CASE (blk%cell(i,j)%ybctype)
                   CASE (FLOWBC_ELEV, FLOWBC_BOTH)
                      cp(i,j) = cp(i,j) + cn(i,j)
                      cn(i,j) = 0.0
                   CASE DEFAULT
                      cp(i,j) = cp(i,j) - cn(i,j)
                      cn(i,j) = 0.0
                   END SELECT
                CASE DEFAULT
                   bp(i,j) = bp(i,j) + cn(i,j)*blk%dp(i,j+1)
                   cn(i,j) = 0.0
                END SELECT
             END IF

          END IF



          ! IF there is a wall blocking longitudinal flow, we need
          ! to disconnect from the d cell on the other side
          IF ((blk%isdead(i-1,j)%u) .AND. .NOT. &
               &blk%isdead(i-1,j)%p) THEN
             cp(i,j) = cp(i,j) - cw(i,j)
             cw(i,j) = 0.0
          END IF
          IF ((blk%isdead(i,j)%u) .AND. .NOT. &
               &blk%isdead(i+1,j)%p) THEN
             cp(i,j) = cp(i,j) - ce(i,j)
             ce(i,j) = 0.0
          END IF

          ! IF there is a wall blocking lateral flow, we need to
          ! disconnect from the cell on the other side 
          IF ((blk%isdead(i,j)%v) .AND. .NOT. &
               &blk%isdead(i,j+1)%p) THEN 
             cp(i,j) = cp(i,j) - cn(i,j)
             cn(i,j) = 0.0
          END IF
          IF ((blk%isdead(i,j-1)%v) .AND. .NOT. &
               &blk%isdead(i,j-1)%p) THEN
             cp(i,j) = cp(i,j) - cs(i,j)
             cs(i,j) = 0.0
          END IF
       END DO
    END DO

    blk%dp(imin:imax,jmin:jmax) = 0.0
! FIXME
    junk = solver(blkidx, SOLVE_DP, imin, imax, jmin, jmax, depth_sweep, &
         &cp(imin:imax,jmin:jmax), cw(imin:imax,jmin:jmax), &
         &ce(imin:imax,jmin:jmax), cs(imin:imax,jmin:jmax), &
         &cn(imin:imax,jmin:jmax), &
         &bp(imin:imax,jmin:jmax), &
         &blk%dp(imin:imax,jmin:jmax))

    ! compute updated depth with some underrelaxation
    ! depth = rel*depth_new + (1 - rel)*depth_old
    IF(update_depth)THEN
       DO i=imin,imax
          DO j=jmin,jmax
             blk%depth(i,j) = blk%depth(i,j) + relax_dp*blk%dp(i,j)
             blk%wsel(i,j) = blk%depth(i,j) + blk%zbot(i,j)
          END DO
       END DO
    ENDIF

    CALL block_var_put(blk%bv_dp)
    CALL block_var_put(blk%bv_depth)
    CALL block_var_put(blk%bv_wsel)
    CALL ga_sync()
    CALL block_var_get(blk%bv_dp)
    CALL block_var_get(blk%bv_depth)
    CALL block_var_get(blk%bv_wsel)

  END SUBROUTINE depth_solve

  ! ----------------------------------------------------------------
  ! SUBROUTINE correct_velocity
  ! ----------------------------------------------------------------
  SUBROUTINE correct_velocity(blk)

    IMPLICIT NONE
    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER :: x_beg, y_beg, x_end, y_end, i, j
    DOUBLE PRECISION :: correction

    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    ! correct u velocity

    DO i=x_beg,x_end
       DO j=y_beg,y_end
          IF (.NOT. block_owns(blk, i, j)) CYCLE
          correction = blk%lud(i,j)*(blk%dp(i,j)-blk%dp(i+1,j))
          IF (i .EQ. x_end) THEN
             SELECT CASE (blk%cell(i,j)%xtype)
             CASE (CELL_BOUNDARY_TYPE)

                ! if a flow or velocity was specified downstream, do not
                ! correct it.

                SELECT CASE (blk%cell(i,j)%xbctype)
                CASE (FLOWBC_FLOW, FLOWBC_VEL)
                   correction = 0.0
                END SELECT
             END SELECT
          END IF
          blk%uvel(i,j) = blk%uvelstar(i,j) + correction
          blk%uvelstar(i,j) = blk%uvel(i,j)
       END DO
    END DO

    DO i=x_beg,x_end
       DO j=y_beg,y_end
          IF (.NOT. block_owns(blk, i, j)) CYCLE
          correction =  blk%lvd(i,j)*(blk%dp(i,j)-blk%dp(i,j+1))
          IF (j .EQ. y_end) THEN
             SELECT CASE (blk%cell(i,j)%ytype)
             CASE (CELL_BOUNDARY_TYPE)
                SELECT CASE (blk%cell(i,j)%ybctype)
                CASE (FLOWBC_FLOW, FLOWBC_VEL)
                   correction = 0.0
                END SELECT
             END SELECT
          END IF
          blk%vvel(i,j) = blk%vvelstar(i,j) + correction
          blk%vvelstar(i,j) = blk%vvel(i,j)
       END DO
    END DO

    CALL block_var_put(blk%bv_uvel)
    CALL block_var_put(blk%bv_uvel, BLK_VAR_STAR)
    CALL block_var_put(blk%bv_vvel)
    CALL block_var_put(blk%bv_vvel, BLK_VAR_STAR)
    CALL ga_sync()
    CALL block_var_get(blk%bv_uvel)
    CALL block_var_get(blk%bv_uvel, BLK_VAR_STAR)
    CALL block_var_get(blk%bv_vvel)
    CALL block_var_get(blk%bv_vvel, BLK_VAR_STAR)

  END SUBROUTINE correct_velocity


END MODULE hydro_solve

