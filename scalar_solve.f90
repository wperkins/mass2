! ----------------------------------------------------------------
! file: scalar_solve.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August 19, 2003 by William A. Perkins
! Last Change: 2014-06-03 09:37:34 d3g096
! ----------------------------------------------------------------
! $Id$

! ----------------------------------------------------------------
! MODULE scalar_solve_module
! ----------------------------------------------------------------
MODULE scalar_solve_module

  USE globals
  USE block_hydro_bc
  USE scalars
  USE scalars_source
  USE scalar_bc_module
  USE transport_only

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_precalc
  ! This routine precalculates various hydrodynamic values needed
  ! ----------------------------------------------------------------
  SUBROUTINE transport_precalc(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
    DOUBLE PRECISION :: xdiffp, ydiffp
    INTEGER :: i, j, x_end, y_end
    INTEGER :: imin, imax, jmin, jmax

    x_end = blk%xmax
    y_end = blk%ymax

    i = 1
    ! blk%inlet_area(2:y_end) = blk%depth(i,2:y_end)*blk%hu2(i,2:y_end)

    blk%k_e = 0.0
    blk%k_w = 0.0
    blk%k_n = 0.0
    blk%k_s = 0.0

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, 2)
    imax = MIN(imax, x_end)
    jmin = MAX(jmin, 2)
    jmax = MIN(jmax, y_end)

    DO i= imin, imax
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

          ! do not allow diffusion in dead cells 

          IF (blk%isdead(i,j)%p) THEN
             xdiffp = 0.0
             ydiffp = 0.0
          ELSE 
             xdiffp = blk%kx_diff(i,j)
             ydiffp = blk%ky_diff(i,j)
          END IF

          ! use the harmonic, rather than
          ! arithmatic, mean of diffusivity

          IF (xdiffp .GT. 0.0) THEN
             IF (.NOT. blk%isdead(i,j)%u) THEN
                blk%k_e(i,j) = &
                     &2.0*xdiffp*blk%kx_diff(i+1,j)/&
                     &(xdiffp + blk%kx_diff(i+1,j))
             END IF
             IF (.NOT. blk%isdead(i-1,j)%u) THEN
                blk%k_w(i,j) = &
                     &2.0*xdiffp*blk%kx_diff(i-1,j)/&
                     &(xdiffp + blk%kx_diff(i-1,j))
             END IF
          END IF

          IF (ydiffp .GT. 0.0) THEN
             IF (.NOT. blk%isdead(i,j)%v) THEN
                blk%k_n(i,j) = &
                     &2.0*ydiffp*blk%ky_diff(i,j+1)/&
                     &(ydiffp + blk%ky_diff(i,j+1))
             END IF
             IF (.NOT. blk%isdead(i,j-1)%v) THEN
                blk%k_s(i,j) = &
                     &2.0*ydiffp*blk%ky_diff(i,j-1)/&
                     &(ydiffp + blk%ky_diff(i,j-1))
             END IF
          END IF

          ! blk%k_e(i,j) = 0.5*(blk%kx_diff(i,j)+blk%kx_diff(i+1,j))
          ! blk%k_w(i,j) = 0.5*(blk%kx_diff(i,j)+blk%kx_diff(i-1,j))
          ! blk%k_n(i,j) = 0.5*(blk%ky_diff(i,j)+blk%ky_diff(i,j+1))
          ! blk%k_s(i,j) = 0.5*(blk%ky_diff(i,j)+blk%ky_diff(i,j-1))

          blk%depth_e(i,j) = 0.5*(blk%depth(i,j)+blk%depth(i+1,j))
          blk%depth_w(i,j) = 0.5*(blk%depth(i,j)+blk%depth(i-1,j))
          blk%depth_n(i,j) = 0.5*(blk%depth(i,j)+blk%depth(i,j+1))
          blk%depth_s(i,j) = 0.5*(blk%depth(i,j)+blk%depth(i,j-1))

          ! IF(i == 2) blk%depth_w(i,j) = blk%depth(i-1,j)
          ! IF(i == x_end) blk%depth_e(i,j) = blk%depth(i+1,j)
          IF(j == 2) blk%depth_s(i,j) = blk%depth(i,j-1)
          IF(j == y_end) blk%depth_n(i,j) = blk%depth(i,j+1)

          blk%flux_e(i,j) = he2*blk%uvel(i,j)*blk%depth_e(i,j)
          blk%flux_w(i,j) = hw2*blk%uvel(i-1,j)*blk%depth_w(i,j)
          blk%flux_n(i,j) = hn1*blk%vvel(i,j)*blk%depth_n(i,j)
          blk%flux_s(i,j) = hs1*blk%vvel(i,j-1)*blk%depth_s(i,j)
          blk%diffu_e(i,j) = blk%k_e(i,j)*blk%depth_e(i,j)*he2/he1
          blk%diffu_w(i,j) = blk%k_w(i,j)*blk%depth_w(i,j)*hw2/hw1
          blk%diffu_n(i,j) = blk%k_n(i,j)*blk%depth_n(i,j)*hn1/hn2
          blk%diffu_s(i,j) = blk%k_s(i,j)*blk%depth_s(i,j)*hs1/hs2

          blk%apo(i, j) = blk%hp1(i,j)*blk%hp2(i,j)*blk%depthold(i,j)/delta_t
       END DO
    END DO

  END SUBROUTINE transport_precalc


  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_solve
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_solve(blkidx, blk, scalar, relax, scheme, blend, &
       &xstart, ystart)

    USE differencing
    USE solver_module

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (scalar_struct), INTENT(INOUT) :: scalar
    INTEGER, INTENT(IN) :: blkidx, xstart, ystart
    INTEGER, INTENT(IN) :: scheme
    DOUBLE PRECISION, INTENT(IN) :: relax, blend

    INTEGER :: xend, yend

    DOUBLE PRECISION :: &
         & ap(xstart:blk%xmax, ystart:blk%ymax),&
         & ae(xstart:blk%xmax, ystart:blk%ymax), &
         & aw(xstart:blk%xmax, ystart:blk%ymax), &
         & an(xstart:blk%xmax, ystart:blk%ymax), &
         & as(xstart:blk%xmax, ystart:blk%ymax), &
         & bp(xstart:blk%xmax, ystart:blk%ymax)
    DOUBLE PRECISION :: ap2, ae2, aw2, an2, as2, aee, aww, ann, ass

    INTEGER :: i, j, junk
    INTEGER :: imin, imax, jmin, jmax

    xend = blk%xmax
    yend = blk%ymax

    bp = 0.0

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, xstart)
    imax = MIN(imax, xend)
    jmin = MAX(jmin, ystart)
    jmax = MIN(jmax, yend)

    DO i= imin, imax
       DO j=jmin, jmax

          CALL differ2(scheme, blk%flux_w(i,j), blk%diffu_w(i,j), blk%flux_e(i,j), blk%diffu_e(i,j), &
               &scalar%conc(i-2,j), scalar%conc(i-1,j), scalar%conc(i,j), scalar%conc(i+1,j), scalar%conc(i+2,j),&
               &aw(i,j), aw2, aww, ae(i,j), ae2, aee)
          CALL differ2(scheme, blk%flux_s(i,j), blk%diffu_s(i,j), blk%flux_n(i,j), blk%diffu_n(i,j), &
               &scalar%conc(i,j-2), scalar%conc(i,j-1), scalar%conc(i,j), scalar%conc(i,j+1), scalar%conc(i,j+2),&
               &as(i,j), as2, ass, an(i,j), an2, ann)

          ap(i,j) = ae(i,j) + aw(i,j) + an(i,j) + as(i,j)
          ap2 = ae2 + aw2 + an2 + as2 + aee + aww + ann + ass

          bp(i,j) = bp(i,j) + blend*(-ae(i,j) + ae2)*scalar%conc(i+1,j)
          bp(i,j) = bp(i,j) + blend*(           aee)*scalar%conc(i+2,j)
          bp(i,j) = bp(i,j) + blend*(-aw(i,j) + aw2)*scalar%conc(i-1,j)
          bp(i,j) = bp(i,j) + blend*(           aww)*scalar%conc(i-2,j)
          bp(i,j) = bp(i,j) + blend*(-an(i,j) + an2)*scalar%conc(i,j+1)
          bp(i,j) = bp(i,j) + blend*(           ann)*scalar%conc(i,j+2)
          bp(i,j) = bp(i,j) + blend*(-as(i,j) + as2)*scalar%conc(i,j-1)
          bp(i,j) = bp(i,j) + blend*(           ass)*scalar%conc(i,j-2)
          bp(i,j) = bp(i,j) + blend*( ap(i,j) - ap2)*scalar%conc(i,j)

          ! blended 3-time level time discretization

          ap(i, j) = ap(i, j) + blk%apo(i, j)

          bp(i, j) = bp(i, j) + blk%apo(i,j)*scalar%concold(i,j) + blend_time*( &
               &   -0.5*blk%apo(i,j)*scalar%conc(i,j) + &
               &        blk%apo(i,j)*scalar%concold(i,j) - &
               &    0.5*blk%apo(i,j)*scalar%concoldold(i,j) &
               &  )

          bp(i,j) = bp(i,j) + scalar%srcterm(i,j) 



          SELECT CASE (scalar%cell(i,j)%xtype)
          CASE (SCALAR_BOUNDARY_TYPE)
             SELECT CASE (scalar%cell(i,j)%xbctype)
             CASE (SCALBC_CONC)
                IF (i .EQ. xstart) THEN
                   bp(i,j) = bp(i,j) + 2.0*aw(i,j)* scalar%conc(i-1,j)
                   ap(i,j) = ap(i,j) + aw(i,j)
                   aw(i,j) = 0.0
                END IF
             CASE DEFAULT
                IF (i .EQ. xstart) THEN
                   ap(i,j) = ap(i,j) - aw(i,j)
                   aw(i,j) = 0.0
                ELSE IF (i .EQ. xend) THEN
                   ap(i,j) = ap(i,j) - ae(i,j)
                   ae(i,j) = 0.0
                END IF
             END SELECT
          CASE DEFAULT
             IF (i .EQ. xstart) THEN
                bp(i,j) = bp(i,j) + aw(i,j)*scalar%conc(i-1,j)
                aw(i,j) = 0.0
             ELSE IF (i .EQ. xend) THEN
                bp(i,j) = bp(i,j) + ae(i,j)*scalar%conc(i+1,j)
                ae(i,j) = 0.0
             END IF
          END SELECT

          SELECT CASE (scalar%cell(i,j)%ytype)
          CASE (SCALAR_BOUNDARY_TYPE)
             SELECT CASE (scalar%cell(i,j)%ybctype)
             CASE (SCALBC_CONC)
                IF (j .EQ. ystart) THEN
                   bp(i,j) = bp(i,j) + 2.0*as(i,j)*scalar%conc(i,j-1)
                   ap(i,j) = ap(i,j) + as(i,j)
                   as(i,j) = 0.0
                END IF
             CASE DEFAULT
                IF (j .EQ. ystart) THEN
                   ap(i,j) = ap(i,j) - as(i,j)
                   as(i,j) = 0.0
                ELSE IF (j .EQ. yend) THEN
                   ap(i,j) = ap(i,j) - an(i,j)
                   an(i,j) = 0.0
                END IF
             END SELECT
          CASE DEFAULT
             IF (j .EQ. ystart) THEN
                bp(i,j) = bp(i,j) + as(i,j)*scalar%conc(i,j-1)
                as(i,j) = 0.0
             ELSE IF (j .EQ. yend) THEN
                bp(i,j) = bp(i,j) + an(i,j)*scalar%conc(i,j+1)
                an(i,j) = 0.0
             END IF
          END SELECT

          ! implicit underrelaxation

          bp(i, j) = bp(i, j) + (1 - relax)/relax*ap(i,j)*scalar%conc(i,j)
          ap(i, j) = ap(i, j)/relax 

       END DO
    END DO

    junk = solver(blkidx, SOLVE_SCALAR, imin, imax, jmin, jmax, scalar_sweep, &
         &ap(imin:imax,jmin:jmax), aw(imin:imax,jmin:jmax), &
         &ae(imin:imax,jmin:jmax), as(imin:imax,jmin:jmax), &
         &an(imin:imax,jmin:jmax), bp(imin:imax,jmin:jmax), &
         &scalar%conc(imin:imax,jmin:jmax))

    CALL block_var_put(scalar%concvar, BLK_VAR_CURRENT)
    CALL block_var_sync()
    CALL block_var_get(scalar%concvar, BLK_VAR_CURRENT)

  END SUBROUTINE scalar_solve


  ! ----------------------------------------------------------------
  ! SUBROUTINE default_scalar_bc
  ! ----------------------------------------------------------------
  SUBROUTINE default_scalar_bc(blk, sclr)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (scalar_struct), INTENT(INOUT) :: sclr

    INTEGER :: x_beg, y_beg, x_end, y_end, i, j, ig
    INTEGER :: imin, imax, jmin, jmax

    x_beg = 2
    y_beg = 2
    x_end = blk%xmax
    y_end = blk%ymax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)
    imin = MAX(imin, x_beg)
    imax = MIN(imax, x_end)
    jmin = MAX(jmin, y_beg)
    jmax = MIN(jmax, y_end)

    ! fill ghost cells with edge values (zero gradient at boundary)

    DO ig = 1,nghost
       IF (block_owns_i(blk, x_beg)) &
            &sclr%conc(x_beg - ig, jmin:jmax) = sclr%conc(x_beg,jmin:jmax)
       IF (block_owns_i(blk, x_end)) &
            &sclr%conc(x_end + ig, jmin:jmax) = sclr%conc(x_end,jmin:jmax)
       IF (block_owns_j(blk, y_beg)) &
            &sclr%conc(imin-1:imax+1, y_beg - ig) = sclr%conc(imin-1:imax+1,y_beg)
       IF (block_owns_j(blk, y_end)) &
            &sclr%conc(imin-1:imax+1, y_end + ig) = sclr%conc(imin-1:imax+1,y_end)
    END DO

    ! set cell type around edges
    IF (block_owns_i(blk, x_beg)) THEN
       sclr%cell(x_beg,jmin:jmax)%xtype = SCALAR_BOUNDARY_TYPE
       sclr%cell(x_beg,jmin:jmax)%xbctype = SCALBC_NONE
    END IF

    IF (block_owns_i(blk, x_end)) THEN
       sclr%cell(x_end,jmin:jmax)%xtype = SCALAR_BOUNDARY_TYPE
       sclr%cell(x_end,jmin:jmax)%xbctype = SCALBC_NONE
    END IF

    IF (block_owns_j(blk, y_beg)) THEN
       sclr%cell(imin:imax,y_beg)%ytype = SCALAR_BOUNDARY_TYPE
       sclr%cell(imin:imax,y_beg)%ybctype = SCALBC_NONE
    END IF

    IF (block_owns_j(blk, y_end)) THEN
       sclr%cell(imin:imax,y_end)%ytype = SCALAR_BOUNDARY_TYPE
       sclr%cell(imin:imax,y_end)%ybctype = SCALBC_NONE
    END IF

    sclr%srcconc = 0.0
    sclr%srcflux = 0.0

  END SUBROUTINE default_scalar_bc


  ! ----------------------------------------------------------------
  ! SUBROUTINE apply_scalar_bc
  ! ----------------------------------------------------------------
  SUBROUTINE apply_scalar_bc(blk, sclr, spec, xstart, ystart)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    TYPE (scalar_struct) :: sclr
    TYPE (scalar_bc_spec_struct) :: spec
    INTEGER, INTENT(INOUT) :: xstart, ystart

    INTEGER :: con_block, k
    INTEGER :: i, ii, i_beg, i_end, x_end
    INTEGER :: j, jj, j_beg, j_end, y_end
    INTEGER :: imin, imax, jmin, jmax
    INTEGER :: owned
    DOUBLE PRECISION :: input_total, tmp

    CHARACTER (LEN=1024) :: buf

    SELECT CASE(spec%bc_type)
    CASE("BLOCK")
!!$       con_block = spec%con_block
!!$       CALL fillghost_scalar(blk, sclr, block(con_block), &
!!$            &species(spec%species)%scalar(con_block), spec)
       RETURN
    CASE("TABLE", "SOURCE")
       CALL scalar_table_interp(current_time%time, spec%table_num,&
            & table_input, spec%num_cell_pairs)
    END SELECT

    x_end = blk%xmax
    y_end = blk%ymax

    SELECT CASE(spec%bc_loc)

    CASE("US")
       i=1
       xstart = i + 1
       SELECT CASE(spec%bc_type)

       CASE("ZEROG")
          DO j=1,spec%num_cell_pairs
             j_beg = spec%start_cell(j)+1
             j_end = spec%end_cell(j)+1
             DO jj = j_beg, j_end
                IF (block_owns(blk, i, jj)) THEN
                   sclr%conc(i,jj) = sclr%conc(i+1,jj)
                   sclr%cell(i+1,jj)%xtype = SCALAR_BOUNDARY_TYPE
                   sclr%cell(i+1,jj)%xbctype = SCALBC_ZG
                END IF
             END DO
          END DO

       CASE("TABLE")
          SELECT CASE(spec%bc_kind)
          CASE("FLUX")
             GOTO 100
!!$             DO j=1,spec%num_cell_pairs
!!$                j_beg = spec%start_cell(j)+1
!!$                j_end = spec%end_cell(j)+1
!!$                DO jj = j_beg, j_end
!!$                   IF (block_owns(blk, i, jj)) THEN
!!$
!!$                      ! compute the inflow cell by cell
!!$
!!$                      CALL compute_uflow_area(blk, i, jj, jj, inlet_area, tmp)
!!$                      WHERE (blk%uvel(i,jj) .GE. 0.0) 
!!$                         inlet_area(jj) = &
!!$                              &inlet_area(jj)*blk%uvel(i,jj)
!!$                      ELSEWHERE
!!$                         inlet_area(jj) = 0.0
!!$                      END WHERE
!!$                      tmp = SUM(inlet_area(jj))
!!$                      sclr%conc(i,jj) =  table_input(j)/tmp
!!$                      sclr%concold(i,j_beg:j_end) = sclr%conc(i,j_beg:j_end)
!!$                      sclr%cell(xstart,j_beg:j_end)%xtype = SCALAR_BOUNDARY_TYPE
!!$                      sclr%cell(xstart,j_beg:j_end)%xbctype = SCALBC_CONC
!!$                   END IF
!!$                END DO
!!$             END DO

          CASE("CONC")
             i = spec%x_start
             xstart = i + 1
             DO j=1,spec%num_cell_pairs
                j_beg = spec%start_cell(j)+1
                j_end = spec%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      sclr%conc(i,jj) = &
                           &table_input(j)*scalar_source(spec%species)%conversion
                      sclr%concold(i,jj) = sclr%conc(i,jj)
                      sclr%cell(xstart,jj)%xtype = SCALAR_BOUNDARY_TYPE
                      sclr%cell(xstart,jj)%xbctype = SCALBC_CONC
                   END IF
                END DO
             END DO

          CASE DEFAULT
             GOTO 100
          END SELECT
       CASE DEFAULT
          GOTO 100
       END SELECT

    CASE("DS")
       i = x_end+1
       SELECT CASE(spec%bc_type)

       CASE("ZEROG")
          DO j=1,spec%num_cell_pairs
             j_beg = spec%start_cell(j)+1
             j_end = spec%end_cell(j)+1
             DO jj = j_beg, j_end
                IF (block_owns(blk, i, jj)) THEN
                   sclr%conc(i, jj) = sclr%conc(i-1, jj)
                   sclr%concold(i,jj) = sclr%conc(i,jj)
                   sclr%cell(i-1,jj)%xtype = SCALAR_BOUNDARY_TYPE
                   sclr%cell(i-1,jj)%xbctype = SCALBC_ZG
                END IF
             END DO
          END DO

       CASE DEFAULT
          GOTO 100
       END SELECT

    CASE ("RB")
       j = 1
       ystart = j + 1
       SELECT CASE(spec%bc_type)

       CASE ("ZEROG")
          DO k = 1, spec%num_cell_pairs
             i_beg = spec%start_cell(k)+1
             i_end = spec%end_cell(k)+1
             DO ii = i_beg, i_end
                IF (block_owns(blk, ii, j)) THEN 
                   sclr%conc(ii,j) = sclr%conc(ii,j+1)
                   sclr%cell(ii,j+1)%ytype = SCALAR_BOUNDARY_TYPE
                   sclr%cell(ii,j+1)%ybctype = SCALBC_ZG
                END IF
             END DO
          END DO

       CASE("TABLE")

          SELECT CASE(spec%bc_kind)

          CASE("CONC")
             DO k = 1, spec%num_cell_pairs
                i_beg = spec%start_cell(k)+1
                i_end = spec%end_cell(k)+1
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, ystart)) THEN 
                      sclr%conc(i_beg:i_end, j) = &
                           &table_input(k)*scalar_source(spec%species)%conversion
                      sclr%concold(ii, j) = sclr%conc(ii, j)
                      sclr%cell(ii,ystart)%ytype = SCALAR_BOUNDARY_TYPE
                      sclr%cell(ii,ystart)%ybctype = SCALBC_CONC
                   END IF
                END DO
             END DO

          CASE("FLUX")
             GOTO 100
!!$             DO k = 1, spec%num_cell_pairs
!!$                i_beg = spec%start_cell(k)+1
!!$                i_end = spec%end_cell(k)+1
!!$                CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, tmp)
!!$                WHERE (blk%vvel(i_beg:i_end, j) .GT. 0.0)
!!$                   inlet_area(i_beg:i_end) = &
!!$                        &inlet_area(i_beg:i_end)*blk%vvel(i_beg:i_end,j)
!!$                ELSEWHERE
!!$                   inlet_area(i_beg:i_end) = 0.0
!!$                END WHERE
!!$                tmp = SUM(inlet_area(i_beg:i_end))
!!$                DO ii = i_beg, i_end
!!$                   sclr%conc(ii,j) =  table_input(k)/tmp
!!$                END DO
!!$                sclr%concold(i_beg:i_end, j) = sclr%conc(i_beg:i_end, j)
!!$                sclr%cell(i_beg:i_end, j+1)%ytype = SCALAR_BOUNDARY_TYPE
!!$                sclr%cell(i_beg:i_end, j+1)%ybctype = SCALBC_CONC
!!$             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT
       CASE DEFAULT
          GOTO 100
       END SELECT

    CASE ("LB")
       j = y_end+1

       SELECT CASE(spec%bc_type)

       CASE("ZEROG")
          DO k=1,spec%num_cell_pairs
             i_beg = spec%start_cell(k)+1
             i_end = spec%end_cell(k)+1
             DO ii = i_beg, i_end
                IF (block_owns(blk, ii, j)) THEN 
                   sclr%conc(ii,j) = sclr%conc(ii,j-1)
                   sclr%concold(ii,j) = sclr%conc(ii,j)
                   sclr%cell(ii,j-1)%ytype = SCALAR_BOUNDARY_TYPE
                   sclr%cell(ii,j-1)%ybctype = SCALBC_ZG
                END IF
             END DO
          END DO

       CASE DEFAULT
          GOTO 100
       END SELECT

    CASE ("IN")
       CALL block_owned_window(blk, imin, imax, jmin, jmax)
       i_beg = MAX(spec%start_cell(1)+1, imin)
       i_end = MIN(spec%end_cell(1)+1, imax)
       j_beg = MAX(spec%start_cell(2)+1, jmin)
       j_end = MIN(spec%end_cell(2)+1, jmax)

       SELECT CASE(spec%bc_type)
       CASE("SOURCE")
          SELECT CASE(spec%bc_kind)
          CASE ("CONC")
             DO i = i_beg, i_end
                DO j = j_beg, j_end
                   sclr%srcconc(i, j) = table_input(1)*&
                        &scalar_source(spec%species)%conversion
                END DO
             END DO
          CASE ("FLUX") 
             ! if the source is a flux, we find the
             ! total area over which it applies
             
             owned = 0
             input_total = 0.0
             DO i = i_beg, i_end
                DO j =  j_beg, j_end
                   IF (.NOT. blk%isdry(i,j)) THEN
                      input_total = input_total + blk%hp1(i,j)*blk%hp2(i,j)
                   END IF
                   owned = owned + 1
                   ! WRITE(*,*) "AREA: ", i, j, input_total
                END DO
             END DO

             ! FIXME: need to sum input_total over all processors

             IF (owned .GT. 0) THEN 
                IF ((spec%start_cell(1)+1 .LT. imin) .OR. &
                     &(spec%end_cell(1)+1 .GT. imax) .OR. &
                     &(spec%start_cell(2)+1 .LT. jmin) .OR. &
                     &(spec%end_cell(2)+1 .GT. jmax)) THEN
                   ! WRITE(buf,*) " apply_scalar_bc: error: partial IN area, local ownership: ", &
                   !      &"(", imin, ",", jmin, ") to (", imax, ",", jmax, ")"
                   CALL error_message(buf, fatal=.FALSE.)
                   GOTO 100
                END IF
             END IF

             ! Compute the appropriate fraction of the flux for each active cell.

             IF (input_total .GT. 0) THEN 
                DO i = i_beg, i_end
                   DO j = j_beg, j_end
                      IF (.NOT. blk%isdry(i,j)) THEN
                         sclr%srcflux(i, j) = table_input(1)*&
                              &blk%hp1(i,j)*blk%hp2(i,j)/input_total
                      END IF
                      ! WRITE (*,*) "FLUX: ", i, j, blk%hp1(i,j)*blk%hp2(i,j), &
                      !      &input_total, sclr%srcflux(i, j)
                   END DO
                END DO
             END IF

          END SELECT
       CASE DEFAULT
          GOTO 100
       END SELECT
       
    CASE DEFAULT
       GOTO 100
    END SELECT

    RETURN

100 CONTINUE
    WRITE(buf,*) " apply_scalar_bc: cannot handle: ", &
         &TRIM(spec%bc_loc), " ", TRIM(spec%bc_type), " ", &
         &TRIM(spec%bc_kind), " for scalar "
    CALL error_message(buf, FATAL=.TRUE.)
  END SUBROUTINE apply_scalar_bc

  ! ----------------------------------------------------------------
  ! SUBROUTINE apply_scalar_source
  ! This routine computes the scalar source term and stores it for later
  ! use.  This can be precomputed because all of the scalar source terms
  ! are explicit, depending only on the previous concentration
  ! estimates.
  ! ----------------------------------------------------------------
  SUBROUTINE apply_scalar_source(iblock, ispecies, xstart, ystart)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock, ispecies, xstart, ystart

    INTEGER :: xend, yend
    INTEGER :: imin, imax, jmin, jmax
    INTEGER :: i, j
    DOUBLE PRECISION :: srctmp, src, t_water

    xend = block(iblock)%xmax
    yend = block(iblock)%ymax

    CALL block_owned_window(block(iblock), imin, imax, jmin, jmax)
    imin = MAX(imin, xstart)
    imax = MIN(imax, xend)
    jmin = MAX(jmin, ystart)
    jmax = MIN(jmax, yend)

    species(ispecies)%scalar(iblock)%srcterm = 0.0

    DO i = imin,imax
       DO j = jmin,jmax

          IF (source_doing_temp) THEN
             t_water = species(source_temp_idx)%scalar(iblock)%conc(i,j)
          ELSE
             t_water = 0
          END IF

          IF (.NOT. block(iblock)%isdead(i,j)%p) THEN
             src = &
                  &scalar_source_term(iblock, i, j, ispecies, &
                  &species(ispecies)%scalar(iblock)%conc(i,j),&
                  &block(iblock)%depth(i,j), block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j), &
                  &t_water, salinity)

             ! Include the affects of any fluid sources.  A positive xsource (ft/s)
             ! indicates an increase in fluid volume. This must include a
             ! scalar concentration or temperature from the scalar bc specifications.  
        
             IF (block(iblock)%xsource(i,j) .GT. 0.0) THEN
                srctmp = block(iblock)%xsource(i,j)*&
                     &species(ispecies)%scalar(iblock)%srcconc(i,j)
!!$                WRITE (*,'(I2, 2I6, 3E12.4)') &
!!$                     &ispecies, i, j, &
!!$                     &block(iblock)%xsource(i,j), &
!!$                     &block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j),&
!!$                     &srctmp*block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j)
                src = src + srctmp
             END IF

             src = src*block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j)

             ! Include scalar flux sources. At this point, srcflux is
             ! in mass/s, so just add it in

             IF (species(ispecies)%scalar(iblock)%srcflux(i,j) .GT. 0.0) THEN
                src = src + species(ispecies)%scalar(iblock)%srcflux(i,j)
                ! WRITE(*,*) "SCALAR: ", i, j, species(ispecies)%scalar(iblock)%srcflux(i,j)
             END IF

          ELSE 
             src = 0.0
          END IF

          species(ispecies)%scalar(iblock)%srcterm(i,j) = src
       END DO
    END DO
  END SUBROUTINE apply_scalar_source

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport
  ! ----------------------------------------------------------------
  SUBROUTINE transport()

    IMPLICIT NONE

    INTEGER :: var, iter
    INTEGER :: ispecies, iblock, num_bc
    INTEGER :: x_start, y_start
    DOUBLE PRECISION :: dum_val

    IF(.NOT. do_flow)THEN
       ! set up for next time step
       dum_val = current_time%time + delta_t/86400.0d0
       CALL transport_only_update(dum_val)
    END IF

    CALL scalar_source_timestep(current_time%time, delta_t)
!!$    IF (source_doing_sed) CALL bed_dist_bedsrc(delta_t)
    DO iblock = 1,max_blocks
       CALL transport_precalc(block(iblock))
    END DO

    ! INTERNAL ITERATION AT THIS TIME LEVEL LOOP
    DO iter = 1,number_scalar_iterations

       ! BLOCK LOOP
       DO iblock = 1,max_blocks

          x_start = 2
          y_start = 2

          ! SPECIES LOOP - multiple numbers of scalar variables
          DO ispecies = 1, max_species

             CALL default_scalar_bc(block(iblock), species(ispecies)%scalar(iblock))

             ! WRITE(*,*) 'Transport: block = ', iblock, ', species = ', ispecies

             ! set boundary conditions for this time

             ! loop over the total number of bc specifications
             DO num_bc = 1, scalar_bc(iblock)%num_bc
                IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
                     &CALL apply_scalar_bc(block(iblock), &
                     &species(ispecies)%scalar(iblock), &
                     &scalar_bc(iblock)%bc_spec(num_bc), x_start, y_start)
             END DO ! num bc loop

             CALL apply_scalar_source(iblock, ispecies, x_start, y_start)

             CALL scalar_solve(iblock, block(iblock), species(ispecies)%scalar(iblock), &
                  &scalar_source(ispecies)%relax, scalar_source(ispecies)%scheme, &
                  &scalar_source(ispecies)%cds_blend, x_start, y_start)

          END DO ! species loop

       END DO ! block loop end

    END DO ! internal time loop end for concentration

    ! end scalar transport soultion
    !----------------------------------------------------------------------------
  END SUBROUTINE transport


END MODULE scalar_solve_module


