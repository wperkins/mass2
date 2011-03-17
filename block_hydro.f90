! ----------------------------------------------------------------
! file: block_hydro.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  3, 2011 by William A. Perkins
! Last Change: Wed Mar 16 10:31:22 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------


! ----------------------------------------------------------------
! MODULE block_hydro
!
! Basically just a set of routines to operate on hydrodynamics
! variables
! ----------------------------------------------------------------
MODULE block_hydro

  USE config
  USE block_module
  USE differencing

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"


CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE velocity_shift
  ! This routine computes velocity components at the cell center
  ! ----------------------------------------------------------------
  SUBROUTINE velocity_shift()

    IMPLICIT NONE

    INTEGER :: iblk, i, j
    DOUBLE PRECISION :: flux1, flux2

    ! compute fluxes for all blocks first

    DO iblk = 1, max_blocks

       ! cell center velocity components 
       ! with fluxes !
       block(iblk)%uflux = 0.0
       block(iblk)%vflux = 0.0
       DO i = i_index_min+1, block(iblk)%xmax+i_index_extra
          DO j=2, block(iblk)%ymax 
             IF (block_owns(block(iblk), i, j)) THEN
                flux2 = uflux(block(iblk), i, j, j)
                block(iblk)%uflux(i,j) = flux2
             END IF
          END DO
          IF (block_owns(block(iblk), i, 1)) THEN
             block(iblk)%uflux(i,1) = block(iblk)%uflux(i,2)
          END IF
          IF (block_owns(block(iblk), i, block(iblk)%ymax+1)) THEN
             block(iblk)%uflux(i,block(iblk)%ymax+1) = &
                  &block(iblk)%uflux(i,block(iblk)%ymax)
          END IF
       END DO

       DO j= j_index_min+1, block(iblk)%ymax + j_index_extra
          DO i = 2, block(iblk)%xmax
             IF (block_owns(block(iblk), i, j)) THEN
                flux2 = vflux(block(iblk), i, i, j)
                block(iblk)%vflux(i,j) = flux2
             END IF
          END DO
          IF (block_owns(block(iblk), 1, j)) THEN
             block(iblk)%vflux(1,j) = block(iblk)%vflux(2,j)
          END IF
          IF (block_owns(block(iblk), block(iblk)%xmax+1, j)) THEN
             block(iblk)%vflux(block(iblk)%xmax+1, j) = &
                  &block(iblk)%vflux(block(iblk)%xmax, j)
          END IF
       END DO

       CALL block_var_put(block(iblk)%bv_uflux)
       CALL block_var_put(block(iblk)%bv_vflux)

    END DO

    CALL ga_sync()

    ! Next, compute the p-cell center velocity components from the fluxes

    DO iblk = 1, max_blocks

       CALL block_var_get(block(iblk)%bv_uflux)
       CALL block_var_get(block(iblk)%bv_vflux)

       DO i = i_index_min+1, block(iblk)%xmax+i_index_extra
          DO j=2, block(iblk)%ymax 
             IF (block_owns(block(iblk), i, j)) THEN
                flux1 = block(iblk)%uflux(i-1,j)
                flux2 = block(iblk)%uflux(i,j)
                block(iblk)%uvel_p(i,j) = &
                     &0.5*(flux1+flux2)/block(iblk)%hp2(i,j)/&
                     &block(iblk)%depth(i,j)
             END IF
          END DO
       END DO

       DO j= j_index_min+1, block(iblk)%ymax + j_index_extra
          DO i = 2, block(iblk)%xmax
             IF (block_owns(block(iblk), i, j)) THEN
                flux1 = block(iblk)%vflux(i,j-1)
                flux2 = block(iblk)%vflux(i,j)
                block(iblk)%vvel_p(i,j) = 0.5*(flux1+flux2)/&
                     &block(iblk)%hp1(i,j)/block(iblk)%depth(i,j)
             END IF
          END DO
          IF (block_owns(block(iblk), 1, j)) THEN
             block(iblk)%vflux(1,j) = block(iblk)%vflux(2,j)
          END IF
          IF (block_owns(block(iblk), block(iblk)%xmax+1, j)) THEN
             block(iblk)%vflux(block(iblk)%xmax+1, j) = &
                  &block(iblk)%vflux(block(iblk)%xmax, j)
          END IF
       END DO

       i = 1
       DO j = 2, block(iblk)%ymax
          IF (block_owns(block(iblk), i, j)) THEN
             block(iblk)%uvel_p(i,j) = block(iblk)%uvel(i+1,j)
          END IF
       END DO
       j = 1
       DO i = 2, block(iblk)%xmax
          IF (block_owns(block(iblk), i, j)) THEN
             block(iblk)%vvel_p(i,j) = block(iblk)%vvel(i,j+1)
          END IF
       END DO

       DO i = 2, block(iblk)%xmax
          DO j= 2, block(iblk)%ymax
             IF (block_owns(block(iblk), i, j)) THEN
                block(iblk)%vmag(i, j) = SQRT(&
                     &block(iblk)%uvel_p(i,j)*block(iblk)%uvel_p(i,j) + &
                     &block(iblk)%vvel_p(i,j)*block(iblk)%vvel_p(i,j))
             END IF
          END DO
       END DO
       

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

       CALL block_var_put(block(iblk)%bv_uvel_p)
       CALL block_var_put(block(iblk)%bv_vvel_p)
       CALL block_var_put(block(iblk)%bv_u_cart)
       CALL block_var_put(block(iblk)%bv_v_cart)
       CALL block_var_put(block(iblk)%bv_vmag)
       
    END DO
  END SUBROUTINE velocity_shift

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
  ! SUBROUTINE depth_check
  ! 
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE depth_check(blk, date_str, time_str)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    CHARACTER (LEN=*), INTENT(IN) :: date_str, time_str

#include "mafdecls.fh"
#include "global.fh"

    INTEGER :: x_beg, x_end, y_beg, y_end, i, j
    INTEGER :: crash(1)

    crash(1) = 0

    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    DO i = x_beg, x_end
       DO j = y_beg, y_end
          IF (.NOT. block_owns(blk, i, j)) CYCLE
          IF (blk%depth(i,j) <= 0.0) THEN
             IF (do_wetdry) THEN
                WRITE(utility_error_iounit,*)" ERROR: Negative Depth = ",blk%depth(i,j)
                WRITE(utility_error_iounit,*)"     Simulation Time: ", date_str, " ", time_str
                WRITE(utility_error_iounit,*)"     Block Number = ",blk%index
                WRITE(utility_error_iounit,*)"     I,J Location of negative depth = (", i, ", ", j, ")"

                WRITE(*,*)" ERROR: Negative Depth = ",blk%depth(i,j)
                WRITE(*,*)"     Simulation Time: ", date_str, " ", time_str
                WRITE(*,*)"     Block Number = ",blk%index
                WRITE(*,*)"     I,J Location of negative depth = (", i, ", ", j, ")"

                blk%depth(i,j) = dry_zero_depth
             ELSE

                WRITE(utility_error_iounit,*)" FATAL ERROR: Negative Depth = ",MINVAL(blk%depth)
                WRITE(utility_error_iounit,*)"     Block Number = ",blk%index
                WRITE(utility_error_iounit,*)"     I,J Location of negative depth = ",MINLOC(blk%depth)

                WRITE(*,*)" FATAL ERROR: Negative Depth = ",MINVAL(blk%depth)
                WRITE(*,*)"     Block Number = ",blk%index
                WRITE(*,*)"     I,J Location of negative depth = ",MINLOC(blk%depth)

                crash(1) = 1

             END IF
          ELSE IF (.NOT. do_wetdry .AND. blk%depth(i,j) <= 0.20) THEN
             WRITE(utility_error_iounit,*)" WARNING: Small Depth = ", blk%depth(i,j)
             WRITE(utility_error_iounit,*)"     Simulation Time: ", date_str, " ", time_str
             WRITE(utility_error_iounit,*)"     Block Number = ",blk%index
             WRITE(utility_error_iounit,*)"     I,J Location of small depth = ",i, j
          END IF
       END DO
    END DO

    CALL ga_igop(MT_F_INT, crash, 1, '+');

    CALL block_var_put(blk%bv_depth)
    CALL ga_sync()
    CALL block_var_get(blk%bv_depth)

  END SUBROUTINE depth_check

  ! ----------------------------------------------------------------
  ! SUBROUTINE bedshear
  ! (Collective) computes the shear used by biota/sediment scalars
  ! ----------------------------------------------------------------
  SUBROUTINE bedshear(blk)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    INTEGER :: i, j
    DOUBLE PRECISION :: roughness, u, v

    blk%shear = 0 
    DO i = 1, blk%xmax + 1
       DO j = 2, blk%ymax
          IF (.NOT. block_owns(blk, i, j)) CYCLE
          IF (i .EQ. 1) THEN
             u = blk%uvel(i,j)
             v = 0.0
          ELSE IF (i .EQ. blk%xmax + 1) THEN
             u = blk%uvel(i-1,j)
             v = 0.0
          ELSE 
             u = 0.5*(blk%uvel(i-1,j) + blk%uvel(i,j))
             v = 0.5*(blk%vvel(i-1,j) + blk%vvel(i,j))
          END IF
          IF(manning)THEN
             roughness = 0.0
             IF (blk%depth(i,j) .GT. 0.0) THEN
                roughness = (grav*blk%chezy(i,j)**2)/&
                     &(mann_con*blk%depth(i,j)**0.3333333)
             END IF
          ELSE
             roughness = blk%chezy(i,j)
          ENDIF
          blk%shear(i,j) = roughness*density*(u*u + v*v)
       END DO
    END DO

    CALL block_var_put(blk%bv_shear)
    ! no need to get, since ghost cell values are not used

  END SUBROUTINE bedshear

  ! ----------------------------------------------------------------
  ! SUBROUTINE calc_eddy_viscosity
  ! (Collective)
  ! ----------------------------------------------------------------
  SUBROUTINE calc_eddy_viscosity(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk

    INTEGER :: i, j
    DOUBLE PRECISION :: eddystar

    DO i = 2, blk%xmax
       DO j = 2, blk%ymax
          IF (block_owns(blk, i, j)) THEN
             eddystar = viscosity_water + &
                  &vonkarmon/6.0*sqrt(blk%shear(i,j)/density)*blk%depth(i,j)
             blk%eddy(i,j) = relax_eddy*eddystar +&
                  &(1.0 - relax_eddy)*blk%eddy(i,j)
          END IF
       END DO
    END DO

    CALL block_var_put(blk%bv_eddy)
    CALL ga_sync()
    CALL block_var_get(blk%bv_eddy)

  END SUBROUTINE calc_eddy_viscosity


  ! ----------------------------------------------------------------
  ! SUBROUTINE check_wetdry
  ! (Collective)
  ! ----------------------------------------------------------------
  SUBROUTINE check_wetdry(blk)

    IMPLICIT NONE

    TYPE(block_struct) :: blk
    INTEGER :: i, j, nx, ny
    INTEGER :: i1, j1, n
    LOGICAL :: flag
    DOUBLE PRECISION :: wsavg, wscell, wsn

    nx = blk%xmax
    ny = blk%ymax

    blk%isdrystar = blk%isdry

    DO i = 2, nx + 1
       DO j = 2, ny + 1 

          IF (.NOT. block_owns(blk, i, j)) CYCLE

          IF (blk%isdrystar(i,j)) THEN

             wscell = blk%depth(i, j) + blk%zbot(i, j)

             ! Condition: all wet neighbors must
             ! have a higher w.s. elevation.

             flag = .FALSE.       ! becomes .TRUE. if *any* wet neighbors are higher
             n = 0                ! count wet neighbors

             DO i1 = i - 1, i + 1, 2
                IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. blk%isdrystar(i1,j))) THEN
                   wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                   IF (wsn .GE. wscell) flag = .TRUE.
                   n = n + 1
                ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                   SELECT CASE (blk%cell(i,j)%xtype)
                   CASE (CELL_BOUNDARY_TYPE)
                      ! ignore
                   CASE DEFAULT
                      ! should be a ghost cell, use it directly
                      wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                      IF (wsn .GE. wscell) flag = .TRUE.
                      n = n + 1
                   END SELECT
                END IF
             END DO
             DO j1 = j - 1, j + 1, 2
                wsn = blk%depth(i, j1) + blk%zbot(i, j1)
                IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. blk%isdrystar(i,j1))) THEN
                   IF (wsn .GE. wscell) flag = .TRUE.
                   n = n + 1
                END IF
             END DO

             IF (n .GT. 0 .AND. flag) THEN

                ! if everything is evened
                ! out the cell and its neighbors will
                ! be at the same elevation -- the
                ! average, compute it

                wsavg = wscell
                n = 1

                DO i1 = i - 1, i + 1, 2
                   IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. blk%isdrystar(i1,j))) THEN
                      wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                      wsavg = wsavg + wsn
                      n = n + 1
                   ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                      SELECT CASE (blk%cell(i,j)%xtype)
                      CASE (CELL_BOUNDARY_TYPE)
                         ! ignore
                      CASE DEFAULT
                         ! should be a ghost cell, use it directly
                         wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                         wsavg = wsavg + wsn
                         n = n + 1
                      END SELECT
                   END IF
                END DO
                DO j1 = j - 1, j + 1, 2
                   wsn = blk%depth(i, j1) + blk%zbot(i, j1)
                   IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. blk%isdrystar(i,j1))) THEN
                      wsavg = wsavg + wsn
                      n = n + 1
                   END IF
                END DO
                wsavg = wsavg/REAL(n)

!!$                                ! Alternative: A cell becomes wet if
!!$                                ! the average elevation of the cell
!!$                                ! and all its wet neighbors is high
!!$                                ! enough to exceed the dry depth
!!$
!!$           blk%isdry(i,j) = (.NOT. ((wsavg - blk%zbot(i, j)) .GT. dry_depth))

                ! Alternative: A cell becomes wet if
                ! the cell and all its wet neighbors
                ! are high enough to exceed the dry
                ! depth *and* have all of the wet
                ! cells remain wet.

                IF (N .GT. 1 .AND. wsavg .GT. blk%zbot(i, j) + dry_rewet_depth ) THEN
                   flag = .TRUE.
                   DO i1 = i - 1, i + 1, 2
                      IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. blk%isdrystar(i1,j))) THEN
                         flag = flag .AND. ((wsavg - blk%zbot(i1, j)) .GT. dry_depth) 
                      ELSEIF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                         SELECT CASE (blk%cell(i,j)%xtype)
                         CASE (CELL_BOUNDARY_TYPE)
                            ! ignore
                         CASE DEFAULT
                            ! should be a ghost cell, use it directly
                            flag = flag .AND. ((wsavg - blk%zbot(i1, j)) .GT. dry_depth) 
                         END SELECT
                      END IF
                   END DO
                   DO j1 = j - 1, j + 1, 2
                      IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. blk%isdrystar(i,j1))) THEN
                         flag = flag .AND. ((wsavg - blk%zbot(i, j1)) .GT. dry_depth) 
                      END IF
                   END DO

                   ! if all the neighbors can stay wet,
                   ! then it's ok to be wet again

                   ! we may need a volume check here

                   IF (flag) blk%isdry(i,j) = .FALSE.

                END IF
             END IF

          ELSE 

             ! check the wet cells to see if they
             ! should be dry

             blk%isdry(i,j) = (blk%depth(i,j) .LE. dry_depth)

          END IF
       END DO
    END DO

    ! fill out isdry array for plotting

    IF (block_owns_j(blk, 1)) blk%isdry(:,1) = blk%isdry(:,2)
    IF (block_owns_j(blk, ny + 1)) blk%isdry(:,ny + 1) = blk%isdry(:,ny+1)
    IF (block_owns_i(blk, 1)) blk%isdry(1,:) = blk%isdry(2,:)
    IF (block_owns_i(blk, nx + 1)) blk%isdry(nx + 1,:)  = blk%isdry(nx+1,:)

    CALL block_var_put_logical(blk%bv_isdry, blk%isdry)
    CALL ga_sync()
    CALL block_var_get_logical(blk%bv_isdry, blk%isdry)

  END SUBROUTINE check_wetdry

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_mass_source_sum
  ! ----------------------------------------------------------------
  SUBROUTINE block_mass_source_sum(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk

    INTEGER :: imin, imax, jmin, jmax
    INTEGER :: lo(1), hi(1), ld(1)
    INTEGER :: ierr
    DOUBLE PRECISION :: djunk

    lo = 1
    hi = 1
    ld = 1
    
    CALL block_owned_window(blk, imin, jmin, imax, jmax)
    djunk = &
         &SUM(ABS(blk%mass_source))
    blk%mass_source_sum(1) = djunk
    CALL ga_dgop(MT_F_DBL, blk%mass_source_sum, 1, '+');
    ! WRITE(*,*) djunk, blk%mass_source_sum(1)

    blk%mass_source = 0.0

  END SUBROUTINE block_mass_source_sum


  ! ----------------------------------------------------------------
  ! SUBROUTINE calc_diag
  ! ----------------------------------------------------------------
  SUBROUTINE calc_diag()

    IMPLICIT NONE

    INTEGER :: i, iblock

    DO iblock = 1, max_blocks

       WHERE (block(iblock)%depth .GT. 0.0 .AND. &
            &block(iblock)%hp1 .NE. 0.0 .AND. &
            &block(iblock)%hp2 .NE. 0.0)
          block(iblock)%froude_num = &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
               &SQRT(grav*block(iblock)%depth)
          block(iblock)%courant_num = &
               &delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
               &SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)
       ELSEWHERE 
          block(iblock)%froude_num = 0.0
          block(iblock)%courant_num = 0.0
       END WHERE

       CALL block_var_put(block(iblock)%bv_froude_num)
       CALL block_var_put(block(iblock)%bv_courant_num)

    END DO

  END SUBROUTINE calc_diag


END MODULE block_hydro
