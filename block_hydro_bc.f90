! ----------------------------------------------------------------
! MODULE block_hydro_bc
!
! Routines to apply boundary condi 
! ----------------------------------------------------------------
MODULE block_hydro_bc

  USE config
  USE block_grid
  USE hydro_bc

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION check_hydro_block_connection
  ! RETURNS .TRUE. if the block connection matches
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION check_hydro_block_connection(bc, conbc)

    IMPLICIT NONE

    TYPE (bc_spec_struct), INTENT(INOUT) :: bc
    TYPE (bc_spec_struct), INTENT(IN) :: conbc
    INTEGER :: i, coni, j, conj, n, cells, concells
    DOUBLE PRECISION :: x1, y1, x2, y2, rdist
    CHARACTER (LEN=1024) :: msg

    DOUBLE PRECISION :: distance
    EXTERNAL distance

    check_hydro_block_connection = .FALSE.

    IF (debug) THEN 
       WRITE(msg, "('Checking connection between block ', I2, ' and ', I2)") bc%block, conbc%block
       CALL status_message(msg)
    END IF

    ! these two bc's match only if they
    ! refer to each other

    IF (bc%con_block .EQ. conbc%block .AND. conbc%con_block .EQ. bc%block)THEN

       IF (debug) THEN 
          CALL status_message('    GOOD: indices match')
       END IF

       ! in order to match, the two bc's
       ! should refer have the same number of
       ! cell groupings
       IF (bc%num_cell_pairs .NE. conbc%num_cell_pairs) RETURN

       IF (debug) THEN 
          CALL status_message('    GOOD: cell index pairs match')
       END IF


       DO n = 1, bc%num_cell_pairs

          SELECT CASE (bc%bc_loc)
          CASE("US")
             i = 1
             coni = block(conbc%block)%xmax
             j = bc%start_cell(n)
             conj = conbc%start_cell(n)
          CASE ("DS")
             i = block(bc%block)%xmax
             coni = 1
             j = bc%start_cell(n)
             conj = conbc%start_cell(n)
          CASE ("RB")
             i = bc%start_cell(n)
             coni = conbc%start_cell(n)
             j = 1
             conj = block(conbc%block)%ymax
          CASE ("LB")
             i = bc%start_cell(n)
             coni = conbc%start_cell(n)
             j = block(bc%block)%ymax
             conj = 1
          END SELECT

          ! check to see if the ends of the
          ! block connection are in about the
          ! same location. we do this by
          ! computing the distances between the
          ! ends.  If that distance is a
          ! significant portion of a cell width,
          ! the bc's don't match. We cannot do
          ! this because the grid has not been
          ! read yet

!!$          x1 = block(bc%block)%x_grid(i, j)
!!$          y1 = block(bc%block)%y_grid(i, j)
!!$          x2 = block(conbc%block)%x_grid(coni, conj)
!!$          y2 = block(conbc%block)%y_grid(coni, conj)
!!$
!!$          rdist = distance(x1, y1, x2, y2)
!!$
!!$          SELECT CASE (bc%bc_loc)
!!$          CASE ("US","DS")
!!$             x2 = block(bc%block)%x_grid(i, j+1)
!!$             y2 = block(bc%block)%y_grid(i, j+1)
!!$          CASE ("LB","RB")
!!$             x2 = block(bc%block)%x_grid(i+1, j)
!!$             y2 = block(bc%block)%y_grid(i+1, j)
!!$          END SELECT
!!$
!!$          rdist = rdist/distance(x1, y1, x2, y2)
!!$
!!$          IF (rdist .GT. 0.01) THEN
!!$             IF (debug) THEN 
!!$                WRITE (msg,*) '   REJECT: cell pair ', n, ' starting point different'
!!$                CALL status_message(msg)
!!$             END IF
!!$             RETURN
!!$          END IF
!!$
!!$          SELECT CASE (bc%bc_loc)
!!$          CASE ("US","DS")
!!$             j = bc%end_cell(n)
!!$             x1 = block(bc%block)%x_grid(i, j+1)
!!$             y1 = block(bc%block)%y_grid(i, j+1)
!!$             conj = conbc%end_cell(n)
!!$             x2 = block(conbc%block)%x_grid(coni, conj+1)
!!$             y2 = block(conbc%block)%y_grid(coni, conj+1)
!!$          CASE ("LB","RB")
!!$             i =  bc%end_cell(n)
!!$             x1 = block(bc%block)%x_grid(i+1, j)
!!$             y1 = block(bc%block)%y_grid(i+1, j)
!!$             coni = conbc%end_cell(n)
!!$             x2 = block(conbc%block)%x_grid(coni+1, conj)
!!$             y2 = block(conbc%block)%y_grid(coni+1, conj)
!!$          END SELECT
!!$
!!$          rdist = distance(x1, y1, x2, y2)
!!$
!!$          x2 = block(bc%block)%x_grid(i, j)
!!$          y2 = block(bc%block)%y_grid(i, j)
!!$          rdist = rdist/distance(x1, y1, x2, y2)
!!$
!!$          IF (rdist .GT. 0.01) THEN
!!$             IF (debug) THEN 
!!$                WRITE (msg,*) '   REJECT: cell pair ', n, ' ending point different'
!!$                CALL status_message(msg)
!!$             END IF
!!$             RETURN
!!$          END IF

          ! check to see if there is an integral
          ! ratio of fine to coarse cells on
          ! either side of the block boundary

          cells = bc%end_cell(n) - bc%start_cell(n) + 1
          concells = conbc%end_cell(n) - conbc%start_cell(n) + 1

          IF (cells .GE. concells) THEN
             IF (MOD(cells, concells) .NE. 0) THEN
                WRITE (msg,*) 'Cell mismatch in connection between block ', &
                     &bc%block, ' and ', conbc%block
                CALL error_message(msg, fatal=.FALSE.)
                RETURN
             END IF
          ELSE
             IF (MOD(concells, cells) .NE. 0) THEN
                WRITE (msg,*) 'Cell mismatch in connection between block ', &
                     &bc%block, ' and ', conbc%block
                CALL error_message(msg, fatal=.FALSE.)
                RETURN
             END IF
          END IF

          bc%con_start_cell(n) = conbc%start_cell(n)
          bc%con_end_cell(n) = conbc%end_cell(n)
       END DO

       ! all tests passed

       check_hydro_block_connection = .TRUE.
    END IF


  END FUNCTION check_hydro_block_connection

  ! ----------------------------------------------------------------
  ! SUBROUTINE set_block_connections
  ! ----------------------------------------------------------------
  SUBROUTINE set_block_connections(max_blocks, error_iounit, status_iounit)

    IMPLICIT NONE

    INTEGER :: max_blocks, error_iounit, status_iounit
    INTEGER :: iblk,num_bc,con_block, i, ierr
    LOGICAL :: bcfound
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    DO iblk = 1, max_blocks

       DO num_bc = 1, block_bc(iblk)%num_bc
          IF(block_bc(iblk)%bc_spec(num_bc)%bc_type == "BLOCK")THEN
             con_block = block_bc(iblk)%bc_spec(num_bc)%con_block
             bcfound = .FALSE.
             DO i=1,block_bc(con_block)%num_bc
                IF(block_bc(con_block)%bc_spec(i)%con_block .EQ. iblk) THEN
                   bcfound = check_hydro_block_connection(&
                        &block_bc(iblk)%bc_spec(num_bc),&
                        &block_bc(con_block)%bc_spec(i))
                END IF
             END DO
             IF (.NOT. bcfound) THEN
                WRITE (msg, *) 'No match for BC connecting block ', iblk, &
                     &' to ', con_block, ': ',&
                     &TRIM(block_bc(iblk)%bc_spec(num_bc)%bc_type), ' ',&
                     &TRIM(block_bc(iblk)%bc_spec(num_bc)%bc_loc), ' ',&
                     &TRIM(block_bc(iblk)%bc_spec(num_bc)%bc_kind), ' ',&
                     &TRIM(block_bc(iblk)%bc_spec(num_bc)%bc_extent), ' '
                CALL error_message(msg, FATAL=.FALSE.)
                ierr = ierr + 1
             END IF
          END IF
       END DO

    END DO

    IF (ierr .GT. 0) THEN
       CALL error_message('Block connection errors. Unable to continue.', fatal = .TRUE.)
    END IF


  END SUBROUTINE set_block_connections

  ! ----------------------------------------------------------------
  ! SUBROUTINE default_hydro_bc
  ! ----------------------------------------------------------------
  SUBROUTINE default_hydro_bc(blk)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    INTEGER :: x_beg, y_beg, x_end, y_end, i, j, ig
    INTEGER :: imin, imax, jmin, jmax

    CALL block_used_window(blk, imin, imax, jmin, jmax)

    x_beg = 2
    y_beg = 2
    x_end = blk%xmax
    y_end = blk%ymax

    imin = MAX(imin, x_beg)
    jmin = MAX(jmin, y_beg)
    imax = MIN(imax, x_end)
    jmax = MIN(jmax, y_end)

    IF (block_uses_i(blk, x_beg)) THEN
       blk%cell(x_beg,:)%xtype = CELL_BOUNDARY_TYPE
       blk%cell(x_beg,:)%xbctype = FLOWBC_NONE
    END IF
    IF (block_uses_i(blk, x_end)) THEN
       blk%cell(x_end,:)%xtype = CELL_BOUNDARY_TYPE
       blk%cell(x_end,:)%xbctype = FLOWBC_NONE
    END IF

    IF (block_uses_j(blk, y_beg)) THEN
       blk%cell(:,y_beg)%ytype = CELL_BOUNDARY_TYPE
       blk%cell(:,y_beg)%ybctype = FLOWBC_NONE
    END IF
    IF (block_uses_j(blk, y_end)) THEN
       blk%cell(:,y_end)%ytype = CELL_BOUNDARY_TYPE
       blk%cell(:,y_end)%ybctype = FLOWBC_NONE
    END IF

    DO ig = 1, nghost

       i = x_beg - ig
       IF (block_uses_i(blk, i)) THEN
          blk%uvel(i,:) = 0.0
          blk%vvel(i,:) = blk%vvel(x_beg,:)
          CALL extrapolate_udepth(blk, i, jmin, jmax, level=.FALSE.)
          blk%eddy(i,:) = blk%eddy(x_beg,:)
       END IF

       i = blk%xmax + ig
       IF (block_uses_i(blk, i)) THEN
          blk%uvel(i,:) = 0.0
          blk%vvel(i,:) = blk%vvel(x_end,:)
          CALL extrapolate_udepth(blk, i, jmin, jmax, level=.FALSE.)
          blk%eddy(i,:) = blk%eddy(x_end,:)
       END IF

       j = y_beg - ig
       IF (block_uses_j(blk, j)) THEN
          blk%uvel(:,j) = blk%uvel(:,y_beg)
          blk%vvel(:,j) = 0.0
          CALL extrapolate_vdepth(blk, imin, imax, j, level=.FALSE.)
          blk%eddy(:,j) = blk%eddy(:,y_beg)
       END IF

       j = blk%ymax + ig
       IF (block_uses_j(blk, j)) THEN
          blk%uvel(:,j) = blk%uvel(:,y_end)
          blk%vvel(:,j) = 0.0
          CALL extrapolate_vdepth(blk, imin, imax, j, level=.FALSE.)
          blk%eddy(:,j) = blk%eddy(:,y_end)
       END IF

    END DO

    blk%isdead(:,:)%u = .FALSE.
    blk%isdead(:,:)%v = .FALSE.
    blk%isdead(:,:)%p = .FALSE.
    blk%xsource = 0.0

  END SUBROUTINE default_hydro_bc

  ! ----------------------------------------------------------------
  ! SUBROUTINE extrapolate_udepth
  ! ----------------------------------------------------------------
  SUBROUTINE extrapolate_udepth(blk, i, jmin, jmax, level)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    INTEGER, INTENT(IN) :: i, jmin, jmax
    LOGICAL, INTENT(IN), OPTIONAL :: level

    INTEGER :: ioff
    LOGICAL :: dolvl

    ioff = 1                      ! by default, do the upstream end.
    IF (i .GE. 2) ioff = -1

    dolvl = .TRUE.
    IF (PRESENT(level)) dolvl = level

    IF (block_owns_i(blk, i)) THEN
       IF (dolvl) THEN
          ! by level w.s. elevation
          
          blk%depth(i,jmin:jmax) = (blk%depth(i+ioff,jmin:jmax) +&
               & blk%zbot(i+ioff,jmin:jmax)) - blk%zbot(i,jmin:jmax)
       ELSE 
          ! true linear extrapolation of w.s. elevation
          
          blk%depth(i,jmin:jmax) = (&
               &(blk%depth(i+ioff,jmin:jmax) + blk%zbot(i+ioff,jmin:jmax)) - &
               &(blk%depth(i+2*ioff,jmin:jmax) + blk%zbot(i+2*ioff,jmin:jmax)))*&
               &blk%hu1(i,jmin:jmax)/blk%hu1(i+ioff,jmin:jmax) + &
               &(blk%depth(i+ioff,jmin:jmax) + blk%zbot(i+ioff,jmin:jmax)) - &
               &blk%zbot(i,jmin:jmax)
       END IF

       IF (do_wetdry) THEN
          WHERE (blk%depth(i,jmin:jmax) .LT. dry_zero_depth) &
               &blk%depth(i,jmin:jmax) = dry_zero_depth
       END IF
    END IF
  END SUBROUTINE extrapolate_udepth

  ! ----------------------------------------------------------------
  ! SUBROUTINE extrapolate_vdepth
  ! ----------------------------------------------------------------
  SUBROUTINE extrapolate_vdepth(blk, imin, imax, j, level)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    INTEGER, INTENT(IN) :: imin, imax, j
    LOGICAL, INTENT(IN), OPTIONAL :: level

    INTEGER :: joff
    LOGICAL :: dolvl

    joff = 1                      ! by default, do the upstream end.
    IF (j .GE. 2) joff = -1

    dolvl = .TRUE.
    IF (PRESENT(level)) dolvl = level

    IF (block_owns_j(blk, j)) THEN
       IF (dolvl) THEN
          ! by level w.s. elevation
          
          blk%depth(imin:imax,j) = (blk%depth(imin:imax,j+joff) +&
               & blk%zbot(imin:imax,j+joff)) - blk%zbot(imin:imax,j)
       ELSE 
          ! true linear extrapolation of w.s. elevation
          
          blk%depth(imin:imax,j) = (&
               &(blk%depth(imin:imax,j+joff) + blk%zbot(imin:imax,j+joff)) - &
               &(blk%depth(imin:imax,j+2*joff) + blk%zbot(imin:imax,j+2*joff)))*&
               &blk%hv2(imin:imax,j)/blk%hv2(imin:imax,j+joff) + &
               &(blk%depth(imin:imax,j+joff) + blk%zbot(imin:imax,j+joff)) - &
               &blk%zbot(imin:imax,j)
       END IF
       
       IF (do_wetdry) THEN
          WHERE (blk%depth(imin:imax,j) .LT. dry_zero_depth) &
               &blk%depth(imin:imax,j) = dry_zero_depth
       END IF
    END IF
  END SUBROUTINE extrapolate_vdepth

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_compute_bc_flux
  ! 
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE block_compute_bc_flux(blk, blkbc)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (block_bc_struct), INTENT(INOUT) :: blkbc

    INTEGER :: ibc
    INTEGER :: imin, imax, jmin, jmax
    INTEGER :: ibeg, iend, jbeg, jend
    INTEGER :: cmin, cmax
    INTEGER :: i, j
    DOUBLE PRECISION :: junk
    LOGICAL :: isu

    CALL block_owned_window(blk, imin, imax, jmin, jmax)

    DO ibc = 1, blkbc%num_bc
       i = blkbc%bc_spec(ibc)%num_cell_pairs
       cmin = MINVAL(blkbc%bc_spec(ibc)%start_cell(1:i))+1
       cmax = MAXVAL(blkbc%bc_spec(ibc)%end_cell(1:i))+1
       
       SELECT CASE (blkbc%bc_spec(ibc)%bc_kind)
       CASE ("FLUX")

          blkbc%bc_spec(ibc)%flux_area = 0.0

          SELECT CASE (blkbc%bc_spec(ibc)%bc_loc)
          CASE ("US")
             ibeg = 1
             iend = 1
             jbeg = cmin
             jend = cmax
             isu = .TRUE.
          CASE ("DS")
             ibeg = blk%xmax+1
             iend = blk%xmax+1
             jbeg = cmin
             jend = cmax
             isu = .TRUE.
          CASE ("RB")
             ibeg = cmin
             iend = cmax
             jbeg = 1
             jend = 1
             isu = .FALSE.
          CASE ("LB")
             ibeg = cmin
             iend = cmax
             jbeg = blk%ymax+1
             jend = blk%ymax+1
             isu = .FALSE.
          END SELECT

          DO i = ibeg, iend
             DO j = jbeg, jend
                IF (block_owns(blk, i, j)) THEN
                   IF (isu) THEN
                      CALL compute_uflow_area(blk, i, j, j, &
                           &blkbc%bc_spec(ibc)%flux_area(j:j), junk)
                   ELSE 
                      CALL compute_vflow_area(blk, i, i, j, &
                           &blkbc%bc_spec(ibc)%flux_area(i:i), junk)
                   END IF
                END IF
             END DO
          END DO
          CALL ga_dgop(MT_F_DBL, blkbc%bc_spec(ibc)%flux_area(cmin:cmax), &
               &cmax - cmin + 1, '+')
       END SELECT
    END DO
  END SUBROUTINE block_compute_bc_flux



  ! ----------------------------------------------------------------
  ! SUBROUTINE apply_hydro_bc
  ! ----------------------------------------------------------------
  SUBROUTINE apply_hydro_bc(blk, bc, dsonly, ds_flux_given)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    TYPE (bc_spec_struct) :: bc
    LOGICAL, INTENT(IN) :: dsonly
    LOGICAL, INTENT(INOUT) :: ds_flux_given

    INTEGER :: x_end, y_end, i, j, k, jj, ii
    INTEGER :: i_beg, i_end, j_beg, j_end
    DOUBLE PRECISION :: input_total

    CHARACTER (LEN=1024) :: buf

    x_end = blk%xmax
    y_end = blk%ymax

    ! set default boundary conditions (zero flux)

    IF (.NOT. dsonly) ds_flux_given = .FALSE.


    ! Get boundary condition values from the table. 

    SELECT CASE(bc%bc_type)
    CASE("TABLE")

       SELECT CASE (bc%bc_kind)
       CASE ("ELEVELO")
          CALL table_interp(current_time%time, bc%table_num, table_input, &
               &2*bc%num_cell_pairs)
       CASE DEFAULT
          CALL table_interp(current_time%time, bc%table_num, table_input, &
               &bc%num_cell_pairs)
       END SELECT
    CASE ("SOURCE", "SINK")
       CALL table_interp(current_time%time,&
            & bc%table_num,&
            & table_input, 1)

    CASE ("BLOCK")

       CALL fillghost_hydro(blk, block(bc%con_block), bc)
       RETURN

    END SELECT

    ! Assign values to specified boundary

    SELECT CASE(bc%bc_loc)

       ! ----------------------------------------------------------------
       ! UPSTREAM (US)
       ! ----------------------------------------------------------------
    CASE("US")
       i=1
       IF (.NOT. block_owns_i(blk, i)) GOTO 50
       SELECT CASE(bc%bc_type)
       CASE("TABLE")
          SELECT CASE(bc%bc_kind)
          CASE("FLUX")
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                input_total = SUM(bc%flux_area(j_beg:j_end))
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      IF (bc%flux_area(jj) .GT. 0.0) THEN
                         blk%uvel(i,jj) = table_input(j)/input_total
                         IF (blk%uvel(i,jj) .GT. 0) THEN
                            blk%vvel(i, jj-1) = 0.0
                            blk%vvel(i, jj) = 0.0
                         ELSE 
                            blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                            blk%vvel(i, jj) = blk%vvel(i+1, jj)
                         END IF
                      ELSE 
                         blk%uvel(i,jj) = 0.0
                         blk%vvel(i,jj-1) = blk%vvel(i+1, jj-1)
                         blk%vvel(i,jj) = blk%vvel(i+1, jj)
                      END IF
                      blk%uvelstar(i,jj) = blk%uvel(i,jj)
                      blk%uvelold(i,jj) =  blk%uvel(i,jj)
                      blk%vvelstar(i,jj) = blk%vvel(i,jj)
                      blk%vvelold(i,jj)  = blk%vvel(i,jj)
                      blk%cell(i+1,jj)%xtype = CELL_BOUNDARY_TYPE
                      blk%cell(i+1,jj)%xbctype = FLOWBC_FLOW
                      IF (dsonly) blk%lud(i+1,jj) = 0.0
                   END IF
                END DO
                IF (block_owns(blk, i, j_beg-1)) THEN
                   blk%vvelstar(i,j_beg-1) = blk%vvel(i,j_beg-1)
                   blk%vvelold(i,j_beg-1)  = blk%vvel(i,j_beg-1)
                END IF
             END DO

          CASE("VELO")
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      blk%uvel(i,jj) = table_input(j)
                      IF (blk%uvel(i,jj) .GT. 0.0) THEN
                         blk%vvel(i, jj-1) = 0.0
                         blk%vvel(i, jj) = 0.0
                      ELSE 
                         blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                         blk%vvel(i, jj) = blk%vvel(i+1, jj)
                      END IF
                      blk%uvelstar(i,jj) = blk%uvel(i,jj)
                      blk%uvelold(i,jj) =  blk%uvel(i,jj)
                      blk%vvelstar(i,jj) = blk%vvel(i,jj)
                      blk%vvelold(i,jj)  = blk%vvel(i,jj)
                      blk%cell(i+1,jj)%xtype = CELL_BOUNDARY_TYPE
                      blk%cell(i+1,jj)%xbctype = FLOWBC_VEL
                      IF (dsonly) blk%lud(i+1,jj) = 0.0
                   END IF
                END DO
                IF (block_owns(blk, i, j_beg-1)) THEN
                   blk%vvelstar(i,j_beg-1) = blk%vvel(i,j_beg-1)
                   blk%vvelold(i,j_beg-1)  = blk%vvel(i,j_beg-1)
                END IF
             END DO

          CASE("ELEV")
             IF (dsonly) RETURN
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      blk%dp(i,jj) = 0.0
                      blk%depth(i,jj) = 2*table_input(j) - &
                           &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                      IF (do_wetdry) blk%depth(i,jj) =  &
                           &MAX(blk%depth(i,jj), dry_zero_depth)
                   END IF
                   blk%depthstar(i,jj) = blk%depth(i,jj)
                   blk%depthold(i,jj) = blk%depth(i,jj)
                   blk%uvel(i,jj) = blk%uvel(i+1,jj)
                   blk%vvel(i,jj) = blk%vvel(i+1,jj)
                   blk%cell(i+1,jj)%xtype = CELL_BOUNDARY_TYPE
                   blk%cell(i+1,jj)%xbctype = FLOWBC_ELEV
                END DO
                IF (block_owns(blk, i, j_beg-1)) THEN
                   blk%vvel(i,j_beg-1) = blk%vvel(i+1,j_beg-1)
                END IF
             END DO
          CASE ("ELEVELO")
             IF (dsonly) RETURN
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      blk%depth(i,jj) = 2*table_input(j*2-1) - &
                           &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                      blk%uvel(i,jj) = table_input(j*2)
                      IF (blk%uvel(i,jj) .GT. 0.0) THEN
                         blk%vvel(i, jj-1) = 0.0
                         blk%vvel(i,jj) = 0.0
                      ELSE
                         blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                         blk%vvel(i,jj) = blk%vvel(i+1,jj)
                      END IF
                   END IF
                   blk%depthstar(i,jj) = blk%depth(i,jj)
                   blk%depthold(i,jj) = blk%depth(i,jj)
                   blk%uvelstar(i,jj) = blk%uvel(i,jj)
                   blk%uvelold(i,jj) =  blk%uvel(i,jj)
                   blk%cell(i+1,jj)%xtype = CELL_BOUNDARY_TYPE
                   blk%cell(i+1,jj)%xbctype = FLOWBC_BOTH
                END DO
                IF (block_owns(blk, i, j_beg-1)) THEN
                   blk%vvelstar(i,j_beg-1) = blk%vvel(i,j_beg-1)
                   blk%vvelold(i,j_beg-1)  = blk%vvel(i,j_beg-1)
                END IF
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT
       CASE ("ZEROG")
          GOTO 100
       CASE DEFAULT
          GOTO 100
       END SELECT

       ! ----------------------------------------------------------------
       ! DOWNSTREAM (DS)
       ! ----------------------------------------------------------------
    CASE("DS")
       i = x_end+1
       SELECT CASE(bc%bc_type)
       CASE("TABLE")

          SELECT CASE(bc%bc_kind)

             !--------------------------------------------------------------------------
             !*** reusing inlet_area and inlet_flow variables here for outlet conditions

          CASE("FLUX") ! can specify the outflow discharge; need to convert to velocity
             ds_flux_given = .TRUE.
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1

                input_total = SUM(bc%flux_area(j_beg:j_end))
                DO jj=j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      IF (bc%flux_area(jj) .GT. 0.0) THEN
                         blk%uvel(i,jj) =  table_input(j)/input_total
                      ELSE 
                         blk%uvel(i,jj) = 0.0
                      END IF
                      blk%uvelstar(i,jj) = blk%uvel(i,jj)
                      blk%uvelold(i,jj) = blk%uvel(i,jj)
                      blk%vvel(i, jj) = blk%vvel(i-1, jj)
                      blk%vvelstar(i,jj) = blk%vvel(i,jj)
                      blk%vvelold(i,jj)  = blk%vvel(i,jj)
                      blk%cell(i-1,jj)%xtype = CELL_BOUNDARY_TYPE
                      blk%cell(i-1,jj)%xbctype = FLOWBC_FLOW
                      IF (dsonly) blk%lud(i-1,jj) = 0.0
                   END IF
                END DO
             END DO

          CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
             ds_flux_given = .TRUE.

             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end	 = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      blk%uvel(i,jj) = table_input(j)
                      blk%uvelstar(i,jj) = blk%uvel(i,jj)
                      blk%uvelold(i,jj) = blk%uvel(i,jj)
                      blk%vvel(i, jj) =  blk%vvel(i-1,jj)
                      blk%vvelstar(i,jj) = blk%vvel(i,jj)
                      blk%vvelold(i,jj)  = blk%vvel(i,jj)
                      blk%cell(i-1,jj)%xtype = CELL_BOUNDARY_TYPE
                      blk%cell(i-1,jj)%xbctype = FLOWBC_VEL
                      IF (dsonly) blk%lud(i-1,jj) = 0.0
                   END IF
                END DO
                IF (block_owns(blk, i, j_beg-1)) THEN
                   blk%vvel(i, j_beg-1) =  blk%vvel(i-1, j_beg-1)
                   blk%vvelstar(i,j_beg-1) = blk%vvel(i,j_beg-1)
                   blk%vvelold(i,j_beg-1)  = blk%vvel(i,j_beg-1)
                END IF
             END DO

          CASE("ELEV")
             IF (dsonly) RETURN
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end	 = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   IF (block_owns(blk, i, jj)) THEN
                      blk%dp(i,jj) = 0.0
                      blk%depth(i,jj) = 2*table_input(j) - &
                           &(blk%depth(i-1,jj) + blk%zbot(i-1,jj)) - blk%zbot(i,jj)
                      IF (do_wetdry) blk%depth(i,jj) =  &
                           &MAX(blk%depth(i,jj), dry_zero_depth)
                       blk%depthstar(i,jj) = blk%depth(i,jj)
                       blk%depthold(i,jj) = blk%depth(i,jj)
                       blk%uvel(i,jj) = blk%uvel(i-1,jj)
                       blk%vvel(i, jj) = blk%vvel(i-1, jj)
                       blk%cell(i-1,jj)%xtype = CELL_BOUNDARY_TYPE
                       blk%cell(i-1,jj)%xbctype = FLOWBC_ELEV
                   END IF
                END DO
             END DO
          CASE ("ELEVELO")
             GOTO 100
          CASE DEFAULT
             GOTO 100
          END SELECT

       CASE ("ZEROG")
          DO j=1,bc%num_cell_pairs
             j_beg = bc%start_cell(j)+1
             j_end = bc%end_cell(j)+1
             DO jj = j_beg, j_end
                IF (block_owns(blk, i, jj)) THEN
                   blk%uvel(i,jj) = blk%uvel(i-1,jj)
                   blk%vvel(i, jj) = blk%vvel(i-1, jj)
                   blk%cell(i-1,jj)%xtype = CELL_BOUNDARY_TYPE
                   blk%cell(i-1,jj)%xbctype = FLOWBC_ZEROG
                END IF
             END DO
          END DO
       CASE DEFAULT
          GOTO 100
       END SELECT

       ! ----------------------------------------------------------------
       ! RIGHT BANK (RB)
       ! ----------------------------------------------------------------
    CASE("RB")
       j = 1
       SELECT CASE(bc%bc_type)
       CASE("TABLE")
          SELECT CASE(bc%bc_kind)
          CASE("FLUX")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                input_total = SUM(bc%flux_area(i_beg:i_end))
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, j)) THEN
                      IF (bc%flux_area(ii) .GT. 0.0) THEN
                         blk%vvel(ii,j) = table_input(k)/input_total
                         IF (blk%vvel(ii,j) .GT. 0.0) THEN
                            blk%uvel(ii,j) = 0.0
                         ELSE
                            blk%uvel(ii,j) =  blk%uvel(ii,j+1)
                         END IF
                      ELSE 
                         blk%vvel(ii,j) = 0.0
                         blk%uvel(ii,j) = blk%uvel(ii,j+1)
                      END IF
                      blk%uvelstar(ii,j) = blk%uvel(ii,j)
                      blk%uvelold(ii,j) =  blk%uvel(ii,j)
                      blk%vvelstar(ii,j) = blk%vvel(ii,j)
                      blk%vvelold(ii,j)  = blk%vvel(ii,j)
                      blk%cell(ii,j+1)%ytype = CELL_BOUNDARY_TYPE
                      blk%cell(ii,j+1)%ybctype = FLOWBC_FLOW
                   END IF
                END DO
             END DO
          CASE("VELO")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, j)) THEN
                      blk%vvel(ii,j) = table_input(k)
                      IF (blk%vvel(ii,j) .GT. 0.0) THEN
                         blk%uvel(ii,j) = 0.0
                      ELSE 
                         blk%uvel(ii,j) = blk%uvel(ii,j+1)
                      END IF
                      blk%uvelstar(ii,j) = blk%uvel(ii,j)
                      blk%uvelold(ii,j) =  blk%uvel(ii,j)
                      blk%vvelstar(ii,j) = blk%vvel(ii,j)
                      blk%vvelold(ii,j)  = blk%vvel(ii,j)
                      blk%cell(ii,j+1)%ytype = CELL_BOUNDARY_TYPE
                      blk%cell(ii,j+1)%ybctype = FLOWBC_VEL
                   END IF
                END DO
             END DO
          CASE("ELEV")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, j)) THEN
                      blk%dp(ii,j) = 0.0
                      blk%depth(ii,j) = 2*table_input(k) - &
                           &(blk%depth(ii,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                      IF (do_wetdry) blk%depth(ii,j) =  &
                           &MAX(blk%depth(ii,j), dry_zero_depth)
                   END IF
                   blk%depthstar(ii,j) = blk%depth(ii,j)
                   blk%depthold(ii,j) = blk%depth(ii,j)
                   blk%uvel(ii,j) = blk%uvel(ii,j+1)
                   blk%vvel(ii,j) = blk%vvel(ii,j+1)
                   blk%uvelstar(ii,j) = blk%uvel(ii,j)
                   blk%uvelold(ii,j) =  blk%uvel(ii,j)
                   blk%vvelstar(ii,j) = blk%vvel(ii,j)
                   blk%vvelold(ii,j)  = blk%vvel(ii,j)
                   blk%cell(ii,j+1)%ytype = CELL_BOUNDARY_TYPE
                   blk%cell(ii,j+1)%ybctype = FLOWBC_ELEV
                END DO
             END DO

          CASE ("ELEVELO")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, j)) THEN
                      blk%depth(ii,j) = 2*table_input(k*2-1) - &
                           &(blk%depth(i,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                      blk%vvel(ii,j) = table_input(k*2)
                      blk%depthstar(ii,j) = blk%depth(ii,j)
                      blk%depthold(ii,j) = blk%depth(ii,j)
                      blk%uvel(ii,j) = blk%uvel(ii,j+1)
                      blk%uvelstar(ii,j) = blk%uvel(ii,j)
                      blk%uvelold(ii,j) =  blk%uvel(ii,j)
                      blk%vvelstar(ii,j) = blk%vvel(ii,j)
                      blk%vvelold(ii,j)  = blk%vvel(ii,j)
                      blk%cell(ii,j+1)%ytype = CELL_BOUNDARY_TYPE
                      blk%cell(ii,j+1)%ybctype = FLOWBC_BOTH
                   END IF
                END DO
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT

       CASE DEFAULT
          GOTO 100
       END SELECT

       ! ----------------------------------------------------------------
       ! LEFT BANK (LB)
       ! ----------------------------------------------------------------
    CASE("LB")
       j = y_end + 1
       SELECT CASE(bc%bc_type)
       CASE("TABLE")
          SELECT CASE(bc%bc_kind)

          CASE("FLUX")

             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                CALL extrapolate_vdepth(blk, i_beg, i_end, j, level=.FALSE.)
                CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, input_total)
                DO ii = i_beg, i_end
                   IF (inlet_area(ii) .GT. 0.0) THEN
                      blk%vvel(ii,j) = table_input(k)/input_total
                      IF (blk%vvel(ii,j) .LT. 0.0) THEN
                         blk%uvel(ii,j) = 0.0
                      ELSE 
                         blk%uvel(ii,j) = blk%uvel(ii,j-1)
                      END IF
                   ELSE 
                      blk%vvel(ii,j) = 0.0
                   END IF
                END DO
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_FLOW
             END DO

          CASE("VELO")

             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end	 = bc%end_cell(k)+1
                CALL extrapolate_vdepth(blk, i_beg, j_end, j, level = .FALSE.)
                DO ii = i_beg, i_end
                   blk%vvel(ii, j) = table_input(k)
                   IF (blk%vvel(ii, j) .LT. 0.0) THEN
                      blk%uvel(ii, j) = 0.0
                   ELSE 
                      blk%uvel(ii, j) = blk%uvel(ii, j-1)
                   END IF
                END DO
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_VEL
             END DO

          CASE("ELEV")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end	 = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   IF (block_owns(blk, ii, j)) THEN
                      blk%dp(ii,j) = 0.0
                      blk%depth(ii,j) = 2*table_input(k) - &
                           &(blk%depth(ii,j-1) + blk%zbot(ii,j-1)) - blk%zbot(ii,j)
                      ! blk%depth(ii,j) = table_input(k) - blk%zbot(ii,j)
                      IF (do_wetdry) blk%depth(ii,j) =  &
                           &MAX(blk%depth(ii,j), dry_zero_depth)
                      blk%depthstar(ii,j) = blk%depth(ii,j)
                      blk%depthold(ii,j) = blk%depth(ii,j)
                      blk%uvel(ii,j) = blk%uvel(ii,j-1)
                      blk%vvel(ii,j) = blk%vvel(ii,j-1)
                      blk%cell(ii,j-1)%ytype = CELL_BOUNDARY_TYPE
                      blk%cell(ii,j-1)%ybctype = FLOWBC_ELEV
                   END IF
                END DO
                IF (block_owns(blk, i_beg-1, j)) THEN
                   blk%uvel(i_beg-1,j) = blk%uvel(i_beg-1,j-1)
                END IF
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT
       CASE ("ZEROG")
          DO k=1,bc%num_cell_pairs
             i_beg = bc%start_cell(j)+1
             i_end = bc%end_cell(j)+1
             DO ii = i_beg, i_end
                IF (block_owns(blk, i, j)) THEN
                   blk%uvel(ii,j) = blk%uvel(ii,j-1)
                   blk%vvel(ii,j) = blk%vvel(ii,j-1)
                   blk%cell(ii,j-1)%ytype = CELL_BOUNDARY_TYPE
                   blk%cell(ii,j-1)%ybctype = FLOWBC_ZEROG
                END IF
             END DO
          END DO
       CASE DEFAULT
          GOTO 100
       END SELECT

    CASE ("IN") 
       IF (dsonly) RETURN
       SELECT CASE(bc%bc_type)

       CASE ("SOURCE", "SINK")

          ! if this is labeled as a "SINK"
          ! negate whatever is in the table

          SELECT  CASE(bc%bc_type)
          CASE ("SINK") 
             table_input = -table_input
          END SELECT

          ! an "ALL" extent applies to the
          ! entire block (it would be nice to do
          ! this when the bcspecs are read

          SELECT CASE (bc%bc_extent)
          CASE ("ALL")
             bc%start_cell(1) = 1
             bc%end_cell(1) = x_end - 1
             bc%start_cell(2) = 1
             bc%end_cell(2) = y_end - 1
          END SELECT

          SELECT CASE (bc%bc_kind)
          CASE("FLUX")

             ! if the source is a flux, we find the
             ! total area over which it applies

             input_total = 0.0
             DO i = bc%start_cell(1), bc%end_cell(1)
                DO j =  bc%start_cell(2), bc%end_cell(2)
                   IF (do_wetdry .AND. .NOT. blk%isdry(i+1,j+1)) &
                        &input_total = input_total + blk%hp1(i+1,j+1)*blk%hp2(i+1,j+1)
                END DO
             END DO
          CASE ("VELO")
             ! if a rate is specified, do not alter
             ! the table value
             input_total = 1.0
          CASE DEFAULT
             GOTO 100
          END SELECT

          DO i = bc%start_cell(1), bc%end_cell(1)
             DO j = bc%start_cell(2), bc%end_cell(2)
                IF (do_wetdry .AND. .NOT. blk%isdry(i+1,j+1)) &
                     &blk%xsource(i+1, j+1) = table_input(1)/input_total
             END DO
          END DO


       CASE ("WALL")
          SELECT CASE(bc%bc_kind)

             ! a UVEL wall blocks the longitudinal
             ! flow at the upstream edge of the
             ! specified cells.
          CASE ("UVEL")

             i = bc%con_block
             DO k = 1, bc%num_cell_pairs
                DO j = bc%start_cell(k),bc%end_cell(k)
                   IF (block_uses(blk, i, j+1)) blk%isdead(i,j+1)%u = .TRUE.
                END DO
             END DO

          CASE ("VVEL")

             j = bc%con_block + 1
             DO k = 1, bc%num_cell_pairs
                DO i = bc%start_cell(k),bc%end_cell(k)
                   IF (block_uses(blk, i+1, j)) blk%isdead(i+1,j)%v = .TRUE.
                END DO
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT

       CASE ("DEAD")

          DO i = bc%start_cell(1), bc%end_cell(1)
             DO j = bc%start_cell(2), bc%end_cell(2)
                IF (block_uses(blk, i+1, j+1)) THEN
                   blk%isdead(i+1, j+1)%p = .TRUE.
                   blk%isdead(i+1, j+1)%u = .TRUE.
                   blk%isdead(i+1, j+1)%v = .TRUE.
                END IF
                IF (block_uses(blk, i+1, j)) THEN
                   blk%isdead(i+1, j  )%v = .TRUE.
                END IF
                IF (block_uses(blk, i, j+1)) THEN
                   blk%isdead(i  , j+1)%u = .TRUE.
                END IF
             END DO
          END DO
       CASE DEFAULT
          GOTO 100
       END SELECT
    CASE DEFAULT
       GOTO 100
    END SELECT

50  CONTINUE

    RETURN
100 CONTINUE
    WRITE(buf,*) " apply_hydro_bc: cannot handle: ", &
         &TRIM(bc%bc_loc), " ", TRIM(bc%bc_type), " ", &
         &TRIM(bc%bc_kind), " "
    CALL error_message(buf, fatal=.TRUE.)
  END SUBROUTINE apply_hydro_bc


  ! ----------------------------------------------------------------
  ! SUBROUTINE compute_uflow_area
  ! ----------------------------------------------------------------
  SUBROUTINE compute_uflow_area(blk, i, jmin, jmax, area, total)

    IMPLICIT NONE
    TYPE (block_struct) :: blk
    INTEGER, INTENT(IN) :: i, jmin, jmax
    DOUBLE PRECISION, INTENT(OUT) :: area(jmin:jmax), total

    INTEGER :: ioff, j
    DOUBLE PRECISION :: d, w

    ioff = 1                      ! by default, do the upstream end.
    IF (i .GE. 2) ioff = -1

    area = 0.0
    DO j = jmin, jmax
       d = 0.5*(blk%depth(i,j) + blk%depth(i+ioff,j))
       w = blk%hu2(i,j)
       IF (do_wetdry .AND. blk%depth(i,j) .LE. dry_depth) THEN
          area(j) = 0.0
       ELSE 
          area(j) = d*w
       END IF
    END DO
    total = SUM(area)
  END SUBROUTINE compute_uflow_area

  ! ----------------------------------------------------------------
  ! SUBROUTINE compute_vflow_area
  ! ----------------------------------------------------------------
  SUBROUTINE compute_vflow_area(blk, imin, imax, j, area, total)

    IMPLICIT NONE
    TYPE (block_struct) :: blk
    INTEGER, INTENT(IN) :: imin, imax, j
    DOUBLE PRECISION, INTENT(OUT) :: area(imin:imax), total

    INTEGER :: joff, i
    DOUBLE PRECISION :: d, w

    joff = 1                      ! by default, do the right bank
    IF (j .GE. 2) joff = -1

    area = 0.0
    DO i = imin, imax
       w = blk%hv1(i,j)
       d = 0.5*(blk%depth(i,j) + blk%depth(i,j+joff))
       IF (do_wetdry .AND. d .LE. dry_depth) THEN
          area(i) = 0.0
       ELSE 
          area(i) = d*w
       END IF
    END DO
    total = SUM(area)
  END SUBROUTINE compute_vflow_area


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_ghost_transfer
  ! ----------------------------------------------------------------
  SUBROUTINE block_ghost_transfer(blk, var, ibeg, iend, jbeg, jend, &
       &cblk, cvar, conibeg, coniend, conjbeg, conjend)

    IMPLICIT NONE
    
    TYPE (block_struct), INTENT(INOUT) :: blk, cblk
    TYPE (block_var), INTENT(INOUT) :: var, cvar
    INTEGER, INTENT(IN) :: ibeg, iend, jbeg, jend
    INTEGER, INTENT(IN) :: conibeg, coniend, conjbeg, conjend

    INTEGER :: i, j, coni, conj
    
    CALL block_var_get_some(cvar, &
         &conibeg, coniend, conjbeg, conjend, &
         &cblk%buffer(conibeg:coniend, conjbeg:conjend))

    coni = conibeg
    DO i = ibeg, iend
       conj = conjbeg
       DO j = jbeg, jend
          IF (block_owns(blk, i, j)) THEN
             var%current(i,j) = cblk%buffer(coni,conj)
             ! WRITE(*, *) i, j, var%current(i,j), coni, conj, cblk%buffer(coni,conj)
          END IF
          conj = conj + 1
       END DO
       coni = coni + 1
    END DO

  END SUBROUTINE block_ghost_transfer


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_build_one_ghost
  ! ----------------------------------------------------------------
  SUBROUTINE block_build_one_ghost(blk, bc, cblk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (block_struct), INTENT(INOUT) :: cblk
    TYPE (bc_spec_struct), INTENT(IN) :: bc

    INTEGER :: k, cells, con_cells, ifcells, jfcells
    INTEGER :: i, ibeg, iend, coni, conibeg, coniend, icoff
    INTEGER :: j, jbeg, jend, conj, conjbeg, conjend, jcoff
    DOUBLE PRECISION :: fract

    DO k = 1, bc%num_cell_pairs

       SELECT CASE(bc%bc_loc)
       CASE("US")
          ibeg = 0 - (nghost - 1)
          iend = ibeg + (nghost - 1)
          coniend = cblk%xmax - 1
          conibeg = coniend - (nghost - 1)
          jbeg = bc%start_cell(k)
          jend = bc%end_cell(k) + 1
          conjbeg = bc%con_start_cell(k)
          conjend = bc%con_end_cell(k) + 1
          cells = jend - jbeg
          con_cells = conjend - conjbeg
          icoff = 0
          jcoff = 1
       CASE ("DS")
          ibeg = blk%xmax + 1
          iend = ibeg + (nghost - 1)
          conibeg = 2
          coniend = conibeg + (nghost - 1)
          jbeg = bc%start_cell(k)
          jend = bc%end_cell(k) + 1
          conjbeg = bc%con_start_cell(k)
          conjend = bc%con_end_cell(k) + 1
          cells = jend - jbeg
          con_cells = conjend - conjbeg
          icoff = 0
          jcoff = 1
       CASE ("RB") 
          jbeg = 0 - (nghost - 1)
          jend = jbeg + (nghost - 1)
          conjend = cblk%ymax - 1
          conjbeg = conjend - (nghost - 1)
          ibeg = bc%start_cell(k)
          iend = bc%end_cell(k) + 1
          conibeg = bc%con_start_cell(k)
          coniend = bc%con_end_cell(k) + 1
          cells = iend - ibeg
          con_cells = coniend - conibeg
          icoff = 1
          jcoff = 0
       CASE ("LB") 
          jbeg = blk%ymax + 1
          jend = jbeg + (nghost - 1)
          conjbeg = 2
          conjend = conjbeg + (nghost - 1)
          ibeg = bc%start_cell(k)
          iend = bc%end_cell(k) + 1
          conibeg = bc%con_start_cell(k)
          coniend = bc%con_end_cell(k) + 1
          cells = iend - ibeg 
          con_cells = coniend - conibeg
          icoff = 1
          jcoff = 0
       END SELECT

       ! fine cells per coarse cell

       SELECT CASE(bc%bc_loc)
       CASE ("US", "DS")
          ifcells = 1
          IF (cells .GE. con_cells) THEN
             jfcells = cells/con_cells
          ELSE 
             jfcells = con_cells/cells
          END IF
       CASE ("LB", "RB")
          IF (cells .GE. con_cells) THEN
             ifcells = cells/con_cells
          ELSE 
             ifcells = con_cells/cells
          END IF
          jfcells = 1
       END SELECT

       IF (cells .EQ. con_cells) THEN

          ! if the number of cells is equal on both sides, we
          ! just copy the ghost cell corners from the connecting
          ! block

          CALL block_ghost_transfer(blk, blk%bv_x_grid, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_x_grid, conibeg, coniend, conjbeg, conjend)
          
          CALL block_ghost_transfer(blk, blk%bv_y_grid, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_y_grid, conibeg, coniend, conjbeg, conjend)
          
          CALL block_ghost_transfer(blk, blk%bv_zbot_grid, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_zbot_grid, conibeg, coniend, conjbeg, conjend)
          
      ELSE IF (cells .GT. con_cells) THEN

          ! if this is the fine block, we need to interpolate
          ! ghost cell corners from the coarse cell corners

          CALL error_message("block_build_ghost: 1-to-many connection not allowed", &
               &fatal=.TRUE.)

!!$                DO i = ibeg, iend
!!$                   coni = conibeg + (i - ibeg)/ifcells
!!$                   DO j = jbeg, jend
!!$                      conj = conjbeg + (j - jbeg)/jfcells
!!$
!!$                      IF (ifcells .EQ. 1) THEN
!!$                         fract = DBLE(MOD(j - jbeg, jfcells))
!!$                         fract = fract/DBLE(jfcells)
!!$                         icoff = 0
!!$                         jcoff = 1
!!$                      ELSE 
!!$                         fract = DBLE(MOD(i - ibeg, ifcells))
!!$                         fract = fract/DBLE(ifcells)
!!$                         icoff = 1
!!$                         jcoff = 0
!!$                      END IF
!!$
!!$                      CALL interpolate_point(fract,&
!!$                           &cblk%x_grid(coni,conj),&
!!$                           &cblk%y_grid(coni,conj),&
!!$                           &cblk%zbot_grid(coni,conj),&
!!$                           &cblk%x_grid(coni+icoff,conj+jcoff),&
!!$                           &cblk%y_grid(coni+icoff,conj+jcoff),&
!!$                           &cblk%zbot_grid(coni+icoff,conj+jcoff),&
!!$                           &blk%x_grid(i,j), &
!!$                           &blk%y_grid(i,j), &
!!$                           &blk%zbot_grid(i,j))
!!$
!!$                   END DO
!!$                END DO

       ELSE IF (cells .LT. con_cells) THEN

          ! if this is the coarse block, we copy selected fine
          ! block corners for the ghost cells

          CALL error_message("block_build_ghost: 1-to-many connection not allowed", &
               &fatal=.TRUE.)
!!$                DO i = ibeg, iend
!!$                   coni = conibeg + (i - ibeg)*ifcells
!!$                   DO j = jbeg, jend
!!$
!!$                      conj = conjbeg + (j - jbeg)*jfcells
!!$                      blk%x_grid(i,j) = cblk%x_grid(coni,conj)
!!$                      blk%y_grid(i,j) = cblk%y_grid(coni,conj)
!!$                      blk%zbot_grid(i,j) = cblk%zbot_grid(coni,conj)
!!$
!!$                   END DO
!!$                END DO
       END IF
    END DO



  END SUBROUTINE block_build_one_ghost



  ! ----------------------------------------------------------------
  ! SUBROUTINE block_build_ghost
  ! ----------------------------------------------------------------
  SUBROUTINE block_build_ghost(iblk)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk
    INTEGER :: num_bc, cblk

                                ! copy ghost cell coordinates for those
                                ! cells connecting with another block

    DO num_bc = 1, block_bc(iblk)%num_bc
       IF (block_bc(iblk)%bc_spec(num_bc)%bc_type .EQ. "BLOCK") THEN
          cblk = block_bc(iblk)%bc_spec(num_bc)%con_block
          CALL block_build_one_ghost(block(iblk),  &
               &block_bc(iblk)%bc_spec(num_bc), block(cblk))
          CALL block_var_put(block(iblk)%bv_x_grid)
          CALL block_var_put(block(iblk)%bv_y_grid)
          CALL block_var_put(block(iblk)%bv_zbot_grid)
       END IF
       CALL block_var_sync()
       CALL block_var_get(block(iblk)%bv_x_grid)
       CALL block_var_get(block(iblk)%bv_y_grid)
       CALL block_var_get(block(iblk)%bv_zbot_grid)
    END DO

  END SUBROUTINE block_build_ghost


  ! ----------------------------------------------------------------
  ! SUBROUTINE connect_indexes
  ! ----------------------------------------------------------------
  SUBROUTINE connect_indexes(blk, bc, cblk, ipair, ibeg, iend, jbeg, jend, &
       &conibeg, coniend, conjbeg, conjend, cells, concells)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (bc_spec_struct), INTENT(IN) :: bc
    TYPE (block_struct), INTENT(IN) :: cblk
    INTEGER, INTENT(IN) :: ipair
    INTEGER, INTENT(OUT) :: ibeg, iend, jbeg, jend
    INTEGER, INTENT(OUT) :: conibeg, coniend, conjbeg, conjend
    INTEGER, INTENT(OUT) :: cells, concells


    SELECT CASE(bc%bc_loc)
    CASE("US")
       ibeg = 2 - nghost
       iend = ibeg + (nghost - 1)
       coniend = cblk%xmax
       conibeg = coniend - (nghost - 1)
       jbeg = bc%start_cell(ipair)+1
       jend = bc%end_cell(ipair)+1
       cells = jend - jbeg + 1
       conjbeg = bc%con_start_cell(ipair)+1
       conjend = bc%con_end_cell(ipair)+1
       concells = conjend - conjbeg + 1
    CASE ("DS")
       iend = blk%xmax + nghost
       ibeg = iend - (nghost - 1)
       conibeg = 2
       coniend = conibeg + (nghost - 1)
       jbeg = bc%start_cell(ipair)+1
       jend = bc%end_cell(ipair)+1
       cells = jend - jbeg + 1
       conjbeg = bc%con_start_cell(ipair)+1
       conjend = bc%con_end_cell(ipair)+1
       concells = conjend - conjbeg + 1
    CASE ("RB")
       ibeg = bc%start_cell(ipair)+1
       iend = bc%end_cell(ipair)+1
       cells = iend - ibeg + 1
       conibeg = bc%con_start_cell(ipair)+1
       coniend = bc%con_end_cell(ipair)+1
       concells = coniend - conibeg + 1
       jbeg = 2 - nghost
       jend = jbeg + (nghost - 1)
       conjend = cblk%ymax
       conjbeg = conjend - (nghost - 1)
    CASE ("LB")
       ibeg = bc%start_cell(ipair)+1
       iend = bc%end_cell(ipair)+1
       cells = iend - ibeg + 1
       conibeg = bc%con_start_cell(ipair)+1
       coniend = bc%con_end_cell(ipair)+1
       concells = coniend - conibeg + 1
       jbeg = blk%ymax + nghost
       jend = jbeg + (nghost - 1)
       conjbeg = 2 
       conjend = conjbeg + (nghost - 1)
    CASE DEFAULT
       CALL error_message("This should never happen in connect_indexes", &
            &fatal=.TRUE.)
    END SELECT

  END SUBROUTINE connect_indexes


  ! ----------------------------------------------------------------
  ! SUBROUTINE fillghost
  ! ----------------------------------------------------------------
  SUBROUTINE fillghost(iblock)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock
    INTEGER :: i, ibeg, iend, coni, conibeg, coniend
    INTEGER :: j, jbeg, jend, conj, conjbeg, conjend
    INTEGER :: k, con_block
    INTEGER :: ig, jg
    INTEGER :: num_bc, cells, concells, ifcells, jfcells

    ! cell-centered quantities
    ! for extrapolated cells, use
    ! parameters from the neighboring real
    ! cell (metrics are computed elsewhere)


    DO ig = 1, nghost
       IF (block_owns_i(block(iblock), 2-ig)) THEN
          block(iblock)%eddy(2-ig,:) = block(iblock)%eddy(2,:)
          block(iblock)%kx_diff(2-ig,:) = block(iblock)%kx_diff(2,:)
          block(iblock)%ky_diff(2-ig,:) = block(iblock)%ky_diff(2,:)
          block(iblock)%chezy(2-ig,:) = block(iblock)%chezy(2,:)
       END IF
       IF (block_owns_i(block(iblock), block(iblock)%xmax+ig)) THEN
          block(iblock)%eddy(block(iblock)%xmax+ig,:) = &
               &block(iblock)%eddy(block(iblock)%xmax,:)
          block(iblock)%kx_diff(block(iblock)%xmax+ig,:) = &
               &block(iblock)%kx_diff(block(iblock)%xmax,:)
          block(iblock)%ky_diff(block(iblock)%xmax+ig,:) = &
               &block(iblock)%ky_diff(block(iblock)%xmax,:)
          block(iblock)%chezy(block(iblock)%xmax+ig,:) = &
               &block(iblock)%chezy(block(iblock)%xmax,:)
       END IF
    END DO
    DO jg = 1, nghost 
       IF (block_owns_j(block(iblock), 2-jg)) THEN
          block(iblock)%eddy(:, 2-jg) = block(iblock)%eddy(:,2)
          block(iblock)%kx_diff(:, 2-jg) = block(iblock)%kx_diff(:,2)
          block(iblock)%ky_diff(:, 2-jg) = block(iblock)%ky_diff(:,2)
          block(iblock)%chezy(:, 2-jg) = block(iblock)%chezy(:,2)
       END IF
       IF (block_owns_j(block(iblock), block(iblock)%ymax+ig)) THEN
          block(iblock)%eddy(:,block(iblock)%ymax+ig) = &
               &block(iblock)%eddy(:,block(iblock)%ymax)
          block(iblock)%kx_diff(:,block(iblock)%ymax+ig) = &
               &block(iblock)%kx_diff(:,block(iblock)%ymax)
          block(iblock)%ky_diff(:,block(iblock)%ymax+ig) = &
               &block(iblock)%ky_diff(:,block(iblock)%ymax)
          block(iblock)%chezy(:,block(iblock)%ymax+ig) = &
               &block(iblock)%chezy(:,block(iblock)%ymax)
       END IF
    END DO

    CALL block_var_put(block(iblock)%bv_eddy)
    CALL block_var_put(block(iblock)%bv_kx_diff)
    CALL block_var_put(block(iblock)%bv_ky_diff)
    CALL block_var_put(block(iblock)%bv_chezy)

    ! copy ghost cell metrics and
    ! parameters from connecting block

    ! FIXME: block connections

    DO num_bc = 1, block_bc(iblock)%num_bc

       SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type) 
       CASE ("BLOCK")
          con_block = block_bc(iblock)%bc_spec(num_bc)%con_block

          DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs

             CALL connect_indexes(block(iblock), block_bc(iblock)%bc_spec(num_bc),&
                  &block(con_block), k, ibeg, iend, jbeg, jend, &
                  &conibeg, coniend, conjbeg, conjend, cells, concells)

             SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
             CASE ("US", "DS")
                ifcells = 1
                IF (cells .GE. concells) THEN
                   jfcells = cells/concells
                ELSE 
                   jfcells = concells/cells
                END IF
             CASE ("LB", "RB")
                IF (cells .GE. concells) THEN
                   ifcells = cells/concells
                ELSE 
                   ifcells = concells/cells
                END IF
                jfcells = 1
             END SELECT


             ! FIXME: block connections
             IF (cells .EQ. concells) THEN

                ! grid metrics should be computed correctly for ghost
                ! cells, there's no need to transfer them.


!!$                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_hp1, &
!!$                     &ibeg, iend, jbeg, jend, &
!!$                     &block(con_block), block(con_block)%bv_hp1, &
!!$                     &conibeg, coniend, conjbeg, conjend)
!!$                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_hp2, &
!!$                     &ibeg, iend, jbeg, jend, &
!!$                     &block(con_block), block(con_block)%bv_hp2, &
!!$                     &conibeg, coniend, conjbeg, conjend)
!!$                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_hv1, &
!!$                     &ibeg, iend, jbeg, jend, &
!!$                     &block(con_block), block(con_block)%bv_hv1, &
!!$                     &conibeg, coniend, conjbeg, conjend)
!!$                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_hv2, &
!!$                     &ibeg, iend, jbeg-1, jend, &
!!$                     &block(con_block), block(con_block)%bv_hv2, &
!!$                     &conibeg, coniend, conjbeg-1, conjend)
!!$                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_gp12, &
!!$                     &ibeg, iend, jbeg, jend, &
!!$                     &block(con_block), block(con_block)%bv_gp12, &
!!$                     &conibeg, coniend, conjbeg, conjend)

                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_eddy, &
                     &ibeg, iend, jbeg, jend, &
                     &block(con_block), block(con_block)%bv_eddy, &
                     &conibeg, coniend, conjbeg, conjend)
                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_kx_diff, &
                     &ibeg, iend, jbeg, jend, &
                     &block(con_block), block(con_block)%bv_kx_diff, &
                     &conibeg, coniend, conjbeg, conjend)
                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_ky_diff, &
                     &ibeg, iend, jbeg, jend, &
                     &block(con_block), block(con_block)%bv_ky_diff, &
                     &conibeg, coniend, conjbeg, conjend)
                CALL block_ghost_transfer(block(iblock), block(iblock)%bv_chezy, &
                     &ibeg, iend, jbeg, jend, &
                     &block(con_block), block(con_block)%bv_chezy, &
                     &conibeg, coniend, conjbeg, conjend)

             ELSE 

                DO i = ibeg, iend
                   coni = conibeg + (i - ibeg)/ifcells
                   DO j = jbeg, jend
                      conj = conjbeg + (j - jbeg)/jfcells
                      ! do not copy metrics, they are calculated
!!$                      block(iblock)%eddy(i,j) = block(con_block)%eddy(coni,conj)
!!$                      block(iblock)%kx_diff(i,j) = block(con_block)%kx_diff(coni,conj)
!!$                      block(iblock)%ky_diff(i,j) = block(con_block)%ky_diff(coni,conj)
!!$                      block(iblock)%chezy(i,j) = block(con_block)%chezy(coni,conj)
                   END DO
                END DO

             END IF
          END DO
       END SELECT
    END DO

    CALL block_var_put(block(iblock)%bv_eddy)
    CALL block_var_put(block(iblock)%bv_kx_diff)
    CALL block_var_put(block(iblock)%bv_ky_diff)
    CALL block_var_put(block(iblock)%bv_chezy)
    CALL block_var_sync()
    CALL block_var_get(block(iblock)%bv_eddy)
    CALL block_var_get(block(iblock)%bv_kx_diff)
    CALL block_var_get(block(iblock)%bv_ky_diff)
    CALL block_var_get(block(iblock)%bv_chezy)

  END SUBROUTINE fillghost


  ! ----------------------------------------------------------------
  ! SUBROUTINE fillghost_hydro
  ! ----------------------------------------------------------------
  SUBROUTINE fillghost_hydro(blk, cblk, bc)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (block_struct), INTENT(INOUT) :: cblk
    TYPE (bc_spec_struct), INTENT(IN) :: bc

    INTEGER :: n, cells, concells, ifcells, jfcells
    INTEGER :: i, j, iu, ju
    INTEGER :: conjbeg, conjend, conj, jbeg, jend
    INTEGER :: conibeg, coniend, coni, ibeg, iend
    LOGICAL :: side
    DOUBLE PRECISION :: carea, cflux
    CHARACTER (LEN=1024) :: msg

    DO n = 1, bc%num_cell_pairs

       CALL connect_indexes(blk, bc,&
            &cblk, n, ibeg, iend, jbeg, jend, &
            &conibeg, coniend, conjbeg, conjend, cells, concells)

       SELECT CASE (bc%bc_loc)
       CASE ("US", "DS")
          ifcells = 1
          IF (cells .GE. concells) THEN
             jfcells = cells/concells
          ELSE 
             jfcells = concells/cells
          END IF
          side = .FALSE.
       CASE ("LB", "RB")
          IF (cells .GE. concells) THEN
             ifcells = cells/concells
          ELSE 
             ifcells = concells/cells
          END IF
          jfcells = 1
          side = .TRUE.
       END SELECT

       IF (cells .EQ. concells) THEN

          ! if the same number of cells are on both sides of the
          ! boundary, just copy the state variables from the connected
          ! cells

          CALL block_ghost_transfer(&
               &blk, blk%bv_uvel, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_uvel, conibeg, coniend, conjbeg, conjend)

          CALL block_ghost_transfer(&
               &blk, blk%bv_vvel, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_vvel, conibeg, coniend, conjbeg, conjend)

          CALL block_ghost_transfer(&
               &blk, blk%bv_depth, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_depth, conibeg, coniend, conjbeg, conjend)

          CALL block_ghost_transfer(&
               &blk, blk%bv_eddy, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_eddy, conibeg, coniend, conjbeg, conjend)

          CALL block_ghost_transfer(blk, blk%bv_isdry, ibeg, iend, jbeg, jend, &
               &cblk, cblk%bv_isdry, conibeg, coniend, conjbeg, conjend)

          ! FIXME, fill isdry()

!!$       ELSE IF (cells .GT. concells) THEN
!!$
!!$          ! this block has more cells than the neighboring block, we
!!$          ! need to interpolate some of the variables from the coarse
!!$          ! block
!!$
!!$          ! copy the first cross vel value
!!$
!!$          IF (.NOT. side) THEN
!!$             blk%vvel(ibeg:iend,jbeg-1) = cblk%vvel(conibeg:coniend,conjbeg-1)
!!$          ELSE 
!!$             blk%uvel(ibeg-1,jbeg:jend) = cblk%uvel(conibeg-1,conjbeg:conjend)
!!$          END IF
!!$
!!$          ! we need to do the depth first so
!!$          ! that flow area calculations are
!!$          ! accurate
!!$
!!$          DO i = ibeg, iend
!!$             coni = conibeg + (i - ibeg)/ifcells
!!$             DO j = jbeg, jend
!!$                conj = conjbeg + (j - jbeg)/jfcells
!!$
!!$                ! all the fine ghost cells are dry if
!!$                ! the neighboring coarse cell is dry
!!$
!!$                blk%isdry(i,j) = cblk%isdry(coni,conj)
!!$
!!$                blk%wsel(i,j) = wsinterp(cblk, blk%x(i,j), blk%y(i,j), coni, conj)
!!$                blk%depth(i,j) = blk%wsel(i,j) - blk%zbot(i,j)
!!$                IF (do_wetdry) THEN
!!$                   blk%wsel(i,j) = MAX(blk%wsel(i,j), blk%zbot(i,j))
!!$                   blk%depth(i,j) = MAX(blk%depth(i,j), dry_zero_depth)
!!$                   blk%isdry(i,j) = blk%isdry(i,j) .OR. (blk%depth(i,j) .LE. dry_depth)
!!$                END IF
!!$
!!$
!!$                ! linearly interpolate cross vel within the
!!$                ! neighboring coarse cell
!!$
!!$                IF (.NOT. side) THEN
!!$                   blk%vvel(i,j) = (cblk%vvel(coni, conj) - cblk%vvel(coni, conj-1))*&
!!$                        &(DBLE(MOD(j - jbeg + 1, jfcells)))/DBLE(jfcells) + cblk%vvel(coni, conj-1)
!!$                ELSE 
!!$                   blk%uvel(i,j) = (cblk%uvel(coni, conj) - cblk%uvel(coni-1, conj))*&
!!$                        &(DBLE(MOD(i - ibeg + 1, ifcells)))/DBLE(ifcells) + cblk%uvel(coni-1, conj)
!!$                END IF
!!$             END DO
!!$          END DO
!!$
!!$          ! now we can calculate the total local
!!$          ! flow area and u
!!$
!!$          IF (.NOT. side) THEN
!!$             DO i = ibeg, iend
!!$                coni = conibeg + (i - ibeg)/ifcells
!!$                DO conj = conjbeg, conjend
!!$                   IF (i .GT. 2) THEN
!!$                      cflux = uflux(cblk, coni, conj, conj)
!!$                   ELSE
!!$                      cflux = uflux(cblk, coni, conj, conj)
!!$                   END IF
!!$                   ju = jbeg + (conj - conjbeg)*jfcells
!!$                   carea = uarea(blk, i, ju, ju + jfcells - 1)
!!$                   DO j = ju, ju + jfcells - 1
!!$                      IF (carea .GT. 0.0) THEN
!!$                         blk%uvel(i,j) = cflux/carea
!!$                      ELSE 
!!$                         blk%uvel(i,j) = 0.0
!!$                      END IF
!!$                      ! WRITE (*,101) i, j, coni, conj, cflux, carea
!!$                      ! 101 FORMAT('In fillghost_hydro: ', 4(1X, I3), 2(1X, E15.6))
!!$                   END DO
!!$                END DO
!!$             END DO
!!$          ELSE
!!$             DO j = jbeg, jend
!!$                conj = conjbeg + (j - jbeg)/jfcells
!!$                DO coni = conibeg, coniend
!!$                   IF (j .GT. 2) THEN
!!$                      cflux = vflux(cblk, coni, coni, conj-1)
!!$                   ELSE
!!$                      cflux = vflux(cblk, coni, coni, conj)
!!$                   END IF
!!$                   iu = ibeg + (coni - conibeg)*ifcells
!!$                   carea = varea(blk, iu, iu  + ifcells - 1, j)
!!$                   DO i = iu, iu + ifcells - 1
!!$                      IF (carea .GT. 0.0) THEN
!!$                         blk%vvel(i,j) = cflux/carea
!!$                      ELSE 
!!$                         blk%vvel(i,j) = 0.0
!!$                      END IF
!!$                   END DO
!!$                END DO
!!$             END DO
!!$          END IF
!!$
!!$       ELSE IF (cells .LT. concells) THEN
!!$
!!$          ! this block is the coarse block; we should be able to copy
!!$          ! most of what we need
!!$
!!$
!!$          ! this ghost cell is dry if all
!!$          ! neighboring cells are dry, so
!!$          ! initialize all here
!!$          blk%isdry(ibeg:iend,jbeg:jend) = .FALSE.
!!$
!!$          ! copy the first cross vel value
!!$
!!$          IF (.NOT. side) THEN
!!$             blk%vvel(ibeg:iend, jbeg-1) = cblk%vvel(conibeg:coniend, conjbeg-1)
!!$          ELSE
!!$             blk%uvel(ibeg-1, jbeg:jend) = cblk%uvel(conibeg-1, conjbeg:conjend)
!!$          END IF
!!$
!!$          DO i = ibeg, iend
!!$             coni = conibeg + (i - ibeg)*ifcells
!!$             DO j = jbeg, jend
!!$                conj = conjbeg + (j - jbeg)*jfcells
!!$
!!$                ! interpolate depth at the ghost cell
!!$                ! centroid, using the closest
!!$                ! neighboring cell as a hint
!!$
!!$                blk%wsel(i,j) = wsinterp(cblk, blk%x(i,j), blk%y(i,j), &
!!$                     &coni, conj + jfcells/2)
!!$                blk%depth(i,j) = blk%wsel(i,j) - blk%zbot(i,j)
!!$                IF (do_wetdry) THEN
!!$                   blk%wsel(i,j) = MAX(blk%wsel(i,j), blk%zbot(i,j))
!!$                   blk%depth(i,j) = MAX(blk%depth(i,j), dry_zero_depth)
!!$                END IF
!!$
!!$                IF (.NOT. side) THEN
!!$
!!$                   ! copy next cross v value
!!$                   blk%vvel(i,j) = cblk%vvel(coni, conj + jfcells - 1)
!!$
!!$                   ! compute u using fluxes
!!$                   IF (i .GT. 2) THEN
!!$                      cflux = uflux(cblk, coni, conj, conj + jfcells - 1)
!!$                   ELSE 
!!$                      cflux = uflux(cblk, coni, conj, conj + jfcells - 1)
!!$                   END IF
!!$                   carea = uarea(blk, i, j, j)
!!$                   IF (carea .GT. 0.0) THEN
!!$                      blk%uvel(i,j) = cflux/carea
!!$                   ELSE 
!!$                      blk%uvel(i,j) = 0.0
!!$                   END IF
!!$
!!$                ELSE 
!!$                   blk%uvel(i,j) = cblk%uvel(coni + ifcells - 1, conj)
!!$
!!$                   carea = varea(blk, i, i, j)
!!$                   IF (jbeg .GT. 2) THEN
!!$                      cflux = vflux(cblk, coni, coni + ifcells - 1, conj-1)
!!$                   ELSE 
!!$                      cflux = vflux(cblk, coni, coni + ifcells - 1, conj)
!!$                   END IF
!!$                   IF (carea .GT. 0.0) THEN
!!$                      blk%vvel(i,j) = cflux/carea
!!$                   ELSE 
!!$                      blk%vvel(i,j) = 0.0
!!$                   END IF
!!$                END IF
!!$
!!$                ! check wetdry, all neighboring cells
!!$                ! must be dry for this cell to be dry
!!$                blk%isdry(i,j) = (blk%isdry(i,j) .OR. cblk%isdry(coni, conj))
!!$
!!$             END DO
!!$          END DO
       END IF

       ! Not sure why we're worried about old values here.  Old values
       ! in ghost cells should not be used. Star values are needed however. 
       
       DO i = ibeg-1, iend
          DO j = jbeg-1, jend
             IF (block_uses(blk, i, j)) THEN
                blk%uvelold(i,j) = blk%uvel(i,j)
                blk%uvelstar(i,j) = blk%uvel(i,j)
                blk%vvelold(i,j) = blk%vvel(i,j)
                blk%vvelstar(i,j) = blk%vvel(i,j)
                blk%depthold(i,j) = blk%depth(i,j)
                blk%depthstar(i,j) = blk%depth(i,j)
                blk%dp(i,j) = 0.0
             END IF
          END DO
       END DO

       IF (.NOT. side) THEN
          IF (ibeg .LT. 2) THEN
             i = 2
          ELSE 
             i = blk%xmax
          END IF
          DO j = jbeg, jend
             IF (block_uses(blk, i, j)) THEN
                blk%cell(i,j)%xtype = CELL_NORMAL_TYPE
             END IF
          END DO
       ELSE 
          IF (jbeg .LT. 2) THEN
             j = 2
          ELSE 
             j = blk%ymax
          END IF
          DO i = ibeg, iend
             IF (block_uses(blk, i, j)) THEN
                blk%cell(i,j)%ytype = CELL_NORMAL_TYPE
             END IF
          END DO
       END IF
    END DO

  END SUBROUTINE fillghost_hydro


END MODULE block_hydro_bc
