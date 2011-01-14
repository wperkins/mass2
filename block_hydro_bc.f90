! ----------------------------------------------------------------
! MODULE block_hydro_bc
!
! Routines to apply boundary condi 
! ----------------------------------------------------------------
MODULE block_hydro_bc

  USE config
  USE block_module
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

          x1 = block(bc%block)%x_grid(i, j)
          y1 = block(bc%block)%y_grid(i, j)
          x2 = block(conbc%block)%x_grid(coni, conj)
          y2 = block(conbc%block)%y_grid(coni, conj)

          rdist = distance(x1, y1, x2, y2)

          SELECT CASE (bc%bc_loc)
          CASE ("US","DS")
             x2 = block(bc%block)%x_grid(i, j+1)
             y2 = block(bc%block)%y_grid(i, j+1)
          CASE ("LB","RB")
             x2 = block(bc%block)%x_grid(i+1, j)
             y2 = block(bc%block)%y_grid(i+1, j)
          END SELECT

          rdist = rdist/distance(x1, y1, x2, y2)

          IF (rdist .GT. 0.01) THEN
             IF (debug) THEN 
                WRITE (msg,*) '   REJECT: cell pair ', n, ' starting point different'
                CALL status_message(msg)
             END IF
             RETURN
          END IF

          SELECT CASE (bc%bc_loc)
          CASE ("US","DS")
             j = bc%end_cell(n)
             x1 = block(bc%block)%x_grid(i, j+1)
             y1 = block(bc%block)%y_grid(i, j+1)
             conj = conbc%end_cell(n)
             x2 = block(conbc%block)%x_grid(coni, conj+1)
             y2 = block(conbc%block)%y_grid(coni, conj+1)
          CASE ("LB","RB")
             i =  bc%end_cell(n)
             x1 = block(bc%block)%x_grid(i+1, j)
             y1 = block(bc%block)%y_grid(i+1, j)
             coni = conbc%end_cell(n)
             x2 = block(conbc%block)%x_grid(coni+1, conj)
             y2 = block(conbc%block)%y_grid(coni+1, conj)
          END SELECT

          rdist = distance(x1, y1, x2, y2)

          x2 = block(bc%block)%x_grid(i, j)
          y2 = block(bc%block)%y_grid(i, j)
          rdist = rdist/distance(x1, y1, x2, y2)

          IF (rdist .GT. 0.01) THEN
             IF (debug) THEN 
                WRITE (msg,*) '   REJECT: cell pair ', n, ' ending point different'
                CALL status_message(msg)
             END IF
             RETURN
          END IF

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
    CALL ga_sync()
    CALL block_var_get(block(iblock)%bv_eddy)
    CALL block_var_get(block(iblock)%bv_kx_diff)
    CALL block_var_get(block(iblock)%bv_ky_diff)
    CALL block_var_get(block(iblock)%bv_chezy)

    ! copy ghost cell metrics and
    ! parameters from connecting block

    ! FIXME: block connections

    DO num_bc = 1, block_bc(iblock)%num_bc

       SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type) 
       CASE ("BLOCK")
          con_block = block_bc(iblock)%bc_spec(num_bc)%con_block

          DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
             SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
             CASE("US")
                ibeg = 2 - nghost
                iend = ibeg + (nghost - 1)
                coniend = block(con_block)%xmax
                conibeg = coniend - (nghost - 1)
                jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
                jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
                cells = jend - jbeg + 1
                conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
                conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
                concells = conjend - conjbeg + 1
             CASE ("DS")
                iend = block(iblock)%xmax + nghost
                ibeg = iend - (nghost - 1)
                conibeg = 2
                coniend = conibeg + (nghost - 1)
                jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
                jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
                cells = jend - jbeg + 1
                conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
                conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
                concells = conjend - conjbeg + 1
             CASE ("RB")
                ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
                iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
                cells = iend - ibeg + 1
                conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
                coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
                concells = coniend - conibeg + 1
                jbeg = 2 - nghost
                jend = jbeg + (nghost - 1)
                conjend = block(con_block)%ymax + nghost
                conjbeg = conjend - (nghost - 1)
             CASE ("LB")
                ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
                iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
                cells = iend - ibeg + 1
                conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
                coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
                concells = coniend - conibeg + 1
                jbeg = block(iblock)%ymax + nghost
                jend = jbeg + (nghost - 1)
                conjbeg = 2 
                conjend = conjbeg + (nghost - 1)
             CASE DEFAULT
                ! FIXME: should never happen  need to crash here
             END SELECT

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

                coni = conibeg
                DO i = ibeg, iend
                   conj = conjbeg
                   DO j = jbeg, jend

!!$                      block(iblock)%hp1(i,j) = block(con_block)%hp1(coni,conj)
!!$                      block(iblock)%hp2(i,j) = block(con_block)%hp2(coni,conj)
!!$                      block(iblock)%hv1(i,j-1) = block(con_block)%hv1(coni,conj-1)
!!$                      block(iblock)%hv2(i,j-1) = block(con_block)%hv2(coni,conj-1)
!!$                      block(iblock)%hv1(i,j) = block(con_block)%hv1(coni,conj)
!!$                      block(iblock)%hv2(i,j) = block(con_block)%hv2(coni,conj)
!!$
!!$                      ! do not copy hu1,hu2 - they are calculated
!!$
!!$                      block(iblock)%gp12(i,j) = block(con_block)%gp12(coni,conj)
!!$                      block(iblock)%gp12(i,j) = block(con_block)%gp12(coni,conj)
!!$
!!$                      block(iblock)%eddy(i,j) = block(con_block)%eddy(coni,conj)
!!$                      block(iblock)%kx_diff(i,j) = block(con_block)%kx_diff(coni,conj)
!!$                      block(iblock)%ky_diff(i,j) = block(con_block)%ky_diff(coni,conj)
!!$                      block(iblock)%chezy(i,j) = block(con_block)%chezy(coni,conj)
                      conj = conj + 1
                   END DO
                   coni = coni + 1
                END DO

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

  END SUBROUTINE fillghost

  ! ----------------------------------------------------------------
  ! SUBROUTINE default_hydro_bc
  ! ----------------------------------------------------------------
  SUBROUTINE default_hydro_bc(blk)

    IMPLICIT NONE

    TYPE (block_struct) :: blk
    INTEGER :: x_beg, y_beg, x_end, y_end, i, j, ig
    INTEGER :: imin, imax, jmin, jmax

    CALL block_owned_window(blk, imin, imax, jmin, jmax)

    x_beg = 2
    y_beg = 2
    x_end = blk%xmax
    y_end = blk%ymax

    imin = MAX(imin, x_beg)
    jmin = MAX(jmin, y_beg)
    imax = MIN(imax, x_end)
    jmax = MIN(jmax, y_end)

    IF (block_owns_i(blk, x_beg)) THEN
       blk%cell(x_beg,:)%xtype = CELL_BOUNDARY_TYPE
       blk%cell(x_beg,:)%xbctype = FLOWBC_NONE
    END IF
    IF (block_owns_i(blk, x_end)) THEN
       blk%cell(x_end,:)%xtype = CELL_BOUNDARY_TYPE
       blk%cell(x_end,:)%xbctype = FLOWBC_NONE
    END IF

    IF (block_owns_j(blk, y_beg)) THEN
       blk%cell(:,y_beg)%ytype = CELL_BOUNDARY_TYPE
       blk%cell(:,y_beg)%ybctype = FLOWBC_NONE
    END IF
    IF (block_owns_j(blk, y_end)) THEN
       blk%cell(:,y_end)%ytype = CELL_BOUNDARY_TYPE
       blk%cell(:,y_end)%ybctype = FLOWBC_NONE
    END IF

    DO ig = 1, nghost

       i = x_beg - ig
       IF (block_owns_i(blk, i)) THEN
          blk%uvel(i,:) = 0.0
          blk%vvel(i,:) = blk%vvel(x_beg,:)
          CALL extrapolate_udepth(blk, i, jmin, jmax, level=.FALSE.)
          blk%eddy(i,:) = blk%eddy(x_beg,:)
       END IF

       i = blk%xmax + ig
       IF (block_owns_i(blk, i)) THEN
          blk%uvel(i,:) = 0.0
          blk%vvel(i,:) = blk%vvel(x_end,:)
          CALL extrapolate_udepth(blk, i, jmin, jmax, level=.FALSE.)
          blk%eddy(i,:) = blk%eddy(x_end,:)
       END IF

       CALL block_var_put(blk%bv_uvel)
       CALL block_var_put(blk%bv_vvel)
       CALL ga_sync()
       CALL block_var_get(blk%bv_uvel)
       CALL block_var_get(blk%bv_vvel)

       j = y_beg - ig
       IF (block_owns_j(blk, j)) THEN
          blk%uvel(:,j) = blk%uvel(:,y_beg)
          blk%vvel(:,j) = 0.0
          CALL extrapolate_vdepth(blk, imin, imax, j, level=.FALSE.)
          blk%eddy(:,j) = blk%eddy(:,y_beg)
       END IF

       j = blk%ymax + ig
       IF (block_owns_j(blk, j)) THEN
          blk%uvel(:,j) = blk%uvel(:,y_end)
          blk%vvel(:,j) = 0.0
          CALL extrapolate_vdepth(blk, imin, imax, j, level=.FALSE.)
          blk%eddy(:,j) = blk%eddy(:,y_end)
       END IF

       CALL block_var_put(blk%bv_uvel)
       CALL block_var_put(blk%bv_vvel)
       CALL ga_sync()
       CALL block_var_get(blk%bv_uvel)
       CALL block_var_get(blk%bv_vvel)

    END DO

    CALL block_var_put(blk%bv_eddy)
    CALL ga_sync()
    CALL block_var_put(blk%bv_eddy)

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
                           &blkbc%bc_spec(ibc)%flux_area(j:j), junk)
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

       ! FIXME: CALL fillghost_hydro(blk, block(bc%con_block), bc)
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
                ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level=.FALSE.)
                CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, input_total)
                DO jj = j_beg, j_end
                   IF (inlet_area(jj) .GT. 0.0) THEN
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
                END DO
                blk%uvelstar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%uvelold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
                blk%vvelstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
                blk%vvelold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
                blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_FLOW
                IF (dsonly) blk%lud(i+1,j) = 0.0
             END DO

          CASE("VELO")
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level=.FALSE.)
                DO jj = j_beg, j_end
                   blk%uvel(i,jj) = table_input(j)
                   IF (blk%uvel(i,jj) .GT. 0.0) THEN
                      blk%vvel(i, jj-1) = 0.0
                      blk%vvel(i, jj) = 0.0
                   ELSE 
                      blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                      blk%vvel(i, jj) = blk%vvel(i+1, jj)
                   END IF
                END DO
                blk%uvelstar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%uvelold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
                blk%vvelstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
                blk%vvelold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
                blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_VEL
                IF (dsonly) blk%lud(i+1,j) = 0.0
             END DO

          CASE("ELEV")
             IF (dsonly) RETURN
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                DO jj = j_beg, j_end
                   blk%dp(i,jj) = 0.0
                   blk%depth(i,jj) = 2*table_input(j) - &
                        &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                   IF (do_wetdry) blk%depth(i,jj) =  &
                        &MAX(blk%depth(i,jj), dry_zero_depth)
                END DO
                blk%depthstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
                blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
                blk%uvel(i,j_beg:j_end) = blk%uvel(i+1,j_beg:j_end)
                blk%vvel(i,j_beg-1:j_end) = blk%vvel(i+1,j_beg-1:j_end)
                blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_ELEV
             END DO
          CASE ("ELEVELO")
             IF (dsonly) RETURN
             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end = bc%end_cell(j)+1
                DO jj = j_beg, j_end
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
                END DO
                blk%depthstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
                blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
                blk%uvelstar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%uvelold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
                blk%vvelstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
                blk%vvelold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
                blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_BOTH
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

                ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
                CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, input_total)

                DO jj=j_beg, j_end
                   IF (inlet_area(jj) .GT. 0.0) THEN
                      blk%uvel(i,jj) =  table_input(j)/input_total
                   ELSE 
                      blk%uvel(i,jj) = 0.0
                   END IF
                END DO
                blk%uvelstar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%uvelold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%vvel(i, j_beg:j_end) = blk%vvel(i-1, j_beg:j_end)
                blk%vvelstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
                blk%vvelold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
                blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_FLOW
                IF (dsonly) blk%lud(i-1,j_beg:j_end) = 0.0
             END DO

          CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
             ds_flux_given = .TRUE.

             DO j=1,bc%num_cell_pairs
                j_beg = bc%start_cell(j)+1
                j_end	 = bc%end_cell(j)+1
                ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
                blk%uvel(i,j_beg:j_end) = table_input(j)
                blk%uvelstar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%uvelold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
                blk%vvel(i, j_beg-1:j_end) =  blk%vvel(i-1, j_beg-1:j_end)
                blk%vvelstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
                blk%vvelold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
                blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
                blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_VEL
                IF (dsonly) blk%lud(i-1,j_beg:j_end) = 0.0
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
             ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
             blk%uvel(i,j_beg:j_end) = blk%uvel(i-1,j_beg:j_end)
             blk%vvel(i, j_beg:j_end) = blk%vvel(i-1, j_beg:j_end)
             blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
             blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_ZEROG
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
                ! CALL extrapolate_vdepth(blk, i_beg, i_end, j, level=.FALSE.)
                CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, input_total)
                DO ii = i_beg, i_end
                   IF (inlet_area(ii) .GT. 0.0) THEN
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
                END DO
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_FLOW
             END DO
          CASE("VELO")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                CALL extrapolate_vdepth(blk, i_beg, i_end, j, level = .FALSE.)
                DO ii = i_beg, i_end
                   blk%vvel(ii,j) = table_input(k)
                   IF (blk%vvel(ii,j) .GT. 0.0) THEN
                      blk%uvel(ii,j) = 0.0
                   ELSE 
                      blk%uvel(ii,j) = blk%uvel(ii,j+1)
                   END IF
                END DO
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_VEL
             END DO
          CASE("ELEV")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   blk%dp(ii,j) = 0.0
                   blk%depth(ii,j) = 2*table_input(k) - &
                        &(blk%depth(ii,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                   IF (do_wetdry) blk%depth(ii,j) =  &
                        &MAX(blk%depth(ii,j), dry_zero_depth)
                END DO
                blk%depthstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j+1)
                blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j+1)
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_ELEV
             END DO

          CASE ("ELEVELO")
             DO k=1,bc%num_cell_pairs
                i_beg = bc%start_cell(k)+1
                i_end = bc%end_cell(k)+1
                DO ii = i_beg, i_end
                   blk%depth(ii,j) = 2*table_input(k*2-1) - &
                        &(blk%depth(i,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                   blk%vvel(ii,j) = table_input(k*2)
                END DO
                blk%depthstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j+1)
                blk%uvelstar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
                blk%uvelold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
                blk%vvelstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
                blk%vvelold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
                blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_BOTH
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
                   blk%dp(ii,j) = 0.0
                   blk%depth(ii,j) = 2*table_input(k) - &
                        &(blk%depth(ii,j-1) + blk%zbot(ii,j-1)) - blk%zbot(ii,j)
                   ! blk%depth(ii,j) = table_input(k) - blk%zbot(ii,j)
                   IF (do_wetdry) blk%depth(ii,j) =  &
                        &MAX(blk%depth(ii,j), dry_zero_depth)
                END DO
                blk%depthstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
                blk%uvel(i_beg-1:i_end,j) = blk%uvel(i_beg-1:i_end,j-1)
                blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j-1)
                blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
                blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_ELEV
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT
       CASE ("ZEROG")
          DO k=1,bc%num_cell_pairs
             i_beg = bc%start_cell(j)+1
             i_end = bc%end_cell(j)+1
             CALL extrapolate_vdepth(blk, i_beg, i_end, j, level = .FALSE.)
             blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j-1)
             blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j-1)
             blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
             blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_ZEROG
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
                   blk%isdead(i,j+1)%u = .TRUE.
                END DO
             END DO

          CASE ("VVEL")

             j = bc%con_block + 1
             DO k = 1, bc%num_cell_pairs
                DO i = bc%start_cell(k),bc%end_cell(k)
                   blk%isdead(i+1,j)%v = .TRUE.
                END DO
             END DO
          CASE DEFAULT
             GOTO 100
          END SELECT

       CASE ("DEAD")

          DO i = bc%start_cell(1), bc%end_cell(1)
             DO j = bc%start_cell(2), bc%end_cell(2)
                blk%isdead(i+1, j+1)%p = .TRUE.
                blk%isdead(i  , j+1)%u = .TRUE.
                blk%isdead(i+1, j+1)%u = .TRUE.
                blk%isdead(i+1, j  )%v = .TRUE.
                blk%isdead(i+1, j+1)%v = .TRUE.
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


END MODULE block_hydro_bc
