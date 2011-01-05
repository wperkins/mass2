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

          x1 = block(bc%block)%x_grid%current(i, j)
          y1 = block(bc%block)%y_grid%current(i, j)
          x2 = block(conbc%block)%x_grid%current(coni, conj)
          y2 = block(conbc%block)%y_grid%current(coni, conj)

          rdist = distance(x1, y1, x2, y2)

          SELECT CASE (bc%bc_loc)
          CASE ("US","DS")
             x2 = block(bc%block)%x_grid%current(i, j+1)
             y2 = block(bc%block)%y_grid%current(i, j+1)
          CASE ("LB","RB")
             x2 = block(bc%block)%x_grid%current(i+1, j)
             y2 = block(bc%block)%y_grid%current(i+1, j)
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
             x1 = block(bc%block)%x_grid%current(i, j+1)
             y1 = block(bc%block)%y_grid%current(i, j+1)
             conj = conbc%end_cell(n)
             x2 = block(conbc%block)%x_grid%current(coni, conj+1)
             y2 = block(conbc%block)%y_grid%current(coni, conj+1)
          CASE ("LB","RB")
             i =  bc%end_cell(n)
             x1 = block(bc%block)%x_grid%current(i+1, j)
             y1 = block(bc%block)%y_grid%current(i+1, j)
             coni = conbc%end_cell(n)
             x2 = block(conbc%block)%x_grid%current(coni+1, conj)
             y2 = block(conbc%block)%y_grid%current(coni+1, conj)
          END SELECT

          rdist = distance(x1, y1, x2, y2)

          x2 = block(bc%block)%x_grid%current(i, j)
          y2 = block(bc%block)%y_grid%current(i, j)
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
          block(iblock)%eddy%current(2-ig,:) = block(iblock)%eddy%current(2,:)
          block(iblock)%kx_diff%current(2-ig,:) = block(iblock)%kx_diff%current(2,:)
          block(iblock)%ky_diff%current(2-ig,:) = block(iblock)%ky_diff%current(2,:)
          block(iblock)%chezy%current(2-ig,:) = block(iblock)%chezy%current(2,:)
       END IF
       IF (block_owns_i(block(iblock), block(iblock)%xmax+ig)) THEN
          block(iblock)%eddy%current(block(iblock)%xmax+ig,:) = &
               &block(iblock)%eddy%current(block(iblock)%xmax,:)
          block(iblock)%kx_diff%current(block(iblock)%xmax+ig,:) = &
               &block(iblock)%kx_diff%current(block(iblock)%xmax,:)
          block(iblock)%ky_diff%current(block(iblock)%xmax+ig,:) = &
               &block(iblock)%ky_diff%current(block(iblock)%xmax,:)
          block(iblock)%chezy%current(block(iblock)%xmax+ig,:) = &
               &block(iblock)%chezy%current(block(iblock)%xmax,:)
       END IF
    END DO
    DO jg = 1, nghost 
       IF (block_owns_j(block(iblock), 2-jg)) THEN
          block(iblock)%eddy%current(:, 2-jg) = block(iblock)%eddy%current(:,2)
          block(iblock)%kx_diff%current(:, 2-jg) = block(iblock)%kx_diff%current(:,2)
          block(iblock)%ky_diff%current(:, 2-jg) = block(iblock)%ky_diff%current(:,2)
          block(iblock)%chezy%current(:, 2-jg) = block(iblock)%chezy%current(:,2)
       END IF
       IF (block_owns_j(block(iblock), block(iblock)%ymax+ig)) THEN
          block(iblock)%eddy%current(:,block(iblock)%ymax+ig) = &
               &block(iblock)%eddy%current(:,block(iblock)%ymax)
          block(iblock)%kx_diff%current(:,block(iblock)%ymax+ig) = &
               &block(iblock)%kx_diff%current(:,block(iblock)%ymax)
          block(iblock)%ky_diff%current(:,block(iblock)%ymax+ig) = &
               &block(iblock)%ky_diff%current(:,block(iblock)%ymax)
          block(iblock)%chezy%current(:,block(iblock)%ymax+ig) = &
               &block(iblock)%chezy%current(:,block(iblock)%ymax)
       END IF
    END DO

    CALL block_var_put(block(iblock)%eddy)
    CALL block_var_put(block(iblock)%kx_diff)
    CALL block_var_put(block(iblock)%ky_diff)
    CALL block_var_put(block(iblock)%chezy)
    CALL ga_sync()
    CALL block_var_get(block(iblock)%eddy)
    CALL block_var_get(block(iblock)%kx_diff)
    CALL block_var_get(block(iblock)%ky_diff)
    CALL block_var_get(block(iblock)%chezy)

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


END MODULE block_hydro_bc
