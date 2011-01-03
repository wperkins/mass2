! ----------------------------------------------------------------
! MODULE block_hydro_bc
! ----------------------------------------------------------------
MODULE block_hydro_bc

  USE config
  USE block_module
  USE hydro_bc

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE set_block_connections
  ! ----------------------------------------------------------------
  SUBROUTINE set_block_connections(max_blocks, error_iounit, status_iounit)

    IMPLICIT NONE

    INTEGER :: max_blocks, error_iounit, status_iounit
    INTEGER :: block,num_bc,con_block, i, ierr
    LOGICAL :: bcfound
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    DO block = 1, max_blocks

       DO num_bc = 1, block_bc(block)%num_bc
          IF(block_bc(block)%bc_spec(num_bc)%bc_type == "BLOCK")THEN
             con_block = block_bc(block)%bc_spec(num_bc)%con_block
             bcfound = .FALSE.
             DO i=1,block_bc(con_block)%num_bc
                IF(block_bc(con_block)%bc_spec(i)%con_block .EQ. block) THEN
                   bcfound = check_hydro_block_connection(&
                        &block_bc(block)%bc_spec(num_bc),&
                        &block_bc(con_block)%bc_spec(i))
                END IF
             END DO
             IF (.NOT. bcfound) THEN
                WRITE (msg, *) 'No match for BC connecting block ', block, &
                     &' to ', con_block, ': ',&
                     &TRIM(block_bc(block)%bc_spec(num_bc)%bc_type), ' ',&
                     &TRIM(block_bc(block)%bc_spec(num_bc)%bc_loc), ' ',&
                     &TRIM(block_bc(block)%bc_spec(num_bc)%bc_kind), ' ',&
                     &TRIM(block_bc(block)%bc_spec(num_bc)%bc_extent), ' '
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


END MODULE block_hydro_bc
