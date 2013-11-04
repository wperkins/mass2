!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 module file
!
! VERSION and DATE: 0.23 4-27-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS: sets up the bc's for each block. note that block connections are also included here.
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!
!
!***************************************************************
!
MODULE block_boundary_conditions

  USE utility
  USE table_boundary_conditions
  USE globals

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

TYPE  bc_spec_struct

	CHARACTER (LEN=10) :: bc_loc
	CHARACTER (LEN=10) :: bc_type, bc_kind, bc_extent
	INTEGER :: block, table_num != 0
	INTEGER :: con_block, con_start_cell(max_cell_values), con_end_cell(max_cell_values)
	INTEGER :: start_cell(max_cell_values), end_cell(max_cell_values)
	INTEGER :: num_cell_pairs != 0

END TYPE bc_spec_struct

TYPE block_bc_struct

	INTEGER  :: num_bc != 0
	TYPE(bc_spec_struct) :: bc_spec(200)

END TYPE block_bc_struct

!--------------------------------------------------
TYPE  scalar_bc_spec_struct

	CHARACTER (LEN=10) :: bc_loc
	CHARACTER (LEN=10) :: bc_type, bc_kind, bc_extent
	INTEGER :: block, species, x_start, table_num != 0
	INTEGER :: con_block, con_start_cell(max_cell_values), con_end_cell(max_cell_values), con_row
	INTEGER :: start_cell(max_cell_values), end_cell(max_cell_values)
	INTEGER :: num_cell_pairs != 0

END TYPE scalar_bc_spec_struct

TYPE scalar_bc_struct

	INTEGER  :: num_bc != 0
	TYPE(scalar_bc_spec_struct) :: bc_spec(200)

END TYPE scalar_bc_struct
!---------------------------------------------------



TYPE(block_bc_struct), ALLOCATABLE :: block_bc(:)
TYPE(scalar_bc_struct), ALLOCATABLE :: scalar_bc(:)

CHARACTER (LEN=80), PRIVATE :: bcspecs_name = "bcspecs.dat"
CHARACTER (LEN=80), PRIVATE :: scalar_bcspecs_name = "scalar_bcspecs.dat"

!#########################################################################
CONTAINS

SUBROUTINE allocate_block_bc(max_blocks)
  IMPLICIT NONE
  INTEGER :: max_blocks, alloc_stat, i

  ALLOCATE(block_bc(max_blocks), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     CALL  error_message('allocation failed for the array of block bc ', fatal=.TRUE.)
  END IF
  CALL status_message('allocation successful for array of block bc')

  block_bc%num_bc = 0
  DO i=1,max_blocks
     block_bc(i)%bc_spec%table_num = 0
     block_bc(i)%bc_spec%num_cell_pairs = 0
  END DO

END SUBROUTINE allocate_block_bc

!##########################################################################
SUBROUTINE allocate_scalar_block_bc(max_blocks)
  IMPLICIT NONE
  INTEGER :: max_blocks, alloc_stat, i

  ALLOCATE(scalar_bc(max_blocks), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     CALL error_message('allocation failed for the array of scalar bc ', fatal=.TRUE.)
  ENDIF
  CALL status_message('allocation successful for array of scalar bc')
  scalar_bc%num_bc = 0
  DO i=1,max_blocks
     scalar_bc(i)%bc_spec%table_num = 0
     scalar_bc(i)%bc_spec%num_cell_pairs = 0
  END DO

END SUBROUTINE allocate_scalar_block_bc

!##########################################################################

SUBROUTINE read_bcspecs(iounit, max_blocks, xmax, ymax)

  USE misc_vars, ONLY: do_rptdead

  ! reads the bc spec file
  ! format: block#	bc_loc	bc_type	bc_kind	bc_extent	'connect_block OR filename' 'cell pairs'			

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iounit, max_blocks, xmax(:), ymax(:)

  INTEGER :: junk1, block, num_bc, con_block, cells(2*max_cell_values) = -999 
  INTEGER :: table_count = 0, num_cells, num_cell_pairs
  CHARACTER (LEN=10) :: junk_char1, junk_char2
  CHARACTER (LEN=10) :: bc_loc, bc_type, bc_kind, bc_extent
  CHARACTER (LEN=80) :: file_name
  CHARACTER (LEN=1024) :: msg

  INTEGER :: line, lerr, ierr, maxidx

  CALL open_existing(bcspecs_name, iounit)

  ! first we need to count how many TABLE bc types 

  DO WHILE(.TRUE.)

     READ(iounit,*,END=100)junk1,junk_char1,junk_char2
     SELECT CASE (junk_char2)
     CASE ("TABLE", "SOURCE", "SINK")
        max_tables = max_tables + 1
     END SELECT
  END DO
	
	! now allocate the number of table bc structs that we need
100 CALL allocate_table_bc(max_tables)

  ! now start over to fill and parse
  REWIND(iounit)

  line = 0
  ierr = 0

  DO WHILE(.TRUE.)
     READ(iounit,*,END=200)block, bc_loc, bc_type, bc_kind, bc_extent
     line = line + 1
     lerr = 0
		
     block_bc(block)%num_bc = block_bc(block)%num_bc + 1
     num_bc = block_bc(block)%num_bc

     block_bc(block)%bc_spec(num_bc)%bc_loc = bc_loc
     block_bc(block)%bc_spec(num_bc)%bc_type =	bc_type
     block_bc(block)%bc_spec(num_bc)%bc_kind =	bc_kind
     block_bc(block)%bc_spec(num_bc)%bc_extent = bc_extent

                                ! check block number
     
     IF (block .GT. max_blocks .OR. block .LT. 1) THEN
        lerr = lerr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": block number out of range: ", block
        CALL error_message(msg, fatal=.FALSE.)
        ierr = ierr + lerr
        CYCLE
     END IF
     block_bc(block)%bc_spec(num_bc)%block = block

                                ! make sure we understand the BC
                                ! location
		
     SELECT CASE(bc_loc)
     CASE ("US", "DS")
        maxidx = ymax(block) - 1
     CASE ("LB", "RB")
        maxidx = xmax(block) - 1
     CASE ("IN")
        ! decide maxidx below
     CASE DEFAULT
        lerr = lerr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": ", TRIM(bc_loc), "?"
        CALL error_message(msg, fatal=.FALSE.)
     END SELECT

                                ! make sure we understand the BC kind
     
     SELECT CASE (bc_kind)
     CASE ("FLUX", "VELO", "ELEV", "ELEVELO")
     CASE ("UVEL", "VVEL")
     CASE ("CELL")
     CASE DEFAULT
        lerr = lerr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": ", TRIM(bc_kind), "?"
        CALL error_message(msg, fatal=.FALSE.)
     END SELECT


     IF (lerr .EQ. 0) THEN

                                ! reread BC specific information from the line
     
        BACKSPACE(iounit)
        
        SELECT CASE(bc_type)
        
        CASE("BLOCK")

           SELECT CASE(bc_extent)
           CASE("ALL")
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,con_block
              block_bc(block)%bc_spec(num_bc)%con_block = con_block
              block_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              block_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              block_bc(block)%bc_spec(num_bc)%end_cell(1) = maxidx
           CASE("PART")
              cells = -999
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,con_block,cells(:)
              block_bc(block)%bc_spec(num_bc)%con_block = con_block
              CALL set_bc_part(iounit, block_bc(block)%bc_spec(num_bc), &
                   &cells, maxidx, line, lerr)
              ! num_cells = COUNT(cells /= -999)
              ! num_cell_pairs = num_cells/2
              ! block_bc(block)%bc_spec(num_bc)%num_cell_pairs = num_cells/2
              ! block_bc(block)%bc_spec(num_bc)%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
              ! block_bc(block)%bc_spec(num_bc)%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
              READ (iounit, *)
           END SELECT
        
        CASE("TABLE")
           SELECT CASE(bc_extent)
           CASE("ALL")
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,file_name
              table_count = table_count + 1 ! keep a running count of the number of tables
              block_bc(block)%bc_spec(num_bc)%table_num = table_count
              table_bc(table_count)%file_name = file_name ! associate file and table number
              block_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              block_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              block_bc(block)%bc_spec(num_bc)%end_cell(1) = maxidx
           CASE("PART")
              cells = -999
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,file_name,cells(:)
              table_count = table_count + 1 ! keep a running count of the number of tables
              block_bc(block)%bc_spec(num_bc)%table_num = table_count
              table_bc(table_count)%file_name = file_name ! associate file and table number
              CALL set_bc_part(iounit, block_bc(block)%bc_spec(num_bc), &
                   &cells, maxidx, line, lerr)
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
              READ (iounit, *)
           END SELECT
           
        CASE ("SOURCE","SINK")
           
           cells = -999
           
           READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,file_name, cells
           table_count = table_count + 1 ! keep a running count of the number of tables
           block_bc(block)%bc_spec(num_bc)%table_num = table_count
           table_bc(table_count)%file_name = file_name ! associate file and table number
           
           SELECT CASE(bc_extent)
           CASE ("PART")
              CALL set_bc_area(block_bc(block)%bc_spec(num_bc), cells, &
                   &xmax(block) - 1, ymax(block) - 1, line, lerr)
           CASE ("ALL") 
              block_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              block_bc(block)%bc_spec(num_bc)%end_cell(1) = xmax(block) - 1
              block_bc(block)%bc_spec(num_bc)%start_cell(2) = 1
              block_bc(block)%bc_spec(num_bc)%end_cell(2) = ymax(block) - 1
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
              READ (iounit, *)
           END SELECT
        
        CASE ("WALL")
           cells = -999
           READ(iounit,*)block,bc_loc,bc_type,bc_kind,junk1,cells(:)

                                ! the first number indicates the row
                                ! (uvel) or column (vvel) cell 
           
           block_bc(block)%bc_spec(num_bc)%con_block = junk1
       
                                ! the remaining numbers indicate the
                                ! range of cells over which the wall
                                ! is to be placed.

           SELECT CASE (bc_kind)
           CASE ("UVEL")
              maxidx = ymax(block) - 1
              IF (junk1 .GT. xmax(block) - 1) THEN
                 lerr = lerr + 1
                 WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                      &": x (up/down) index out of range: ", junk1
                 CALL error_message(msg, fatal=.FALSE.)
              END IF
           CASE ("VVEL") 
              maxidx = xmax(block) - 1
              IF (junk1 .GT. ymax(block) - 1) THEN
                 lerr = lerr + 1
                 WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                      &": y (left/right) index out of range: ", junk1
                 CALL error_message(msg, fatal=.FALSE.)
              END IF
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                   &": this should not happen (fix me!)"
              CALL error_message(msg, fatal=.FALSE.)
           END SELECT
           CALL set_bc_part(iounit, block_bc(block)%bc_spec(num_bc), &
                &cells, maxidx, line, lerr)
        CASE ("DEAD")
           
           ! whatever is in bc_kind is not used
           
           cells = -999
           READ(iounit,*)block,bc_loc,bc_type,bc_kind,cells(:)
           
                                !
                                ! the dead zone is specified as a
                                ! rectangle of cells: imin, jmin,
                                ! imax, jmax (like src/sink)
           
           CALL set_bc_area(block_bc(block)%bc_spec(num_bc), cells, &
                &xmax(block) - 1, ymax(block) - 1, line, lerr)

           do_rptdead = .TRUE.

        CASE ("ZEROG")
         
           ! whatever is in bc_kind is ignored
           
           SELECT CASE(bc_extent)
           CASE("ALL")
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent
              block_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              block_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              block_bc(block)%bc_spec(num_bc)%end_cell(1) = maxidx
           CASE("PART")
              cells = -999
              READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,cells(:)
              CALL set_bc_part(iounit, block_bc(block)%bc_spec(num_bc), &
                   &cells, maxidx, line, lerr)
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
              READ (iounit, *)
           END SELECT
        CASE DEFAULT
           lerr = lerr + 1
           WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
                &": ", TRIM(bc_type), "?"
           CALL error_message(msg, fatal=.FALSE.)
           READ (iounit, *)
        END SELECT
     END IF
     ierr = ierr + lerr
  END DO

  
  
200 CLOSE(iounit)
     
  IF (ierr .GT. 0) THEN
     WRITE(msg, *) TRIM(bcspecs_name), ": ", ierr, " errors"
     CALL error_message(msg, fatal=.TRUE.)
  END IF
  CALL status_message("done reading bc specifications file")

END SUBROUTINE read_bcspecs

!###############################################################################

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

  USE misc_vars, ONLY: debug
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


!###############################################################################

SUBROUTINE set_scalar_block_connections(max_blocks, max_species)

  IMPLICIT NONE

  INTEGER :: max_blocks, max_species, error_iounit, status_iounit
  INTEGER :: block,num_bc,con_block, i, start_cell, end_cell
  INTEGER :: species_con_block, species_num_bc, species_block, ispecies

  ! for each hydro block-to-block connection, build a scalar BC for
  ! each species that copies the hydro connection

  DO block = 1, max_blocks

     DO num_bc = 1, block_bc(block)%num_bc
        
        IF (block_bc(block)%bc_spec(num_bc)%bc_type .EQ. 'BLOCK') THEN

           DO ispecies = 1, max_species
           
              scalar_bc(block)%num_bc = scalar_bc(block)%num_bc + 1
              species_num_bc = scalar_bc(block)%num_bc

              scalar_bc(block)%bc_spec(species_num_bc)%bc_type =	'BLOCK'
              scalar_bc(block)%bc_spec(species_num_bc)%bc_kind =	'CONC'
              scalar_bc(block)%bc_spec(species_num_bc)%bc_loc = &
                   &block_bc(block)%bc_spec(num_bc)%bc_loc
              scalar_bc(block)%bc_spec(species_num_bc)%bc_extent = &
                   &block_bc(block)%bc_spec(num_bc)%bc_extent
              scalar_bc(block)%bc_spec(species_num_bc)%block = block
              scalar_bc(block)%bc_spec(species_num_bc)%species = ispecies
              scalar_bc(block)%bc_spec(species_num_bc)%con_block = &
                   &block_bc(block)%bc_spec(num_bc)%con_block
              scalar_bc(block)%bc_spec(species_num_bc)%num_cell_pairs = &
                   &block_bc(block)%bc_spec(num_bc)%num_cell_pairs
              scalar_bc(block)%bc_spec(species_num_bc)%start_cell = &
                   &block_bc(block)%bc_spec(num_bc)%start_cell
              scalar_bc(block)%bc_spec(species_num_bc)%end_cell = &
                   &block_bc(block)%bc_spec(num_bc)%end_cell
              scalar_bc(block)%bc_spec(species_num_bc)%con_start_cell = &
                   &block_bc(block)%bc_spec(num_bc)%con_start_cell
              scalar_bc(block)%bc_spec(species_num_bc)%con_end_cell = &
                   &block_bc(block)%bc_spec(num_bc)%con_end_cell
             END DO
        END IF
     END DO
  END DO

  CALL status_message("done setting scalar block connections")

END SUBROUTINE set_scalar_block_connections

!##########################################################################

SUBROUTINE read_scalar_bcspecs(iounit, max_blocks, max_species, xmax, ymax)


! reads the bc spec file
! format: block#	bc_loc	bc_type	bc_kind	bc_extent	'connect_block OR filename' 'cell pairs'			

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iounit, max_blocks, max_species
  INTEGER, INTENT(IN) :: xmax(:), ymax(:)

  INTEGER :: junk1, block, species, cells(2*max_cell_values) = -999 , num_bc, con_block, i
  INTEGER :: table_count = 0, num_cells, num_cell_pairs, x_start
  INTEGER :: maxidx, lerr, ierr, line
  CHARACTER (LEN=10) :: junk_char1, junk_char2
  CHARACTER (LEN=10) :: bc_loc, bc_type, bc_kind, bc_extent
  CHARACTER (LEN=80) :: file_name
  CHARACTER (LEN=1024) :: msg

  CALL open_existing(scalar_bcspecs_name, iounit)

  ! first we need to count how many SCALAR TABLE bc types 

  DO WHILE(.TRUE.)
     READ(iounit,*,END=100)junk1,junk_char1,junk_char2
     IF(junk_char2 == "TABLE" .OR. junk_char2 == "SOURCE") &
          &max_scalar_tables = max_scalar_tables + 1
  END DO
	
  ! now allocate the number of table bc structs that we need
100 CALL allocate_scalar_table_bc(max_scalar_tables)

  scalar_bc(:)%num_bc = 0
  
  ! now start over to fill and parse
  REWIND(iounit)

  ierr = 0
  line = 0

  DO WHILE(.TRUE.)
     READ(iounit,*,END=200)block, bc_loc, bc_type, species, bc_kind, bc_extent
     line = line + 1
     lerr = 0

     scalar_bc(block)%num_bc = scalar_bc(block)%num_bc + 1
     num_bc = scalar_bc(block)%num_bc

     scalar_bc(block)%bc_spec(num_bc)%bc_loc = bc_loc
     scalar_bc(block)%bc_spec(num_bc)%bc_type =	bc_type
     scalar_bc(block)%bc_spec(num_bc)%bc_kind =	bc_kind
     scalar_bc(block)%bc_spec(num_bc)%bc_extent = bc_extent
     scalar_bc(block)%bc_spec(num_bc)%species = species

                                ! check block number
     
     IF (block .GT. max_blocks .OR. block .LT. 1) THEN
        lerr = lerr + 1
        WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
             &": block number out of range: ", block
        CALL error_message(msg, fatal=.FALSE.)
        ierr = ierr + lerr
        CYCLE
     END IF

                                ! check species number

     IF (species .GT. max_species .OR. species .LT. 1) THEN
        lerr = lerr + 1
        WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
             &": species number out of range: ", species
        CALL error_message(msg, fatal=.FALSE.)
     END IF

                                ! make sure we understand the BC
                                ! location
		
     SELECT CASE(bc_loc)
     CASE ("US", "DS")
        maxidx = ymax(block) - 1
     CASE ("RB", "LB")
        maxidx = xmax(block) - 1
     CASE ("IN")
        ! decide maxidx below
     CASE DEFAULT
        lerr = lerr + 1
        WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
             &": ", TRIM(bc_loc), "?"
        CALL error_message(msg, fatal=.FALSE.)
     END SELECT

     IF (lerr .EQ. 0) THEN

                                ! reread BC specific information from the line
     
        BACKSPACE(iounit)

        SELECT CASE(bc_type)

        CASE("SOURCE")
           cells = -999
           READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,file_name,cells
           table_count = table_count + 1
           scalar_bc(block)%bc_spec(num_bc)%table_num = table_count
           scalar_table_bc(table_count)%file_name = file_name

           SELECT CASE(bc_extent)
           CASE("PART")
              CALL area_cell_check(cells, xmax(block) - 1, ymax(block) - 1, &
                   &scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs, &
                   &scalar_bc(block)%bc_spec(num_bc)%start_cell, &
                   &scalar_bc(block)%bc_spec(num_bc)%end_cell, &
                   &line, ierr)
           CASE("ALL")
              scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              scalar_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              scalar_bc(block)%bc_spec(num_bc)%end_cell(1) = xmax(block) - 1
              scalar_bc(block)%bc_spec(num_bc)%start_cell(2) = 1
              scalar_bc(block)%bc_spec(num_bc)%end_cell(2) = ymax(block) - 1
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
           END SELECT
           
        CASE("ZEROG")
           SELECT CASE(bc_extent)
           CASE("ALL")
              READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent
              scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              scalar_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              scalar_bc(block)%bc_spec(num_bc)%end_cell(1) = maxidx
           CASE("PART")
              cells = -999
              READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,cells(:)
              CALL set_scalar_part(iounit, scalar_bc(block)%bc_spec(num_bc), &
                   &cells, maxidx, line, lerr)
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
           END SELECT
           
        CASE("BLOCK")
           READ(iounit,*)block, bc_loc, bc_type, species, bc_kind, bc_extent, con_block

           scalar_bc(block)%num_bc = scalar_bc(block)%num_bc - 1
           num_bc = scalar_bc(block)%num_bc

           WRITE (msg, *) TRIM(scalar_bcspecs_name), ": warning: line ", line, &
                &': block connection ignored'
           CALL error_message(msg, fatal=.FALSE.)
           
        CASE("TABLE")
           SELECT CASE(bc_extent)
           CASE("ALL")
              READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,file_name,x_start
              table_count = table_count + 1 ! keep a running count of the number of tables
              scalar_bc(block)%bc_spec(num_bc)%table_num = table_count
              scalar_table_bc(table_count)%file_name = file_name ! associate file and table number
              scalar_bc(block)%bc_spec(num_bc)%x_start = x_start ! starting x grid line for BC
              scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
              scalar_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
              scalar_bc(block)%bc_spec(num_bc)%end_cell(1) = maxidx
           CASE("PART")
              cells = -999
              READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,file_name,x_start,cells(:)
              table_count = table_count + 1 ! keep a running count of the number of tables
              scalar_bc(block)%bc_spec(num_bc)%table_num = table_count
              scalar_table_bc(table_count)%file_name = file_name ! associate file and table number
              scalar_bc(block)%bc_spec(num_bc)%x_start = x_start ! starting x grid line for BC
              CALL set_scalar_part(iounit, scalar_bc(block)%bc_spec(num_bc), &
                   &cells, maxidx, line, lerr)
           CASE DEFAULT
              lerr = lerr + 1
              WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
                   &": ", TRIM(bc_extent), "?"
              CALL error_message(msg, fatal=.FALSE.)
           END SELECT
        CASE DEFAULT
           lerr = lerr + 1
           WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
                &": ", TRIM(bc_type), "?"
           CALL error_message(msg, fatal=.FALSE.)
           READ(iounit, *) junk1
        END SELECT
     END IF
     ierr = ierr + lerr
  END DO


200 CONTINUE

  CLOSE(iounit)

  IF (ierr .GT. 0) THEN
     WRITE(msg, *) TRIM(scalar_bcspecs_name), ": ", ierr, " errors"
     CALL error_message(msg, fatal=.TRUE.)
  END IF
  CALL status_message("done reading scalar bc specifications file")
  
END SUBROUTINE read_scalar_bcspecs

! ----------------------------------------------------------------
! SUBROUTINE set_scalar_part
! ----------------------------------------------------------------
SUBROUTINE set_scalar_part(iounit, spec, cells, maxidx, line, ierr)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iounit, cells(:), maxidx, line
  TYPE (scalar_bc_spec_struct), INTENT(INOUT) :: spec
  INTEGER, INTENT(INOUT) :: ierr

  INTEGER :: num_cells, num_cell_pairs, i
  CHARACTER (LEN=1024) :: msg

  num_cells = COUNT(cells .NE. -999)
  num_cell_pairs = num_cells/2
  spec%num_cell_pairs = num_cell_pairs
  spec%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
  spec%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)
  
                                ! check the cell numbers

  DO i = 1, spec%num_cell_pairs
     IF (spec%start_cell(i) .LT. 1 .OR. spec%start_cell(i) .GT. maxidx) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
             &": start index ", spec%start_cell(i), ' out of range'
        CALL error_message(msg, fatal=.FALSE.)
     END IF
     IF (spec%end_cell(i) .LT. 1 .OR. spec%end_cell(i) .GT. maxidx) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(scalar_bcspecs_name), ": error: line ", line, &
             &": end index ", spec%end_cell(i), ' out of range'
        CALL error_message(msg, fatal=.FALSE.)
     END IF
  END DO
     

END SUBROUTINE set_scalar_part

! ----------------------------------------------------------------
! SUBROUTINE set_bc_part
! ----------------------------------------------------------------
SUBROUTINE set_bc_part(iounit, spec, cells, maxidx, line, ierr)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iounit, cells(:), maxidx, line
  TYPE (bc_spec_struct), INTENT(INOUT) :: spec
  INTEGER, INTENT(INOUT) :: ierr

  INTEGER :: num_cells, num_cell_pairs, i
  CHARACTER (LEN=1024) :: msg

  num_cells = COUNT(cells .NE. -999)
  num_cell_pairs = num_cells/2
  spec%num_cell_pairs = num_cell_pairs
  spec%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
  spec%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)
  
                                ! check the cell numbers

  DO i = 1, spec%num_cell_pairs
     IF (spec%start_cell(i) .LT. 1 .OR. spec%start_cell(i) .GT. maxidx) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": start index ", spec%start_cell(i), ' out of range'
        CALL error_message(msg, fatal=.FALSE.)
     END IF
     IF (spec%end_cell(i) .LT. 1 .OR. spec%end_cell(i) .GT. maxidx) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": end index ", spec%end_cell(i), ' out of range'
        CALL error_message(msg, fatal=.FALSE.)
     END IF
  END DO
     

END SUBROUTINE set_bc_part

! ----------------------------------------------------------------
! SUBROUTINE area_cell_check
! ----------------------------------------------------------------
SUBROUTINE area_cell_check(cells, xmax, ymax, &
     &num_cell_pairs, start_cell, end_cell, line, ierr)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cells(:), xmax, ymax
  INTEGER, INTENT(OUT) :: num_cell_pairs, start_cell(:), end_cell(:)
  INTEGER, INTENT(IN) :: line
  INTEGER, INTENT(INOUT) :: ierr
  INTEGER :: num_cells
  CHARACTER (LEN=1024) :: msg

  num_cells = COUNT(cells /= -999)
  
  IF (num_cells .NE. 4) THEN
     ierr = ierr + 1
     WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
          &": four indices expected, got ", num_cells
     CALL error_message(msg, fatal=.FALSE.)
  ELSE
     num_cell_pairs = num_cells/2
     start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
     end_cell(1:num_cell_pairs) = cells(2:num_cells:2)
     
     IF (start_cell(1) .LT. 1 .OR. &
          &start_cell(1) .GT. xmax) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": minimum x index out of range: ", start_cell(1)
        CALL error_message(msg, fatal=.FALSE.)
     END IF

     IF (end_cell(1) .LT. 1 .OR. &
          &end_cell(1) .GT. xmax) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": maximum x index out of range: ", end_cell(1)
        CALL error_message(msg, fatal=.FALSE.)
     END IF

     IF (end_cell(1) .LT. start_cell(1)) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": invalid x index range: ", start_cell(1), " to ", &
             &end_cell(1)
        CALL error_message(msg, fatal=.FALSE.)
     END IF

     IF (start_cell(2) .LT. 1 .OR. &
          &start_cell(2) .GT. ymax) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": minimum y index out of range: ", start_cell(2)
        CALL error_message(msg, fatal=.FALSE.)
     END IF
     
     IF (end_cell(2) .LT. 1 .OR. &
          &end_cell(2) .GT. ymax) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": maximum y index out of range: ", end_cell(2)
        CALL error_message(msg, fatal=.FALSE.)
     END IF

     IF (end_cell(2) .LT. start_cell(2)) THEN
        ierr = ierr + 1
        WRITE(msg, *) TRIM(bcspecs_name), ": error: line ", line, &
             &": invalid y index range: ", start_cell(2), " to ", &
             &end_cell(2)
        CALL error_message(msg, fatal=.FALSE.)
     END IF

  END IF
  

END SUBROUTINE area_cell_check


! ----------------------------------------------------------------
! SUBROUTINE set_bc_area
! ----------------------------------------------------------------
SUBROUTINE set_bc_area(spec, cells, xmax, ymax, line, ierr)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cells(:), xmax, ymax, line
  TYPE (bc_spec_struct), INTENT(INOUT) :: spec
  INTEGER, INTENT(INOUT) :: ierr
  
  CALL area_cell_check(cells, xmax, ymax, &
       &spec%num_cell_pairs, spec%start_cell, spec%end_cell, line, ierr)

END SUBROUTINE set_bc_area


END MODULE block_boundary_conditions

