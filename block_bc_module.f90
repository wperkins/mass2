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

USE table_boundary_conditions

IMPLICIT NONE

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
	TYPE(bc_spec_struct) :: bc_spec(100)

END TYPE block_bc_struct

!--------------------------------------------------
TYPE  scalar_bc_spec_struct

	CHARACTER (LEN=10) :: bc_loc
	CHARACTER (LEN=10) :: bc_type, bc_kind, bc_extent
	INTEGER :: block, species, x_start, table_num != 0
	INTEGER :: con_block, con_start_cell(max_cell_values), con_end_cell(max_cell_values)
	INTEGER :: start_cell(max_cell_values), end_cell(max_cell_values)
	INTEGER :: num_cell_pairs != 0

END TYPE scalar_bc_spec_struct

TYPE scalar_bc_struct

	INTEGER  :: num_bc != 0
	TYPE(scalar_bc_spec_struct) :: bc_spec(100)

END TYPE scalar_bc_struct
!---------------------------------------------------



TYPE(block_bc_struct), ALLOCATABLE :: block_bc(:)
TYPE(scalar_bc_struct), ALLOCATABLE :: scalar_bc(:)

!#########################################################################
CONTAINS

SUBROUTINE allocate_block_bc(max_blocks, error_iounit, status_iounit)
	IMPLICIT NONE
	INTEGER :: max_blocks, error_iounit, status_iounit, alloc_stat, i

	ALLOCATE(block_bc(max_blocks), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of block bc '
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of block bc'
		block_bc%num_bc = 0
		DO i=1,max_blocks
			block_bc(i)%bc_spec%table_num = 0
			block_bc(i)%bc_spec%num_cell_pairs = 0
		END DO
	ENDIF

END SUBROUTINE allocate_block_bc

!##########################################################################
SUBROUTINE allocate_scalar_block_bc(max_blocks, error_iounit, status_iounit)
	IMPLICIT NONE
	INTEGER :: max_blocks, error_iounit, status_iounit, alloc_stat, i

	ALLOCATE(scalar_bc(max_blocks), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of scalar bc '
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of scalar bc'
		scalar_bc%num_bc = 0
		DO i=1,max_blocks
			scalar_bc(i)%bc_spec%table_num = 0
			scalar_bc(i)%bc_spec%num_cell_pairs = 0
		END DO
	ENDIF

END SUBROUTINE allocate_scalar_block_bc

!##########################################################################

SUBROUTINE read_bcspecs(iounit, error_iounit, status_iounit, xmax, ymax)


! reads the bc spec file
! format: block#	bc_loc	bc_type	bc_kind	bc_extent	'connect_block OR filename' 'cell pairs'			

IMPLICIT NONE
	INTEGER :: iounit, error_iounit, status_iounit
	INTEGER :: junk1, block, num_bc, con_block, cells(2*max_cell_values) = -999 
	INTEGER :: table_count = 0, num_cells, num_cell_pairs
	INTEGER :: xmax(:), ymax(:)
	CHARACTER (LEN=10) :: junk_char1, junk_char2
	CHARACTER (LEN=10) :: bc_loc, bc_type, bc_kind, bc_extent
	CHARACTER (LEN=80) :: file_name

	OPEN(iounit, file='bcspecs.dat')

	! first we need to count how many TABLE bc types 

	DO WHILE(.TRUE.)

		READ(iounit,*,END=100)junk1,junk_char1,junk_char2
		IF(junk_char2 == "TABLE") max_tables = max_tables + 1

	END DO
	
	! now allocate the number of table bc structs that we need
100	CALL allocate_table_bc(max_tables, error_iounit, status_iounit)

	! now start over to fill and parse
	REWIND(iounit)

	DO WHILE(.TRUE.)
		READ(iounit,*,END=200)block, bc_loc, bc_type, bc_kind, bc_extent
		
		block_bc(block)%num_bc = block_bc(block)%num_bc + 1
		num_bc = block_bc(block)%num_bc

		block_bc(block)%bc_spec(num_bc)%bc_loc = bc_loc
		block_bc(block)%bc_spec(num_bc)%bc_type =	bc_type
		block_bc(block)%bc_spec(num_bc)%bc_kind =	bc_kind
		block_bc(block)%bc_spec(num_bc)%bc_extent = bc_extent

		BACKSPACE(iounit)

		SELECT CASE(bc_type)

			CASE("BLOCK")

			SELECT CASE(bc_extent)
				CASE("ALL")
					READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,con_block
					block_bc(block)%bc_spec(num_bc)%con_block = con_block
					block_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
					block_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
					block_bc(block)%bc_spec(num_bc)%end_cell(1) = ymax(block) - 1
				CASE("PART")
					cells = -999
					READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,con_block,cells(:)
					block_bc(block)%bc_spec(num_bc)%con_block = con_block
					num_cells = COUNT(cells /= -999)
					num_cell_pairs = num_cells/2
					block_bc(block)%bc_spec(num_bc)%num_cell_pairs = num_cells/2
					block_bc(block)%bc_spec(num_bc)%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
					block_bc(block)%bc_spec(num_bc)%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)
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
						block_bc(block)%bc_spec(num_bc)%end_cell(1) = ymax(block) - 1
					CASE("PART")
						cells = -999
						READ(iounit,*)block,bc_loc,bc_type,bc_kind,bc_extent,file_name,cells(:)
						table_count = table_count + 1 ! keep a running count of the number of tables
						block_bc(block)%bc_spec(num_bc)%table_num = table_count
						table_bc(table_count)%file_name = file_name ! associate file and table number
						num_cells = COUNT(cells /= -999)
						num_cell_pairs = num_cells/2
						block_bc(block)%bc_spec(num_bc)%num_cell_pairs = num_cells/2
						block_bc(block)%bc_spec(num_bc)%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
						block_bc(block)%bc_spec(num_bc)%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)

				END SELECT

		END SELECT


	END DO
	
200	CLOSE(iounit)

END SUBROUTINE read_bcspecs

!###############################################################################

SUBROUTINE set_block_connections(max_blocks, error_iounit, status_iounit)

IMPLICIT NONE

INTEGER :: max_blocks, error_iounit, status_iounit
INTEGER :: block,num_bc,con_block, i

DO block = 1, max_blocks

DO num_bc = 1, block_bc(block)%num_bc

IF(block_bc(block)%bc_spec(num_bc)%bc_type == "BLOCK")THEN
	con_block = block_bc(block)%bc_spec(num_bc)%con_block
	DO i=1,block_bc(con_block)%num_bc
		IF(block_bc(con_block)%bc_spec(i)%con_block == block)THEN
			block_bc(block)%bc_spec(num_bc)%con_start_cell = block_bc(con_block)%bc_spec(i)%start_cell
			block_bc(block)%bc_spec(num_bc)%con_end_cell = block_bc(con_block)%bc_spec(i)%end_cell
			EXIT
		END IF
	END DO
END IF

END DO

END DO


END SUBROUTINE set_block_connections

!###############################################################################

SUBROUTINE set_scalar_block_connections(max_blocks, max_species, error_iounit, status_iounit)

IMPLICIT NONE

INTEGER :: max_blocks, max_species, error_iounit, status_iounit
INTEGER :: block,num_bc,con_block, i, start_cell, end_cell
INTEGER :: species_con_block, species_num_bc, species_block

DO block = 1, max_blocks

DO species_num_bc = 1, scalar_bc(block)%num_bc

IF(scalar_bc(block)%bc_spec(species_num_bc)%bc_type == "BLOCK")THEN
	species_block = scalar_bc(block)%bc_spec(species_num_bc)%block
	species_con_block = scalar_bc(block)%bc_spec(species_num_bc)%con_block


DO num_bc = 1, block_bc(block)%num_bc
	IF(block_bc(block)%bc_spec(num_bc)%bc_type == "BLOCK")THEN
		IF(species_con_block == block_bc(block)%bc_spec(num_bc)%con_block)THEN
			scalar_bc(block)%bc_spec(species_num_bc)%con_start_cell = block_bc(block)%bc_spec(num_bc)%con_start_cell
			scalar_bc(block)%bc_spec(species_num_bc)%con_end_cell = block_bc(block)%bc_spec(num_bc)%con_end_cell
			scalar_bc(block)%bc_spec(species_num_bc)%start_cell = block_bc(block)%bc_spec(num_bc)%start_cell
			scalar_bc(block)%bc_spec(species_num_bc)%end_cell = block_bc(block)%bc_spec(num_bc)%end_cell
			scalar_bc(block)%bc_spec(species_num_bc)%num_cell_pairs = block_bc(block)%bc_spec(num_bc)%num_cell_pairs
			EXIT
		END IF
	END IF
END DO

END IF

END DO

END DO

WRITE(status_iounit,*)"done setting scalar block connections"

END SUBROUTINE set_scalar_block_connections

!##########################################################################

SUBROUTINE read_scalar_bcspecs(iounit, error_iounit, status_iounit, xmax, ymax)


! reads the bc spec file
! format: block#	bc_loc	bc_type	bc_kind	bc_extent	'connect_block OR filename' 'cell pairs'			

IMPLICIT NONE
	INTEGER :: iounit, error_iounit, status_iounit
	INTEGER :: junk1, block, species, cells(2*max_cell_values) = -999 , num_bc, con_block
	INTEGER :: table_count = 0, num_cells, num_cell_pairs, x_start
	INTEGER :: xmax(:), ymax(:)
	CHARACTER (LEN=10) :: junk_char1, junk_char2
	CHARACTER (LEN=10) :: bc_loc, bc_type, bc_kind, bc_extent
	CHARACTER (LEN=80) :: file_name



	OPEN(iounit, file='scalar_bcspecs.dat')

	! first we need to count how many SCALAR TABLE bc types 

	DO WHILE(.TRUE.)

		READ(iounit,*,END=100)junk1,junk_char1,junk_char2
		IF(junk_char2 == "TABLE") max_scalar_tables = max_scalar_tables + 1

	END DO
	
	! now allocate the number of table bc structs that we need
100	CALL allocate_scalar_table_bc(max_scalar_tables, error_iounit, status_iounit)

	! now start over to fill and parse
	REWIND(iounit)

	DO WHILE(.TRUE.)
		READ(iounit,*,END=200)block, bc_loc, bc_type, species, bc_kind, bc_extent
		
		scalar_bc(block)%num_bc = scalar_bc(block)%num_bc + 1
		num_bc = scalar_bc(block)%num_bc

		scalar_bc(block)%bc_spec(num_bc)%bc_loc = bc_loc
		scalar_bc(block)%bc_spec(num_bc)%bc_type =	bc_type
		scalar_bc(block)%bc_spec(num_bc)%bc_kind =	bc_kind
		scalar_bc(block)%bc_spec(num_bc)%bc_extent = bc_extent
		scalar_bc(block)%bc_spec(num_bc)%species = species

		BACKSPACE(iounit)

		SELECT CASE(bc_type)

			CASE("ZEROG")
				SELECT CASE(bc_extent)
					CASE("ALL")
						READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent
						scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = 1
						scalar_bc(block)%bc_spec(num_bc)%start_cell(1) = 1
						scalar_bc(block)%bc_spec(num_bc)%end_cell(1) = ymax(block) - 1
					CASE("PART")
						cells = -999
						READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,cells(:)
						num_cells = COUNT(cells /= -999)
						num_cell_pairs = num_cells/2
						scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = num_cells/2
						scalar_bc(block)%bc_spec(num_bc)%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
						scalar_bc(block)%bc_spec(num_bc)%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)

				END SELECT

			CASE("BLOCK")
				READ(iounit,*)block, bc_loc, bc_type, species, bc_kind, bc_extent, con_block
				scalar_bc(block)%bc_spec(num_bc)%con_block = con_block

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
						scalar_bc(block)%bc_spec(num_bc)%end_cell(1) = ymax(block) - 1
					CASE("PART")
						cells = -999
						READ(iounit,*)block,bc_loc,bc_type,species,bc_kind,bc_extent,file_name,x_start,cells(:)
						table_count = table_count + 1 ! keep a running count of the number of tables
						scalar_bc(block)%bc_spec(num_bc)%table_num = table_count
						scalar_table_bc(table_count)%file_name = file_name ! associate file and table number
						scalar_bc(block)%bc_spec(num_bc)%x_start = x_start ! starting x grid line for BC
						num_cells = COUNT(cells /= -999)
						num_cell_pairs = num_cells/2
						scalar_bc(block)%bc_spec(num_bc)%num_cell_pairs = num_cells/2
						scalar_bc(block)%bc_spec(num_bc)%start_cell(1:num_cell_pairs) = cells(1:num_cells:2)
						scalar_bc(block)%bc_spec(num_bc)%end_cell(1:num_cell_pairs) = cells(2:num_cells:2)

				END SELECT

		END SELECT


	END DO

200	WRITE(status_iounit,*)"done reading scalar bc specifications file"
	
	CLOSE(iounit)

END SUBROUTINE read_scalar_bcspecs

!###############################################################################

END MODULE block_boundary_conditions
