!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 module file
!
! VERSION and DATE: 0.241 9-28-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!							9-28-98 fixed non-F95 derived type initialization; mcr
!
!
!***************************************************************
!
MODULE table_boundary_conditions

USE date_time

IMPLICIT NONE

INTEGER, PARAMETER  :: max_cell_values = 1000
INTEGER :: max_tables = 0, max_scalar_tables = 0


TYPE table_entry_struct
	
	TYPE(datetime_struct) :: datetime
	DOUBLE PRECISION :: value(max_cell_values)

END TYPE table_entry_struct

TYPE table_bc_struct

	CHARACTER (LEN=80) :: file_name
	CHARACTER (LEN=10) :: table_type
	!INTEGER :: max_entries = 0				! fails for non-F95 compilers
	INTEGER :: max_entries

	TYPE(table_entry_struct) :: table_entry(10000)


END TYPE table_bc_struct


TYPE(table_bc_struct), ALLOCATABLE :: table_bc(:)
TYPE(table_bc_struct), ALLOCATABLE :: scalar_table_bc(:)


!#########################################################################
CONTAINS

SUBROUTINE allocate_table_bc(max_tables, error_iounit, status_iounit)
	IMPLICIT NONE
	INTEGER :: max_tables, error_iounit, status_iounit, alloc_stat

	ALLOCATE(table_bc(max_tables), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of table bc '
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of table bc - maxtables =', max_tables
		table_bc%max_entries = 0
	ENDIF

END SUBROUTINE allocate_table_bc
!#########################################################################
SUBROUTINE allocate_scalar_table_bc(max_tables, error_iounit, status_iounit)
	IMPLICIT NONE
	INTEGER :: max_tables, error_iounit, status_iounit, alloc_stat

	ALLOCATE(scalar_table_bc(max_tables), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of scalar table bc '
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of scalar table bc - maxtables =', max_tables
		scalar_table_bc%max_entries = 0
	ENDIF

END SUBROUTINE allocate_scalar_table_bc
!########################################################################

SUBROUTINE read_bc_tables
	IMPLICIT NONE
	INTEGER :: iounit = 50, i, j = 0

	DO i = 1, max_tables
		OPEN(iounit, file = table_bc(i)%file_name)
		j = 0
		DO WHILE(.TRUE.)
                   j = j + 1
                   READ(iounit,*,END=100)table_bc(i)%table_entry(j)%datetime%date_string,&
                        & table_bc(i)%table_entry(j)%datetime%time_string, table_bc(i)%table_entry(j)%value(:)
                   table_bc(i)%max_entries = table_bc(i)%max_entries + 1
                   table_bc(i)%table_entry(j)%datetime%time = date_to_decimal(table_bc(i)%table_entry(j)%datetime%date_string,&
                        & table_bc(i)%table_entry(j)%datetime%time_string)
		END DO
100		CLOSE(iounit)
	END DO

END SUBROUTINE read_bc_tables
!########################################################################

SUBROUTINE read_scalar_bc_tables
	IMPLICIT NONE
	INTEGER :: iounit = 50, i, j = 0

	DO i = 1, max_scalar_tables
		OPEN(iounit, file = scalar_table_bc(i)%file_name)
		j = 0
		DO WHILE(.TRUE.)
                   j = j + 1
                   READ(iounit,*,END=100)scalar_table_bc(i)%table_entry(j)%datetime%date_string,&
                        & scalar_table_bc(i)%table_entry(j)%datetime%time_string,scalar_table_bc(i)%table_entry(j)%value(:)
                   scalar_table_bc(i)%max_entries = scalar_table_bc(i)%max_entries + 1
                   scalar_table_bc(i)%table_entry(j)%datetime%time =&
                        & date_to_decimal(scalar_table_bc(i)%table_entry(j)%datetime%date_string,&
                        & scalar_table_bc(i)%table_entry(j)%datetime%time_string)
		
		END DO
100		CLOSE(iounit)
	END DO

END SUBROUTINE read_scalar_bc_tables
!############################################################################################

SUBROUTINE table_interp(time, table_num, table_input, num_values)

! returns a VECTOR of the linearly interpolated values in a bc table

	IMPLICIT NONE
	DOUBLE PRECISION :: interp(max_cell_values), table_input(:)
	DOUBLE PRECISION :: time
	INTEGER :: table_num, i, j, num_values
	DOUBLE PRECISION :: factor

	DO j=1,table_bc(table_num)%max_entries-1

           IF((time >= table_bc(table_num)%table_entry(j)%datetime%time)&
                & .AND. (time <= table_bc(table_num)%table_entry(j+1)%datetime%time)) EXIT

	END DO
        factor = (time - table_bc(table_num)%table_entry(j)%datetime%time)/ &
             (table_bc(table_num)%table_entry(j+1)%datetime%time - table_bc(table_num)%table_entry(j)%datetime%time)

	interp = table_bc(table_num)%table_entry(j)%value + &
             factor*(table_bc(table_num)%table_entry(j+1)%value - table_bc(table_num)%table_entry(j)%value	)					
	
	table_input(1:num_values) = interp(1:num_values)
END SUBROUTINE table_interp

!############################################################################################################

SUBROUTINE scalar_table_interp(time, table_num, table_input, num_values)

! returns a VECTOR of the linearly interpolated values in a bc table

	IMPLICIT NONE
	DOUBLE PRECISION :: interp(max_cell_values), table_input(:)
	DOUBLE PRECISION :: time
	INTEGER :: table_num, i, j, num_values
	DOUBLE PRECISION :: factor

	DO j=1,scalar_table_bc(table_num)%max_entries-1

           IF((time >= scalar_table_bc(table_num)%table_entry(j)%datetime%time)&
                & .AND. (time <= scalar_table_bc(table_num)%table_entry(j+1)%datetime%time)) EXIT

	END DO
        factor = (time - scalar_table_bc(table_num)%table_entry(j)%datetime%time)/ &
             (scalar_table_bc(table_num)%table_entry(j+1)%datetime%time - scalar_table_bc(table_num)%table_entry(j)%datetime%time)

	interp = scalar_table_bc(table_num)%table_entry(j)%value + &
             factor*(scalar_table_bc(table_num)%table_entry(j+1)%value - scalar_table_bc(table_num)%table_entry(j)%value	)					
	
	table_input(1:num_values) = interp(1:num_values)
END SUBROUTINE scalar_table_interp

END MODULE table_boundary_conditions
