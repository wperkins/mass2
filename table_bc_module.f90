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

USE utility
USE cumulative_time_series

IMPLICIT NONE

INTEGER, PARAMETER  :: max_cell_values = 40
INTEGER :: max_tables = 0
INTEGER :: max_precip_tables = 0
INTEGER :: max_scalar_tables = 0


                                ! this is a (hopefully) temporary
                                ! adaptation.  The time_series should
                                ! be more directly integrated

TYPE table_bc_struct
   CHARACTER (LEN=80) :: file_name
   TYPE(time_series_rec), POINTER :: ts
END TYPE table_bc_struct

TYPE(table_bc_struct), ALLOCATABLE :: table_bc(:)
TYPE(table_bc_struct), ALLOCATABLE :: scalar_table_bc(:)

TYPE cumulative_bc_struct
   CHARACTER (LEN=80) :: file_name
   TYPE(cumulative_time_series_rec), POINTER :: ts
END type cumulative_bc_struct

TYPE (cumulative_bc_struct), ALLOCATABLE :: precip_bc(:)

CHARACTER (LEN=1024), PRIVATE :: buffer

!#########################################################################
CONTAINS

SUBROUTINE allocate_table_bc()
	IMPLICIT NONE
	INTEGER :: alloc_stat

	ALLOCATE(table_bc(max_tables), precip_bc(max_precip_tables), STAT = alloc_stat)
    
	IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the array of table bc ', fatal=.TRUE.)
	ELSE
       WRITE(buffer,*) 'allocation successful for array of table bc - maxtables =', max_tables
       CALL status_message(buffer)
	ENDIF

END SUBROUTINE allocate_table_bc
!#########################################################################
SUBROUTINE allocate_scalar_table_bc(max_tables)
	IMPLICIT NONE
	INTEGER :: max_tables, alloc_stat

	ALLOCATE(scalar_table_bc(max_tables), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the array of scalar table bc ', &
            &fatal=.TRUE.)
	ELSE
       WRITE(buffer,*)'allocation successful for array of scalar table bc - maxtables =', max_tables
       CALL status_message(buffer)
	ENDIF

END SUBROUTINE allocate_scalar_table_bc
!########################################################################

SUBROUTINE read_bc_tables
	IMPLICIT NONE
	INTEGER :: iounit = 50, i, j = 0
    CHARACTER (LEN=100) :: junk

	DO i = 1, max_tables
       table_bc(i)%ts => time_series_read(table_bc(i)%file_name, fields = max_cell_values)
	END DO

 DO i = 1, max_precip_tables
    precip_bc(i)%ts => cumulative_time_series_read(precip_bc(i)%file_name)
 END DO

END SUBROUTINE read_bc_tables
!########################################################################

SUBROUTINE read_scalar_bc_tables
	IMPLICIT NONE
	INTEGER :: iounit = 50, i, j = 0
    CHARACTER (LEN=100) :: junk

	DO i = 1, max_scalar_tables
       scalar_table_bc(i)%ts =>  time_series_read(scalar_table_bc(i)%file_name, fields = max_cell_values)
	END DO

END SUBROUTINE read_scalar_bc_tables
!############################################################################################

SUBROUTINE table_interp(time, table_num, table_input, num_values)

! returns a VECTOR of the linearly interpolated values in a bc table

	IMPLICIT NONE
	DOUBLE PRECISION, INTENT(IN) :: time
	INTEGER, INTENT(IN) :: table_num, num_values
	DOUBLE PRECISION :: interp(max_cell_values), table_input(:)

    CALL time_series_interp(table_bc(table_num)%ts, time)
	table_input(1:num_values) = table_bc(table_num)%ts%current(1:num_values)
END SUBROUTINE table_interp

!############################################################################################

SUBROUTINE precip_table_interp(time, table_num, table_input)

! returns a VECTOR of the linearly interpolated values in a bc table

  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(IN) :: time
  INTEGER, INTENT(IN) :: table_num
  DOUBLE PRECISION :: table_input
 
  CALL cumulative_time_series_update(precip_bc(table_num)%ts, time)
  table_input = precip_bc(table_num)%ts%rate
END SUBROUTINE precip_table_interp

!############################################################################################################

SUBROUTINE scalar_table_interp(time, table_num, table_input, num_values)

! returns a VECTOR of the linearly interpolated values in a bc table

	IMPLICIT NONE
	DOUBLE PRECISION, INTENT(IN) :: time
	DOUBLE PRECISION, INTENT(INOUT) :: table_input(:)
	INTEGER, INTENT(IN) :: table_num, num_values

    CALL time_series_interp(scalar_table_bc(table_num)%ts, time)
	table_input(1:num_values) = scalar_table_bc(table_num)%ts%current(1:num_values)

END SUBROUTINE scalar_table_interp

END MODULE table_boundary_conditions
