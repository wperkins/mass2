MODULE met_data_module

USE table_boundary_conditions
USE date_time

IMPLICIT NONE

DOUBLE PRECISION :: net_solar ! net incoming solar radiation W/m2
DOUBLE PRECISION :: t_air			! air temperature deg. C
DOUBLE PRECISION :: t_dew			!	dewpoint temperature deg. C
DOUBLE PRECISION :: t_water		! water temperature deg. C (from model simulation)
DOUBLE PRECISION :: windspeed	! wind speed m/s
DOUBLE PRECISION :: baro_press ! barometric pressure mm Hg

TYPE(table_bc_struct) :: met_data

CONTAINS

!##################################################################################################
SUBROUTINE read_met_data(weather_filename)
	IMPLICIT NONE
	CHARACTER(LEN=80) :: weather_filename
	INTEGER :: iounit = 50, i, j = 0

	
		OPEN(iounit, file = weather_filename)
		j = 0
		DO WHILE(.TRUE.)
                   j = j + 1
                   READ(iounit,*,END=100)met_data%table_entry(j)%datetime%date_string,&
                        & met_data%table_entry(j)%datetime%time_string,met_data%table_entry(j)%value(:)
                   met_data%max_entries = met_data%max_entries + 1
                   met_data%table_entry(j)%datetime%time =&
                        & date_to_decimal(met_data%table_entry(j)%datetime%date_string,&
                        & met_data%table_entry(j)%datetime%time_string)
		
		END DO
100		CLOSE(iounit)


END SUBROUTINE read_met_data

!#####################################################################################################
SUBROUTINE update_met_data(time)
	IMPLICIT NONE
	DOUBLE PRECISION :: interp(5)
	DOUBLE PRECISION :: time
	INTEGER ::  i, j, num_values = 5
	DOUBLE PRECISION :: factor

	DO j=1,met_data%max_entries-1

           IF((time >= met_data%table_entry(j)%datetime%time)&
                & .AND. (time <= met_data%table_entry(j+1)%datetime%time)) EXIT

	END DO
        factor = (time - met_data%table_entry(j)%datetime%time)/ &
             (met_data%table_entry(j+1)%datetime%time - met_data%table_entry(j)%datetime%time)

	interp(1:num_values) = met_data%table_entry(j)%value(1:num_values) + &
             factor*(met_data%table_entry(j+1)%value(1:num_values) - met_data%table_entry(j)%value(1:num_values)	)					
	
	t_air = interp(1)
	t_dew = interp(2)
	windspeed = interp(3)
	baro_press = interp(4)
	net_solar = interp(5)


END SUBROUTINE update_met_data


END MODULE met_data_module
