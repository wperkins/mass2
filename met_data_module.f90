MODULE met_data_module

USE time_series
USE date_time

IMPLICIT NONE

DOUBLE PRECISION :: net_solar ! net incoming solar radiation W/m2
DOUBLE PRECISION :: t_air			! air temperature deg. C
DOUBLE PRECISION :: t_dew			!	dewpoint temperature deg. C
! DOUBLE PRECISION :: t_water		! water temperature deg. C (from model simulation)
DOUBLE PRECISION :: windspeed	! wind speed m/s
DOUBLE PRECISION :: baro_press ! barometric pressure mm Hg

TYPE (time_series_rec), POINTER :: met_data

CONTAINS

!##################################################################################################
SUBROUTINE read_met_data(weather_filename)
  IMPLICIT NONE
  CHARACTER(LEN=80) :: weather_filename
  INTEGER, PARAMETER :: weather_fields = 5
  INTEGER :: iounit = 50, i, j = 0

  met_data => time_series_read(weather_filename, weather_fields, iounit)

END SUBROUTINE read_met_data

!#####################################################################################################
SUBROUTINE update_met_data(time)
  IMPLICIT NONE
  DOUBLE PRECISION :: interp(5)
  DOUBLE PRECISION :: time
  INTEGER ::  i, j, num_values = 5
  DOUBLE PRECISION :: factor

  CALL time_series_interp(met_data, time)
  t_air = met_data%current(1)
  t_dew = met_data%current(2)
  windspeed = met_data%current(3)
  baro_press = met_data%current(4)
  net_solar = met_data%current(5)
  

END SUBROUTINE update_met_data


END MODULE met_data_module
