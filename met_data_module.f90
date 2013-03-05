MODULE met_data_module

  USE met_time_series
  USE date_time

  IMPLICIT NONE

  DOUBLE PRECISION :: net_solar ! net incoming solar radiation W/m2
  DOUBLE PRECISION :: t_air			! air temperature deg. C
  DOUBLE PRECISION :: t_dew			!	dewpoint temperature deg. C
  DOUBLE PRECISION :: t_water		! water temperature deg. C (from model simulation)
  DOUBLE PRECISION :: windspeed	! wind speed m/s
  DOUBLE PRECISION :: baro_press ! barometric pressure mm Hg

  TYPE (met_time_series_rec), POINTER :: met_data

CONTAINS

  !##################################################################################################
  SUBROUTINE read_met_data(weather_filename)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: weather_filename
    INTEGER :: i, j = 0

    met_data => met_time_series_read(weather_filename)

  END SUBROUTINE read_met_data

  !#####################################################################################################
  SUBROUTINE update_met_data(time)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: time

    CALL met_time_series_update(met_data, time)
    t_air = met_data%current(MET_AIRT)
    t_dew = met_data%current(MET_DEWT)
    windspeed = met_data%current(MET_WIND)
    baro_press = met_data%current(MET_BARO)
    net_solar = met_data%current(MET_SWRAD)

  END SUBROUTINE update_met_data


END MODULE met_data_module
