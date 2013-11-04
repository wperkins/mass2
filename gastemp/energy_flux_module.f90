
MODULE energy_flux

  IMPLICIT NONE

  PRIVATE :: net_solar_rad, net_longwave, back_radiation
  PRIVATE :: conduction, windspeed, rel_humid, sat_vapor_press

  DOUBLE PRECISION, PUBLIC, PARAMETER :: &
       & stephan_boltz = 5.67e-8  ! stephan-boltzmann constant in W/m2-K4

  INTEGER, PUBLIC, PARAMETER :: &
       &ENERGY_COEFF_WINDA = 1, &
       &ENERGY_COEFF_WINDB = 2, &
       &ENERGY_COEFF_CONDUCTION = 3, &
       &ENERGY_COEFF_BRUNT = 4, &
       &ENERGY_COEFF_EMISS = 5, &
       &ENERGY_COEFF_REFLECT = 6, &
       &ENERGY_COEFF_ALBEDO = 7, &
       &ENERGY_COEFF_EXTINCT = 8

  INTEGER, PUBLIC, PARAMETER :: ENERGY_COEFF_MAX = ENERGY_COEFF_EXTINCT

  DOUBLE PRECISION, PARAMETER, PUBLIC :: energy_coeff_default(ENERGY_COEFF_MAX) = (/ &
       &0.46, &           ! wind function modifier
       &9.2, &            ! wind function offset
       &0.47, &           ! conduction coefficient
       &0.80, &           ! "brunt" coefficient for lw back radiation
       &0.97, &           ! emissivity for outgoing lw radiation
       &0.03, &           ! reflectivity for lw back radiation
       &0.00, &           ! albedo/reflectivity for incoming sw radiation
       &0.00 /)           ! light extinction coefficient

  CHARACTER (len=20), PARAMETER, PUBLIC :: energy_coeff_name(ENERGY_COEFF_MAX) = (/&
       &'WINDA', &
       &'WINDB', &
       &'CONDUCTION', &
       &'BRUNT', &
       &'EMISS', &
       &'REFLECT', &
       &'ALBEDO', &
       &'EXTINCT' /)

CONTAINS
  !######################################################################
  DOUBLE PRECISION FUNCTION net_heat_flux(coeff, net_solar, t_water, depth, t_air, t_dew, wind_speed)
    !
    ! Hnet - (watts/m^2)
    !
    ! computes the net heat flux at the water surface
    ! equations in the report
    !
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: net_solar,t_water, depth, t_air, t_dew, wind_speed


    net_heat_flux = net_solar_rad(coeff, net_solar, depth) + &
         &net_longwave(coeff, t_air, t_dew) + &
         &back_radiation(coeff, t_water) + &
         &evaporation(coeff, t_water, t_dew, wind_speed) + &
         &conduction(coeff, t_water, t_air, wind_speed) 

  END FUNCTION net_heat_flux
  !######################################################################
  DOUBLE PRECISION FUNCTION net_solar_rad(coeff, sr0, depth)
    IMPLICIT NONE
    ! Hso
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: sr0, depth, srbed

    net_solar_rad = sr0*(1.0 - coeff(ENERGY_COEFF_ALBEDO))
    IF (coeff(ENERGY_COEFF_EXTINCT) .GT. 0.0) THEN
       srbed = net_solar_rad*exp(-coeff(ENERGY_COEFF_EXTINCT)*depth)
    ELSE 
       srbed = 0.0
    END IF
    net_solar_rad = net_solar_rad - srbed

  END FUNCTION net_solar_rad

  !######################################################################
  DOUBLE PRECISION FUNCTION net_longwave(coeff, t_air, t_dew)
    !
    ! Han - (Watts/meter^2)
    !
    ! net longwave atmospheric radiation ( W/m2 )
    ! Formula 2.1.1 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_air, t_dew
    DOUBLE PRECISION :: reflect ! relflectance 

    reflect = coeff(ENERGY_COEFF_REFLECT)

    net_longwave = 4.4e-8*(t_air + 273.15)**4 * &
         ( coeff(ENERGY_COEFF_BRUNT) + &
         & 0.031*SQRT(sat_vapor_press(t_dew)) )*(1.0 - reflect)

  END FUNCTION net_longwave

  !#########################################################################
  DOUBLE PRECISION FUNCTION back_radiation(coeff, t_water)
    !
    ! Hb - (Watts/meter^2)
    !
    ! longwave back radiation (heat flux OUT) (black body radiation)
    ! formula 2.1.4 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_water				! water surface temperature in degrees C
    DOUBLE PRECISION :: emiss   ! emissivity of water
    emiss = coeff(ENERGY_COEFF_EMISS)
    back_radiation = -emiss*stephan_boltz*(t_water + 273.15)**4

  END FUNCTION back_radiation

  !#########################################################################
  DOUBLE PRECISION FUNCTION evaporation(coeff, t_water, t_dew, wind_speed)
    !
    ! He - (Watts/m^2)
    !
    ! heat flux OUT due to water evaporation
    ! formula 2.1.5 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_water	! water surface temperature in degrees C
    DOUBLE PRECISION :: t_dew 	! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

    evaporation = -windspeed(coeff, wind_speed)*&
         &( sat_vapor_press(t_water) - sat_vapor_press(t_dew) )

  END FUNCTION evaporation

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION latent_heat
  !
  ! Computes the latent heat of vaporization for water (kJ/kg) given the
  ! temperature (degree C).  
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION latent_heat(T)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: T

    ! Got this from Wikipedia (I know, I know), which referenced
    !
    !   Quartic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A
    !   Short Course in Cloud Physics, 2e,(1989), Pergamon press

    ! latent_heat = - 0.0000614342*T**3 + 0.00158927*T**2 - 2.36418*T + 2500.79

    ! This is a little more defendable and very close to the above, from
    !
    !   Sinokrot B and H Stefan. 1994. “Stream Water-Temperature
    !   Sensitivity to Weather and Bed Pa- rameters.”  Journal of
    !   Hydraulic Engineering 120(6):722–736.

    latent_heat = (597.31 - 0.5631*T)*4.186

  END FUNCTION latent_heat


  !#########################################################################
  DOUBLE PRECISION FUNCTION conduction(coeff, t_water, t_air, wind_speed)
    !
    ! Hc - (Watts/m^2)
    !
    ! heat flux OUT due to conduction
    ! formula 2.1.11 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_water	! water surface temperature in degrees C
    DOUBLE PRECISION :: t_air		! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

    conduction = -coeff(ENERGY_COEFF_CONDUCTION)*&
         &windspeed(coeff, wind_speed)*(t_water - t_air)

  END FUNCTION conduction

  !#########################################################################
  DOUBLE PRECISION FUNCTION windspeed(coeff, wind_speed)
    !
    ! wind speed function required by conduction and evaporative heat fluxes
    ! has units of Watts/(m^2 mmHg)
    !
    ! formula 2.4.6 of in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

    ! formula 2.4.6 of in Edinger, Brady, Geyer (1974)
    ! windspeed = 9.2 + 0.46*wind_speed**2
    ! windspeed = 0.5*windspeed

    ! from the QUAL2E formulation (with unit conversions)

!!$windspeed = 1000.0              ! density: kg/m3 
!!$windspeed = windspeed*(595 - 0.5*temp)*4186.8 ! latent heat of vaporization J/kg
!!$windspeed = windspeed*(2.3D-09 + 2.0D-09*wind_speed)
    windspeed = coeff(ENERGY_COEFF_WINDB) + &
         &coeff(ENERGY_COEFF_WINDA)*wind_speed**2

  END FUNCTION windspeed


  !#########################################################################
  DOUBLE PRECISION FUNCTION rel_humid(t_air,t_dew)
    !
    ! returns relative humidity (fraction) as a function of 
    ! air temp and dew point temp
    ! eqn. 2-9 in Hydrology for Engineers
    !
    IMPLICIT NONE
    DOUBLE PRECISION :: t_air, t_dew ! air temp and dew-point temp in degrees C

    rel_humid = ( (112 - 0.1*t_air + t_dew)/(112 + 0.9*t_air) )**8

  END FUNCTION rel_humid

  !########################################################################
  DOUBLE PRECISION FUNCTION sat_vapor_press(t_air)
    !
    ! returns saturation vapor pressure of air in mmHg
    ! given an air temp in degrees C
    !
    ! Raudkivi (1979) formula
    !
    ! Note that the air vapor pressure is the air saturation vapor pressure
    !			evaluated at the dew point temperature
    !
    ! See also: 
    !   Sinokrot B and H Stefan. 1994. “Stream WaterTemperature
    !   Sensitivity to Weather and Bed Pa- rameters.” Journal of
    !   Hydraulic Engineering 120(6):722–736.
    ! equation 15
    IMPLICIT NONE
    DOUBLE PRECISION :: t_air

    sat_vapor_press = 4.596*EXP( (17.27*t_air)/(237.3+ t_air) )

  END FUNCTION sat_vapor_press

  !#######################################################################
  !DOUBLE PRECISION FUNCTION vapor_press
  !
  ! not needed as dew point temp is an input to model
  !
  !IMPLICIT NONE
  !
  !vapor_press = rel_humid(t_air,t_dew)*sat_vapor_press(t_air)
  !END FUNCTION vapor_press



END MODULE energy_flux
