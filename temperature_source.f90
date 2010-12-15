!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 temperature source/sink module file
!
! VERSION and DATE: $Revision$ $Date$
!
! PURPOSE: Enscapsulates the temperature transport source term
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: Created July 24, 2000 by William A. Perkins
! Last Change: Mon Sep 28 07:48:11 2009 by William A. Perkins <d3g096@bearflag.pnl.gov>
!
!***************************************************************
! $Id$

! ----------------------------------------------------------------
! MODULE temperature_source
! This module handles the necessary
! ----------------------------------------------------------------
MODULE temperature_source

  TYPE temperature_source_block_rec
     DOUBLE PRECISION, POINTER :: evaporation(:,:) ! Evaporation rate, ft/s
  END TYPE temperature_source_block_rec

  TYPE temperature_source_rec
     LOGICAL :: doexchange
     TYPE (temperature_source_block_rec), POINTER :: block(:)
  END TYPE temperature_source_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! FUNCTION temperature_parse_options
  ! ----------------------------------------------------------------
  TYPE(temperature_source_rec) FUNCTION temperature_parse_options(options)
    
    USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra
    USE utility
    USE globals
   
    IMPLICIT NONE

    POINTER temperature_parse_options

    CHARACTER (LEN=*) :: options(:)
    CHARACTER (LEN=1024) :: msg
    INTEGER :: nopt
    INTEGER :: i = 1
    INTEGER :: iblk

    nopt = UBOUND(options, 1)

    ALLOCATE(temperature_parse_options)

    temperature_parse_options%doexchange = .FALSE.

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('AIREXCH')
          temperature_parse_options%doexchange = .TRUE.
       CASE DEFAULT
          WRITE(msg, *) 'temperature option "', &
               &TRIM(options(i)), '" not understood and ignored'
          CALL error_message(msg)
       END SELECT
       i = i + 1
    END DO

    ALLOCATE(temperature_parse_options%block(max_blocks))
    DO iblk = 1, max_blocks
       ALLOCATE(temperature_parse_options%block(iblk)%evaporation(&
            &i_index_min:block(iblk)%xmax + i_index_extra, &
            &j_index_min:block(iblk)%ymax + i_index_extra))
       temperature_parse_options%block(iblk)%evaporation = 0.0
    END DO

  END FUNCTION temperature_parse_options

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION temperature_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION temperature_source_term(rec, t)

    USE energy_flux
    USE met_data_module

    IMPLICIT NONE

    TYPE(temperature_source_rec) :: rec
    DOUBLE PRECISION :: t

    temperature_source_term = 0.0

    IF (rec%doexchange) THEN
       temperature_source_term = &
            &net_heat_flux(net_solar, t, t_air, t_dew, windspeed) &
            &/(metric_density*specific_heat/3.2808) ! rho*specifc heat*depth in feet
    END IF
    RETURN
  END FUNCTION temperature_source_term

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION evaporation_rate
  !
  ! This routine uses the evaporation() function to estimate a an water
  ! evaporation rate, in in/day.
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION evaporation_rate(t)
    
    USE energy_flux
    USE met_data_module, ONLY: t_dew, windspeed

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: t ! water surface temperature in degrees C
    
    DOUBLE PRECISION :: lheat
    
    lheat = latent_heat(t)  ! kJ/kg
    lheat = lheat*metric_density  ! kJ/m^3
    lheat = 1000.0*lheat          ! J/m^3
    
    evaporation_rate = evaporation(t, t_dew, windspeed)     ! W/m^2 = J/s/m^2
    evaporation_rate = evaporation_rate/lheat ! m/s
    evaporation_rate = evaporation_rate/0.3048 ! ft/s
    evaporation_rate = evaporation_rate*12.0*3600.0*24.0 ! in/day
    evaporation_rate = -evaporation_rate
  END FUNCTION evaporation_rate

END MODULE temperature_source
