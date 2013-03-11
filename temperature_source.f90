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

  USE constants
  USE const_series

  IMPLICIT NONE

  TYPE temperature_source_block_rec
     DOUBLE PRECISION, POINTER :: evaporation(:,:) ! Evaporation rate, ft/s
  END TYPE temperature_source_block_rec

  TYPE temperature_source_rec
     LOGICAL :: doexchange
     LOGICAL :: doevaporate
     TYPE (const_series_rec), POINTER :: specific_heat_ts ! J/kg
     TYPE (temperature_source_block_rec), POINTER :: block(:)
  END TYPE temperature_source_rec

  DOUBLE PRECISION, PARAMETER, PRIVATE :: const_specific_heat = 4186.0 ! J/kg

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
    INTEGER :: iounit = 50

    nopt = UBOUND(options, 1)

    ALLOCATE(temperature_parse_options)

    temperature_parse_options%doexchange = .FALSE.
    temperature_parse_options%doevaporate = .FALSE.
    NULLIFY(temperature_parse_options%specific_heat_ts)

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('AIREXCH')
          temperature_parse_options%doexchange = .TRUE.
       CASE ('EVAPORATE')
          temperature_parse_options%doevaporate = .TRUE.
       CASE ('SPECIFIC_HEAT') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'SPECIFIC_HEAT'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          temperature_parse_options%specific_heat_ts => &
               &const_series_read(options(i+1)) 
          i = i + 1
       CASE DEFAULT
          WRITE(msg, *) 'temperature option "', &
               &TRIM(options(i)), '" not understood and ignored'
          CALL error_message(msg)
       END SELECT
       i = i + 1
    END DO

    IF (.NOT. ASSOCIATED(temperature_parse_options%specific_heat_ts)) THEN
       temperature_parse_options%specific_heat_ts => &
            &const_series_alloc(const_specific_heat)
    END IF

    ALLOCATE(temperature_parse_options%block(max_blocks))
    DO iblk = 1, max_blocks
       ALLOCATE(temperature_parse_options%block(iblk)%evaporation(&
            &i_index_min:block(iblk)%xmax + i_index_extra, &
            &j_index_min:block(iblk)%ymax + i_index_extra))
       temperature_parse_options%block(iblk)%evaporation = 0.0
    END DO
100 FORMAT('additional argument missing for ', A10, ' keyword')
  END FUNCTION temperature_parse_options

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION temperature_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION temperature_source_term(rec, t)

    USE energy_flux
    USE met_zone

    IMPLICIT NONE

    TYPE(temperature_source_rec) :: rec
    DOUBLE PRECISION :: t
    DOUBLE PRECISION :: spheat


    temperature_source_term = 0.0

    IF (rec%doexchange) THEN
       spheat = const_specific_heat
       IF (ASSOCIATED(rec%specific_heat_ts)) THEN
          spheat = rec%specific_heat_ts%current
       END IF
       
       temperature_source_term = &
            &met_zone_heat_flux(met_zones(1), t)&
            &/(metric_density*spheat/3.2808) ! rho*specifc heat*depth in feet
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
    USE met_zone

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: t ! water surface temperature in degrees C

    evaporation_rate = met_zone_evaporation_rate(met_zones(1), t)     ! W/m^2 = J/s/m^2
    evaporation_rate = -evaporation_rate
  END FUNCTION evaporation_rate

END MODULE temperature_source
