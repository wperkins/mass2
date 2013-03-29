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
  USE thermal_bed

  IMPLICIT NONE

  TYPE temperature_source_block_rec
     DOUBLE PRECISION, POINTER :: evaporation(:,:) ! Evaporation rate, ft/s
     TYPE (thermal_bed_rec), POINTER :: bed(:, :)
  END TYPE temperature_source_block_rec

  TYPE temperature_source_rec
     LOGICAL :: doexchange
     LOGICAL :: doevaporate
     LOGICAL :: dobed
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
    INTEGER :: i = 1, j
    INTEGER :: iblk
    INTEGER :: iounit = 50

    DOUBLE PRECISION :: bed_cond, bed_spgrav, bed_spheat, bed_depth, bed_initial
    INTEGER :: bed_layers
    
    bed_depth = 0.0
    bed_layers = 0.0

    ! default thermal properties represent a sandy soil with 40% porosity
    bed_cond = 0.3
    bed_spgrav = 1.6
    bed_spheat = 800

    bed_initial = 0.0

    nopt = UBOUND(options, 1)

    ALLOCATE(temperature_parse_options)

    temperature_parse_options%doexchange = .FALSE.
    temperature_parse_options%doevaporate = .FALSE.
    temperature_parse_options%dobed = .FALSE.
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
       CASE ('BED_DEPTH') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_DEPTH'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_depth
          i = i + 1
       CASE ('BED_LAYERS') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_LAYERS'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_layers
          i = i + 1
       CASE ('BED_SPECIFIC_HEAT') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_SPECIFIC_HEAT'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_spheat
          i = i + 1
       CASE ('BED_SPECIFIC_GRAVITY') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_SPECIFIC_GRAVITY'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_spgrav
          i = i + 1
       CASE ('BED_CONDUCTIVITY') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_CONDUCTIVITY'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_cond
          i = i + 1
       CASE ('BED_INITIAL') 
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'BED_INITIAL'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) bed_initial
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

    IF (bed_depth .GT. 0.0 .AND. bed_layers .GT. 0) THEN
       temperature_parse_options%dobed = .TRUE.
       DO iblk = 1, max_blocks
          ALLOCATE(temperature_parse_options%block(iblk)%bed(&
               &2:block(iblk)%xmax, &
               &2:block(iblk)%ymax))
          DO i = 2, block(iblk)%xmax
             DO j = 2, block(iblk)%ymax
                CALL thermal_bed_initialize(temperature_parse_options%block(iblk)%bed(i,j), &
                     & bed_depth, bed_layers, &
                     & bed_cond, bed_spheat, bed_spgrav, bed_initial, bed_initial)
             END DO
          END DO
       END DO
    END IF
100 FORMAT('additional argument missing for ', A10, ' keyword')
  END FUNCTION temperature_parse_options

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION temperature_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION temperature_source_term(rec, iblk, i, j, t, d)

    USE energy_flux
    USE met_zone

    IMPLICIT NONE

    TYPE(temperature_source_rec) :: rec
    INTEGER :: iblk, i, j
    DOUBLE PRECISION :: t, d
    DOUBLE PRECISION :: spheat


    temperature_source_term = 0.0

    spheat = const_specific_heat
    IF (rec%doexchange .OR. rec%dobed) THEN
       IF (ASSOCIATED(rec%specific_heat_ts)) THEN
          spheat = rec%specific_heat_ts%current
       END IF
    END IF

    IF (rec%doexchange) THEN
       temperature_source_term = temperature_source_term + &
            &met_zone_heat_flux(met_zones(1), t, d)&
            &/(metric_density*spheat/3.2808) ! rho*specifc heat*depth in feet
    END IF

    IF (rec%dobed) THEN
       temperature_source_term = temperature_source_term - &
            &thermal_bed_flux(rec%block(iblk)%bed(i,j)) &
            &/(metric_density*spheat/3.2808)
    END IF

    RETURN
  END FUNCTION temperature_source_term

  ! ----------------------------------------------------------------
  ! SUBROUTINE temperature_source_timestep
  ! ----------------------------------------------------------------
  SUBROUTINE temperature_source_timestep(rec, tempidx, time, delta_t)

    USE globals, ONLY: block, max_blocks
    USE scalars, ONLY: species

    IMPLICIT NONE

    TYPE (temperature_source_rec), INTENT(INOUT) :: rec
    DOUBLE PRECISION, INTENT(IN) :: time, delta_t
    INTEGER, INTENT(IN) :: tempidx

    INTEGER :: iblk, i, j
    DOUBLE PRECISION :: t_water

    CALL const_series_update(rec%specific_heat_ts, time)
    
    DO iblk = 1, max_blocks
       DO i = 2, block(iblk)%xmax
          DO j = 2, block(iblk)%ymax
             t_water = species(tempidx)%scalar(iblk)%conc(i, j)
             CALL thermal_bed_solve(rec%block(iblk)%bed(i,j), delta_t, t_water)
          END DO
       END DO
    END DO

  END SUBROUTINE temperature_source_timestep


  ! ! ----------------------------------------------------------------
  ! ! DOUBLE PRECISION FUNCTION evaporation_rate
  ! !
  ! ! This routine uses the evaporation() function to estimate a an water
  ! ! evaporation rate, in in/day.
  ! ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION evaporation_rate(t)
    
  !   USE energy_flux
  !   USE met_zone

  !   IMPLICIT NONE

  !   DOUBLE PRECISION, INTENT(IN) :: t ! water surface temperature in degrees C

  !   evaporation_rate = met_zone_evaporation_rate(met_zones(1), t) 
  !   evaporation_rate = -evaporation_rate
  ! END FUNCTION evaporation_rate

END MODULE temperature_source
