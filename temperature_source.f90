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
! Last Change: Wed Jul 26 08:02:29 2000 by William A. Perkins <perk@dora.pnl.gov>
!
!***************************************************************
! $Id$

! ----------------------------------------------------------------
! MODULE temperature_source
! This module handles the necessary
! ----------------------------------------------------------------
MODULE temperature_source

  TYPE temperature_source_rec
     LOGICAL :: doexchange
  END TYPE temperature_source_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! FUNCTION temperature_parse_options
  ! ----------------------------------------------------------------
  TYPE(temperature_source_rec) FUNCTION temperature_parse_options(options)
    
    USE misc_vars, ONLY: error_iounit
    
    IMPLICIT NONE

    POINTER temperature_parse_options

    CHARACTER (LEN=*) :: options(:)
    INTEGER :: nopt
    INTEGER :: i = 1

    nopt = UBOUND(options, 1)

    ALLOCATE(temperature_parse_options)

    temperature_parse_options%doexchange = .FALSE.

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('AIREXCH')
          temperature_parse_options%doexchange = .TRUE.
       CASE DEFAULT
          WRITE(error_iounit, *) 'WARNING: temperature option "', &
               &TRIM(options(i)), '" not understood and ignored'
       END SELECT
       i = i + 1
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
            &net_heat_flux(net_solar, t_water, t_air, t_dew, windspeed) &
            &/(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet
    END IF
    RETURN
  END FUNCTION temperature_source_term
END MODULE temperature_source
