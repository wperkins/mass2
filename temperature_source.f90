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
! Last Change: Mon Nov 28 09:53:11 2011 by William A. Perkins <d3g096@flophouse>
!
!***************************************************************
! $Id$

! ----------------------------------------------------------------
! MODULE temperature_source
! This module handles the necessary
! ----------------------------------------------------------------
MODULE temperature_source

  INTEGER, PARAMETER, PRIVATE :: met_ncoeff = 4

  TYPE temperature_source_rec
     LOGICAL :: doexchange
     ! exchange_coeff: 1,2 wind function, 3 conduction, 4 brunt
     DOUBLE PRECISION :: exchange_coeff(met_ncoeff)
  END TYPE temperature_source_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! FUNCTION temperature_parse_options
  ! ----------------------------------------------------------------
  TYPE(temperature_source_rec) FUNCTION temperature_parse_options(options)
    
    USE utility
    
    IMPLICIT NONE

    POINTER temperature_parse_options

    CHARACTER (LEN=*) :: options(:)
    CHARACTER (LEN=1024) :: msg
    INTEGER :: nopt
    INTEGER :: i = 1, k

    nopt = UBOUND(options, 1)

    ALLOCATE(temperature_parse_options)

    temperature_parse_options%doexchange = .FALSE.

    ! These are the coefficients presented by Edinger:
    temperature_parse_options%exchange_coeff(1) = 0.46 ! wind function multiplier (aw)
    temperature_parse_options%exchange_coeff(2) = 9.2  ! wind function offset (bw)
    temperature_parse_options%exchange_coeff(3) = 0.47 ! conduction (Cc)
    temperature_parse_options%exchange_coeff(4) = 0.65 ! Brunt (Ca)

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('AIREXCH')
          temperature_parse_options%doexchange = .TRUE.
       CASE ('COEFF')
          IF (i + met_ncoeff .GT. nopt) THEN
             WRITE(msg, 100) 'COEFF'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          DO k = 1, 4
             IF (LEN_TRIM(options(i+1)) .LE. 0) THEN
                WRITE(msg, 100) 'COEFF'
                CALL error_message(msg, fatal=.TRUE.)
             END IF
             READ(options(i+1), *) temperature_parse_options%exchange_coeff(k)
             i = i + 1
          END DO
          
       CASE DEFAULT
          WRITE(msg, *) 'temperature option "', &
               &TRIM(options(i)), '" not understood and ignored'
          CALL error_message(msg)
       END SELECT
       i = i + 1
    END DO

    IF (temperature_parse_options%doexchange) THEN 
       CALL status_message("Atmospheric energy exchange enabled")
       WRITE(msg, 200) &
            & temperature_parse_options%exchange_coeff(1),&
            & temperature_parse_options%exchange_coeff(2),&
            & temperature_parse_options%exchange_coeff(3),&
            & temperature_parse_options%exchange_coeff(4)
       CALL status_message(msg)
    ELSE
       CALL status_message("Atmospheric energy exchange disabled")
    END IF

100 FORMAT('additional argument missing for ', A10, ' keyword')
200 FORMAT("Atmospheric energy exchange coefficients: ", F6.3, 3(", ", F6.3))
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
            &net_heat_flux(rec%exchange_coeff, net_solar, t, t_air, t_dew, windspeed) &
            &/(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet
    END IF
    RETURN
  END FUNCTION temperature_source_term
END MODULE temperature_source
