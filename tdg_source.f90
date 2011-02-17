! ----------------------------------------------------------------
! file: tdg_source.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 24, 2000 by William A. Perkins
! Last Change: Thu Feb 17 11:14:29 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------
! $Id$

! ----------------------------------------------------------------
! MODULE tdg_source
! ----------------------------------------------------------------
MODULE tdg_source

  TYPE tdg_source_rec
     LOGICAL :: doexchange
     DOUBLE PRECISION :: gasx_a, gasx_b, gasx_c, gasx_d
  END TYPE tdg_source_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! FUNCTION tdg_parse_options
  ! ----------------------------------------------------------------
  TYPE(tdg_source_rec) FUNCTION tdg_parse_options(options)
    
    USE utility
    
    IMPLICIT NONE

    POINTER tdg_parse_options

    CHARACTER (LEN=*) :: options(:)
    CHARACTER (LEN=1024) :: msg
    INTEGER :: nopt
    INTEGER :: i = 1
    LOGICAL :: param_read = .FALSE.

    nopt = UBOUND(options, 1)

    ALLOCATE(tdg_parse_options)

    tdg_parse_options%doexchange = .FALSE.
    tdg_parse_options%gasx_a = 0.0
    tdg_parse_options%gasx_b = 0.0
    tdg_parse_options%gasx_c = 0.0
    tdg_parse_options%gasx_d = 0.0

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('AIREXCH')
          tdg_parse_options%doexchange = .TRUE.
       CASE ('PARAMETERS')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'tdg'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          CALL tdg_read_param(tdg_parse_options, options(i+1))
          param_read = .TRUE.
          i = i + 1
       CASE DEFAULT
          WRITE(msg, *) 'WARNING: TDG option "', &
               &TRIM(options(i)), '" not understood and ignored'
          CALL error_message(msg)
       END SELECT
       i = i + 1
    END DO

                                ! if we specify air/water exchange
                                ! with "AIREXCH", then we must read
                                ! the parameters, otherwise we don't
                                ! care about parameters

    IF (tdg_parse_options%doexchange .AND. .NOT. param_read) THEN
       WRITE(msg, *) 'air/water exchange parameters ',&
            &'not specified for the TDG species'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
100 FORMAT('file name missing for ', A10, ' PARAMETER keyword')
  END FUNCTION tdg_parse_options

  ! ----------------------------------------------------------------
  ! SUBROUTINE tdg_read_param
  ! ----------------------------------------------------------------
  SUBROUTINE tdg_read_param(tdg_rec, filename)
    USE utility
    IMPLICIT NONE

    TYPE(tdg_source_rec) :: tdg_rec
    CHARACTER (LEN=*) :: filename

    INTEGER, PARAMETER :: grid_iounit=15
    INTEGER :: istat

    CALL open_existing(filename, grid_iounit)
	READ(grid_iounit,*)tdg_rec%gasx_a, tdg_rec%gasx_b, tdg_rec%gasx_c, tdg_rec%gasx_d
	CLOSE(grid_iounit)
    CALL status_message('completed reading surface gas exchange coefficients from ' // &
         &TRIM(filename))

  END SUBROUTINE tdg_read_param


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION tdg_source
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION tdg_source_term(rec, conc, temp, sal)

    USE gas_functions
    USE met_data_module

    IMPLICIT NONE

    TYPE(tdg_source_rec) :: rec
    DOUBLE PRECISION :: conc, temp, sal
    DOUBLE PRECISION :: ccstar, transfer_coeff

    tdg_source_term = 0.0
    IF (rec%doexchange) THEN
       ! c* will be the conc at Barometric Press.
       ccstar = TDGasConc( baro_press, temp,  sal ) 
       transfer_coeff = &
            &rec%gasx_a + rec%gasx_b*windspeed + &
            &rec%gasx_c*windspeed**2 + rec%gasx_d*windspeed**3
       ! convert from meters/day to feet/sec
       transfer_coeff = transfer_coeff*3.2808/86400.0 
       tdg_source_term = transfer_coeff*( ccstar - conc )

    END IF
    RETURN
  END FUNCTION tdg_source_term


END MODULE tdg_source
