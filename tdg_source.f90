! ----------------------------------------------------------------
! file: tdg_source.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 24, 2000 by William A. Perkins
! Last Change: Wed Jul 26 13:01:41 2000 by William A. Perkins <perk@dora.pnl.gov>
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
    
    USE misc_vars, ONLY: error_iounit
    
    IMPLICIT NONE

    POINTER tdg_parse_options

    CHARACTER (LEN=*) :: options(:)
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
             WRITE(*, 100) 'tdg'
             WRITE(error_iounit, 100) 'tdg'
             CALL EXIT(8)
          END IF
          CALL tdg_read_param(tdg_parse_options, options(i+1))
          param_read = .TRUE.
          i = i + 1
       CASE DEFAULT
          WRITE(error_iounit, *) 'WARNING: TDG option "', &
               &TRIM(options(i)), '" not understood and ignored'
       END SELECT
       i = i + 1
    END DO

                                ! if we specify air/water exchange
                                ! with "AIREXCH", then we must read
                                ! the parameters, otherwise we don't
                                ! care about parameters

    IF (tdg_parse_options%doexchange .AND. .NOT. param_read) THEN
       WRITE(error_iounit, *) 'FATAL ERROR: air/water exchange parameters ',&
            &'not specified for the TDG species'
       WRITE(*, *) 'FATAL ERROR: air/water exchange parameters ',&
            &'not specified for the TDG species'
       CALL EXIT(8)
    END IF
100 FORMAT('FATAL ERROR: file name missing for ', A10, ' PARAMETER keyword')
  END FUNCTION tdg_parse_options

  ! ----------------------------------------------------------------
  ! SUBROUTINE tdg_read_param
  ! ----------------------------------------------------------------
  SUBROUTINE tdg_read_param(tdg_rec, filename)
    USE misc_vars, ONLY: grid_iounit, status_iounit

    IMPLICIT NONE

    TYPE(tdg_source_rec) :: tdg_rec
    CHARACTER (LEN=*) :: filename

    INTEGER :: istat

	OPEN(grid_iounit,FILE=filename, STATUS='old', IOSTAT=istat)
	READ(grid_iounit,*)tdg_rec%gasx_a, tdg_rec%gasx_b, tdg_rec%gasx_c, tdg_rec%gasx_d
	CLOSE(grid_iounit)
	WRITE(status_iounit,*)'completed reading surface gas exchange coefficients'

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
