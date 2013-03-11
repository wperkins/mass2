  ! ----------------------------------------------------------------
  ! file: met_zone.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created March  4, 2013 by William A. Perkins
  ! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
  ! ----------------------------------------------------------------
  
! ----------------------------------------------------------------
! MODULE met_zone
! ----------------------------------------------------------------
MODULE met_zone

  USE constants
  USE const_series
  USE met_time_series
  USE energy_flux

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE met_zone_rec
     INTEGER :: id
     TYPE (met_time_series_rec), POINTER :: met
     DOUBLE PRECISION, POINTER :: current(:)
     TYPE (const_series_rec), POINTER :: winda
     TYPE (const_series_rec), POINTER :: windb
     TYPE (const_series_rec), POINTER :: conduction
     TYPE (const_series_rec), POINTER :: brunt
     DOUBLE PRECISION :: coeff(ENERGY_COEFF_MAX)
  END type met_zone_rec

  TYPE (met_zone_rec), ALLOCATABLE, PUBLIC :: met_zones(:)
  INTEGER, ALLOCATABLE, PUBLIC :: met_zone_index(:)

  INTEGER, PARAMETER, PRIVATE :: metspec_iounit = 21
  INTEGER, PARAMETER, PRIVATE :: metspec_max_option = 100

  DOUBLE PRECISION, PARAMETER, PUBLIC :: default_baro_press = 760.0

  INTERFACE met_zone_heat_flux
     MODULE PROCEDURE met_zone_heat_flux_zone
     MODULE PROCEDURE met_zone_heat_flux_byid
  END INTERFACE met_zone_heat_flux
     
  INTERFACE met_zone_evaporation_rate
     MODULE PROCEDURE met_zone_evaporation_zone
     MODULE PROCEDURE met_zone_evaporation_byid
  END INTERFACE met_zone_evaporation_rate
     
CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_set_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_set_coeff(crec, options, idx, ierr)

    IMPLICIT NONE
    
    TYPE (const_series_rec), POINTER :: crec
    CHARACTER (LEN=*), INTENT(IN) :: options(:)
    INTEGER, INTENT(INOUT) :: idx, ierr

    INTEGER :: nopt
    DOUBLE PRECISION :: tmp

    nopt = UBOUND(options, 1)
    
    IF (idx .LT. nopt) THEN
       IF (options(idx+1) == "TABLE") THEN
          CALL const_series_destroy(crec)
          crec => const_series_read(options(idx+2))
          idx = idx + 2
       ELSE 
          READ (options(idx+1), *, ERR=100) tmp
          crec%defvalue = tmp
          idx = idx + 1
       END IF
    ELSE 
       ierr = ierr + 1
    END IF

    RETURN
100 CONTINUE
    ierr = ierr + 1

  END SUBROUTINE met_zone_set_coeff


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_read
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_read(mzone, options, ierr)

    IMPLICIT NONE

    TYPE (met_zone_rec) :: mzone
    CHARACTER (LEN=*), INTENT(IN) :: options(:)
    INTEGER, INTENT(INOUT) :: ierr
    
    CHARACTER (LEN=1024) :: msg
    INTEGER :: i, nopt
    DOUBLE PRECISION :: tmp

    nopt = UBOUND(options, 1)

    ! The first option word should be an integer identifier

    i = 1
    READ(options(i), *, ERR=100) mzone%id
    i = i + 1
    GOTO 200 

100 CONTINUE
    WRITE(msg, *) "trouble with ZONE word: ", TRIM(options(i))
    CALL error_message(msg, fatal=.FALSE.)
    ierr = ierr + 1

200 CONTINUE

    ! set the zone coefficients to default

    mzone%winda => const_series_alloc(0.46d00)
    mzone%windb => const_series_alloc(9.2d00)
    mzone%brunt => const_series_alloc(0.8d00)
    mzone%conduction => const_series_alloc(0.47d00)

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ("TABLE")
          mzone%met => met_time_series_read(options(i+1))
          mzone%current => mzone%met%current
          i = i + 1
       CASE ("WINDA")
          CALL met_zone_set_coeff(mzone%winda, options, i, ierr)
       CASE ("WINDB")
          CALL met_zone_set_coeff(mzone%windb, options, i, ierr)
       CASE ("CONDUCTION")
          CALL met_zone_set_coeff(mzone%conduction, options, i, ierr)
       CASE ("BRUNT")
          CALL met_zone_set_coeff(mzone%brunt, options, i, ierr)
       CASE DEFAULT
          WRITE(msg, *) "unknown ZONE keyword: ", TRIM(options(i))
          CALL error_message(msg, fatal=.FALSE.)
          ierr = ierr + 1
       END SELECT
       i = i + 1
    END DO

    IF (ierr .GT. 0) THEN 
       WRITE(msg, *) "trouble reading zone record starting with ", TRIM(options(1))
       CALL error_message(msg, fatal=.FALSE.)
    END IF

  END SUBROUTINE met_zone_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_read_specs
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_read_specs(filename)

    IMPLICIT NONE
   
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CHARACTER (LEN=1024) :: msg
    CHARACTER (LEN=256) :: kword, options(metspec_max_option)
    INTEGER :: nzone, izone, i, nopt, ierr, maxzoneid

    CALL open_existing(filename, metspec_iounit)
    CALL status_message("Reading met specs from " // &
         &TRIM(filename))

    nzone = 0
    ierr = 0
    DO WHILE (.TRUE.)
       READ (metspec_iounit, *, END=100) kword, options
       SELECT CASE (kword)
       CASE ("ZONE")
          nzone = nzone + 1
       CASE DEFAULT
          WRITE(msg, *) TRIM(filename), ": unknown keyword: ", TRIM(kword)
          CALL error_message(msg, fatal=.FALSE.)
          ierr = ierr + 1
       END SELECT
    END DO

100 CONTINUE
    REWIND(metspec_iounit)

    IF (ierr .GT. 0) GOTO 300

    ALLOCATE(met_zones(nzone))
    izone = 0
    nopt = UBOUND(options, 1)
    DO WHILE (.TRUE.) 
       options = ""
       READ (metspec_iounit, *, END=200) kword, options
       SELECT CASE (kword)
       CASE ("ZONE")
          izone = izone + 1
          CALL met_zone_read(met_zones(izone), options, ierr)
       CASE DEFAULT
          WRITE(msg, *) TRIM(filename), ": unknown keyword: ", TRIM(kword)
          CALL error_message(msg, fatal=.FALSE.)
          ierr = ierr + 1
       END SELECT
    END DO
    
    
200 CONTINUE

    CLOSE(metspec_iounit)

    maxzoneid = MAXVAL(met_zones(:)%id)
    ALLOCATE(met_zone_index(maxzoneid))
    DO izone = 1, nzone
       met_zone_index(met_zones(izone)%id) = izone
    END DO
       
    RETURN

300 CONTINUE
     WRITE(msg, *) TRIM(filename), ": ", ierr, " errors"
     CALL error_message(msg, fatal=.TRUE.)
    CLOSE(metspec_iounit)
  END SUBROUTINE met_zone_read_specs

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_update
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_update(time)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: time
    INTEGER :: i

    DO i = 1, UBOUND(met_zones, 1)
       CALL const_series_update(met_zones(i)%winda, time)
       CALL const_series_update(met_zones(i)%windb, time)
       CALL const_series_update(met_zones(i)%conduction, time)
       CALL const_series_update(met_zones(i)%brunt, time)
       CALL met_time_series_update(met_zones(i)%met, time)
       met_zones(i)%coeff(ENERGY_COEFF_WINDA) = met_zones(i)%winda%current
       met_zones(i)%coeff(ENERGY_COEFF_WINDB) = met_zones(i)%windb%current
       met_zones(i)%coeff(ENERGY_COEFF_CONDUCTION) = met_zones(i)%conduction%current
       met_zones(i)%coeff(ENERGY_COEFF_BRUNT) = met_zones(i)%brunt%current
    END DO
  END SUBROUTINE met_zone_update


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_destroy()

    IMPLICIT NONE

    INTEGER :: i

    DO i = 1, UBOUND(met_zones, 1)
       CALL const_series_destroy(met_zones(i)%winda)
       CALL const_series_destroy(met_zones(i)%windb)
       CALL const_series_destroy(met_zones(i)%brunt)
       CALL const_series_destroy(met_zones(i)%conduction)
       CALL met_time_series_destroy(met_zones(i)%met)
    END DO
    DEALLOCATE(met_zones)

  END SUBROUTINE met_zone_destroy

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_summary
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_summary(iounit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit

    INTEGER :: i, nzone

    nzone = UBOUND(met_zones, 1)

    DO i = 1, nzone
       WRITE (iounit, 10)
       WRITE (iounit, 11) i, nzone
       WRITE (iounit, 10)
       WRITE (iounit, 16) met_zones(i)%id
       IF (ASSOCIATED(met_zones(i)%met)) THEN
          WRITE(iounit, 12) TRIM(met_zones(i)%met%ts%filename)
       ELSE 
          WRITE(iounit, 12) 'none'
       END IF
       WRITE (iounit, 15)
       IF (ASSOCIATED(met_zones(i)%winda%ts)) THEN
          WRITE(iounit, 14) 'WINDA', TRIM(met_zones(i)%winda%ts%filename), &
               &met_zones(i)%winda%current
       ELSE 
          WRITE(iounit, 13) 'WINDA', met_zones(i)%winda%current
       END IF

       IF (ASSOCIATED(met_zones(i)%windb%ts)) THEN
          WRITE(iounit, 14) 'WINDB', TRIM(met_zones(i)%windb%ts%filename), &
               &met_zones(i)%windb%current
       ELSE 
          WRITE(iounit, 13) 'WINDB', met_zones(i)%windb%current
       END IF

       IF (ASSOCIATED(met_zones(i)%brunt%ts)) THEN
          WRITE(iounit, 14) 'BRUNT', TRIM(met_zones(i)%brunt%ts%filename), &
               &met_zones(i)%brunt%current
       ELSE 
          WRITE(iounit, 13) 'BRUNT', met_zones(i)%brunt%current
       END IF

       IF (ASSOCIATED(met_zones(i)%conduction%ts)) THEN
          WRITE(iounit, 14) 'CONDUCTION', TRIM(met_zones(i)%conduction%ts%filename), &
               &met_zones(i)%conduction%current
       ELSE 
          WRITE(iounit, 13) 'CONDUCTION', met_zones(i)%conduction%current
       END IF

       WRITE (iounit, 10)
       
    END DO
    
10  FORMAT(50(1H-))
11  FORMAT("MET ZONE ", I2, ' OF ', I2)
12  FORMAT("DATA: ", A)
13  FORMAT(A13, ': CONSTANT: ', G10.5)
14  FORMAT(A13, ': VARIABLE: ', A, ', current = ', G10.5)
15  FORMAT("COEFFICIENTS:")
16  FORMAT("ID: ", I2)
  END SUBROUTINE met_zone_summary

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION met_zone_heat_flux
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION met_zone_heat_flux_zone(mzone, t_water)

    IMPLICIT NONE

    TYPE (met_zone_rec), INTENT(IN) :: mzone
    DOUBLE PRECISION, INTENT(IN) :: t_water

    met_zone_heat_flux_zone = net_heat_flux(&
         &mzone%coeff, &
         &mzone%met%current(MET_SWRAD), &
         &t_water, &
         &mzone%met%current(MET_AIRT), &
         &mzone%met%current(MET_DEWT), &
         &mzone%met%current(MET_WIND))
  END FUNCTION met_zone_heat_flux_zone

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION met_zone_heat_flux_byid
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION met_zone_heat_flux_byid(zoneid, t_water)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: zoneid
    DOUBLE PRECISION, INTENT(IN) :: t_water

    INTEGER :: izone

    izone = met_zone_index(zoneid)

    met_zone_heat_flux_byid = met_zone_heat_flux_zone(met_zones(izone), t_water)
    
  END FUNCTION met_zone_heat_flux_byid

  
  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION met_zone_evaporation_zone
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION met_zone_evaporation_zone(mzone, t_water)

    IMPLICIT NONE
    
    TYPE (met_zone_rec), INTENT(IN) :: mzone
    DOUBLE PRECISION, INTENT(IN) :: t_water

    DOUBLE PRECISION :: lheat, evaporation_rate
    
    lheat = latent_heat(t_water)  ! kJ/kg
    lheat = lheat*metric_density  ! kJ/m^3
    lheat = 1000.0*lheat          ! J/m^3
    
    evaporation_rate = evaporation(mzone%coeff, &
         &t_water, mzone%current(MET_DEWT), mzone%current(MET_WIND))     ! W/m^2 = J/s/m^2
    evaporation_rate = evaporation_rate/lheat ! m/s
    evaporation_rate = evaporation_rate/0.3048 ! ft/s
    !evaporation_rate = evaporation_rate*12.0*3600.0*24.0 ! in/day
    evaporation_rate = -evaporation_rate

    met_zone_evaporation_zone = evaporation_rate
  END FUNCTION met_zone_evaporation_zone

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION met_zone_evaporation_byid
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION met_zone_evaporation_byid(zoneid, t_water)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: zoneid
    DOUBLE PRECISION, INTENT(IN) :: t_water
    
    INTEGER :: izone

    izone = met_zone_index(zoneid)

    met_zone_evaporation_byid = met_zone_evaporation_zone(met_zones(izone), t_water)
  END FUNCTION met_zone_evaporation_byid

END MODULE met_zone
