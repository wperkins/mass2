! ----------------------------------------------------------------
! file: bed_source.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August  2, 2000 by William A. Perkins
! Last Change: Fri Apr  4 21:35:46 2003 by William A. Perkins <perk@localhost.localdomain>
! ----------------------------------------------------------------

! RCS ID: $Id$

! ----------------------------------------------------------------
! MODULE bed_source

! This module encapsulates the representation of non-point contaminant
! sources.  This was initially applied to the Hanford Reach, in which
! contaminant influx occurs through the channel bed.

! ----------------------------------------------------------------

MODULE bed_source

  USE table_boundary_conditions
  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=*), PARAMETER :: bed_source_filename = 'bed_source.dat'

                                ! This holds a *cumulative mass* time
                                ! series

  TYPE bedsrc_ts_rec
     INTEGER :: id
     CHARACTER (LEN=1024) :: filename
     INTEGER :: start
     INTEGER :: n
     TYPE (table_entry_struct), POINTER :: data(:)
  END TYPE bedsrc_ts_rec

                                ! This is allocated for each block in
                                ! the domain, it identifies which bed
                                ! source time series is used and what
                                ! fraction of that mass is allocated
                                ! to this cell

  TYPE bedsrc_map_rec
     INTEGER, POINTER :: tsid(:,:)
     INTEGER, POINTER :: tsidx(:,:)
     DOUBLE PRECISION, POINTER :: fraction(:,:)
     DOUBLE PRECISION, POINTER :: cellrate(:,:)
  END TYPE bedsrc_map_rec

                                ! This record is allocate for each
                                ! scalar and contains the map and the
                                ! time series data

  TYPE bedsrc_rec
     CHARACTER (LEN=1024) :: ts_list_file, map_file
     INTEGER, POINTER :: idmap(:)
     TYPE (bedsrc_map_rec), POINTER :: map(:)
     INTEGER :: nts
     TYPE (bedsrc_ts_rec), POINTER :: data(:)
     DOUBLE PRECISION, POINTER :: current(:), last(:), rate(:)
  END TYPE bedsrc_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE(bedsrc_rec) FUNCTION bedsrc_read
  ! ----------------------------------------------------------------
  TYPE(bedsrc_rec) FUNCTION bedsrc_read(listname, mapname)

    USE misc_vars, ONLY: delta_t, start_time

    IMPLICIT NONE

    CHARACTER (LEN=*) :: listname, mapname
    POINTER bedsrc_read

    ALLOCATE(bedsrc_read)
    bedsrc_read%ts_list_file = listname
    bedsrc_read%map_file = mapname
    bedsrc_read%nts = 0
    
    CALL bedsrc_read_ts(bedsrc_read)
    CALL bedsrc_read_map(bedsrc_read)

                                ! do initial mass interpolation

    CALL bedsrc_interp(start_time%time, delta_t, bedsrc_read)

  END FUNCTION bedsrc_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE bedsrc_read_ts
  ! ----------------------------------------------------------------
  SUBROUTINE bedsrc_read_ts(rec)

    USE globals, ONLY: max_blocks
    USE misc_vars, ONLY: restart_iounit

    IMPLICIT NONE
    TYPE(bedsrc_rec) :: rec
    INTEGER :: i, count, istat
    INTEGER :: id
    CHARACTER (LEN=1024) :: filename, buffer

    count = 0

    CALL open_existing(rec%ts_list_file, restart_iounit)
    CALL status_message('reading bed source time series files from ' //&
         & TRIM(rec%ts_list_file))

                                ! read it once to get a count

    DO WHILE (.TRUE.) 
       READ(restart_iounit, *, END=100) id, filename
       count = count + 1
    END DO
100 CONTINUE

    IF (count .le. 0) THEN
       CALL error_message('unable to read bed source time series list from ' // &
            &TRIM(rec%ts_list_file), fatal=.TRUE.)
    ELSE
       WRITE(buffer, *) 'counted ', count, ' time series names from ', &
            &TRIM(rec%ts_list_file)
       CALL status_message(buffer)
    END IF

                                ! do the allocation based on the
                                ! number of time series found
    rec%nts = count
    ALLOCATE(rec%idmap(rec%nts))
    ALLOCATE(rec%data(rec%nts))
    ALLOCATE(rec%current(rec%nts), rec%last(rec%nts), rec%rate(rec%nts))
    rec%current = 0.0
    rec%last = 0.0
    rec%rate = 0.0

                                ! reread the file and fill the record
    
    REWIND(restart_iounit)
    DO i = 1, count
       READ(restart_iounit, *) id, filename
       rec%idmap(i) = id
       rec%data(i)%id = id
       rec%data(i)%filename = filename
       rec%data(i)%start = 0
       CALL bedsrc_read_ts_file(rec%data(i))
    END DO
    CLOSE(restart_iounit)

    WRITE(buffer, *) 'successfully read ', rec%nts, &
         &' time series files specified in ', TRIM(rec%ts_list_file)
    CALL status_message(buffer)
    
  END SUBROUTINE bedsrc_read_ts

  ! ----------------------------------------------------------------
  ! SUBROUTINE bedsrc_read_ts_file
  ! ----------------------------------------------------------------
  SUBROUTINE bedsrc_read_ts_file(rec)

    USE misc_vars, ONLY: grid_iounit, start_time, end_time

    IMPLICIT NONE
    INTEGER :: count
    TYPE(bedsrc_ts_rec) :: rec
    INTEGER :: istat, i
    CHARACTER (LEN=1024) :: junk
    DOUBLE PRECISION :: tmpreal

    CALL open_existing(rec%filename, grid_iounit)
    CALL status_message('reading bed source data from ' // TRIM(rec%filename))

    
    count = 0
    READ(grid_iounit, *) junk      ! skip first line
    DO WHILE (.TRUE.)           ! count table entries and allocate
       READ(grid_iounit, *, END=100, ERR=100) junk, junk, tmpreal
       count = count + 1
    END DO
100 CONTINUE
    ALLOCATE(rec%data(count))

    REWIND(grid_iounit)
    READ(grid_iounit, *) junk      ! skip first line
    DO i = 1, count
       READ(grid_iounit, *) rec%data(i)%datetime%date_string, &
            &rec%data(i)%datetime%time_string,tmpreal
       rec%data(i)%datetime%time = &
            &date_to_decimal(rec%data(i)%datetime%date_string,&
            &rec%data(i)%datetime%time_string)
       rec%data(i)%value(1) = tmpreal
       IF ((i .EQ. 1) .AND. (rec%data(i)%datetime%time .GT. start_time%time)) THEN
          CALL error_message('bed source time (' // rec%data(i)%datetime%date_string // ' ' // &
               &rec%data(i)%datetime%time_string // ') in file ' // &
               &TRIM(rec%filename) // ' is past simulation start time')
       ELSE IF ((i .EQ. count) .AND. (rec%data(i)%datetime%time .LT. end_time%time)) THEN
          CALL error_message('bed source time (' // rec%data(i)%datetime%date_string // &
               & ' ' // rec%data(i)%datetime%time_string // ') in file ' //&
               &TRIM(rec%filename) // ' is before simulation end time')
       END IF
          
       IF (i .GT. 1) THEN
          IF (rec%data(i)%datetime%time .LT. rec%data(i-1)%datetime%time) THEN
             CALL error_message('bed source time series out of order in file ' //&
                  &TRIM(rec%filename))
          END IF
          IF (rec%data(i)%value(1) .LT. rec%data(i-1)%value(1)) THEN
             CALL error_message('bed source mass not monotonically increasing in file ' //&
                  &TRIM(rec%filename))
          END IF
       END IF
    END DO
    CLOSE(grid_iounit)

    rec%n = count

    WRITE(junk, *) 'successfully read ', count, ' data points from ',&
         &TRIM(rec%filename)
    CALL status_message(junk)
       
  END SUBROUTINE bedsrc_read_ts_file

  ! ----------------------------------------------------------------
  ! SUBROUTINE bedsrc_read_map
  ! ----------------------------------------------------------------
  SUBROUTINE bedsrc_read_map(rec)

    USE globals, ONLY: max_blocks, block
    USE misc_vars, ONLY: grid_iounit, restart_iounit, &
         &i_index_min, i_index_extra, j_index_min, j_index_extra

    IMPLICIT NONE

    TYPE(bedsrc_rec) :: rec
    INTEGER :: istat, iblk, i, j, ijunk, jjunk
    CHARACTER (LEN = 1024) :: filename, buffer
    INTEGER :: imin, imax, jmin, jmax

    CALL open_existing(rec%map_file, restart_iounit)

    ALLOCATE(rec%map(max_blocks))
    DO iblk = 1, max_blocks
       READ(restart_iounit, *) filename

       CALL open_existing(filename, grid_iounit)

       imin = i_index_min
       imax = block(iblk)%xmax + i_index_extra
       jmin = j_index_min
       jmax = block(iblk)%ymax + j_index_extra
    
       ALLOCATE(rec%map(iblk)%tsid(imin:imax, jmin:jmax))
       ALLOCATE(rec%map(iblk)%tsidx(imin:imax, jmin:jmax))
       ALLOCATE(rec%map(iblk)%fraction(imin:imax, jmin:jmax))
       ALLOCATE(rec%map(iblk)%cellrate(imin:imax, jmin:jmax))

       rec%map(iblk)%tsid = 0
       rec%map(iblk)%tsidx = 0
       rec%map(iblk)%fraction = 0.0
       
       READ(grid_iounit, *)  ijunk, jjunk ! skip first line
       IF (ijunk .NE. block(iblk)%xmax - 1 .OR. jjunk .NE. block(iblk)%ymax - 1) THEN
          WRITE(*,*) 'WARNING: x and y max values on first line of ', &
               &TRIM(filename), ' do not match that for block ', iblk
       END IF

       DO i = 2, block(iblk)%xmax
          DO j = 2, block(iblk)%ymax
             READ(grid_iounit, *) ijunk, jjunk, rec%map(iblk)%tsid(i, j), &
                  &rec%map(iblk)%fraction(i, j)
             IF (rec%map(iblk)%tsid(i, j) .GT. 0 .AND. &
                  &rec%map(iblk)%fraction(i, j) .GT. 0.0) THEN
                rec%map(iblk)%tsidx(i,j) = -1
                DO ijunk = 1, rec%nts
                   IF (rec%idmap(ijunk) .EQ. rec%map(iblk)%tsid(i, j)) THEN
                      rec%map(iblk)%tsidx(i,j) = ijunk
                      EXIT
                   END IF
                END DO
                IF (rec%map(iblk)%tsidx(i,j) .LT. 0) THEN
                   WRITE(buffer,*) 'bed source time series id ', &
                        &rec%map(iblk)%tsid(i,j), ' not defined in file ', &
                        &TRIM(filename)
                   CALL error_message(buffer, fatal=.TRUE.)
                END IF
             END IF
          END DO
       END DO

       CLOSE(grid_iounit)

    END DO

    CLOSE(restart_iounit)

  END SUBROUTINE bedsrc_read_map

  ! ----------------------------------------------------------------
  ! SUBROUTINE bedsrc_interp
  ! ----------------------------------------------------------------
  SUBROUTINE bedsrc_interp(time, deltat, rec)

    USE globals

    IMPLICIT NONE

    DOUBLE PRECISION :: time, deltat
    TYPE(bedsrc_rec) :: rec

    INTEGER :: start, i, j, its, iblk, idx
    DOUBLE PRECISION :: factor

    rec%last = rec%current
    DO its = 1, rec%nts
       IF (time .LE. rec%data(its)%data(1)%datetime%time) THEN
          rec%current(its) = rec%data(its)%data(1)%value(1)
       ELSE IF (time .GE. rec%data(its)%data(rec%data(its)%n)%datetime%time) THEN
          rec%current(its) = rec%data(its)%data(rec%data(its)%n)%value(1)
       ELSE
          start = MAX(1, rec%data(its)%start)
          DO i = start, rec%data(its)%n - 1
             IF (time .GE. rec%data(its)%data(i)%datetime%time .AND.&
                  &time .LE. rec%data(its)%data(i+1)%datetime%time) EXIT
          END DO
          rec%current(its) = (time - rec%data(its)%data(i)%datetime%time)/&
               &(rec%data(its)%data(i+1)%datetime%time - rec%data(its)%data(i)%datetime%time)*&
               &(rec%data(its)%data(i+1)%value(1) - rec%data(its)%data(i)%value(1)) + &
               &rec%data(its)%data(i)%value(1)
          rec%data(its)%start = MIN(rec%nts - 2, i - 1)
       END IF
    END DO

                                ! this assumes that this routine gets
                                ! called only once per time step

    rec%rate = (rec%current - rec%last)/deltat

                                ! distribute the rate over the cells
                                ! all indices should start at 2

    DO iblk = 1, max_blocks
       rec%map(iblk)%cellrate = 0.0
       DO i = 2, block(iblk)%xmax
          DO j = 2, block(iblk)%ymax
             idx = rec%map(iblk)%tsidx(i, j)
             IF (idx .GT. 0) THEN
                rec%map(iblk)%cellrate(i, j) = &
                  &rec%rate(idx)*rec%map(iblk)%fraction(i, j)
             END IF
         END DO
       END DO
    END DO

    RETURN
  END SUBROUTINE bedsrc_interp

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION bedsrc_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION bedsrc_source_term(rec, iblk, i, j)

    
    IMPLICIT NONE
    TYPE(bedsrc_rec) :: rec
    INTEGER :: iblk, i, j

    bedsrc_source_term = 0.0
    bedsrc_source_term = rec%map(iblk)%cellrate(i, j)

    RETURN
    
  END FUNCTION bedsrc_source_term


END MODULE bed_source
