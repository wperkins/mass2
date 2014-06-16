
!########################################################################
!
! module to handle aspects to doing transport simulations using 
! pre-computed hydrodynamics
!
! this requires reading a standard mass2 restart file, but we don't need
! to read the transport section (if it exists).
!
! logicals do_flow ( == FALSE) and do_transport ( == TRUE) will be the
! flag to enable this code option
!
!
!-----------------------------------------------------------------------
!

MODULE transport_only

  USE hotstart
  USE block_hydro
  USE block_hydro_bc

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE(datetime_struct), PRIVATE, ALLOCATABLE :: hotstart_datetime(:)
  CHARACTER (LEN=1024), PRIVATE, ALLOCATABLE :: hotstart_filename(:)
  TYPE hydro_interp_data
     TYPE (block_var), POINTER :: junk
     TYPE (block_var), POINTER :: uvel
     TYPE (block_var), POINTER :: vvel
     TYPE (block_var), POINTER :: depth
     TYPE (block_var), POINTER :: eddy
  END TYPE hydro_interp_data
  TYPE hydro_interp_block
     TYPE (hydro_interp_data), POINTER :: time1
     TYPE (hydro_interp_data), POINTER :: time2 ! time2 > time1
  END type hydro_interp_block
  TYPE(hydro_interp_block), PRIVATE, ALLOCATABLE :: hotstart_interp(:)

  INTEGER, PRIVATE :: max_files ! maximum number of hydro restart files
  INTEGER, PRIVATE :: bk_plane = 0, fw_plane = 0

CONTAINS

  !##############################################################################

  SUBROUTINE transport_only_allocate()
    IMPLICIT NONE
    INTEGER :: iblk
    INTEGER :: alloc_stat
    
    ALLOCATE(hotstart_interp(max_blocks), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('allocate_hotstart_interp_blocks: allocation failed for the array of hydro interp blocks', &
            &fatal=.TRUE.)
    ELSE
       CALL status_message('allocate_hotstart_interp_blocks: allocation successful for array of hydro interp blocks')
    END IF
    
    DO iblk = 1, max_blocks
       ALLOCATE(&
            &hotstart_interp(iblk)%time1,&
            &hotstart_interp(iblk)%time2)
       hotstart_interp(iblk)%time1%junk => &
            &block_var_allocate("transport only work", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time2%junk => &
            &block_var_allocate("transport only work", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time1%uvel => &
            &block_var_allocate("transport only uvel1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time2%uvel => &
            &block_var_allocate("transport only uvel1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time1%vvel => &
            &block_var_allocate("transport only vvel1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time2%vvel => &
            &block_var_allocate("transport only vvel1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time1%depth => &
            &block_var_allocate("transport only depth1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time2%depth => &
            &block_var_allocate("transport only depth2", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time1%eddy => &
            &block_var_allocate("transport only eddy1", block(iblk)%varbase, const=.TRUE.)
       hotstart_interp(iblk)%time2%eddy => &
            &block_var_allocate("transport only eddy2", block(iblk)%varbase, const=.TRUE.)
    END DO
    
  END SUBROUTINE transport_only_allocate
  
  !##############################################################################
  !
  ! read the list of date, time, restart file names from the control file
  !   transport_only.dat
  
  SUBROUTINE transport_only_read()
    IMPLICIT NONE
    INTEGER :: iounit = 50, count, i, alloc_stat
    CHARACTER :: junk_char1(10), junk_char2(8), junk_char3(1024)
    CHARACTER (LEN=1024) :: buffer
    
    CALL open_existing('transport_only.dat', iounit)
    
    ! count how many entries are in the transport_only.dat file
    !       and then allocate a date_time struct and filename array to hold these
    count = 0
    READ(iounit,*)junk_char1
    DO WHILE (.TRUE.)
       READ(iounit,*,END=100)junk_char1, junk_char2, junk_char3
       count = count + 1
    END DO
    
100 CONTINUE
    max_files = count
    
    REWIND(iounit)
    
    ALLOCATE(hotstart_datetime(max_files), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('read_transport_only_dat: allocation failed for hotstart_datetime', &
            &fatal=.TRUE.)
    ELSE
       CALL status_message('read_transport_only_dat: allocation successful for hydrodatetime')
    ENDIF
    ALLOCATE(hotstart_filename(max_files), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('read_transport_only_dat: allocation failed for the array of hotstart_filename', &
            &fatal=.TRUE.)
    ELSE
       CALL status_message('read_transport_only_dat: allocation successful for array of hotstart_filename')
    ENDIF
    
    !
    ! now go back and read transport_only.dat to fill the data structures 
    !     make bold assumption that the file still exists (we checked that just above)
    
    READ(iounit,*)junk_char1
    DO i=1,max_files
       READ(iounit,*,END=999,ERR=999) &
            &hotstart_datetime(i)%date_string, &
            &hotstart_datetime(i)%time_string, &
            &hotstart_filename(i)
       hotstart_datetime(i)%time = &
            &date_to_decimal(hotstart_datetime(i)%date_string, &
            &                hotstart_datetime(i)%time_string)
    END DO
    
    RETURN

999 WRITE(buffer,*)'read_transport_only_dat: a read error on unit=', &
         &iounit , ' last entry number was i=', i
    CALL error_message(buffer, fatal=.TRUE.)
    
  END SUBROUTINE transport_only_read

  !#######################################################################################
  ! check the transport_only.dat file
  ! now check this array to sure that time always goes forward and that the
  ! date/time range covers the model simulation start and end date times
  !--------------------------------------------------------------------------------------
  
  SUBROUTINE transport_only_check(start_time,end_time)
    IMPLICIT NONE
    
    INTEGER :: i
    DOUBLE PRECISION :: start_time, end_time
    LOGICAL :: file_exist
    CHARACTER (LEN=1024) :: buffer
    
    ! check to be sure all hydro_iterp files exist
    
    DO i=1, max_files
       INQUIRE(FILE=hotstart_filename(i),EXIST=file_exist)
       IF(file_exist)THEN
          CALL status_message('check_transport_only_dat: hotstart_filename exists: ' // &
               & TRIM(hotstart_filename(i)))
       ELSE
          CALL error_message('check_transport_only_dat: ' // &
               & hotstart_filename(i) // ' hotstart_filename DOES NOT exist', fatal=.TRUE.)
       ENDIF
    END DO
    
    
    ! check to make sure that date/time in transport.dat is always increasing
    
    DO i=2,max_files
       IF(hotstart_datetime(i)%time <= hotstart_datetime(i-1)%time)THEN
          WRITE(buffer, *) 'check_transport_only_dat: date/time value i=',i,' is not increasing in transport.dat'
          CALL error_message(buffer, fatal=.TRUE.)
       ENDIF
    END DO

    ! check overall range
    
    IF(hotstart_datetime(1)%time > start_time)THEN
       WRITE(*, *) TRIM(hotstart_datetime(1)%date_string), &
            &TRIM(hotstart_datetime(1)%time_string), &
            &hotstart_datetime(1)%time, start_time
       CALL error_message('check_transport_only_dat: earliest date/time in transport_only.dat is after model start date/time', &
            &fatal=.TRUE.)
    END IF
    IF(hotstart_datetime(max_files)%time < end_time)THEN
       CALL error_message('check_transport_only_dat: latest date/time in transport_only.dat is prior to model end date/time', &
            &fatal=.TRUE.)
    END IF
        
  END SUBROUTINE transport_only_check

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_only_block_swap
  ! ----------------------------------------------------------------
  SUBROUTINE transport_only_block_swap()

    IMPLICIT NONE

    INTEGER :: iblk
    TYPE (hydro_interp_data), POINTER :: tmp
    
    DO iblk = 1, max_blocks
       tmp => hotstart_interp(iblk)%time1
       hotstart_interp(iblk)%time1 => hotstart_interp(iblk)%time2
       hotstart_interp(iblk)%time2 => tmp
    END DO
  END SUBROUTINE transport_only_block_swap

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_only_update
  ! ----------------------------------------------------------------
  SUBROUTINE transport_only_update(time)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: time

    INTEGER :: idx

    idx = MAX(bk_plane, 1)
    DO WHILE (idx .LT. max_files) 
       IF (time .GE. hotstart_datetime(idx)%time .AND. &
               &time .LT. hotstart_datetime(idx+1)%time) EXIT
    END DO
    
    IF (idx .EQ. fw_plane) THEN
       ! bk_plane has already been read (as current fw_plane); just read a new fw_plane
       CALL transport_only_block_swap()
       CALL transport_only_read_hotstart(hotstart_filename(idx+1))
    ELSE IF (idx .EQ. bk_plane) THEN 
       ! both planes have been read
    ELSE 
       ! neither plane has been read
       CALL transport_only_read_hotstart(hotstart_filename(idx))
       CALL transport_only_block_swap()
       CALL transport_only_read_hotstart(hotstart_filename(idx+1))
    END IF
    bk_plane = idx
    fw_plane = bk_plane + 1

    CALL transport_only_interp(time)

  END SUBROUTINE transport_only_update

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_only_read_block
  ! ----------------------------------------------------------------
  SUBROUTINE transport_only_read_block(iunit, blk, data)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iunit
    TYPE (block_struct), INTENT(INOUT) :: blk
    TYPE (hydro_interp_data), INTENT(INOUT) :: data

    CALL hotstart_read_var(hotstart_iounit, data%uvel, &
         &blk%buffer, BLK_VAR_CURRENT)
    ! discard uvel star, old, and oldold
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
          
    ! vvel current
    CALL hotstart_read_var(hotstart_iounit, data%vvel, &
         &blk%buffer, BLK_VAR_CURRENT)
    ! discard vvel star, old, and oldold
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    
    ! depth current
    CALL hotstart_read_var(hotstart_iounit, data%depth, &
         &blk%buffer, BLK_VAR_CURRENT)
    ! discard depth star, old, and oldold
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    CALL hotstart_read_var(hotstart_iounit, data%junk, &
         &blk%buffer, BLK_VAR_BOGUS)
    
    CALL hotstart_read_var(hotstart_iounit, data%eddy, &
         &blk%buffer, BLK_VAR_CURRENT)

  END SUBROUTINE transport_only_read_block


  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_only_read_hotstart
  ! ----------------------------------------------------------------
  SUBROUTINE transport_only_read_hotstart(fname)

    IMPLICIT NONE

#include "global.fh"

    CHARACTER (LEN=*), INTENT(IN) :: fname
    
    INTEGER :: iblk, i
    CHARACTER (LEN=1024) :: msg
    LOGICAL :: do_transport_restart
    INTEGER :: max_species_in_restart

    IF (ga_nodeid() .EQ. 0) THEN

       CALL open_existing(fname, hotstart_iounit)
       CALL status_message('reading hotstart file "' // TRIM(fname) // '"')
       READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
       DO iblk=1,max_blocks
          CALL transport_only_read_block(hotstart_iounit, &
               &block(iblk), &
               &hotstart_interp(iblk)%time2)
       END DO
       
       ! the transport and bed parts of the hotstart file are ignored

       CLOSE(hotstart_iounit)

       CALL status_message('done reading hotstart file')
    END IF

    CALL block_var_sync()

    DO iblk = 1, max_blocks
       CALL block_var_get(hotstart_interp(iblk)%time2%uvel, BLK_VAR_CURRENT);
       CALL block_var_get(hotstart_interp(iblk)%time2%vvel, BLK_VAR_CURRENT);
       CALL block_var_get(hotstart_interp(iblk)%time2%depth, BLK_VAR_CURRENT);
       CALL block_var_get(hotstart_interp(iblk)%time2%eddy, BLK_VAR_CURRENT);
    END DO

  END SUBROUTINE transport_only_read_hotstart


  
  ! ----------------------------------------------------------------
  ! transport_only_interp
  ! ----------------------------------------------------------------
  SUBROUTINE transport_only_interp(time)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: time
    INTEGER :: iblk
    LOGICAL :: junk

    DOUBLE PRECISION :: factor
    

    ! compute time weighting factor
    
    factor = (time - hotstart_datetime(bk_plane)%time)/ & 
         & (hotstart_datetime(fw_plane)%time - hotstart_datetime(bk_plane)%time)

    DO iblk = 1, max_blocks
       CALL block_var_interpolate(&
            &hotstart_interp(iblk)%time1%uvel,&
            &hotstart_interp(iblk)%time2%uvel,&
            &factor, block(iblk)%bv_uvel)
       CALL block_var_iterate(block(iblk)%bv_uvel)
       CALL block_var_interpolate(&
            &hotstart_interp(iblk)%time1%vvel,&
            &hotstart_interp(iblk)%time2%vvel,&
            &factor, block(iblk)%bv_vvel)
       CALL block_var_iterate(block(iblk)%bv_vvel)
       CALL block_var_interpolate(&
            &hotstart_interp(iblk)%time1%depth,&
            &hotstart_interp(iblk)%time2%depth,&
            &factor, block(iblk)%bv_depth)
       CALL block_var_iterate(block(iblk)%bv_depth)
       CALL block_var_interpolate(&
            &hotstart_interp(iblk)%time1%eddy,&
            &hotstart_interp(iblk)%time2%eddy,&
            &factor, block(iblk)%bv_eddy)
    END DO
    CALL block_var_sync()
    DO iblk = 1, max_blocks
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_CURRENT)
       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_CURRENT)
       CALL block_var_get(block(iblk)%bv_eddy, BLK_VAR_CURRENT)
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_eddy, BLK_VAR_CURRENT)
       CALL bedshear(block(iblk))
       CALL check_wetdry(block(iblk))
       CALL block_hydro_update_bc(block(iblk), block_bc(iblk), junk)
    END DO
    CALL block_var_sync()
    
  END SUBROUTINE transport_only_interp
END MODULE transport_only
