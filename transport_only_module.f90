
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

USE date_time
USE globals
USE utility

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

PRIVATE :: hydro_filename, hydro_interp, hydro_datetime
PRIVATE :: max_files, bk_plane, fw_plane

TYPE hydro_interp_struct

  DOUBLE PRECISION, POINTER :: uvel_fw(:,:)	! u vel at forward time plane
  DOUBLE PRECISION, POINTER :: uvel_bk(:,:)	! u vel at backward time plane
  DOUBLE PRECISION, POINTER :: vvel_fw(:,:)	! v vel at forward time plane
  DOUBLE PRECISION, POINTER :: vvel_bk(:,:)	! v vel at backward time plane
  DOUBLE PRECISION, POINTER :: depth_fw(:,:)	! depth at forward time plane
  DOUBLE PRECISION, POINTER :: depth_bk(:,:)	! depth at backward time plane
  DOUBLE PRECISION, POINTER :: work(:,:)	! work array
END TYPE hydro_interp_struct

 CHARACTER (LEN=80), ALLOCATABLE :: hydro_filename(:)

 TYPE(hydro_interp_struct), ALLOCATABLE :: hydro_interp(:)

 TYPE(datetime_struct), ALLOCATABLE :: hydro_datetime(:)

 INTEGER, SAVE :: max_files ! maximum number of hydro restart files
 INTEGER, SAVE :: bk_plane = 1, fw_plane = 1



CONTAINS

!##############################################################################

SUBROUTINE allocate_hydro_interp_blocks(error_iounit, status_iounit)
  IMPLICIT NONE
  INTEGER :: alloc_stat,error_iounit,status_iounit

  ALLOCATE(hydro_interp(max_blocks), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     CALL error_message('allocate_hydro_interp_blocks: allocation failed for the array of hydro interp blocks', &
          &fatal=.TRUE.)
  ELSE
     CALL status_message('allocate_hydro_interp_blocks: allocation successful for array of hydro interp blocks')
  END IF
END SUBROUTINE allocate_hydro_interp_blocks

!##############################################################################
SUBROUTINE allocate_hydro_interp_comp(n, status_iounit)
  ! this routine allocates each component in the array of hydro_interp blocks
  ! allows minimal memory use for each block
  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra
  IMPLICIT NONE
  INTEGER :: n, imin, imax, jmin, jmax, status_iounit	! block number, max i elements, max j elements
  CHARACTER (LEN=1024) :: buffer

  imin = i_index_min
  imax = block(n)%xmax + i_index_extra
  jmin = j_index_min
  jmax = block(n)%ymax + j_index_extra

  CALL status_message('INITIALIZATION: allocate_hydro_interp_comp')
  WRITE(buffer,*)'    starting component allocation for block number - ',n
  CALL status_message(buffer)
  WRITE(buffer,*)'         maximum number of i elements = ', imax
  CALL status_message(buffer)
  WRITE(buffer,*)'         maximum number of j elements = ', jmax
  CALL status_message(buffer)

  ALLOCATE(hydro_interp(n)%uvel_fw(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%uvel_bk(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%vvel_fw(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%vvel_bk(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%depth_fw(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%depth_bk(imin:imax,jmin:jmax))
  ALLOCATE(hydro_interp(n)%work(imin:imax,jmin:jmax))
  WRITE(status_iounit,*)'   completed component allocation for block number - ',n
  
END SUBROUTINE allocate_hydro_interp_comp

!##############################################################################
!
! read the list of date, time, restart file names from the control file
!   transport_only.dat

SUBROUTINE read_transport_only_dat(status_iounit, error_iounit)
  IMPLICIT NONE
  INTEGER :: iounit = 50, count, i, status_iounit, error_iounit, alloc_stat
  CHARACTER :: junk_char1(10), junk_char2(8), junk_char3(80)
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
  
100  max_files = count
  REWIND(iounit)

  ALLOCATE(hydro_datetime(max_files), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     CALL error_message('read_transport_only_dat: allocation failed for hydro_datetime', &
          &fatal=.TRUE.)
  ELSE
     CALL status_message('read_transport_only_dat: allocation successful for hydrodatetime')
  ENDIF
  ALLOCATE(hydro_filename(max_files), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     CALL error_message('read_transport_only_dat: allocation failed for the array of hydro_filename', &
          &fatal=.TRUE.)
  ELSE
     CALL status_message('read_transport_only_dat: allocation successful for array of hydro_filename')
  ENDIF
  
!
! now go back and read transport_only.dat to fill the data structures 
!     make bold assumption that the file still exists (we checked that just above)

  READ(iounit,*)junk_char1
  DO i=1,max_files
     READ(iounit,*,END=999,ERR=999)hydro_datetime(i)%date_string, hydro_datetime(i)%time_string, hydro_filename(i)
     hydro_datetime(i)%time = date_to_decimal(hydro_datetime(i)%date_string, hydro_datetime(i)%time_string)

  END DO


  RETURN

999 WRITE(buffer,*)'read_transport_only_dat: a read error on unit=', &
         &iounit , ' last entry number was i=', i
  CALL error_message(buffer, fatal=.TRUE.)

END SUBROUTINE

!#######################################################################################
! check the transport_only.dat file
! now check this array to sure that time always goes forward and that the
! date/time range covers the model simulation start and end date times
!--------------------------------------------------------------------------------------

SUBROUTINE check_transport_only_dat(start_time,end_time, status_iounit, error_iounit)
IMPLICIT NONE

INTEGER :: i,status_iounit, error_iounit
DOUBLE PRECISION :: start_time, end_time
LOGICAL :: file_exist
CHARACTER (LEN=1024) :: buffer

! check to be sure all hydro_iterp files exist

DO i=1, max_files
  INQUIRE(FILE=hydro_filename(i),EXIST=file_exist)
  IF(file_exist)THEN
     CALL status_message('check_transport_only_dat: hydro_filename exists: ' // &
          & TRIM(hydro_filename(i)))
  ELSE
     CALL error_message('check_transport_only_dat: ' // &
          & hydro_filename(i) // ' hydro_filename DOES NOT exist', fatal=.TRUE.)
  ENDIF
END DO


! check overall range

IF(MINVAL(hydro_datetime(1:max_files)%time) > start_time)THEN
     WRITE(*,*)'FATAL ERROR - see error_warning.out'
     WRITE(error_iounit,*)'FATAL ERROR : check_transport_only_dat'
     WRITE(error_iounit,*)'-- `earliest date/time in transport_only.dat is after model start date/time'
     CALL EXIT(1)
ENDIF
IF(MAXVAL(hydro_datetime(1:max_files)%time) < end_time)THEN
     WRITE(*,*)'FATAL ERROR - see error_warning.out'
     WRITE(error_iounit,*)'FATAL ERROR : check_transport_only_dat'
     WRITE(error_iounit,*)'-- latest date/time in transport_only.dat is prior to model end date/time'
     CALL EXIT(1)
ENDIF

! check to make sure that date/time in transport.dat is always increasing

DO i=2,max_files
   IF(hydro_datetime(i)%time <= hydro_datetime(i-1)%time)THEN
     WRITE(*,*)'FATAL ERROR - see error_warning.out'
     WRITE(error_iounit,*)'FATAL ERROR : check_transport_only_dat'
     WRITE(error_iounit,*)'-- date/time value i=',i,' is not increasing in transport.dat'
     CALL EXIT(1)
   ENDIF
END DO

END SUBROUTINE check_transport_only_dat

!##############################################################################################

SUBROUTINE hydro_restart_interp(time, iblock, var, array)

  IMPLICIT NONE

  DOUBLE PRECISION :: time

  INTEGER :: var, iblock  ! =1 for U VEL. =2 for V VEL. =3 for DEPTH
  DOUBLE PRECISION :: factor, array(:,:)


! compute time weighting factor

  factor = (time - hydro_datetime(bk_plane)%time)/ & 
             & (hydro_datetime(fw_plane)%time - hydro_datetime(bk_plane)%time)
  
  SELECT CASE(var)

     CASE(1)  ! interpolate U velocity
        array = hydro_interp(iblock)%uvel_bk + &
                   & factor*(hydro_interp(iblock)%uvel_fw - hydro_interp(iblock)%uvel_bk) 

     CASE(2)  ! interpolate V velocity
        array = hydro_interp(iblock)%vvel_bk + &
                   & factor*(hydro_interp(iblock)%vvel_fw - hydro_interp(iblock)%vvel_bk) 
     
     CASE(3)  ! interpolate depth
        array = hydro_interp(iblock)%depth_bk + &
                   & factor*(hydro_interp(iblock)%depth_fw - hydro_interp(iblock)%depth_bk) 

  END SELECT

END SUBROUTINE hydro_restart_interp


!#############################################################################################

SUBROUTINE hydro_restart_read(time)

! figure out which two planes we are between; read in restart files if needed
  
  USE misc_vars, ONLY : error_iounit, status_iounit

  IMPLICIT NONE
  DOUBLE PRECISION :: time
  INTEGER :: i, bk_plane_new, fw_plane_new, plane_type, iblock
  LOGICAL, SAVE :: first_read = .TRUE.
  LOGICAL :: read_file
! initially bk_plane=1, bk_plane is static so we remember last value

  DO i=bk_plane, max_files-1
     IF((time >=hydro_datetime(i)%time).AND.(time <= hydro_datetime(i+1)%time)) EXIT
  END DO
  bk_plane_new = i
  fw_plane_new = i+1

  IF(fw_plane_new > max_files)THEN
     WRITE(*,*)'FATAL ERROR - see error_warning.out'
     WRITE(error_iounit,*)'FATAL ERROR : hydro_restart_read'
     WRITE(error_iounit,*)'-- forward plane value i=',i+1,' is larger than max_files and out of range'
     CALL EXIT(1)
  ENDIF

! time step for transport_only could be greater than spacing of restart files

! first option is we're still betweent the same planes; don't need to read new restart files

  IF(first_read)THEN
     bk_plane = bk_plane_new
     fw_plane = fw_plane_new
     plane_type = 1
     CALL read_restart(plane_type,bk_plane)
     plane_type = 2
     CALL read_restart(plane_type,fw_plane)
     first_read = .FALSE.
  ENDIF

  IF((bk_plane_new == bk_plane).AND.(fw_plane_new == fw_plane))THEN
        read_file = .FALSE.
  ELSE
        read_file = .TRUE.
  ENDIF

  IF(read_file)THEN
     IF(bk_plane_new == fw_plane)THEN
        ! only need a new forward plane; copy old forward plane to back plane
        bk_plane = bk_plane_new
        fw_plane = fw_plane_new
        DO iblock=1,max_blocks
            hydro_interp(iblock)%uvel_bk = hydro_interp(iblock)%uvel_fw
            hydro_interp(iblock)%vvel_bk = hydro_interp(iblock)%vvel_fw
            hydro_interp(iblock)%depth_bk = hydro_interp(iblock)%depth_fw
        END DO
        plane_type = 2
        CALL read_restart(plane_type, fw_plane)
     ELSE
        ! must need two new planes
        bk_plane = bk_plane_new
        fw_plane = fw_plane_new
        plane_type = 1
        CALL read_restart(plane_type, bk_plane)
        plane_type = 2
        CALL read_restart(plane_type,fw_plane)
     ENDIF

  ENDIF



END SUBROUTINE hydro_restart_read

!#########################################################################################

SUBROUTINE read_restart(plane_type, plane)
  
  USE misc_vars, ONLY : error_iounit, status_iounit
  USE scalars,   ONLY : max_species
  USE utility

  IMPLICIT NONE
  INTEGER :: plane_type, plane, iblock, hotstart_iounit=50
  INTEGER :: i, max_species_in_restart
  LOGICAL :: do_transport_restart

! plane type = 1 is the back plane, plane type = 2 is the forward plane

! plane value dictates the file name from the transport.dat file that we're going to read

  SELECT CASE(plane_type)
     CASE(1) ! read a back plane
        CALL open_existing(hydro_filename(plane), hotstart_iounit)
        CALL status_message('reading transport only hotstart file: ' // &
             &TRIM(hydro_filename(plane)))
        READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
        DO iblock=1,max_blocks
           READ(hotstart_iounit,*) hydro_interp(iblock)%uvel_bk
           !WRITE(*,*)'done with uvel read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with uold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with ustar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%vvel_bk
           !WRITE(*,*)'done with vvel read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with vold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with vstar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%depth_bk
           !WRITE(*,*)'done with depth read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with depthold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with dstar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work 
           !WRITE(*,*)'done with eddy read for block -',iblock
        END DO
	IF( do_transport_restart )THEN
		IF(max_species_in_restart > max_species) max_species_in_restart = max_species
		DO i=1,max_species_in_restart
                   DO iblock = 1, max_blocks
                      READ(hotstart_iounit,*) hydro_interp(iblock)%work
			!WRITE(*,*)'done with conc read for species -',i,'and block -',iblock
                      READ(hotstart_iounit,*) hydro_interp(iblock)%work
			!WRITE(*,*)'done with concold read for species -',i,'and block -',iblock
                   END DO
		END DO
	END IF

	CLOSE(hotstart_iounit)
	WRITE(status_iounit,*)'done reading hotstart file for transport only case'

     CASE(2)
        CALL open_existing(hydro_filename(plane), hotstart_iounit)
        CALL status_message('reading transport only hotstart file: ' // &
             &TRIM(hydro_filename(plane)))
        READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
        DO iblock=1,max_blocks
           READ(hotstart_iounit,*) hydro_interp(iblock)%uvel_fw
           !WRITE(*,*)'done with uvel read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with uold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with ustar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%vvel_fw
           !WRITE(*,*)'done with vvel read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with vold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with vstar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%depth_fw
           !WRITE(*,*)'done with depth read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with depthold read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work
           !WRITE(*,*)'done with dstar read for block -',iblock
           READ(hotstart_iounit,*) hydro_interp(iblock)%work 
           !WRITE(*,*)'done with eddy read for block -',iblock
        END DO
	IF( do_transport_restart)THEN
		IF(max_species_in_restart > max_species) max_species_in_restart = max_species
		DO i=1,max_species_in_restart
                   DO iblock = 1, max_blocks
                      READ(hotstart_iounit,*) hydro_interp(iblock)%work
			!WRITE(*,*)'done with conc read for species -',i,'and block -',iblock
                      READ(hotstart_iounit,*) hydro_interp(iblock)%work
			!WRITE(*,*)'done with concold read for species -',i,'and block -',iblock
                   END DO
		END DO
	END IF

	CLOSE(hotstart_iounit)
	WRITE(status_iounit,*)'done reading hotstart file for transport only case'

  END SELECT

END SUBROUTINE read_restart

END MODULE transport_only
