! ----------------------------------------------------------------
! MODULE hotstart
! ----------------------------------------------------------------
MODULE hotstart

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=80), PRIVATE, PARAMETER :: hotstart_name = 'hotstart.bin'

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE read_hotstart
  ! ----------------------------------------------------------------
  SUBROUTINE read_hotstart()

    IMPLICIT NONE

    INTEGER :: iblock, i
    CHARACTER (LEN=1024) :: msg

    CALL open_existing('hotstart.bin', hotstart_iounit)
    CALL status_message('reading hotstart file')
    READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
    DO iblock=1,max_blocks
       READ(hotstart_iounit,*) block(iblock)%uvel
       !WRITE(*,*)'done with uvel read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%uold
       !WRITE(*,*)'done with uold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%uoldold
       !WRITE(*,*)'done with uoldold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%ustar
       !WRITE(*,*)'done with ustar read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%vvel
       !WRITE(*,*)'done with vvel read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%vold
       !WRITE(*,*)'done with vold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%voldold
       !WRITE(*,*)'done with voldold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%vstar
       !WRITE(*,*)'done with vstar read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%depth
       !WRITE(*,*)'done with depth read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%depthold
       !WRITE(*,*)'done with depthold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%deptholdold
       !WRITE(*,*)'done with deptholdold read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%dstar
       !WRITE(*,*)'done with dstar read for block -',iblock
       READ(hotstart_iounit,*) block(iblock)%eddy 
       !WRITE(*,*)'done with eddy read for block -',iblock
    END DO
    IF( (do_transport).AND.(do_transport_restart) )THEN

       ! if a bed is expected in the
       ! hotstart, the number of scalars must
       ! be the same in the hotstart as was
       ! specified for the simulation

       IF (source_doing_sed .AND. (max_species_in_restart .NE. max_species)) THEN
          WRITE (msg,*) 'specified number of scalar species, ', &
               &max_species, ', does not match that in hotstart file (',&
               &max_species_in_restart, ')'
          CALL error_message(msg, fatal=.TRUE.)
       END IF

       ! if we don't expect a bed, don't
       ! worry about the number of scalar
       ! species

       IF(max_species_in_restart > max_species) max_species_in_restart = max_species
       DO i=1,max_species_in_restart
          DO iblock = 1, max_blocks
             READ(hotstart_iounit,*) species(i)%scalar(iblock)%conc
             WRITE(msg,*)'done with conc read for species -',i,'and block -',iblock
             CALL status_message(msg)
             READ(hotstart_iounit,*) species(i)%scalar(iblock)%concold
             WRITE(msg,*)'done with concold read for species -',i,'and block -',iblock
             CALL status_message(msg)
             READ(hotstart_iounit,*) species(i)%scalar(iblock)%concoldold
             WRITE(msg,*)'done with concoldold read for species -',i,'and block -',iblock
             CALL status_message(msg)
          END DO
       END DO

       ! if any sediment species were
       ! specified, we expect to find a bed
       ! in the hotstart file

       IF (source_doing_sed) CALL bed_read_hotstart(hotstart_iounit)

    ELSE IF (do_transport) THEN
       DO i = 1, max_species
          DO iblock =1, max_blocks
             species(i)%scalar(iblock)%conc = conc_initial
             species(i)%scalar(iblock)%concold = conc_initial
          END DO
       END DO
    END IF

    CLOSE(hotstart_iounit)
    WRITE(status_iounit,*)'done reading hotstart file'

  END SUBROUTINE read_hotstart



  !##########################################################################
  !---------------------------------------------------------------------------
  ! write a  restart file
  !---------------------------------------------------------------------------

  SUBROUTINE write_restart(status_flag)
    IMPLICIT NONE

    INTEGER :: i, status_flag, iblock


    IF( (current_time%time >= end_time%time) .OR. (MOD(time_step_count,restart_print_freq) == 0) )THEN

       restart_filename(1:9) = 'hotstart_'
       restart_filename(10:19) = current_time%date_string
       restart_filename(20:20) = '_'
       restart_filename(21:22) = current_time%time_string(1:2)
       restart_filename(23:24) = current_time%time_string(4:5)
       restart_filename(25:26) = current_time%time_string(7:8)
       restart_filename(27:30) = '.bin'

       ! OPEN(unit=restart_iounit,file=restart_filename,form='binary')
       ! OPEN(unit=restart_iounit,file=restart_filename,form='unformatted')
       CALL open_new(restart_filename, restart_iounit)

       IF(do_transport)THEN
          do_transport_restart = .TRUE.
          WRITE(restart_iounit,*) do_transport_restart, max_species
       ELSE
          do_transport_restart = .FALSE.
          WRITE(restart_iounit,*) do_transport_restart, max_species
       END IF

       DO iblock=1,max_blocks
          block(iblock)%work = block(iblock)%uvel
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%uold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%uoldold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%ustar
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%vvel
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%vold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%voldold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%vstar
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%depth
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%depthold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%deptholdold
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%dstar
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

          block(iblock)%work = block(iblock)%eddy
          WHERE(abs(block(iblock)%work) < tiny) &
               &block(iblock)%work = sign(tiny, block(iblock)%work)
          WRITE(restart_iounit,*)block(iblock)%work

!!$		WRITE(restart_iounit,*) block(iblock)%uvel
!!$		WRITE(restart_iounit,*) block(iblock)%uold
!!$		WRITE(restart_iounit,*) block(iblock)%ustar
!!$		WRITE(restart_iounit,*) block(iblock)%vvel
!!$		WRITE(restart_iounit,*) block(iblock)%vold
!!$		WRITE(restart_iounit,*) block(iblock)%vstar

!!$		WRITE(restart_iounit,*) block(iblock)%depth
!!$		WRITE(restart_iounit,*) block(iblock)%depthold
!!$		WRITE(restart_iounit,*) block(iblock)%dstar	
!!$		WRITE(restart_iounit,*) block(iblock)%eddy
       END DO

       IF(do_transport)THEN
          DO i=1,max_species
             DO iblock = 1, max_blocks
                block(iblock)%work = species(i)%scalar(iblock)%conc
                WHERE(abs(block(iblock)%work) < tiny) &
                     &block(iblock)%work = sign(tiny, block(iblock)%work)
                WRITE(restart_iounit,*)block(iblock)%work

                block(iblock)%work = species(i)%scalar(iblock)%concold
                WHERE(abs(block(iblock)%work) < tiny) &
                     &block(iblock)%work = sign(tiny, block(iblock)%work) 
                WRITE(restart_iounit,*)block(iblock)%work

                block(iblock)%work = species(i)%scalar(iblock)%concoldold
                WHERE(abs(block(iblock)%work) < tiny) &
                     &block(iblock)%work = sign(tiny, block(iblock)%work) 
                WRITE(restart_iounit,*)block(iblock)%work

                ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%conc
                ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%concold
             END DO
          END DO

          IF (source_doing_sed) CALL bed_write_hotstart(restart_iounit)
       END IF

       CLOSE(restart_iounit)

    ENDIF

  END SUBROUTINE write_restart




END MODULE hotstart
