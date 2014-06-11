! ----------------------------------------------------------------
! MODULE hotstart
! ----------------------------------------------------------------
MODULE hotstart

  USE config
  USE block_module
  USE scalars
  USE bed_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=80), PRIVATE, PARAMETER :: hotstart_file_name = 'hotstart.bin'
  INTEGER, PUBLIC, PARAMETER :: hotstart_iounit = 16 
  INTEGER, PRIVATE, PARAMETER :: restart_iounit = 17

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE hotstart_read_var
  ! ----------------------------------------------------------------
  SUBROUTINE hotstart_read_var(iunit, var, buffer, index)
    
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iunit
    TYPE (block_var), INTENT(INOUT) :: var
    DOUBLE PRECISION, INTENT(INOUT) :: &
         &buffer(var%base%imin_global:var%base%imax_global, &
         &var%base%jmin_global:var%base%jmax_global)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    READ (iunit, *) buffer(var%base%imin_global:var%base%imax_global, &
         &var%base%jmin_global:var%base%jmax_global)

    IF (myindex > 0) THEN
       CALL block_var_put_all(var, buffer, myindex)
    END IF

  END SUBROUTINE hotstart_read_var


  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_restart_read_var
  ! ----------------------------------------------------------------
  SUBROUTINE bed_restart_read_var(iunit, var, buffer, index)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iunit
    TYPE (bed_var), INTENT(INOUT) :: var
    DOUBLE PRECISION, INTENT(INOUT) :: &
         &buffer(var%base%imin_global:var%base%imax_global, &
         &var%base%jmin_global:var%base%jmax_global)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    myindex = 1

    IF (PRESENT(index)) myindex = index

    READ (iunit, *) buffer(var%base%imin_global:var%base%imax_global, &
         &var%base%jmin_global:var%base%jmax_global)

    IF (myindex > 0) THEN
       CALL bed_var_put_all(var, buffer, myindex)
    END IF

  END SUBROUTINE bed_restart_read_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE read_hotstart
  ! ----------------------------------------------------------------
  SUBROUTINE read_hotstart()

    IMPLICIT NONE

#include "global.fh"

    INTEGER :: iblk, i, ifract, ispecies
    CHARACTER (LEN=1024) :: msg
    LOGICAL :: do_transport_restart
    INTEGER :: max_species_in_restart, restart_fractions, restart_parts

    IF (ga_nodeid() .EQ. 0) THEN

       CALL open_existing(hotstart_file_name, hotstart_iounit)
       CALL status_message('reading hotstart file "' // TRIM(hotstart_file_name) // '"')
       READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
       DO iblk=1,max_blocks
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_uvel, &
               &block(iblk)%buffer, BLK_VAR_CURRENT)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_uvel, &
               &block(iblk)%buffer, BLK_VAR_OLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_uvel, &
               &block(iblk)%buffer, BLK_VAR_OLDOLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_uvel, &
               &block(iblk)%buffer, BLK_VAR_STAR)
          
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_vvel, &
               &block(iblk)%buffer, BLK_VAR_CURRENT)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_vvel, &
               &block(iblk)%buffer, BLK_VAR_OLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_vvel, &
               &block(iblk)%buffer, BLK_VAR_OLDOLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_vvel, &
               &block(iblk)%buffer, BLK_VAR_STAR)
          
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_depth, &
               &block(iblk)%buffer, BLK_VAR_CURRENT)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_depth, &
               &block(iblk)%buffer, BLK_VAR_OLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_depth, &
               &block(iblk)%buffer, BLK_VAR_OLDOLD)
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_depth, &
               &block(iblk)%buffer, BLK_VAR_STAR)
          
          CALL hotstart_read_var(hotstart_iounit, block(iblk)%bv_eddy, &
               &block(iblk)%buffer, BLK_VAR_CURRENT)
          
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
             DO iblk = 1, max_blocks
                CALL hotstart_read_var(hotstart_iounit, &
                     &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                     &BLK_VAR_CURRENT)
                CALL hotstart_read_var(hotstart_iounit, &
                     &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                     &BLK_VAR_OLD)
                CALL hotstart_read_var(hotstart_iounit, &
                     &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                     &BLK_VAR_OLDOLD)
             END DO
          END DO
          
          ! if any sediment species were
          ! specified, we expect to find a bed
          ! in the hotstart file

          IF (source_doing_sed) THEN 
                                ! read the number of sediment
                                ! fractions and particulates from the
                                ! restart

             READ(hotstart_iounit,*) restart_fractions, restart_parts

                                ! let's be hard-nosed and quit with a
                                ! fatal error if the restart does not
                                ! match the current configuration
                                ! (this may have to change)

             IF (restart_fractions .NE. sediment_fractions) THEN
                WRITE(msg,*) 'specified number of sediment scalars, ',&
                     &sediment_fractions, ', does not match those in restart, ',&
                     &restart_fractions
                CALL error_message(msg, fatal=.TRUE.)
             END IF

             IF (restart_parts .NE. particulates) THEN
                WRITE(msg,*) 'specified number of particulate scalars, ',&
                     &particulates, ', does not match those in restart, ',&
                     &restart_parts
                CALL error_message(msg, fatal=.TRUE.)
             END IF

                                ! if we're OK, read the bed part of
                                ! the restart file

             DO ifract = 1, sediment_fractions
                DO iblk = 1, max_blocks
                   CALL bed_restart_read_var(hotstart_iounit,&
                        &bed(iblk)%bv_sediment, &
                        &block(iblk)%buffer, ifract)
                END DO
             END DO
             DO ispecies = 1, max_species
                DO iblk = 1, max_blocks
                   SELECT CASE (scalar_source(ispecies)%srctype)
                   CASE (GEN)
                      CALL bed_restart_read_var(hotstart_iounit,&
                           &bed(iblk)%bv_pore, &
                           &block(iblk)%buffer, ispecies)
                   CASE (PART)
                      CALL bed_restart_read_var(hotstart_iounit,&
                           &bed(iblk)%bv_particulate, &
                           &block(iblk)%buffer, ispecies)
                   END SELECT
                END DO
             END DO
          END IF
       END IF

       CLOSE(hotstart_iounit)

       CALL status_message('done reading hotstart file')
    END IF

    CALL block_var_sync()

    DO iblk=1,max_blocks
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_OLDOLD)
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_OLD)
       CALL block_var_get(block(iblk)%bv_uvel, BLK_VAR_CURRENT)

       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_OLDOLD)
       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_OLD)
       CALL block_var_get(block(iblk)%bv_vvel, BLK_VAR_CURRENT)
    
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_STAR)
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_OLDOLD)
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_OLD)
       CALL block_var_get(block(iblk)%bv_depth, BLK_VAR_CURRENT)

       CALL block_var_get(block(iblk)%bv_eddy, BLK_VAR_CURRENT)

       block(iblk)%wsel = block(iblk)%depth + block(iblk)%zbot
       CALL block_var_put(block(iblk)%bv_wsel, BLK_VAR_CURRENT)
       CALL block_var_sync()
       CALL block_var_get(block(iblk)%bv_wsel, BLK_VAR_CURRENT)

       IF (do_transport) THEN
          DO i=1,max_species
             CALL block_var_get(species(i)%scalar(iblk)%concvar, BLK_VAR_STAR)
             CALL block_var_get(species(i)%scalar(iblk)%concvar, BLK_VAR_OLDOLD)
             CALL block_var_get(species(i)%scalar(iblk)%concvar, BLK_VAR_OLD)
             CALL block_var_get(species(i)%scalar(iblk)%concvar, BLK_VAR_CURRENT)
             CALL block_var_sync()
          END DO
          IF (source_doing_sed) THEN
             DO ifract = 1, sediment_fractions
                CALL bed_var_get(bed(iblk)%bv_sediment, ifract)
             END DO
             DO ispecies = 1, max_species
                CALL bed_var_get(bed(iblk)%bv_pore, ispecies)
                CALL bed_var_get(bed(iblk)%bv_particulate, ispecies)
             END DO
          END IF
       END IF
       CALL block_var_sync()
    END DO

  END SUBROUTINE read_hotstart

  ! ----------------------------------------------------------------
  !  FUNCTION hotstart_name
  ! ----------------------------------------------------------------
  FUNCTION hotstart_name(ds, ts)

    IMPLICIT NONE

    CHARACTER (LEN=80) :: hotstart_name
    CHARACTER (LEN=*), INTENT(IN) :: ds, ts

    CHARACTER (LEN=80) :: buffer
    INTEGER :: i

    buffer = "hotstart_" // TRIM(ds) // "_" // TRIM(ts) // ".bin"

    ! take the colons out 

    i = SCAN(buffer, ':')
    DO WHILE (i .GT. 0)
       buffer(i:) = buffer(i+1:)
       i = SCAN(buffer, ':')
    END DO
    hotstart_name = buffer
  END FUNCTION hotstart_name

  ! ----------------------------------------------------------------
  ! SUBROUTINE restart_write_var
  ! ----------------------------------------------------------------
  SUBROUTINE restart_write_var(iunit, var, buffer, index)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iunit
    TYPE (block_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    CALL block_var_get_all(var, buffer, myindex)
    WHERE(abs(buffer) < tiny) &
         &buffer = sign(tiny, buffer)
#ifndef NOOUTPUT
    WRITE(iunit,*) buffer
#endif

  END SUBROUTINE restart_write_var


  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_restart_write_var
  ! ----------------------------------------------------------------
  SUBROUTINE bed_restart_write_var(iunit, var, buffer, index)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iunit
    TYPE (bed_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:,:)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    CALL bed_var_get_all(var, buffer, myindex)

    WHERE(abs(buffer) < tiny) &
         &buffer = sign(tiny, buffer)
#ifndef NOOUTPUT
    WRITE(iunit,*) buffer
#endif

  END SUBROUTINE bed_restart_write_var


  !##########################################################################
  !---------------------------------------------------------------------------
  ! write a  restart file
  !---------------------------------------------------------------------------

  SUBROUTINE write_restart()
    IMPLICIT NONE

#include "global.fh"

    INTEGER :: i, iblk, ifract, ispecies
    INTEGER :: me
    LOGICAL :: do_transport_restart
    CHARACTER (LEN=1024) :: restart_filename

    me = ga_nodeid()

    IF ((current_time%time >= end_time%time) .OR. &
         &(MOD(time_step_count,restart_print_freq) == 0))THEN

       ! values other than current may not be pushed to their GA's, do it now

       DO iblk=1,max_blocks
          CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_STAR)
          CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLD)
          CALL block_var_put(block(iblk)%bv_uvel, BLK_VAR_OLDOLD)
          CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_STAR)
          CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLD)
          CALL block_var_put(block(iblk)%bv_vvel, BLK_VAR_OLDOLD)
          CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_STAR)
          CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_OLD)
          CALL block_var_put(block(iblk)%bv_depth, BLK_VAR_OLDOLD)
       END DO
       
       IF (do_transport) THEN
          DO i=1,max_species
             DO iblk = 1, max_blocks
                CALL block_var_put(species(i)%scalar(iblk)%concvar, BLK_VAR_OLD)
                CALL block_var_put(species(i)%scalar(iblk)%concvar, BLK_VAR_OLDOLD)
             END DO
          END DO
          IF (source_doing_sed) CALL bed_put()          
       END IF

       CALL block_var_sync()

       IF (me .EQ. 0) THEN

          restart_filename = hotstart_name(current_time%date_string, current_time%time_string)

#ifndef NOOUTPUT
          CALL open_new(restart_filename, restart_iounit)
#endif          
          IF (do_transport) THEN
             do_transport_restart = .TRUE.
             WRITE(restart_iounit,*) do_transport_restart, max_species
          ELSE
             do_transport_restart = .FALSE.
             WRITE(restart_iounit,*) do_transport_restart, 0
          END IF

          DO iblk=1,max_blocks

             CALL restart_write_var(restart_iounit, block(iblk)%bv_uvel, &
                  &block(iblk)%buffer, BLK_VAR_CURRENT)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_uvel, &
                  &block(iblk)%buffer, BLK_VAR_OLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_uvel, &
                  &block(iblk)%buffer, BLK_VAR_OLDOLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_uvel, &
                  &block(iblk)%buffer, BLK_VAR_STAR)

             CALL restart_write_var(restart_iounit, block(iblk)%bv_vvel, &
                  &block(iblk)%buffer, BLK_VAR_CURRENT)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_vvel, &
                  &block(iblk)%buffer, BLK_VAR_OLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_vvel, &
                  &block(iblk)%buffer, BLK_VAR_OLDOLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_vvel, &
                  &block(iblk)%buffer, BLK_VAR_STAR)
             
             CALL restart_write_var(restart_iounit, block(iblk)%bv_depth, &
                  &block(iblk)%buffer, BLK_VAR_CURRENT)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_depth, &
                  &block(iblk)%buffer, BLK_VAR_OLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_depth, &
                  &block(iblk)%buffer, BLK_VAR_OLDOLD)
             CALL restart_write_var(restart_iounit, block(iblk)%bv_depth, &
                  &block(iblk)%buffer, BLK_VAR_STAR)
             
             CALL restart_write_var(restart_iounit, block(iblk)%bv_eddy, &
                  &block(iblk)%buffer, BLK_VAR_CURRENT)
             
          END DO
          
          IF(do_transport)THEN
             DO i=1,max_species
                DO iblk = 1, max_blocks
                   CALL restart_write_var(restart_iounit, &
                        &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                        &BLK_VAR_CURRENT)
                   CALL restart_write_var(restart_iounit, &
                        &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                        &BLK_VAR_OLDOLD)
                   CALL restart_write_var(restart_iounit, &
                        &species(i)%scalar(iblk)%concvar, block(iblk)%buffer, &
                        &BLK_VAR_OLDOLD)
                END DO
             END DO
             IF (source_doing_sed) THEN 

                ! write the number of sediment
                ! fractions and particulates that will
                ! be written to the restart file
                
                WRITE(restart_iounit,*) sediment_fractions, particulates
                
                DO ifract = 1, sediment_fractions
                   DO iblk = 1, max_blocks
                      CALL bed_restart_write_var(restart_iounit,&
                           &bed(iblk)%bv_sediment, &
                           &block(iblk)%buffer, ifract)
                   END DO
                END DO
                DO ispecies = 1, max_species
                   DO iblk = 1, max_blocks
                      SELECT CASE (scalar_source(ispecies)%srctype)
                      CASE (GEN)
                         CALL bed_restart_write_var(restart_iounit,&
                              &bed(iblk)%bv_pore, &
                              &block(iblk)%buffer, ispecies)
                      CASE (PART)
                         CALL bed_restart_write_var(restart_iounit,&
                              &bed(iblk)%bv_particulate, &
                              &block(iblk)%buffer, ispecies)
                      END SELECT
                   END DO
                END DO
             END IF
          END IF

#ifndef NOOUTPUT
          CLOSE(restart_iounit)
#endif
       END IF
    END IF

    CALL block_var_sync()

  END SUBROUTINE write_restart

END MODULE hotstart
