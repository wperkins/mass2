!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	gage_output
!
! VERSION and DATE: MASS2 v0.241 9/28/98
!
! PURPOSE: manages time-series files for gage locations
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY: fixed-non F90 derived type initialization; mcr 9/28/98
!
!
!***************************************************************
!

MODULE gage_output

  USE utility
  USE date_time
  USE config
  USE block_module
  USE scalars
  USE scalars_source
  USE bed_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE gage_specs_struct
     !CHARACTER (LEN=80) :: filename = ''  ! fails for non F95 compilers
     CHARACTER (LEN=80) :: filename
     CHARACTER (LEN=40) :: ident
     INTEGER :: block
     INTEGER :: i_cell, j_cell
  END TYPE gage_specs_struct

  TYPE(gage_specs_struct), POINTER :: gage_specs(:)

  INTEGER, PARAMETER :: gage_iounit = 50
  INTEGER :: num_gages
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: gage_control = 'gage_control.dat'

  LOGICAL, PARAMETER, PRIVATE :: gage_do_text = .TRUE.

  INTEGER, PRIVATE :: tslen = 40, idlen = 40

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_make_ident
  ! ----------------------------------------------------------------
  SUBROUTINE gage_make_ident(gage_rec)

    IMPLICIT NONE
    TYPE(gage_specs_struct) :: gage_rec
    CHARACTER (LEN=80) :: buffer
    INTEGER :: i

    IF (LEN_TRIM(gage_rec%ident) .LE. 0) THEN
       WRITE(buffer, 100) &
            &gage_rec%block, gage_rec%i_cell, gage_rec%j_cell
       gage_rec%ident = buffer
    END IF

100 FORMAT('block=', I2.2, ',i=', I3.3, ',j=', I3.3)
  END SUBROUTINE gage_make_ident
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_read_control
  ! ----------------------------------------------------------------
  SUBROUTINE gage_read_control()

    IMPLICIT NONE

    INTEGER :: dum, alloc_stat, i, ierr
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    ! count up the number of gages and allocate the structure        
    num_gages = 0    
    CALL open_existing(gage_control, 50)
    DO WHILE(.TRUE.)
       READ(50,*,END=100)dum
       num_gages = num_gages + 1	
    END DO
100 CLOSE(50)

    ALLOCATE(gage_specs(num_gages), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the array of gage specs ')
    ENDIF

    CALL open_existing(gage_control, 50)
    DO i=1,num_gages
       gage_specs(i)%ident = ' '
       READ(50,*)gage_specs(i)%block,gage_specs(i)%i_cell,gage_specs(i)%j_cell,gage_specs(i)%ident
       IF (gage_specs(i)%block .LE. 0 .OR. gage_specs(i)%block .GT. max_blocks) THEN
          WRITE (msg, *) 'gage location ', i, ': invalid block number: ', gage_specs(i)%block
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
       CALL gage_make_ident(gage_specs(i))
    END DO
    CLOSE(50)

    IF (ierr .GT. 0) THEN
       CALL error_message('errors in "' // TRIM(gage_control) // '" input file', fatal=.TRUE.)
    END IF

    CALL status_message('allocation successful for array of gage specs')

  END SUBROUTINE gage_read_control

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_header
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_header(iounit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
    INTEGER :: i

    WRITE(iounit, '(A)', ADVANCE='NO') "#"
    WRITE(iounit, 101, ADVANCE='NO') "timestamp"
    WRITE(iounit, 100, ADVANCE='NO') "elapsed"
    
    ! time-dependent data variables

    WRITE(iounit, 100, ADVANCE='NO') "wsel"
    WRITE(iounit, 100, ADVANCE='NO') "depth"
    WRITE(iounit, 100, ADVANCE='NO') "vmag"
    WRITE(iounit, 100, ADVANCE='NO') "uvel"
    WRITE(iounit, 100, ADVANCE='NO') "vvel"
    WRITE(iounit, 100, ADVANCE='NO') "isdry"

    IF (do_transport) THEN
       DO i = 1, max_species
          WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name)
          
          SELECT CASE (scalar_source(i)%srctype)
          CASE (GEN)
             IF (source_doing_sed) THEN
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-pore'
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             END IF
          CASE (PART)
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-depos'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
          CASE (TDG)                             
             WRITE(iounit, 100, ADVANCE='NO') "tdgpress"
             WRITE(iounit, 100, ADVANCE='NO') "tdgdeltap"
             WRITE(iounit, 100, ADVANCE='NO') "tdgsat"
          CASE (SED)                             
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-depos'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-erode'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
          END SELECT
       END DO
       IF (source_doing_sed) THEN
          WRITE(iounit, 100, ADVANCE='NO') "beddepth"
       END IF
    END IF

    WRITE(iounit, *)
  
100 FORMAT(A15, ' ')
101 FORMAT(A19, ' ')
  END SUBROUTINE gage_file_header



  !##################################################################################
  SUBROUTINE gage_file_setup_text(do_transport)

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: do_transport
    INTEGER :: g
    INTEGER :: iblock, icell, jcell

    ! construct the generic file name for the gage output files
    ! file = "gage_block=1_icell=11_jcell=2.out"
    DO g=1,num_gages
       iblock = gage_specs(g)%block
       icell = gage_specs(g)%i_cell + 1 ! convert from cell to i,j
       jcell = gage_specs(g)%j_cell + 1
       IF (block_owns(block(iblock), icell, jcell)) THEN
          gage_specs(g)%filename = "gage_" // TRIM(gage_specs(g)%ident) // ".out"
          OPEN(gage_iounit, file=gage_specs(g)%filename)
          CALL gage_file_header(gage_iounit)
          CLOSE(gage_iounit)
       END IF
    END DO
  END SUBROUTINE gage_file_setup_text
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print_text
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_text(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: date_string, time_string
    DOUBLE PRECISION, INTENT(IN) :: elapsed, salinity, baro_press
    LOGICAL, INTENT(IN) :: do_transport

    INTEGER :: i, j, iblock, icell, jcell, ifract
    DOUBLE PRECISION :: conc_TDG, t_water
    DOUBLE PRECISION :: value
    CHARACTER (LEN=tslen) :: timestamp

    timestamp = TRIM(date_string) // ' ' // TRIM(time_string)

    DO i=1,num_gages
       iblock = gage_specs(i)%block
       icell = gage_specs(i)%i_cell + 1 ! convert from cell to i,j
       jcell = gage_specs(i)%j_cell + 1

       IF (.NOT. block_owns(block(iblock), icell, jcell)) CYCLE
       
       OPEN(gage_iounit, file=gage_specs(i)%filename, POSITION="APPEND")

       WRITE(gage_iounit, '(A25, " ")', ADVANCE='NO') timestamp
       WRITE(gage_iounit, 100, ADVANCE='NO') elapsed

       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%wsel(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%depth(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') &
            &SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%uvel(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%vvel(icell,jcell)

       IF (block(iblock)%isdry(icell,jcell)) THEN
          value = 1.0
       ELSE
          value = 0.0
       END IF
       WRITE (gage_iounit, 102, ADVANCE='NO') value

       IF(do_transport)THEN

          IF (source_doing_temp) THEN
             t_water = species(source_temp_idx)%scalar(iblock)%conc(icell,jcell)
          END IF
          DO j = 1, max_species
             WRITE (gage_iounit, 102, ADVANCE='NO') &
                  &species(j)%scalar(iblock)%conc(icell,jcell)/&
                  &scalar_source(j)%conversion
             SELECT CASE(scalar_source(j)%srctype)
             CASE (GEN)
                IF (scalar_source(j)%generic_param%issorbed) THEN
                   WRITE(gage_iounit, 102, ADVANCE='NO') bed(iblock)%pore(icell, jcell, j)
                   value = 0.0
                   IF (bed(iblock)%depth(icell, jcell, 1) .GT. 0.0) &
                        &value = bed(iblock)%pore(icell, jcell, j)/ &
                        &(bed(iblock)%depth(icell, jcell, 1)* &
                        &bed(iblock)%porosity(icell, jcell, 1))
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') &
                        &bed(iblock)%pore(icell, jcell, j)*&
                        &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                ELSE
                   value = 0.0
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                END IF
                
             CASE (TDG)                             
!!$                conc_TDG = species(j)%scalar(iblock)%conc(icell,jcell)
!!$                value = TDGasPress(conc_TDG,  t_water,  salinity)
!!$                WRITE(gage_iounit, 102, ADVANCE='NO') value
!!$                
!!$                value = TDGasDP(conc_TDG,  t_water,  salinity, baro_press)
!!$                WRITE(gage_iounit, 102, ADVANCE='NO') value
!!$          
!!$                value = TDGasSaturation(conc_TDG,  t_water,  salinity, baro_press)
!!$                WRITE(gage_iounit, 102, ADVANCE='NO') value
!!$
             CASE (SED)                             
                ifract = scalar_source(j)%sediment_param%ifract
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%sediment_param%block(iblock)%deposition(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%sediment_param%block(iblock)%erosion(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%sediment(icell, jcell, ifract)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%sediment(icell, jcell, ifract)
             CASE (PART)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%part_param%block(iblock)%bedexch(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%particulate(icell, jcell, j)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%particulate(icell, jcell, j)
             END SELECT

          END DO

          IF (source_doing_sed) THEN
             WRITE(gage_iounit, 102, ADVANCE='NO')&
                  &bed(iblock)%depth(icell, jcell, 1)
          END IF
             
       END IF
       CLOSE(50)
    END DO
100 FORMAT(F18.8, ' ')
102 FORMAT(E15.8, ' ')

  END SUBROUTINE gage_print_text



  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_setup()

    IMPLICIT NONE

    CALL gage_read_control()

    IF (num_gages .gt. 0) THEN
       CALL gage_file_setup_text(do_transport)
    END IF
       
  END SUBROUTINE gage_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    IMPLICIT NONE
    CHARACTER*(*) :: date_string, time_string
    DOUBLE PRECISION :: elapsed, salinity, baro_press
    LOGICAL :: do_transport

    CALL gage_print_text(date_string, time_string, elapsed, &
         &do_transport, salinity, baro_press)

  END SUBROUTINE gage_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_close()

    IMPLICIT NONE

    CLOSE(gage_iounit)

  END SUBROUTINE gage_file_close
  



END MODULE gage_output
