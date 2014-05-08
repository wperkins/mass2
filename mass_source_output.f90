  ! ----------------------------------------------------------------
  ! file: mass_source_output.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January 10, 2011 by William A. Perkins
  ! Last Change: 2014-05-08 09:53:13 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE mass_source_output
! ----------------------------------------------------------------
MODULE mass_source_output

  USE config
  USE globals
  USE block_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PARAMETER, PRIVATE :: mass_source_iounit = 19
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: mass_source_ioname = 'mass_source_monitor.out'

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_setup(me)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: me
    INTEGER iblock

    IF (me .EQ. 0) THEN 
       CALL open_new(mass_source_ioname, mass_source_iounit)
       WRITE(mass_source_iounit,*)"# mass source history - summation of the mass source in each block "
       WRITE(mass_source_iounit,*)"#      total mass imbalance for each block in ft3/sec"
       WRITE(mass_source_iounit,100,advance='no')
       DO iblock = 1, max_blocks 
          WRITE(mass_source_iounit,200, advance='no') iblock
       END DO
       WRITE(mass_source_iounit,*)
       CALL FLUSH(mass_source_iounit)
    END IF
       
100 FORMAT('#date',8x,'time',5x)
200 FORMAT(i5,5x)

  END SUBROUTINE mass_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_print
  ! ----------------------------------------------------------------
  SUBROUTINE mass_print(me, date_string, time_string)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: me
    INTEGER :: iblock, j
    CHARACTER*(*) :: date_string, time_string

    IF (me .EQ. 0) THEN 
       j = 1
       DO iblock = 1, max_blocks
          IF (j == 1) &
               &WRITE(mass_source_iounit,3013, advance='no')&
               &date_string, time_string,hydro_iteration
          WRITE(mass_source_iounit,3012, advance='no') block(iblock)%mass_source_sum(1)
          IF (j >= 20) THEN
             j = 1
             IF (iblock .ne. max_blocks) WRITE(mass_source_iounit,*)
          ELSE
             j = j + 1
          END IF
       END DO
       WRITE(mass_source_iounit,*)
       CALL FLUSH(mass_source_iounit)
    END IF

3013 FORMAT(a10,2x,a12,1x,I3,1X)
3012 FORMAT((g12.4,1x))


  END SUBROUTINE mass_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_close(me)

    INTEGER, INTENT(IN) :: me

    IF (me .EQ. 0) THEN 
       CLOSE(mass_source_iounit)    
    END IF

  END SUBROUTINE mass_file_close

END MODULE mass_source_output
  
