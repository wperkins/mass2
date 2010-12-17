! ----------------------------------------------------------------
! file: block_flux_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 24, 2010 by William A. Perkins
! Last Change: Mon Mar 29 07:49:01 2010 by William A. Perkins <d3g096@bearflag.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE block_flux_output
! ----------------------------------------------------------------
MODULE block_flux_output

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=80), PRIVATE :: block_flux_ioname = "block_flux.out"

  INTEGER, PARAMETER, PRIVATE :: block_flux_iounit = 39

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_flux_setup
  ! ----------------------------------------------------------------
  SUBROUTINE block_flux_setup()

    USE globals

    IMPLICIT NONE

    CALL open_new(block_flux_ioname, block_flux_iounit)
    !                        
    WRITE(block_flux_iounit, 10) 'Upstream', 'Downstream', 'Right Bank', 'Left Bank'
    WRITE(block_flux_iounit, 11, advance='no')
    WRITE(block_flux_iounit, 12, advance='no')
    WRITE(block_flux_iounit, 12, advance='yes')
    CALL FLUSH(block_flux_iounit)

10 FORMAT('#Date       Time         Blk ',4(1X,A15)) 
11 FORMAT('##########  ############ ### ')
12 FORMAT(4(1X,15(1H#)))


  END SUBROUTINE block_flux_setup


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_flux_write
  ! ----------------------------------------------------------------
  SUBROUTINE block_flux_print(date_string, time_string)
    
    USE globals

    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: date_string, time_string

    INTEGER, PARAMETER:: sides = 4
    INTEGER :: iblock
    INTEGER :: imax, jmax
    DOUBLE PRECISION :: flux(4), area(4)

    DO iblock = 1, max_blocks
       imax = block(iblock)%xmax
       jmax = block(iblock)%ymax
       flux(1) = uflux(block(iblock), 1, 2, jmax) ! upstream
       flux(2) = uflux(block(iblock), imax, 2, jmax) ! downstream
       flux(3) = vflux(block(iblock), 2, imax, 1)  ! right bank
       flux(4) = vflux(block(iblock), 2, imax, jmax)  ! left bank

       area(1) = uarea(block(iblock), 1, 2, jmax) ! upstream
       area(2) = uarea(block(iblock), imax, 2, jmax) ! downstream
       area(3) = varea(block(iblock), 2, imax, 1)  ! right bank
       area(4) = varea(block(iblock), 2, imax, jmax)  ! left bank

       WRITE(block_flux_iounit, 100, advance='no') date_string,time_string, iblock
       WRITE(block_flux_iounit, 200, advance='no') flux(1), flux(2), flux(3), flux(4)
       WRITE(block_flux_iounit, 200, advance='yes') area(1), area(2), area(3), area(4)
    END DO

    CALL FLUSH(block_flux_iounit)

100 FORMAT(a10,2x,a12,1x,I3,1x)
200 FORMAT(4(1X,E15.6))


  END SUBROUTINE block_flux_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_flux_close
  ! ----------------------------------------------------------------
  SUBROUTINE block_flux_close()

    IMPLICIT NONE

    CLOSE(block_flux_iounit)

  END SUBROUTINE block_flux_close



END MODULE block_flux_output
