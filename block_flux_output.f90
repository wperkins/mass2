! ----------------------------------------------------------------
! file: block_flux_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 24, 2010 by William A. Perkins
! Last Change: Wed Mar 24 13:55:51 2010 by William A. Perkins <d3g096@bearflag.pnl.gov>
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
    WRITE(block_flux_iounit, 11)
    CALL FLUSH(block_flux_iounit)

10 FORMAT('#Date       Time         Blk ',4(1X,A15)) 
11 FORMAT('##########  ############ ### ',4(1X,15(1H#)))


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
    DOUBLE PRECISION :: flux(4)

    DO iblock = 1, max_blocks
       imax = block(iblock)%xmax
       jmax = block(iblock)%ymax
       flux(1) = uflux(block(iblock), 1, 2, jmax) ! upstream
       flux(2) = uflux(block(iblock), imax, 2, jmax) ! downstream
       flux(3) = vflux(block(iblock), 2, imax, 1)  ! right bank
       flux(4) = vflux(block(iblock), 2, imax, jmax)  ! left bank

       WRITE(block_flux_iounit, 100, advance='no') date_string,time_string, iblock
       WRITE(block_flux_iounit, 200) flux(1), flux(2), flux(3), flux(4)
    END DO

    CALL FLUSH(block_flux_iounit)

100 FORMAT(a10,2x,a12,1x,I3,1x)
200 FORMAT(4(1X,E15.4))


  END SUBROUTINE block_flux_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_flux_close
  ! ----------------------------------------------------------------
  SUBROUTINE block_flux_close()

    IMPLICIT NONE

    CLOSE(block_flux_iounit)

  END SUBROUTINE block_flux_close



END MODULE block_flux_output
