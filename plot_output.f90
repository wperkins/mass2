! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Tue Jan 11 14:16:33 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

  USE plot_cgns

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

#include "mafdecls.fh"
#include "global.fh"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_zone_name
  ! ----------------------------------------------------------------
  SUBROUTINE plot_zone_name(date_string, time_string, zone_name)
    
    IMPLICIT NONE
    CHARACTER (LEN=*) :: date_string, time_string, zone_name
    CHARACTER blk

    zone_name = TRIM(date_string) // ' ' // TRIM(time_string) // ' ' // 'block '

  END SUBROUTINE plot_zone_name


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup()

    IMPLICIT NONE

    ! this has to be collective
    CALL plot_geometry()
    !FIXME: CALL accum_initialize()

    IF (ga_nodeid() .EQ. 0) THEN
       CALL plot_cgns_setup()
    END IF
  END SUBROUTINE plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print(date_string, time_string, salinity, baro_press)
    
    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t, salinity, baro_press
    CHARACTER (LEN=80) :: zone_name

    IF (do_flow_diag) CALL calc_diag()
    !FIXME: CALL accum_calc()
    CALL velocity_shift()
    CALL ga_sync()
    IF (ga_nodeid() .EQ. 0) THEN
       CALL plot_cgns_write(date_string, time_string, salinity, baro_press)
    END IF
    CALL ga_sync()
    !FIXME: CALL accum_reset()

  END SUBROUTINE plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_close()

    IMPLICIT NONE

    IF (ga_nodeid() .EQ. 0) THEN
       CALL plot_cgns_close()
    END IF

    !FIXME: CALL accum_done()

  END SUBROUTINE plot_file_close



END MODULE plot_output
