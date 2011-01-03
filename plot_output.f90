! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Sat Jan  1 11:05:58 2011 by William A. Perkins <d3g096@PE10588.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

  USE plot_cgns

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

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

    CALL plot_geometry()
    !FIXME: CALL accum_initialize()
    CALL plot_cgns_setup()
  END SUBROUTINE plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print(date_string, time_string, salinity, baro_press)

    USE scalars_source, ONLY: source_doing_sed
    USE bed_module
    USE globals
    
    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t, salinity, baro_press
    CHARACTER (LEN=80) :: zone_name

    !FIXME: CALL accum_calc()
    CALL velocity_shift()
    CALL plot_cgns_write(date_string, time_string, salinity, baro_press)
    !FIXME: CALL accum_reset()

  END SUBROUTINE plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_close()

    IMPLICIT NONE

    CALL plot_cgns_close()

    !FIXME: CALL accum_done()

  END SUBROUTINE plot_file_close



END MODULE plot_output
