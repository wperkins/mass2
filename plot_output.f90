! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Tue Feb 17 10:49:48 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

  USE accumulator
  USE misc_vars, ONLY: do_accumulate, &
       &i_index_min, i_index_extra, j_index_min, j_index_extra
  USE plot_cgns
  USE plot_netcdf

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

                                ! module constants

  INTEGER, PARAMETER, PUBLIC :: plot_iounit=12
  LOGICAL, PUBLIC :: plot_do_tecplot = .false.
  LOGICAL, PUBLIC :: plot_do_cgns = .true.
  LOGICAL, PUBLIC :: plot_do_netcdf = .true. ! (.not. plot_do_cgns)
  CHARACTER (LEN=80), PARAMETER :: plot_ioname = 'plot.dat'

  INTEGER, PARAMETER, PRIVATE :: diag_plot_iounit=20
  CHARACTER (LEN=80), PARAMETER :: diag_plot_ioname = 'diagnostic_plot.dat'

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
  ! SUBROUTINE plot_file_setup_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup_tecplot()

    USE misc_vars, ONLY: do_flow, do_flow_output, do_flow_diag, do_transport, do_wetdry, do_rptdead

    IMPLICIT NONE

    OPEN(plot_iounit,file=plot_ioname)
    WRITE(plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
    WRITE(plot_iounit,*)"variables="
    WRITE(plot_iounit,*)'   "x" "y" "zbot" "ucart" "vcart" "depth"'
    IF (do_flow_diag) THEN
       WRITE(plot_iounit,*)'   "uvel" "vvel" "vmag" "wsel" "shear" "courant" "froude"'
    END IF
    IF (do_wetdry) THEN
       WRITE(plot_iounit,*)'   "isdry"'
    END IF
    IF (do_rptdead) THEN
       WRITE(plot_iounit,*)'   "isdead"'
    END IF

    IF (do_transport) THEN
    END IF
  END SUBROUTINE plot_file_setup_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_tecplot(date_string, time_string, salinity, baro_press)

    USE globals
    USE misc_vars, ONLY: do_flow, do_flow_output, do_flow_diag, do_transport, do_wetdry, do_rptdead
    USE scalars
    USE gas_functions
    
    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: salinity, baro_press, ccstar, conc_TDG, t_water
    CHARACTER*26 :: zone_name
    INTEGER :: i, j, iblock, xlim, ylim

    CALL plot_zone_name(date_string, time_string, zone_name)

    DO iblock=1,max_blocks
       xlim = block(iblock)%xmax+1
       ylim = block(iblock)%ymax+1
       WRITE(plot_iounit,*) "zone f=block", " t=""", zone_name, iblock , """",&
            &" i=", xlim, " j= ",ylim
       WRITE(plot_iounit,*)block(iblock)%x_out(1:xlim, 1:ylim)
       WRITE(plot_iounit,*)block(iblock)%y_out(1:xlim, 1:ylim)
       WRITE(plot_iounit,*)block(iblock)%zbot_out(1:xlim, 1:ylim)
       WRITE(plot_iounit,*)block(iblock)%u_cart(1:xlim, 1:ylim)
       WRITE(plot_iounit,*)block(iblock)%v_cart(1:xlim, 1:ylim)
       WRITE(plot_iounit,*)block(iblock)%depth(1:xlim, 1:ylim)

       IF (do_flow_diag) THEN
          WRITE(plot_iounit,*)block(iblock)%uvel_p(1:xlim, 1:ylim)
          WRITE(plot_iounit,*)block(iblock)%uvel_p(1:xlim, 1:ylim)
          WRITE(plot_iounit,*)block(iblock)%wsel(1:xlim, 1:ylim)
       END IF
                                ! need to fix for transport

!        WRITE(plot_iounit,*)species(1)%scalar(iblock)%conc
!        WRITE(plot_iounit,*)species(2)%scalar(iblock)%conc

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasPress(conc_TDG,  t_water,  salinity)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasDP(conc_TDG, t_water,  salinity,  baro_press)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasSaturation( conc_TDG, t_water,  salinity, baro_press)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

    END DO
  END SUBROUTINE plot_print_tecplot


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_geometry
  ! This routine is used to compute the geometry of the output.  It is
  ! intended to be the ONLY place where the coordinates of output data
  ! are computed.  This routine exists so that output grids can remain
  ! understandable even when the computation grid is complicated with
  ! ghost cells, half cells, etc.
  ! ----------------------------------------------------------------
  SUBROUTINE plot_geometry()

    USE globals
    USE misc_vars, ONLY: i_ghost, j_ghost
    IMPLICIT NONE

    INTEGER :: iblock, i, j

    DO iblock = 1, max_blocks

                                ! without ghost cells, the output
                                ! locations are the same as the
                                ! computation locations

       block(iblock)%x_out = block(iblock)%x
       block(iblock)%y_out = block(iblock)%y
       block(iblock)%zbot_out = block(iblock)%zbot

                                ! with ghost cells at the up/down
                                ! stream end, the output locations
                                ! need to be interpolated at the block
                                ! ends. The number of ghost cells in
                                ! irrevalent, as long as there are
                                ! ghost cells

       IF (i_ghost .GT. 0) THEN
          i = 1
          block(iblock)%x_out(i,1) = block(iblock)%x_grid(i,1)
          block(iblock)%y_out(i,1) = block(iblock)%y_grid(i,1)
          block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i,1)
          DO j = 2, block(iblock)%ymax
             block(iblock)%x_out(i,j) = &
                  &0.5*(block(iblock)%x_grid(i,j-1) + block(iblock)%x_grid(i,j))
             block(iblock)%y_out(i,j) = &
                  &0.5*(block(iblock)%y_grid(i,j-1) + block(iblock)%y_grid(i,j))
             block(iblock)%zbot_out(i,j) = &
                  &0.5*(block(iblock)%zbot_grid(i,j-1) + block(iblock)%zbot_grid(i,j))
          END DO
          j = block(iblock)%ymax + 1
          block(iblock)%x_out(i,j) = block(iblock)%x_grid(i,j-1)
          block(iblock)%y_out(i,j) = block(iblock)%y_grid(i,j-1)
          block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i,j-1)
          
          i =  block(iblock)%xmax + 1
          block(iblock)%x_out(i,1) = block(iblock)%x_grid(i-1,1)
          block(iblock)%y_out(i,1) = block(iblock)%y_grid(i-1,1)
          block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i-1,1)
          DO j = 2, block(iblock)%ymax
             block(iblock)%x_out(i,j) = &
                  &0.5*(block(iblock)%x_grid(i-1,j-1) + block(iblock)%x_grid(i-1,j))
             block(iblock)%y_out(i,j) = &
                  &0.5*(block(iblock)%y_grid(i-1,j-1) + block(iblock)%y_grid(i-1,j))
             block(iblock)%zbot_out(i,j) = &
                  &0.5*(block(iblock)%zbot_grid(i-1,j-1) + block(iblock)%zbot_grid(i-1,j))
          END DO
          j = block(iblock)%ymax + 1
          block(iblock)%x_out(i,j) = block(iblock)%x_grid(i-1,j-1)
          block(iblock)%y_out(i,j) = block(iblock)%y_grid(i-1,j-1)
          block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i-1,j-1)
       END IF
       
                                ! ditto for lateral ghost cells

       IF (j_ghost .GT. 0) THEN
          j = 1
          DO i = 2, block(iblock)%xmax
             block(iblock)%x_out(i,j) = &
                  &0.5*(block(iblock)%x_grid(i,j) + block(iblock)%x_grid(i-1,j))
             block(iblock)%y_out(i,j) = &
                  &0.5*(block(iblock)%y_grid(i,j) + block(iblock)%y_grid(i-1,j))
             block(iblock)%zbot_out(i,j) = &
                  &0.5*(block(iblock)%zbot_grid(i,j) + block(iblock)%zbot_grid(i-1,j))
          END DO
          j = block(iblock)%ymax + 1
          DO i = 2, block(iblock)%xmax
             block(iblock)%x_out(i,j) = &
                  &0.5*(block(iblock)%x_grid(i,j-1) + block(iblock)%x_grid(i-1,j-1))
             block(iblock)%y_out(i,j) = &
                  &0.5*(block(iblock)%y_grid(i,j-1) + block(iblock)%y_grid(i-1,j-1))
             block(iblock)%zbot_out(i,j) = &
                  &0.5*(block(iblock)%zbot_grid(i,j-1) + block(iblock)%zbot_grid(i-1,j-1))
          END DO
       END IF

    END DO
  END SUBROUTINE plot_geometry


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup()

    IMPLICIT NONE

    CALL plot_geometry()
    CALL accum_initialize()
    IF (plot_do_tecplot) &
         &CALL plot_file_setup_tecplot()
    IF (plot_do_netcdf) &
         &CALL plot_file_setup_netcdf()
    IF (plot_do_cgns) &
         &CALL plot_cgns_setup()
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

    CALL accum_calc()
    CALL velocity_shift()

    IF (plot_do_tecplot) &
         &CALL plot_print_tecplot(date_string, time_string, salinity, baro_press)
    IF (plot_do_netcdf) &
         &CALL plot_print_netcdf(date_string, time_string, salinity, baro_press)
    IF (plot_do_cgns) &
         &CALL plot_cgns_write(date_string, time_string, salinity, baro_press)
    CALL accum_reset()

  END SUBROUTINE plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_close()

    IMPLICIT NONE

    IF (plot_do_netcdf) CALL plot_netcdf_close()
    IF (plot_do_tecplot) CLOSE(plot_iounit)
    IF (plot_do_cgns) CALL plot_cgns_close()

    CALL accum_done()

  END SUBROUTINE plot_file_close

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_setup
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_setup()

    IF (plot_do_tecplot) THEN
       CALL diag_plot_file_setup_tecplot()
    END IF

    ! IF (plot_do_netcdf): already taken care of in plot_file_setup_netcdf

  END SUBROUTINE diag_plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_file_setup_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_setup_tecplot()

    IMPLICIT NONE

    OPEN(diag_plot_iounit,file=diag_plot_ioname)
    WRITE(diag_plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
    WRITE(diag_plot_iounit,100)
100 FORMAT("variables=""x"" ""y"" ""Froude No."" ""Courant No.""")

  END SUBROUTINE diag_plot_file_setup_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_print(date_string, time_string, delta_t)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t
    INTEGER :: i, iblock

    DO iblock = 1, max_blocks

       WHERE (block(iblock)%depth .GT. 0.0 .AND. &
            &block(iblock)%hp1 .NE. 0.0 .AND. &
            &block(iblock)%hp2 .NE. 0.0)
          block(iblock)%froude_num = &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
               &SQRT(grav*block(iblock)%depth)
          block(iblock)%courant_num = &
               &delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
               &SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)
       ELSEWHERE 
          block(iblock)%froude_num = 0.0
          block(iblock)%courant_num = 0.0
       END WHERE


    END DO

    IF (plot_do_tecplot) &
         &CALL diag_plot_print_tecplot(date_string, time_string, delta_t)

  END SUBROUTINE diag_plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_print_tecplot(date_string, time_string, delta_t)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t
    CHARACTER*26 :: zone_name
    INTEGER :: i, iblock

    CALL plot_zone_name(date_string, time_string, zone_name)

    DO iblock=1,max_blocks

       !---------------------------------------------------------------
       ! diagnostic plot file output; tecplot format
       WRITE(diag_plot_iounit,*) "zone f=block"," t=""",zone_name,iblock,"""",&
            &" i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
       WRITE(diag_plot_iounit,*)block(iblock)%x
       WRITE(diag_plot_iounit,*)block(iblock)%y

       WRITE(diag_plot_iounit,*)block(iblock)%froude_num
       
       WRITE(diag_plot_iounit,*)block(iblock)%courant_num
    END DO
  END SUBROUTINE diag_plot_print_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_close()

    IF (plot_do_tecplot) THEN
       CLOSE(diag_plot_iounit)
    END IF

  END SUBROUTINE diag_plot_file_close


END MODULE plot_output
