! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Fri May 21 15:11:53 1999 by William A. Perkins <perk@mack.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

                                ! module constants

  INTEGER, PARAMETER, PUBLIC :: plot_iounit=12
  CHARACTER (LEN=10), PARAMETER :: plot_ioname = 'plot.dat'

  INTEGER, PARAMETER, PUBLIC :: diag_plot_iounit=20
  CHARACTER (LEN=80), PARAMETER :: diag_plot_ioname = 'diagnostic_plot.dat'

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_zone_name
  ! ----------------------------------------------------------------
  SUBROUTINE plot_zone_name(date_string, time_string, zone_name)
    
    IMPLICIT NONE
    CHARACTER*(*) :: date_string, time_string, zone_name

    zone_name(1:10) = date_string
    zone_name(11:11) = ' '
    zone_name(12:19) = time_string
    zone_name(20:20) = ' '
    zone_name(21:26) = 'block '

  END SUBROUTINE plot_zone_name


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup_tecplot()

    IMPLICIT NONE

    OPEN(plot_iounit,file=plot_ioname)
    WRITE(plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
    WRITE(plot_iounit,100)
100 FORMAT("variables=""x"" ""y"" ""u cart"" ""v cart"" ""depth"" ""zbot"" ""wsel"" &
           &            ""TDG con"" ""Temp"" ""TGP"" ""TDG delP"" ""%Sat""")
    

  END SUBROUTINE plot_file_setup_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_tecplot(date_string, time_string, salinity, baro_press)

    USE globals
    USE scalars
    USE gas_functions
    
    IMPLICIT NONE

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8) :: time_string
    DOUBLE PRECISION :: salinity, baro_press, ccstar, conc_TDG, t_water
    CHARACTER*26 :: zone_name
    INTEGER :: i, j, iblock

    CALL plot_zone_name(date_string, time_string, zone_name)

    DO iblock=1,max_blocks
       DO i=2, block(iblock)%xmax
          DO j=2, block(iblock)%ymax
             block(iblock)%uvel_p(i,j) = &
                  &0.5*(block(iblock)%uvel(i,j)+block(iblock)%uvel(i-1,j))
             block(iblock)%vvel_p(i,j) = &
                  &0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j-1))
          END DO
       END DO
       DO i=1, 1
          DO j=2, block(iblock)%ymax
             block(iblock)%uvel_p(i,j) = block(iblock)%uvel(i,j)
             block(iblock)%vvel_p(i,j) = &
                  &0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j-1))
          END DO
       END DO
       DO i=block(iblock)%xmax+1, block(iblock)%xmax+1
          DO j=2, block(iblock)%ymax
             block(iblock)%uvel_p(i,j) = block(iblock)%uvel(block(iblock)%xmax,j)
             block(iblock)%vvel_p(i,j) = &
                  &0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j-1))
          END DO
   END DO

       ! transform to cartesian coordinates for plotting
       block(iblock)%u_cart = &
            &(block(iblock)%uvel_p/block(iblock)%hp1)*block(iblock)%x_xsi + &
            &(block(iblock)%vvel_p/block(iblock)%hp2)*block(iblock)%x_eta
       block(iblock)%v_cart = &
            &(block(iblock)%uvel_p/block(iblock)%hp1)*block(iblock)%y_xsi + &
            &(block(iblock)%vvel_p/block(iblock)%hp2)*block(iblock)%y_eta

       WRITE(plot_iounit,*) "zone f=block", " t=""", zone_name, iblock , """",&
            &" i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
       WRITE(plot_iounit,*)block(iblock)%x
       WRITE(plot_iounit,*)block(iblock)%y
       ! WRITE(plot_iounit,*)block(iblock)%uvel_p
       ! WRITE(plot_iounit,*)block(iblock)%vvel_p
       WRITE(plot_iounit,*)block(iblock)%u_cart
       WRITE(plot_iounit,*)block(iblock)%v_cart
       WRITE(plot_iounit,*)block(iblock)%depth
       WRITE(plot_iounit,*)block(iblock)%zbot
       WRITE(plot_iounit,*)block(iblock)%wsel
       WRITE(plot_iounit,*)species(1)%scalar(iblock)%conc
       WRITE(plot_iounit,*)species(2)%scalar(iblock)%conc

       DO i=1,block(iblock)%xmax+1
          DO j=1,block(iblock)%ymax+1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             block(iblock)%TDG_stuff(i,j) = &
                  &TDGasPress(conc_TDG,  t_water,  salinity)
          END DO
       END DO
       WRITE(plot_iounit,*)block(iblock)%TDG_stuff

       DO i=1,block(iblock)%xmax+1
          DO j=1,block(iblock)%ymax+1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             block(iblock)%TDG_stuff(i,j) = &
                  &TDGasDP(conc_TDG, t_water,  salinity,  baro_press)
          END DO
       END DO
       WRITE(plot_iounit,*)block(iblock)%TDG_stuff

       DO i=1,block(iblock)%xmax+1
          DO j=1,block(iblock)%ymax+1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             block(iblock)%TDG_stuff(i,j) = &
                  &TDGasSaturation( conc_TDG, t_water,  salinity, baro_press)
          END DO
       END DO
       WRITE(plot_iounit,*)block(iblock)%TDG_stuff

    END DO
  END SUBROUTINE plot_print_tecplot

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
  ! SUBROUTINE diag_plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_print_tecplot(date_string, time_string, delta_t)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8) :: time_string
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

       block(iblock)%froude_num = &
            &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
            &SQRT(grav*block(iblock)%depth)
       WRITE(diag_plot_iounit,*)block(iblock)%froude_num
       
       block(iblock)%courant_num = &
            &delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
            &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
            &SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)
       WRITE(diag_plot_iounit,*)block(iblock)%courant_num
    END DO
  END SUBROUTINE diag_plot_print_tecplot


END MODULE plot_output
