! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Tue Aug 24 12:29:33 1999 by William A. Perkins <perk@mack.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

                                ! module constants

  INTEGER, PARAMETER, PUBLIC :: plot_iounit=12
  LOGICAL, PARAMETER, PUBLIC :: plot_do_tecplot = .false.
  LOGICAL, PARAMETER, PUBLIC :: plot_do_netcdf = (.not. plot_do_tecplot)
  CHARACTER (LEN=80), PARAMETER :: plot_ioname = 'plot.dat'

  INTEGER, PRIVATE :: plot_ncid
  CHARACTER (LEN=80), PARAMETER :: plot_ncname = 'plot.nc'
  INTEGER, PRIVATE :: tslen = 20
  INTEGER, PRIVATE :: block_dimid, eta_dimid, xi_dimid, time_dimid, tslen_dimid
  INTEGER, PRIVATE :: etamax_varid, ximax_varid
  INTEGER, PRIVATE :: x_varid, y_varid, zbot_varid, time_varid, ts_varid
  INTEGER, PRIVATE :: hp1_varid, hp2_varid, gp12_varid
  INTEGER, PRIVATE :: u_varid, v_varid, ucart_varid, vcart_varid, vmag_varid
  INTEGER, PRIVATE :: depth_varid, wsel_varid
  INTEGER, PRIVATE :: temp_varid, conc_varid, press_varid, dp_varid, sat_varid
  INTEGER, PRIVATE :: courant_varid, froude_varid
  REAL, ALLOCATABLE, PRIVATE :: nctmp_real(:,:,:)
  DOUBLE PRECISION, ALLOCATABLE, PRIVATE :: nctmp_double(:,:,:)

  INTEGER, PARAMETER, PRIVATE :: diag_plot_iounit=20
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
  ! SUBROUTINE plot_fix_velocity
  ! ----------------------------------------------------------------
  SUBROUTINE plot_fix_velocity()

    USE globals
    
    IMPLICIT NONE

    INTEGER :: i, j, iblock


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
    END DO

  END SUBROUTINE plot_fix_velocity


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
  ! SUBROUTINE plot_file_setup_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup_netcdf()

    USE globals

    IMPLICIT NONE

    INTEGER :: ncstat, dimids(10)
    INTEGER :: iblock, index(10), len(10)
    INTEGER :: i, j, max_x, max_y

    INCLUDE 'netcdf.inc'

    ncstat = nf_create (plot_ncname, IOR(IOR(nf_write, nf_clobber), nf_fill), plot_ncid)
    ncstat = nf_def_dim (plot_ncid, "block", max_blocks, block_dimid)

                                ! limits of individual block dimensions

    dimids(1) = block_dimid
    ncstat = nf_def_var (plot_ncid, "etamax", nf_int, 1, dimids, etamax_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "ximax", nf_int, 1, dimids, ximax_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! limits for data

    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1
    ncstat = nf_def_dim (plot_ncid, "eta", max_x, eta_dimid) 
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "xi", max_y, xi_dimid) 
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "time", nf_unlimited, time_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "tslen", tslen, tslen_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! time variables

    dimids(1) = time_dimid
    ncstat = nf_def_var (plot_ncid, "time", nf_double, 1, dimids, time_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text(plot_ncid, time_varid, "Units", &
         &32, "days since 1900-01-01 00:00:00")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    dimids(1) = tslen_dimid
    dimids(2) = time_dimid
    ncstat = nf_def_var (plot_ncid, "timestamp", nf_char, 2, dimids, ts_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! non-time-dependant data variables

    dimids(1) = block_dimid
    dimids(2) = eta_dimid
    dimids(3) = xi_dimid
    ncstat = nf_def_var (plot_ncid, "x", nf_double, 3, dimids, x_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "y", nf_double, 3, dimids, y_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "zbot", nf_float, 3, dimids, zbot_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_put_att_text(plot_ncid, x_varid, "Units", 4, "feet")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_copy_att (plot_ncid, x_varid, "Units", plot_ncid, y_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_copy_att (plot_ncid, x_varid, "Units", plot_ncid, zbot_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, zbot_varid, "Description", &
         &16, "Bottom Elevation")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "hp1", nf_float, 3, dimids, hp1_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, hp1_varid, "Description", &
         &15, "Grid Metric hp1")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "hp2", nf_float, 3, dimids, hp2_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, hp2_varid, "Description", &
         &15, "Grid Metric hp2")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "gp12", nf_float, 3, dimids, gp12_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, gp12_varid, "Description", &
         &16, "Grid Metric gp12")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)


                                ! time-dependant data variables

    dimids(4) = time_dimid

    ncstat = nf_def_var (plot_ncid, "uvel", nf_float, 4, dimids, u_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, u_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, u_varid, "Description", &
         &21, "Longitudinal Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "vvel", nf_float, 4, dimids, v_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, v_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, v_varid, "Description", &
         &16, "Lateral Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "ucart", nf_float, 4, dimids, ucart_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, ucart_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, ucart_varid, "Description", &
         &17, "Eastward Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "vcart", nf_float, 4, dimids, vcart_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, vcart_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, vcart_varid, "Description", &
         &18, "Northward Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "vmag", nf_float, 4, dimids, vmag_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, vmag_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, vmag_varid, "Description", &
         &18, "Velocity Magnitude")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "depth", nf_float, 4, dimids, depth_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_copy_att (plot_ncid, x_varid, "Units", plot_ncid, depth_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, depth_varid, "Description", &
         &11, "Water Depth")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    ncstat = nf_def_var (plot_ncid, "wsel", nf_float, 4, dimids, wsel_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_copy_att (plot_ncid, x_varid, "Units", plot_ncid, wsel_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, wsel_varid, "Description", &
         &23, "Water Surface Elevation")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! diagnostic values

    ncstat = nf_def_var (plot_ncid, "courant", nf_float, 4, dimids, courant_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, courant_varid, "Description", &
         &14, "Courant Number")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    
    ncstat = nf_def_var (plot_ncid, "froude", nf_float, 4, dimids, froude_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, froude_varid, "Description", &
         &13, "Froude Number")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! water quality variables

                                ! TDG concentration 

    ncstat = nf_def_var (plot_ncid, "tdgconc", nf_float, 4, dimids, conc_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, conc_varid, "Units", 15, "milligram/liter")
    ncstat = nf_put_att_text (plot_ncid, conc_varid, "Description", &
         &33, "Total Dissolved Gas Concentration")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! temperature

    ncstat = nf_def_var (plot_ncid, "temperature", nf_float, 4, dimids, temp_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, temp_varid, "Units", 7, "Celcius")
    ncstat = nf_put_att_text (plot_ncid, temp_varid, "Description", &
         &11, "Temperature")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! TDG pressure

    ncstat = nf_def_var (plot_ncid, "tdgpress", nf_float, 4, dimids, press_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, press_varid, "Units", 17, "millimeters of Hg")
    ncstat = nf_put_att_text (plot_ncid, press_varid, "Description", &
         &29, "Total Dissolved Gas Pressure")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! TDG delta P

    ncstat = nf_def_var (plot_ncid, "tdgdeltap", nf_float, 4, dimids, dp_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, dp_varid, "Units", 17, "millimeters of Hg")
    ncstat = nf_put_att_text (plot_ncid, dp_varid, "Description", &
         &35, "Total Dissolved Gas Pressure Delta")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! TDG percent saturation

    ncstat = nf_def_var (plot_ncid, "tdgsat", nf_float, 4, dimids, sat_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, sat_varid, "Units", 7, "percent")
    ncstat = nf_put_att_text (plot_ncid, sat_varid, "Description", &
         &30, "Total Dissolved Gas Saturation")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! done w/ file definition

    ncstat = nf_enddef(plot_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! write block sizes to data 

    ALLOCATE(nctmp_real(1, max_x, max_y))
    ALLOCATE(nctmp_double(1, max_x, max_y))

    DO iblock = 1, max_blocks

       index(1) = iblock

       ncstat = nf_put_var1_int (plot_ncid, etamax_varid, index, block(iblock)%xmax + 1)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       ncstat = nf_put_var1_int (plot_ncid, ximax_varid, index, block(iblock)%ymax + 1)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       index(1) = iblock
       index(2) = 1
       index(3) = 1
       len(1) = 1
       len(2) = max_x
       len(3) = max_y

                                ! grid coordinates: x

       nctmp_double = nf_fill_double
       nctmp_double(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%x(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_double(plot_ncid, x_varid, index, len, nctmp_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! grid coordinates: x

       nctmp_double = nf_fill_double
       nctmp_double(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%y(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_double(plot_ncid, y_varid, index, len, nctmp_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! bottom elevation

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%zbot(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, zbot_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%hp1(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, hp1_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%hp2(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, hp2_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%gp12(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, gp12_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    END DO

  END SUBROUTINE plot_file_setup_netcdf

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_netcdf(date_string, time_string, salinity, baro_press)

    USE globals
    USE scalars
    USE date_time
    USE gas_functions

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8) :: time_string
    CHARACTER (LEN=tslen) :: timestamp
    DOUBLE PRECISION :: salinity, baro_press
    INTEGER :: ncstat
    INTEGER :: start(4), length(4)
    INTEGER :: iblock, i, j, max_x, max_y, trec, max_blkx, max_blky
    DOUBLE PRECISION :: conc_TDG, t_water

    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1

    ncstat = nf_inq_dimlen(plot_ncid, time_dimid, trec)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! do the time stamp

    timestamp(1:10) = date_string
    timestamp(11:11) = ' '
    timestamp(12:19) = time_string
    timestamp(tslen:tslen) = CHAR(0)

    start(1) = 1
    start(2) = trec + 1
    length(1) = tslen
    length(2) = 1
    ncstat = nf_put_vara_text(plot_ncid, ts_varid, start, length, timestamp)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    start(1) = trec + 1
    ncstat = nf_put_var1_double(plot_ncid, time_varid, start, &
         &date_to_decimal(date_string, time_string))
    
    start(2) = 1
    start(3) = 1
    start(4) = trec + 1

    length(1) = 1
    length(2) = max_x
    length(3) = max_y
    length(4) = 1

    DO iblock = 1, max_blocks

       max_blkx = block(iblock)%xmax + 1
       max_blky = block(iblock)%xmax + 1

       start(1) = iblock

                                ! u

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%uvel_p(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, u_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       
                                ! v

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%vvel_p(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, v_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! u_cart

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%u_cart(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, ucart_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       
                                ! v_cart

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%v_cart(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, vcart_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! depth

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%depth(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, depth_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! wsel

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%wsel(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, wsel_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! vmag

       nctmp_real = nf_fill_real
       DO i = 1, block(iblock)%xmax + 1
          DO j = 1, block(iblock)%ymax + 1
             nctmp_real(1, i, j) = &
                  &SQRT(block(iblock)%vvel_p(i,j)*block(iblock)%vvel_p(i,j) + &
                  &block(iblock)%uvel_p(i,j)*block(iblock)%uvel_p(i,j))
          END DO
       END DO
       ncstat = nf_put_vara_real(plot_ncid, vmag_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! Froude number

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%froude_num(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, froude_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! Courant number

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%courant_num(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, courant_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! water quality variables

                                ! TDG conc

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &species(1)%scalar(iblock)%conc(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, conc_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! temp
       
       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &species(2)%scalar(iblock)%conc(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, temp_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! computed: TDG pressure

       nctmp_real = nf_fill_real
       DO i = 1, block(iblock)%xmax + 1
          DO j = 1, block(iblock)%ymax + 1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             nctmp_real(1, i, j) = TDGasPress(conc_TDG, t_water, salinity)
          END DO
       END DO
       ncstat = nf_put_vara_real(plot_ncid, press_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! computed: TDG delta P

       nctmp_real = nf_fill_real
       DO i = 1, block(iblock)%xmax + 1
          DO j = 1, block(iblock)%ymax + 1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             nctmp_real(1, i, j) = TDGasDP(conc_TDG, t_water, salinity, baro_press)
          END DO
       END DO
       ncstat = nf_put_vara_real(plot_ncid, dp_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! computed: TDG delta P

       nctmp_real = nf_fill_real
       DO i = 1, block(iblock)%xmax + 1
          DO j = 1, block(iblock)%ymax + 1
             conc_TDG = species(1)%scalar(iblock)%conc(i,j)
             t_water = species(2)%scalar(iblock)%conc(i,j)
             nctmp_real(1, i, j) = TDGasSaturation(conc_TDG, t_water, salinity, baro_press)
          END DO
       END DO
       ncstat = nf_put_vara_real(plot_ncid, sat_varid, start, length, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    END DO

    ncstat = nf_sync(plot_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

  END SUBROUTINE plot_print_netcdf

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup()

    IMPLICIT NONE

    IF (plot_do_tecplot) &
         &CALL plot_file_setup_tecplot()
    IF (plot_do_netcdf) &
         &CALL plot_file_setup_netcdf()

  END SUBROUTINE plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print(date_string, time_string, salinity, baro_press)
    
    IMPLICIT NONE

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8) :: time_string
    DOUBLE PRECISION :: delta_t, salinity, baro_press

    CALL plot_fix_velocity()

    IF (plot_do_tecplot) &
         &CALL plot_print_tecplot(date_string, time_string, salinity, baro_press)
    IF (plot_do_netcdf) &
         &CALL plot_print_netcdf(date_string, time_string, salinity, baro_press)

  END SUBROUTINE plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_close()

    IMPLICIT NONE

    INTEGER :: ncstat

    INCLUDE 'netcdf.inc'

    IF (plot_do_netcdf) THEN
       ncstat = nf_close(plot_ncid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       DEALLOCATE(nctmp_real)
    ELSE
       CLOSE(plot_iounit)
    END IF

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

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8) :: time_string
    DOUBLE PRECISION :: delta_t
    INTEGER :: i, iblock

    DO iblock = 1, max_blocks

       block(iblock)%froude_num = &
            &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
            &SQRT(grav*block(iblock)%depth)

       block(iblock)%courant_num = &
            &delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
            &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
            &SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)

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
