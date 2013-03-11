!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: MASS2
!
! VERSION and DATE:  0.24 07-29-98
!
! PURPOSE: flow solver for 2D depth-averaged flow and scalar transport
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:   
!
! MOD HISTORY:	7-2000 0.26 version - starting to spilt this monster up
!
!***************************************************************
!

MODULE mass2_main_025

!-------------------------------------------------------------------------------------------------------

USE globals
USE io_routines_module
USE block_boundary_conditions
USE table_boundary_conditions
USE date_time
USE gage_output
USE plot_output
USE block_flux_output
USE scalars
USE scalars_source
USE met_zone
USE energy_flux
USE gas_functions

USE misc_vars
USE transport_only
USE bed_module
USE scalar_mass
USE julian
USE solver_module

!-------------------------------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"


CHARACTER (LEN=80), PARAMETER :: config_file_name = 'mass2_v027.cfg'

!---------------------------------------------------------------------------------
! derived type (structures) declarations

! moved to 
! TYPE(datetime_struct), SAVE :: start_time, end_time, current_time


!!$DOUBLE PRECISION, ALLOCATABLE :: ap(:,:), aw(:,:), ae(:,:), as(:,:), an(:,:), bp(:,:)
!!$DOUBLE PRECISION, ALLOCATABLE :: cp(:,:), cw(:,:), ce(:,:), cs(:,:), cn(:,:), source(:,:)

CONTAINS

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------

SUBROUTINE start_up()

  IMPLICIT NONE

  INTEGER :: imax, jmax
  INTEGER :: i, j 

  CALL read_grid(max_blocks, grid_file_name, readpts=.FALSE.)

  DO i=1,max_blocks
     CALL allocate_block_components(i, status_iounit)
  END DO

  imax = MAXVAL(block(:)%xmax) + 1
  jmax = MAXVAL(block(:)%ymax) + 1

                                ! TEMPORARY: allocate space for hydro
                                ! solution coefficients

!!$  ALLOCATE(ap(imax,jmax),aw(imax,jmax),ae(imax,jmax),as(imax,jmax),an(imax,jmax),bp(imax,jmax))
!!$  ALLOCATE(cp(imax,jmax),cw(imax,jmax),ce(imax,jmax),cs(imax,jmax),cn(imax,jmax),source(imax,jmax))


  ! allocate scalar species stuff

  IF (do_transport) THEN

     CALL allocate_species()

     DO i = 1, max_species
        CALL allocate_scalar(max_blocks, i)
     END DO

     DO i=1, max_species
        DO j =1, max_blocks
           CALL allocate_scalarblock_components(i, j , block(j)%xmax, block(j)%ymax)
        END DO
     END DO
  END IF

  ! ALLOCATE(work(imax,jmax))
  ALLOCATE(inlet_area(MAX(imax,jmax)), table_input(MAX(imax,jmax)))

  !-------------------------------------------------------------------------------
  ! read in the grid files for each block
  CALL read_grid(max_blocks, grid_file_name, readpts=.TRUE.)

  CALL bc_init()

  CALL initialize()

  CALL output_init()

END SUBROUTINE start_up


! ----------------------------------------------------------------
! SUBROUTINE initialize
! ----------------------------------------------------------------
SUBROUTINE initialize()

  IMPLICIT NONE

  INTEGER :: i, iblock, var

  !-----------------------------------------------------------------------------------------------------------
  ! assign initial conditions for each block
  ! using uniform values from cfg file or a restart file

  IF(read_hotstart_file)THEN
     CALL read_hotstart()
  ELSE
     WRITE(status_iounit,*)'-- setting initial values for all blocks'
     DO iblock=1,max_blocks
        block(iblock)%uvel = uvel_initial
        block(iblock)%uold = block(iblock)%uvel
        block(iblock)%uoldold = block(iblock)%uvel
        block(iblock)%ustar = block(iblock)%uvel
        block(iblock)%vvel = vvel_initial
        block(iblock)%vold = block(iblock)%vvel
        block(iblock)%voldold = block(iblock)%vvel
        block(iblock)%vstar = block(iblock)%vvel
        
        IF (.NOT. read_initial_profile) THEN
           IF(given_initial_wsel)THEN
              block(iblock)%depth = wsel_or_depth_initial - block(iblock)%zbot
           ELSE
              block(iblock)%depth = wsel_or_depth_initial
           ENDIF
           IF (do_wetdry) THEN
              WHERE (block(iblock)%depth .LE. dry_zero_depth) 
                 block(iblock)%depth = dry_zero_depth
              END WHERE
           END IF
           block(iblock)%depthold = block(iblock)%depth
           block(iblock)%deptholdold = block(iblock)%depth
           block(iblock)%dstar = block(iblock)%depth
        END IF
     END DO
     IF (do_transport) THEN
        DO i = 1, max_species
           DO iblock =1, max_blocks
              species(i)%scalar(iblock)%conc = conc_initial
              species(i)%scalar(iblock)%concold = conc_initial
              species(i)%scalar(iblock)%concoldold = conc_initial
              species(i)%scalar(iblock)%srcconc = conc_initial
           END DO
        END DO
     END IF
     
     ! handle case if we are doing transport-only and not restarting
     IF((.NOT. do_flow).AND.(do_transport))THEN
        CALL hydro_restart_read(current_time%time)
        DO iblock=1,max_blocks
           var = 1
           CALL hydro_restart_interp(current_time%time, iblock, var, block(iblock)%uvel)
           block(iblock)%uold = block(iblock)%uvel
           block(iblock)%ustar = block(iblock)%uvel
           
           var = 2
           CALL hydro_restart_interp(current_time%time, iblock, var, block(iblock)%vvel)
           block(iblock)%vold = block(iblock)%vvel
           block(iblock)%vstar = block(iblock)%vvel
           
           var = 3
           CALL hydro_restart_interp(current_time%time, iblock, var, block(iblock)%depth)
           block(iblock)%depthold = block(iblock)%depth
           block(iblock)%dstar = block(iblock)%depth
        END DO
        WRITE(status_iounit,*)'done setting transport-only initial values for all blocks'
     END IF
     
     WRITE(status_iounit,*)'done setting initial values for all blocks'
  ENDIF

  !--------------------------------------------------------------------------------
  ! assign uniform parameter values and other derived stuff
  DO iblock=1,max_blocks
     block(iblock)%eddy = eddy_default
     block(iblock)%kx_diff = kx_diff_default
     block(iblock)%ky_diff = ky_diff_default
     block(iblock)%chezy = chezy_con_default
     block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot
  END DO

  ! overwrite the default assignments
  IF(do_spatial_eddy)THEN
     filename = "eddy_coeff.dat"
     CALL open_existing(filename, 50)
     DO WHILE(.TRUE.)
		READ(50,*,END=101)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%eddy(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
     END DO
101  CLOSE(50)
  ENDIF
  IF(do_spatial_kx)THEN
     filename = "kx_coeff.dat"
     CALL open_existing(filename, 50)
     DO WHILE(.TRUE.)
		READ(50,*,END=102)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%kx_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
     END DO
102  CLOSE(50)
  ENDIF
  IF(do_spatial_ky)THEN
     filename = "ky_coeff.dat"
     CALL open_existing(filename, 50)
     DO WHILE(.TRUE.)
		READ(50,*,END=103)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%ky_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
     END DO
103  CLOSE(50)
  ENDIF
  
  IF(do_spatial_chezy)THEN
     filename = "roughness_coeff.dat"
     CALL open_existing(filename, 50)
     DO WHILE(.TRUE.)
		READ(50,*,END=100)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%chezy(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
     END DO
100  CLOSE(50)
  ENDIF
  
  IF (read_initial_profile) THEN
     CALL profile_init(given_initial_wsel, manning, SQRT(mann_con))
  END IF

  IF (do_wetdry) THEN
     DO iblock=1,max_blocks
        CALL check_wetdry(block(iblock))
     END DO
  END IF

  IF (do_calc_eddy) THEN
     DO iblock=1,max_blocks
        CALL bedshear(block(iblock))
        CALL calc_eddy_viscosity(block(iblock))
     END DO
  END IF

END SUBROUTINE initialize


! ----------------------------------------------------------------
! SUBROUTINE bc_init
! ----------------------------------------------------------------
SUBROUTINE bc_init()

  IMPLICIT NONE

  INTEGER :: iblock, i

  !-------------------------------------------------------------------------------
  ! read, allocate, and set up block and table boundary conditions

  ! read hydrodynamics related stuff
  CALL allocate_block_bc(max_blocks)

  CALL read_bcspecs(bcspec_iounit, max_blocks, block%xmax, block%ymax)

  CALL read_bc_tables

  ! now decipher which cells talk to each other in the block connections
  IF(max_blocks > 1)THEN
     CALL set_block_connections(max_blocks, error_iounit, status_iounit)
  END IF

  !---------------------------------------------------------------------------------
  IF(do_transport)THEN
     ! read species related stuff
     CALL allocate_scalar_block_bc(max_blocks)
     CALL read_scalar_bcspecs(bcspec_iounit, max_blocks, max_species, &
          &block%xmax, block%ymax)
     CALL read_scalar_bc_tables()
     CALL set_scalar_block_connections(max_blocks, max_species)
     CALL scalar_source_read()
     IF (source_doing_sed) CALL bed_initialize()
     CALL scalar_mass_init()

     ! transport only mode
     IF(.NOT. do_flow)THEN
        CALL allocate_hydro_interp_blocks()
        DO i=1,max_blocks
           CALL allocate_hydro_interp_comp(i, status_iounit)
        END DO
        CALL read_transport_only_dat()
        CALL check_transport_only_dat(start_time%time,end_time%time)
     END IF

  END IF

  ! read in met data from a file
  IF (source_need_met) THEN
     CALL met_zone_read_specs(weather_filename)
     CALL met_zone_update(current_time%time)
     CALL met_zone_summary(status_iounit)
  END IF

  DO iblock=1,max_blocks
     CALL buildghost(iblock)
     CALL interp_grid(block(iblock))
     CALL metrics(block(iblock))
  END DO

END SUBROUTINE bc_init


! ----------------------------------------------------------------
! SUBROUTINE output_init
! ----------------------------------------------------------------
SUBROUTINE output_init()

  IMPLICIT NONE

  INTEGER :: system_time(8)

  DOUBLE PRECISION :: baro_press

  IF (ALLOCATED(met_zones)) THEN
     baro_press = met_zones(1)%current(MET_BARO)
  ELSE 
     baro_press = 760.0
  END IF

  !------------------------------------------------------------------------------------
  ! set up the gage print files
  IF(do_gage_print) THEN
     CALL gage_file_setup(do_transport,error_iounit, status_iounit)
     CALL mass_file_setup()
     IF (debug) CALL block_flux_setup()
  END IF


  !-----------------------------------------------------------------------------------------------------------
  ! print problem configuration information and initial variable values

  WRITE(output_iounit,*)"2D Depth-Averaged Flow Solver "
  WRITE(output_iounit,*)code_version
  WRITE(output_iounit,*)code_date

  CALL DATE_AND_TIME(VALUES = system_time)
  WRITE(output_iounit,2010)system_time(2),system_time(3),system_time(1),system_time(5),system_time(6),system_time(7)
  
  WRITE(output_iounit,*)"Total Number of Blocks = ",max_blocks
  WRITE(output_iounit,*)"Grids read from these files: "
  DO iblock=1,max_blocks
     WRITE(output_iounit,2000)grid_file_name(iblock)
  END DO
  WRITE(output_iounit,*)

  block_title(1:11) = ' for block '

  IF (debug) THEN
     DO iblock=1,max_blocks

        WRITE(block_title(12:15),'(i4)')iblock
        WRITE(output_iounit,*)'initial values for block number - ',iblock
        
        title = 'X grid values - state plane coord' 
        title(61:75) = block_title

        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%x_grid)

        title = 'Y grid values - state plane coord'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%y_grid)

        title = 'Bottom Elevation grid values - mean sea level'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%zbot_grid)

        title = 'Water Surface Elevation - mean sea level'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%wsel)

        title = "U Velocity (located at U staggered node)"
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%uvel)

        
        title = "V Velocity (located at V staggered node)"
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%vvel)

        title = 'Depth'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%depth)

        title = 'X c.v. node values - state plane coord'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%x)

        title = 'Y c.v. node values - state plane coord'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%y)

        title = 'Bottom Elevation c.v node values - mean sea level'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%zbot)

        title = 'h1 metric coeff at U location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hu1)

        title = 'h2 metric coeff at U location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hu2)

        title = 'h1 metric coeff at V location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hv1)

        title = 'h2 metric coeff at V location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hv2)

        title = 'h1 metric coeff at P location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hp1)

        title = 'h2 metric coeff at P location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%hp2)

        title = 'g12 nonorthogonal component of the metric tensor at P location'
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,&
             &i_index_min,block(iblock)%xmax + i_index_extra,&
             &j_index_min,block(iblock)%ymax + j_index_extra,&
             &block(iblock)%gp12)
        
     END DO
  END IF

  CALL gridplot('gridplot1.dat', DOGHOST=debug)

!!$  IF ( plot_do_tecplot ) THEN
!!$     OPEN(grid_iounit,file='gridplot-metrics.dat')
!!$     WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid Metrics"""
!!$     WRITE(grid_iounit,*)"variables=""x"" ""y"" ""hp1"" ""hp2"" ""gp12"""
!!$     DO iblk=1,max_blocks
!!$        WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblk,""""," i=", block(iblk)%xmax+1, " j= ",block(iblk)%ymax+1
!!$        WRITE(grid_iounit,*)block(iblk)%x
!!$        WRITE(grid_iounit,*)block(iblk)%y
!!$        WRITE(grid_iounit,*)block(iblk)%hp1
!!$        WRITE(grid_iounit,*)block(iblk)%hp2
!!$        WRITE(grid_iounit,*)block(iblk)%gp12
!!$     END DO
!!$     CLOSE(grid_iounit)
!!$  END IF
  
  !-----------------------------------------------------------------------------------------------------------
  ! write initial fields to the plot file

  CALL diag_plot_file_setup()
  CALL plot_file_setup()
  CALL accumulate(start_time%time)
  CALL plot_print(start_time%date_string, start_time%time_string, &
       &salinity, baro_press)

  IF (do_gage_print) THEN
     CALL gage_print(start_time%date_string, start_time%time_string,&
          &(start_time%time - start_time%time)*24, &
          &do_transport, salinity, baro_press)
  END IF

2000 FORMAT(a80)
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)

END SUBROUTINE output_init

! ----------------------------------------------------------------
! SUBROUTINE gridplot
! ----------------------------------------------------------------
SUBROUTINE gridplot(fname, doghost)

  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(IN) :: fname
  LOGICAL, INTENT(IN), OPTIONAL :: doghost

  LOGICAL :: mydoghost = .FALSE.
  INTEGER :: imin, imax, jmin, jmax, ni, nj, iblk

  IF (PRESENT(doghost)) mydoghost = doghost

  CALL open_new(fname, grid_iounit)

  WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
  WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""

  DO iblk=1,max_blocks

     IF (mydoghost) THEN
        imin = 1 - i_ghost
        imax = block(iblk)%xmax + i_ghost
        jmin = 1 - j_ghost
        jmax = block(iblk)%ymax + j_ghost
     ELSE
        imin = 1
        imax = block(iblk)%xmax
        jmin = 1
        jmax = block(iblk)%ymax
     END IF

     ni = imax - imin + 1
     nj = jmax - jmin + 1

     WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblk,""""," i=", ni, " j= ",nj
     WRITE(grid_iounit,'(8G16.8)')block(iblk)%x_grid(imin:imax,jmin:jmax)
     WRITE(grid_iounit,'(8G16.8)')block(iblk)%y_grid(imin:imax,jmin:jmax)
     WRITE(grid_iounit,'(8G16.8)')block(iblk)%zbot_grid(imin:imax,jmin:jmax)
  END DO

  CLOSE(grid_iounit)

END SUBROUTINE gridplot

! ----------------------------------------------------------------
! SUBROUTINE config_error_msg
! ----------------------------------------------------------------
SUBROUTINE config_error_msg(line, msg, fatal)

  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: line
  CHARACTER (LEN=*), INTENT(IN) :: msg
  LOGICAL, INTENT(IN), OPTIONAL :: fatal
  CHARACTER (LEN=1024) :: lbuffer, buffer

  LOGICAL :: die
  
  die = .FALSE.
  IF (PRESENT(fatal)) die = fatal

  WRITE (lbuffer, *) line
  WRITE (buffer, *) TRIM(config_file_name), ", line: ", &
       &TRIM(ADJUSTL(lbuffer)), ': ', TRIM(msg)
  CALL error_message(buffer, die)

END SUBROUTINE config_error_msg

! ----------------------------------------------------------------
! SUBROUTINE read_config
! ----------------------------------------------------------------
SUBROUTINE read_config()

  USE differencing

  IMPLICIT NONE

  INTEGER :: iblock, i, line
  INTEGER, PARAMETER :: cfg_iounit=10
  CHARACTER (LEN=1024) :: msg
  CHARACTER (LEN=80) :: diffbuf

  CALL open_existing(config_file_name, cfg_iounit)

  line = 1

  !-------------------------------------------------------------------------------------------------------
  READ(cfg_iounit,'(A80)', ERR=1000)config_file_version
  line = line + 1
  READ(cfg_iounit,'(A80)', ERR=1000) !sim_title
  line = line + 1

  !-------------------------------------------------------------------------------------------------------
  ! allocation of dynamic arrays
  READ(cfg_iounit,*, ERR=1000)max_blocks
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)max_species
  line = line + 1

  ALLOCATE(grid_file_name(max_blocks))

  DO iblock=1,max_blocks
     READ(cfg_iounit,*, ERR=1000)grid_file_name(iblock)
     line = line + 1
  END DO


  !---------------------------------------------------------------------------------------------------------
  ! finish reading cfg file
  READ(cfg_iounit,*, ERR=1000)do_flow				! on/off switch for hydrodynamics
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)do_transport	! on/off switch for transport calculations
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)do_surface_heatx, do_surface_gasx, weather_filename !*** on/off surface exhange, weather data file
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)debug					! extra debug printing
  line = line + 1
  
  READ(cfg_iounit,*, ERR=1000)manning							! switch between manning or chezy bottom friction equation
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)write_restart_file, restart_print_freq	! write out a binary restart file "hotstart_date_time.bin"
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)read_hotstart_file	! read in a binary hotstart file "hotstart.bin"
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)do_gage_print				! do gage print
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)given_initial_wsel, read_initial_profile	! if TRUE then wsel_or_depth_initial is the initial water elv.
  line = line + 1
																						! IF FALSE then wsel_or_depth_initial is the initial depth



  READ(cfg_iounit,*, ERR=1000)start_time%date_string, start_time%time_string	! model start date & time 
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)end_time%date_string, end_time%time_string			! model end date & time
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)delta_t 		! time step (seconds)
  line = line + 1


  READ(cfg_iounit,*, ERR=1000)number_hydro_iterations, max_mass_source_sum	!*** number of internal iterations at a fixed time level
  line = line + 1
																																! max summation of mass source at any iteration
  READ(cfg_iounit,*, ERR=1000)number_scalar_iterations !*** number of internal iterations for scalar solutions	
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)scalar_sweep	! internal iterations
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)depth_sweep		! depth correction internal iterations
  line = line + 1

  relax_eddy = 0.7
  READ(cfg_iounit,*, ERR=1000)eddy_default, do_spatial_eddy, do_calc_eddy, relax_eddy  ! turb eddy viscosity
  line = line + 1
  
  IF (relax_eddy .LE. 0.0 .OR. relax_eddy .GT. 1.0) THEN
     WRITE (msg, *) "eddy viscosity underrelaxation out of range (", relax_dp, ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END IF

  ! scalar diffusion coeff in xsi (ft^2/sec) ! scalar diffusion coeff in eta (ft^2/sec)
  READ(cfg_iounit,*, ERR=1000)kx_diff_default,do_spatial_kx
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)ky_diff_default,do_spatial_ky 
  line = line + 1
									
									! chezy constant if chezy-type relation is used (manning=FALSE)
  READ(cfg_iounit,*, ERR=1000)chezy_con_default, do_spatial_chezy		! manning n value if manning=TRUE
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)mann_con		! constant in the bed shear stress  =1.0 if metric units
  line = line + 1
  relax_uv = 1.0
  blend_uv = 1.0
  READ(cfg_iounit,*, ERR=1000)relax_dp	! relaxation factor for depth corrections
  line = line + 1

  IF (relax_dp .LE. 0.0 .OR. relax_dp .GT. 1.0) THEN
     WRITE (msg, *) "depth correction underrelaxation out of range (", relax_dp, ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END IF

  READ(cfg_iounit,*, ERR=1000)relax_uv    ! relaxation factor for u/v momentum
  line = line + 1

  IF (relax_uv .LE. 0.0 .OR. relax_uv .GT. 1.0) THEN
     WRITE (msg, *) "momentum underrelaxation out of range (", relax_uv, ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END IF


  ! blending for 2nd-order spatial differencing scheme (e.g. central difference)
  ! blending for 3-time level temporal differencing scheme

  READ(cfg_iounit,*, ERR=1000) diffbuf, blend_uv, blend_time
  line = line + 1

  diff_uv = differ_method_from_string(diffbuf)

  SELECT CASE (diff_uv) 
  CASE (DIFF_UPWIND)
     blend_uv = 0.0             ! ignore what ever was read
  CASE (0)
     WRITE (msg, *) "unknown differencing method (", TRIM(diffbuf) , ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END SELECT

  IF (nghost .LT. 2) THEN
     SELECT CASE (diff_uv)
     CASE (DIFF_SOU, DIFF_MSOU, DIFF_MUSCL)
        WRITE (msg, *) 'uv differencing method "', TRIM(diffbuf), &
             &'" unavailable - too few ghost cells'
        CALL config_error_msg(line, msg)
        GOTO 1000
     END SELECT
  END IF

  IF (blend_uv .LT. 0.0 .OR. blend_uv .GT. 1.0) THEN
     WRITE (msg, *) "momentum 2nd order blending factor out of range (", blend_uv, ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END IF
  
  IF (blend_time .LT. 0.0 .OR. blend_time .GT. 1.0) THEN
     WRITE (msg, *) "temporal 2nd order blending factor out of range (", blend_time, ")"
     CALL config_error_msg(line, msg)
     GOTO 1000
  END IF
  

  READ(cfg_iounit,*, ERR=1000)uvel_initial	! initial uniform value of u velocity
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)vvel_initial	! initial uniform value of v velocity
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)conc_initial	! initial uniform value of concentration
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)wsel_or_depth_initial	! initial uniform value of depth OR water surface elv.
  line = line + 1

  READ(cfg_iounit,*, ERR=1000)uvel_wind		! initial uniform value of u wind velocity
  line = line + 1
  READ(cfg_iounit,*, ERR=1000)vvel_wind		! initial uniform value of v wind velocity
  line = line + 1

                                ! wetting and drying parameters

  READ(cfg_iounit,*, ERR=1000)do_wetdry, dry_depth, dry_rewet_depth, dry_zero_depth
  line = line + 1

                                ! initial bed information

  READ(cfg_iounit,*, ERR=1000)bed_default_porosity, bed_initial_depth, read_bed_init
  line = line + 1

  READ(cfg_iounit,*, ERR=1000) print_freq, do_accumulate		! printout frequency every print_freq time steps
  line = line + 1
  READ(cfg_iounit,*, ERR=1000) plot_do_netcdf, do_flow_diag, do_flow_output	! NetCDF output flags
  line = line + 1
  READ(cfg_iounit,*, ERR=1000) plot_do_cgns, plot_cgns_docell, plot_cgns_dodesc, plot_cgns_maxtime ! CGNS output flags
  line = line + 1
  READ(cfg_iounit,*, ERR=1000) gage_print_freq
  line = line + 1

  CLOSE(cfg_iounit)

  !--------------------------------------------------------------------------------------------------------
  ! do some pre-processing of the config data

  mann_con = mann_con**2

  start_time%time = date_to_decimal(start_time%date_string,start_time%time_string)
  end_time%time = date_to_decimal(end_time%date_string,end_time%time_string)

  i = year(end_time%time)
  i = INT(LOG10(REAL(i)) + 1)
  IF (i .GT. 999) CALL date_time_flags(ydigits = i)

  RETURN

1000 CONTINUE

  WRITE (msg, *) 'read error'
  CALL config_error_msg(line, msg, .TRUE.)
  RETURN

END SUBROUTINE read_config

! ----------------------------------------------------------------
! SUBROUTINE read_grid
! ----------------------------------------------------------------
SUBROUTINE read_grid(nblocks, grid_file_name, readpts)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nblocks
  CHARACTER (LEN=*), INTENT(IN) :: grid_file_name(:)
  LOGICAL, INTENT(IN), OPTIONAL :: readpts
  INTEGER, PARAMETER :: grid_iounit=15
  LOGICAL :: dopts = .FALSE.
  INTEGER :: i, j, iblock, nx, ny, junk
  CHARACTER (LEN=1024) :: msg

  IF (PRESENT(readpts)) dopts = readpts

  DO iblock = 1, nblocks

     CALL open_existing(grid_file_name(iblock), grid_iounit)

     READ(grid_iounit,*) nx, ny
     IF (dopts) THEN

                                ! log what we are doing

        WRITE(msg, 100) iblock
        CALL status_message(msg)

        DO i = 1, nx
           DO j = 1, ny
              READ(grid_iounit,*) junk, junk,&
                   &block(iblock)%x_grid(i,j), block(iblock)%y_grid(i,j), &
                   &block(iblock)%zbot_grid(i,j)
           END DO
        END DO
        
        msg = ''
        WRITE(msg, 200) iblock
        CALL status_message(msg)

     ELSE
        block(iblock)%xmax = nx
        block(iblock)%ymax = ny
     END IF

     CLOSE(grid_iounit)

  END DO

100 FORMAT('reading in x,y,z for block n = ', I3)
200 FORMAT('completed reading x,y,z for block n = ', I3)

END SUBROUTINE read_grid

! ----------------------------------------------------------------
! SUBROUTINE buildghost
! ----------------------------------------------------------------
SUBROUTINE buildghost(iblock)
  
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: iblock
  INTEGER :: k, cells, con_cells, con_block, ifcells, jfcells
  INTEGER :: i, ibeg, iend, coni, conibeg, coniend, icoff
  INTEGER :: j, jbeg, jend, conj, conjbeg, conjend, jcoff
  INTEGER :: num_bc
  DOUBLE PRECISION :: fract

  CALL extrapghost(block(iblock))

                                ! copy ghost cell coordinates for those
                                ! cells connecting with another block

   
   DO num_bc = 1, block_bc(iblock)%num_bc

      IF (block_bc(iblock)%bc_spec(num_bc)%bc_type .EQ. "BLOCK") THEN

         con_block = block_bc(iblock)%bc_spec(num_bc)%con_block

         DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs

            SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
            CASE("US")
               ibeg = 0 - (nghost - 1)
               iend = ibeg + (nghost - 1)
               coniend = block(con_block)%xmax - 1
               conibeg = coniend - (nghost - 1)
               jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)
               jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k) + 1
               conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)
               conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k) + 1
               cells = jend - jbeg
               con_cells = conjend - conjbeg
               icoff = 0
               jcoff = 1
            CASE ("DS")
               ibeg = block(iblock)%xmax + 1
               iend = ibeg + (nghost - 1)
               conibeg = 2
               coniend = conibeg + (nghost - 1)
               jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)
               jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k) + 1
               conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)
               conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k) + 1
               cells = jend - jbeg
               con_cells = conjend - conjbeg
               icoff = 0
               jcoff = 1
            CASE ("RB") 
               jbeg = 0 - (nghost - 1)
               jend = jbeg + (nghost - 1)
               conjend = block(con_block)%ymax - 1
               conjbeg = conjend - (nghost - 1)
               ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)
               iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k) + 1
               conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)
               coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k) + 1
               cells = iend - ibeg
               con_cells = coniend - conibeg
               icoff = 1
               jcoff = 0
            CASE ("LB") 
               jbeg = block(iblock)%ymax + 1
               jend = jbeg + (nghost - 1)
               conjbeg = 2
               conjend = conjbeg + (nghost - 1)
               ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)
               iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k) + 1
               conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)
               coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k) + 1
               cells = iend - ibeg 
               con_cells = coniend - conibeg
               icoff = 1
               jcoff = 0
            END SELECT

                                ! fine cells per coarse cell

            SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
            CASE ("US", "DS")
               ifcells = 1
               IF (cells .GE. con_cells) THEN
                  jfcells = cells/con_cells
               ELSE 
                  jfcells = con_cells/cells
               END IF
            CASE ("LB", "RB")
               IF (cells .GE. con_cells) THEN
                  ifcells = cells/con_cells
               ELSE 
                  ifcells = con_cells/cells
               END IF
               jfcells = 1
            END SELECT

            IF (cells .EQ. con_cells) THEN

               ! if the number of cells is equal on both sides, we
               ! just copy the ghost cell corners from the connecting
               ! block

               coni = conibeg
               DO i = ibeg, iend
                  conj = conjbeg
                  DO j = jbeg, jend
                     block(iblock)%x_grid(i,j) = block(con_block)%x_grid(coni,conj)
                     block(iblock)%y_grid(i,j) = block(con_block)%y_grid(coni,conj)
                     block(iblock)%zbot_grid(i,j) = block(con_block)%zbot_grid(coni,conj)
                     ! block(iblock)%x_grid(i,j+1) = block(con_block)%x_grid(coni,conj+1)
                     ! block(iblock)%y_grid(i,j+1) = block(con_block)%y_grid(coni,conj+1)
                     ! block(iblock)%zbot_grid(i,j+1) = block(con_block)%zbot_grid(coni,conj+1)
                     conj = conj + 1
                  END DO
                  coni = coni + 1
               END DO

            ELSE IF (cells .GT. con_cells) THEN

               ! if this is the fine block, we need to interpolate
               ! ghost cell corners from the coarse cell corners

               DO i = ibeg, iend
                  coni = conibeg + (i - ibeg)/ifcells
                  DO j = jbeg, jend
                     conj = conjbeg + (j - jbeg)/jfcells

                     IF (ifcells .EQ. 1) THEN
                        fract = DBLE(MOD(j - jbeg, jfcells))
                        fract = fract/DBLE(jfcells)
                        icoff = 0
                        jcoff = 1
                     ELSE 
                        fract = DBLE(MOD(i - ibeg, ifcells))
                        fract = fract/DBLE(ifcells)
                        icoff = 1
                        jcoff = 0
                     END IF

                     CALL interpolate_point(fract,&
                          &block(con_block)%x_grid(coni,conj),&
                          &block(con_block)%y_grid(coni,conj),&
                          &block(con_block)%zbot_grid(coni,conj),&
                          &block(con_block)%x_grid(coni+icoff,conj+jcoff),&
                          &block(con_block)%y_grid(coni+icoff,conj+jcoff),&
                          &block(con_block)%zbot_grid(coni+icoff,conj+jcoff),&
                          &block(iblock)%x_grid(i,j), &
                          &block(iblock)%y_grid(i,j), &
                          &block(iblock)%zbot_grid(i,j))
                  
                  END DO
               END DO

            ELSE IF (cells .LT. con_cells) THEN

               ! if this is the coarse block, we copy selected fine
               ! block corners for the ghost cells

               DO i = ibeg, iend
                  coni = conibeg + (i - ibeg)*ifcells
                  DO j = jbeg, jend

                     conj = conjbeg + (j - jbeg)*jfcells
                     block(iblock)%x_grid(i,j) = block(con_block)%x_grid(coni,conj)
                     block(iblock)%y_grid(i,j) = block(con_block)%y_grid(coni,conj)
                     block(iblock)%zbot_grid(i,j) = block(con_block)%zbot_grid(coni,conj)
                     
                  END DO
               END DO
            END IF
         END DO
      END IF
   END DO

   CALL extrapghostcorners(block(iblock))

END SUBROUTINE buildghost

! ----------------------------------------------------------------
! SUBROUTINE extrapghost
! ----------------------------------------------------------------
SUBROUTINE extrapghost(blk)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(INOUT) :: blk
  INTEGER :: i, j, ig

  DO ig = 1, nghost
     i = 1 - ig
     DO j = 1, blk%ymax
        blk%x_grid(i,j) = blk%x_grid(i+1,j) - (blk%x_grid(i+2,j) - blk%x_grid(i+1,j))
        blk%y_grid(i,j) = blk%y_grid(i+1,j) - (blk%y_grid(i+2,j) - blk%y_grid(i+1,j))
        blk%zbot_grid(i,j) = blk%zbot_grid(i+1,j) - &
             &(blk%zbot_grid(i+2,j) - blk%zbot_grid(i+1,j))
        ! blk%zbot_grid(i,j) = blk%zbot_grid(i+1,j)
     END DO
     i = blk%xmax + ig
     DO j = 1, blk%ymax
        blk%x_grid(i,j) = blk%x_grid(i-1,j) - (blk%x_grid(i-2,j) - blk%x_grid(i-1,j))
        blk%y_grid(i,j) = blk%y_grid(i-1,j) - (blk%y_grid(i-2,j) - blk%y_grid(i-1,j))
        blk%zbot_grid(i,j) = blk%zbot_grid(i-1,j) - &
             &(blk%zbot_grid(i-2,j) - blk%zbot_grid(i-1,j))
        ! blk%zbot_grid(i,j) = blk%zbot_grid(i-1,j) 
     END DO
  END DO

  DO ig = 1, nghost
     j = 1 - ig
     DO i = 1, blk%xmax
        blk%x_grid(i,j) = blk%x_grid(i,j+1) - (blk%x_grid(i,j+2) - blk%x_grid(i,j+1))
        blk%y_grid(i,j) = blk%y_grid(i,j+1) - (blk%y_grid(i,j+2) - blk%y_grid(i,j+1))
        blk%zbot_grid(i,j) = blk%zbot_grid(i,j+1) - (blk%zbot_grid(i,j+2) - blk%zbot_grid(i,j+1))
     END DO
     j = blk%ymax + ig
     DO i = 1, blk%xmax
        blk%x_grid(i,j) = blk%x_grid(i,j-1) - (blk%x_grid(i,j-2) - blk%x_grid(i,j-1))
        blk%y_grid(i,j) = blk%y_grid(i,j-1) - (blk%y_grid(i,j-2) - blk%y_grid(i,j-1))
        blk%zbot_grid(i,j) = blk%zbot_grid(i,j-1) - &
             &(blk%zbot_grid(i,j-2) - blk%zbot_grid(i,j-1))
     END DO

  END DO

  CALL extrapghostcorners(blk)

END SUBROUTINE extrapghost

! ----------------------------------------------------------------
! SUBROUTINE extrapghostcorners
! ----------------------------------------------------------------
SUBROUTINE extrapghostcorners(blk)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(INOUT) :: blk
  INTEGER :: ig, jg

  DO ig = 1, nghost
     DO jg = 1, nghost
        CALL extrap_1_corner(blk, 1 - ig, 1 - jg, 1, 1)
        CALL extrap_1_corner(blk, 1 - ig, blk%ymax + jg, 1, -1)
        CALL extrap_1_corner(blk, blk%xmax + ig, 1 - jg, -1, 1)
        CALL extrap_1_corner(blk, blk%xmax + ig, blk%ymax + jg, -1, -1)
     END DO
  END DO
END SUBROUTINE extrapghostcorners

! ----------------------------------------------------------------
! SUBROUTINE extrap_1_corner
! ----------------------------------------------------------------
SUBROUTINE extrap_1_corner(blk, i, j, ioff, joff)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(INOUT) :: blk
  INTEGER, INTENT(IN) :: i, j, ioff, joff
  DOUBLE PRECISION :: mi, mj, bi, bj, zi, zj
  INTEGER :: i1, i2, j1, j2

  i1 = i + ioff
  i2 = i + 2*ioff
  j1 = j + joff
  j2 = j + 2*joff

  IF (blk%x_grid(i, j2) .NE. blk%x_grid(i, j1)) THEN
     mi = (blk%y_grid(i, j2) - blk%y_grid(i, j1))/(blk%x_grid(i, j2) - blk%x_grid(i, j1))
     bi = blk%y_grid(i, j2) - mi*blk%x_grid(i, j2)
  END IF

  IF (blk%x_grid(i2, j) .NE. blk%x_grid(i1, j)) THEN
     mj = (blk%y_grid(i2, j) - blk%y_grid(i1, j))/(blk%x_grid(i2, j) - blk%x_grid(i1, j))
     bj = blk%y_grid(i2, j) - mj*blk%x_grid(i2, j)
  END IF
     
  IF (blk%x_grid(i, j2) .EQ. blk%x_grid(i, j1)) THEN
     blk%x_grid(i, j) = blk%x_grid(i, j2)
     blk%y_grid(i, j) = mj*blk%x_grid(i, j) + bj
  ELSE IF (blk%x_grid(i2, j) .EQ. blk%x_grid(i1, j)) THEN
     blk%x_grid(i, j) = blk%x_grid(i2, j)
     blk%y_grid(i, j) = mi*blk%x_grid(i, j) + bi
  ELSE 
     blk%y_grid(i, j) = (bi - mi/mj*bj)*(mj/(mj - mi))
     blk%x_grid(i, j) = (blk%y_grid(i, j) - bj)/mj
  END IF

  zi = blk%zbot_grid(i,j1) - (blk%zbot_grid(i,j2) - blk%zbot_grid(i,j1))
  zj = blk%zbot_grid(i1,j) - (blk%zbot_grid(i2,j) - blk%zbot_grid(i1,j))

  blk%zbot_grid(i,j) = 0.5*(zi+zj)

END SUBROUTINE extrap_1_corner


! ----------------------------------------------------------------
! SUBROUTINE fillghost
! ----------------------------------------------------------------
SUBROUTINE fillghost(iblock)

  USE misc_vars, ONLY : nghost

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock
  INTEGER :: i, ibeg, iend, coni, conibeg, coniend
  INTEGER :: j, jbeg, jend, conj, conjbeg, conjend
  INTEGER :: k, con_block
  INTEGER :: ig, jg
  INTEGER :: num_bc, cells, concells, ifcells, jfcells

                                ! cell-centered quantities
                                ! for extrapolated cells, use
                                ! parameters from the neighboring real
                                ! cell (metrics are computed elsewhere)

  
  DO ig = 1, nghost
     block(iblock)%eddy(2-ig,:) = block(iblock)%eddy(2,:)
     block(iblock)%kx_diff(2-ig,:) = block(iblock)%kx_diff(2,:)
     block(iblock)%ky_diff(2-ig,:) = block(iblock)%ky_diff(2,:)
     block(iblock)%chezy(2-ig,:) = block(iblock)%chezy(2,:)
     block(iblock)%eddy(block(iblock)%xmax+ig,:) = block(iblock)%eddy(block(iblock)%xmax,:)
     block(iblock)%kx_diff(block(iblock)%xmax+ig,:) = block(iblock)%kx_diff(block(iblock)%xmax,:)
     block(iblock)%ky_diff(block(iblock)%xmax+ig,:) = block(iblock)%ky_diff(block(iblock)%xmax,:)
     block(iblock)%chezy(block(iblock)%xmax+ig,:) = block(iblock)%chezy(block(iblock)%xmax,:)
  END DO
  DO jg = 1, nghost 
     block(iblock)%eddy(:, 2-jg) = block(iblock)%eddy(:,2)
     block(iblock)%kx_diff(:, 2-jg) = block(iblock)%kx_diff(:,2)
     block(iblock)%ky_diff(:, 2-jg) = block(iblock)%ky_diff(:,2)
     block(iblock)%chezy(:, 2-jg) = block(iblock)%chezy(:,2)
     block(iblock)%eddy(:,block(iblock)%ymax+ig) = block(iblock)%eddy(:,block(iblock)%ymax)
     block(iblock)%kx_diff(:,block(iblock)%ymax+ig) = block(iblock)%kx_diff(:,block(iblock)%ymax)
     block(iblock)%ky_diff(:,block(iblock)%ymax+ig) = block(iblock)%ky_diff(:,block(iblock)%ymax)
     block(iblock)%chezy(:,block(iblock)%ymax+ig) = block(iblock)%chezy(:,block(iblock)%ymax)
   END DO
    
                                ! copy ghost cell metrics and
                                ! parameters from connecting block

   
   DO num_bc = 1, block_bc(iblock)%num_bc

      SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type) 
      CASE ("BLOCK")
         con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
         
         DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
            SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
            CASE("US")
               ibeg = 2 - nghost
               iend = ibeg + (nghost - 1)
               coniend = block(con_block)%xmax
               conibeg = coniend - (nghost - 1)
               jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
               jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
               cells = jend - jbeg + 1
               conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
               conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
               concells = conjend - conjbeg + 1
            CASE ("DS")
               iend = block(iblock)%xmax + nghost
               ibeg = iend - (nghost - 1)
               conibeg = 2
               coniend = conibeg + (nghost - 1)
               jbeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
               jend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
               cells = jend - jbeg + 1
               conjbeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
               conjend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
               concells = conjend - conjbeg + 1
            CASE ("RB")
               ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
               iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
               cells = iend - ibeg + 1
               conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
               coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
               concells = coniend - conibeg + 1
               jbeg = 2 - nghost
               jend = jbeg + (nghost - 1)
               conjend = block(con_block)%ymax + nghost
               conjbeg = conjend - (nghost - 1)
            CASE ("LB")
               ibeg = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1
               iend = block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
               cells = iend - ibeg + 1
               conibeg = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
               coniend = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(k)+1
               concells = coniend - conibeg + 1
               jbeg = block(iblock)%ymax + nghost
               jend = jbeg + (nghost - 1)
               conjbeg = 2 
               conjend = conjbeg + (nghost - 1)
            CASE DEFAULT
               ! FIXME: should never happen  need to crash here
            END SELECT

            SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
            CASE ("US", "DS")
               ifcells = 1
               IF (cells .GE. concells) THEN
                  jfcells = cells/concells
               ELSE 
                  jfcells = concells/cells
               END IF
            CASE ("LB", "RB")
               IF (cells .GE. concells) THEN
                  ifcells = cells/concells
               ELSE 
                  ifcells = concells/cells
               END IF
               jfcells = 1
            END SELECT


            IF (cells .EQ. concells) THEN

               coni = conibeg
               DO i = ibeg, iend
                  conj = conjbeg
                  DO j = jbeg, jend

                     block(iblock)%hp1(i,j) = block(con_block)%hp1(coni,conj)
                     block(iblock)%hp2(i,j) = block(con_block)%hp2(coni,conj)
                     block(iblock)%hv1(i,j-1) = block(con_block)%hv1(coni,conj-1)
                     block(iblock)%hv2(i,j-1) = block(con_block)%hv2(coni,conj-1)
                     block(iblock)%hv1(i,j) = block(con_block)%hv1(coni,conj)
                     block(iblock)%hv2(i,j) = block(con_block)%hv2(coni,conj)
                     
                     ! do not copy hu1,hu2 - they are calculated
                     
                     block(iblock)%gp12(i,j) = block(con_block)%gp12(coni,conj)
                     block(iblock)%gp12(i,j) = block(con_block)%gp12(coni,conj)
                     
                     block(iblock)%eddy(i,j) = block(con_block)%eddy(coni,conj)
                     block(iblock)%kx_diff(i,j) = block(con_block)%kx_diff(coni,conj)
                     block(iblock)%ky_diff(i,j) = block(con_block)%ky_diff(coni,conj)
                     block(iblock)%chezy(i,j) = block(con_block)%chezy(coni,conj)
                     conj = conj + 1
                  END DO
                  coni = coni + 1
               END DO

            ELSE 

               DO i = ibeg, iend
                  coni = conibeg + (i - ibeg)/ifcells
                  DO j = jbeg, jend
                     conj = conjbeg + (j - jbeg)/jfcells
                     ! do not copy metrics, they are calculated
                     block(iblock)%eddy(i,j) = block(con_block)%eddy(coni,conj)
                     block(iblock)%kx_diff(i,j) = block(con_block)%kx_diff(coni,conj)
                     block(iblock)%ky_diff(i,j) = block(con_block)%ky_diff(coni,conj)
                     block(iblock)%chezy(i,j) = block(con_block)%chezy(coni,conj)
                  END DO
               END DO

            END IF
         END DO
      END SELECT
   END DO

END SUBROUTINE fillghost

! ----------------------------------------------------------------
! SUBROUTINE interp_grid
! interpolate x_grid,y_grid,zbot_grid onto the c.v. points by simple
! averaging
! ----------------------------------------------------------------
SUBROUTINE interp_grid(blk)

  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra

  IMPLICIT NONE

  TYPE(block_struct), INTENT(INOUT) :: blk
  INTEGER :: i, j, ivec(4),jvec(4),ivec2(4),jvec2(4)

  DO i = i_index_min + 1, blk%xmax + i_index_extra - 1
     DO j = j_index_min + 1, blk%ymax + j_index_extra - 1
        blk%x(i,j) = 0.25*(blk%x_grid(i,j)+blk%x_grid(i-1,j)+&
             & blk%x_grid(i,j-1)+blk%x_grid(i-1,j-1))
        blk%y(i,j) = 0.25*(blk%y_grid(i,j)+blk%y_grid(i-1,j)+&
             & blk%y_grid(i,j-1)+blk%y_grid(i-1,j-1))
        blk%zbot(i,j) = 0.25*(blk%zbot_grid(i,j)+blk%zbot_grid(i-1,j)+&
             & blk%zbot_grid(i,j-1)+blk%zbot_grid(i-1,j-1))
     END DO
  END DO
  ! now take care of the edges of the grid
  ! remember that the c.v.'s have an extra i,j line than the grid
  i=i_index_min
  DO j= j_index_min + 1, blk%ymax + j_index_extra - 1
     blk%x(i,j) = 0.5*(blk%x_grid(i,j)+blk%x_grid(i,j-1))
     blk%y(i,j) = 0.5*(blk%y_grid(i,j)+blk%y_grid(i,j-1))
     blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j)+blk%zbot_grid(i,j-1))
  END DO
  
  i= blk%xmax+i_index_extra
  DO j= j_index_min + 1, blk%ymax + j_index_extra - 1
     blk%x(i,j) = 0.5*(blk%x_grid(i-1,j)+blk%x_grid(i-1,j-1))
     blk%y(i,j) = 0.5*(blk%y_grid(i-1,j)+blk%y_grid(i-1,j-1))
     blk%zbot(i,j) = 0.5*(blk%zbot_grid(i-1,j)+blk%zbot_grid(i-1,j-1))
  END DO
  
  j=j_index_min
  DO i = i_index_min + 1, blk%xmax + i_index_extra - 1
     blk%x(i,j) = 0.5*(blk%x_grid(i,j)+blk%x_grid(i-1,j))
     blk%y(i,j) = 0.5*(blk%y_grid(i,j)+blk%y_grid(i-1,j))
     blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j)+blk%zbot_grid(i-1,j))
  END DO
  
  j= blk%ymax+j_index_extra
  DO i = i_index_min + 1, blk%xmax + i_index_extra - 1
     blk%x(i,j) = 0.5*(blk%x_grid(i,j-1)+blk%x_grid(i-1,j-1))
     blk%y(i,j) = 0.5*(blk%y_grid(i,j-1)+blk%y_grid(i-1,j-1))
     blk%zbot(i,j) = 0.5*(blk%zbot_grid(i,j-1)+blk%zbot_grid(i-1,j-1))
  END DO
  
  ivec = (/i_index_min, i_index_min,&
       &blk%xmax + i_index_extra, blk%xmax+i_index_extra/)
  jvec = (/j_index_min, blk%ymax+j_index_extra,&
       &j_index_min,blk%ymax+j_index_extra/)
  ivec2 = (/i_index_min,i_index_min,&
       &blk%xmax+i_index_extra-1,blk%xmax+i_index_extra-1/)
  jvec2 = (/j_index_min,blk%ymax+j_index_extra-1,&
       &j_index_min,blk%ymax+j_index_extra-1/)
  blk%x(ivec,jvec) = blk%x_grid(ivec2,jvec2)
  blk%y(ivec,jvec) = blk%y_grid(ivec2,jvec2)
  blk%zbot(ivec,jvec) = blk%zbot_grid(ivec2,jvec2)

END SUBROUTINE interp_grid

! ----------------------------------------------------------------
! SUBROUTINE fillghost_hydro
! ----------------------------------------------------------------
SUBROUTINE fillghost_hydro(blk, cblk, bc)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(INOUT) :: blk
  TYPE (block_struct), INTENT(IN) :: cblk
  TYPE (bc_spec_struct), INTENT(IN) :: bc

  INTEGER :: n, cells, concells, ifcells, jfcells
  INTEGER :: i, j, iu, ju
  INTEGER :: conjbeg, conjend, conj, jbeg, jend
  INTEGER :: conibeg, coniend, coni, ibeg, iend
  LOGICAL :: side
  DOUBLE PRECISION :: carea, cflux
  CHARACTER (LEN=1024) :: msg
  
  DO n = 1, bc%num_cell_pairs
  
     SELECT CASE (bc%bc_loc)
     CASE ("US")
        iend = 1
        ibeg = iend - (nghost - 1)
        coniend = cblk%xmax
        conibeg = coniend - (nghost - 1)
        jbeg = bc%start_cell(n)+1
        jend = bc%end_cell(n)+1
        conjbeg = bc%con_start_cell(n)+1
        conjend = bc%con_end_cell(n)+1
        cells = jend - jbeg + 1
        concells = conjend - conjbeg + 1
     CASE ("DS")
        ibeg = blk%xmax + 1
        iend = ibeg + (nghost - 1)
        conibeg = 2
        coniend = conibeg + (nghost - 1)
        jbeg = bc%start_cell(n)+1
        jend = bc%end_cell(n)+1
        conjbeg = bc%con_start_cell(n)+1
        conjend = bc%con_end_cell(n)+1
        cells = jend - jbeg + 1
        concells = conjend - conjbeg + 1
     CASE ("RB")
        ibeg = bc%start_cell(n)+1
        iend = bc%end_cell(n)+1
        conibeg = bc%con_start_cell(n)+1
        coniend = bc%con_end_cell(n)+1
        jend = 1
        jbeg = jend - (nghost - 1)
        conjend = cblk%ymax
        conjbeg = conjend - (nghost - 1)
        cells = iend - ibeg + 1
        concells = coniend - conibeg + 1
     CASE ("LB")
        ibeg = bc%start_cell(n)+1
        iend = bc%end_cell(n)+1
        conibeg = bc%con_start_cell(n)+1
        coniend = bc%con_end_cell(n)+1
        jend = blk%ymax + nghost
        jbeg = jend - (nghost - 1)
        conjbeg = 2
        conjend = conjbeg + (nghost - 1)
        cells = iend - ibeg + 1
        concells = coniend - conibeg + 1
     CASE DEFAULT
        WRITE (msg, *) 'fillghost_hydro: invalid block side "', TRIM(bc%bc_loc), '"'
        CALL error_message(msg)
     END SELECT

     SELECT CASE (bc%bc_loc)
     CASE ("US", "DS")
        ifcells = 1
        IF (cells .GE. concells) THEN
           jfcells = cells/concells
        ELSE 
           jfcells = concells/cells
        END IF
        side = .FALSE.
     CASE ("LB", "RB")
        IF (cells .GE. concells) THEN
           ifcells = cells/concells
        ELSE 
           ifcells = concells/cells
        END IF
        jfcells = 1
        side = .TRUE.
     END SELECT

     IF (cells .EQ. concells) THEN

        ! if the same number of cells are on both sides of the
        ! boundary, just copy the state variables from the connected
        ! cells

        coni = conibeg
        DO i = ibeg, iend
           conj = conjbeg
           DO j = jbeg, jend
              blk%uvel(i,j) = cblk%uvel(coni, conj)
              blk%vvel(i,j) = cblk%vvel(coni, conj)
              blk%depth(i,j) = cblk%depth(coni, conj)
              blk%isdry(i,j) = cblk%isdry(coni, conj)
              blk%eddy(i,j) = cblk%eddy(coni, conj)
              conj = conj + 1
           END DO
           coni = coni + 1
        END DO
           
     ELSE IF (cells .GT. concells) THEN

        ! this block has more cells than the neighboring block, we
        ! need to interpolate some of the variables from the coarse
        ! block

                                ! copy the first cross vel value

        IF (.NOT. side) THEN
           blk%vvel(ibeg:iend,jbeg-1) = cblk%vvel(conibeg:coniend,conjbeg-1)
        ELSE 
           blk%uvel(ibeg-1,jbeg:jend) = cblk%uvel(conibeg-1,conjbeg:conjend)
        END IF

                                ! we need to do the depth first so
                                ! that flow area calculations are
                                ! accurate

        DO i = ibeg, iend
           coni = conibeg + (i - ibeg)/ifcells
           DO j = jbeg, jend
              conj = conjbeg + (j - jbeg)/jfcells
           
                                ! all the fine ghost cells are dry if
                                ! the neighboring coarse cell is dry

              blk%isdry(i,j) = cblk%isdry(coni,conj)

              blk%wsel(i,j) = wsinterp(cblk, blk%x(i,j), blk%y(i,j), coni, conj)
              blk%depth(i,j) = blk%wsel(i,j) - blk%zbot(i,j)
              IF (do_wetdry) THEN
                 blk%wsel(i,j) = MAX(blk%wsel(i,j), blk%zbot(i,j))
                 blk%depth(i,j) = MAX(blk%depth(i,j), dry_zero_depth)
                 blk%isdry(i,j) = blk%isdry(i,j) .OR. (blk%depth(i,j) .LE. dry_depth)
              END IF


                                ! linearly interpolate cross vel within the
                                ! neighboring coarse cell

              IF (.NOT. side) THEN
                 blk%vvel(i,j) = (cblk%vvel(coni, conj) - cblk%vvel(coni, conj-1))*&
                      &(DBLE(MOD(j - jbeg + 1, jfcells)))/DBLE(jfcells) + cblk%vvel(coni, conj-1)
              ELSE 
                 blk%uvel(i,j) = (cblk%uvel(coni, conj) - cblk%uvel(coni-1, conj))*&
                      &(DBLE(MOD(i - ibeg + 1, ifcells)))/DBLE(ifcells) + cblk%uvel(coni-1, conj)
              END IF
           END DO
        END DO

                                ! now we can calculate the total local
                                ! flow area and u
        
        IF (.NOT. side) THEN
           DO i = ibeg, iend
              coni = conibeg + (i - ibeg)/ifcells
              DO conj = conjbeg, conjend
                 IF (i .GT. 2) THEN
                    cflux = uflux(cblk, coni, conj, conj)
                 ELSE
                    cflux = uflux(cblk, coni, conj, conj)
                 END IF
                 ju = jbeg + (conj - conjbeg)*jfcells
                 carea = uarea(blk, i, ju, ju + jfcells - 1)
                 DO j = ju, ju + jfcells - 1
                    IF (carea .GT. 0.0) THEN
                       blk%uvel(i,j) = cflux/carea
                    ELSE 
                       blk%uvel(i,j) = 0.0
                    END IF
                    ! WRITE (*,101) i, j, coni, conj, cflux, carea
                    ! 101 FORMAT('In fillghost_hydro: ', 4(1X, I3), 2(1X, E15.6))
                 END DO
              END DO
           END DO
        ELSE
           DO j = jbeg, jend
              conj = conjbeg + (j - jbeg)/jfcells
              DO coni = conibeg, coniend
                 IF (j .GT. 2) THEN
                    cflux = vflux(cblk, coni, coni, conj-1)
                 ELSE
                    cflux = vflux(cblk, coni, coni, conj)
                 END IF
                 iu = ibeg + (coni - conibeg)*ifcells
                 carea = varea(blk, iu, iu  + ifcells - 1, j)
                 DO i = iu, iu + ifcells - 1
                    IF (carea .GT. 0.0) THEN
                       blk%vvel(i,j) = cflux/carea
                    ELSE 
                       blk%vvel(i,j) = 0.0
                    END IF
                 END DO
              END DO
           END DO
        END IF

     ELSE IF (cells .LT. concells) THEN

        ! this block is the coarse block; we should be able to copy
        ! most of what we need


                                ! this ghost cell is dry if all
                                ! neighboring cells are dry, so
                                ! initialize all here
        blk%isdry(ibeg:iend,jbeg:jend) = .FALSE.

                                ! copy the first cross vel value

        IF (.NOT. side) THEN
           blk%vvel(ibeg:iend, jbeg-1) = cblk%vvel(conibeg:coniend, conjbeg-1)
        ELSE
           blk%uvel(ibeg-1, jbeg:jend) = cblk%uvel(conibeg-1, conjbeg:conjend)
        END IF

        DO i = ibeg, iend
           coni = conibeg + (i - ibeg)*ifcells
           DO j = jbeg, jend
              conj = conjbeg + (j - jbeg)*jfcells

                                ! interpolate depth at the ghost cell
                                ! centroid, using the closest
                                ! neighboring cell as a hint

              blk%wsel(i,j) = wsinterp(cblk, blk%x(i,j), blk%y(i,j), &
                   &coni, conj + jfcells/2)
              blk%depth(i,j) = blk%wsel(i,j) - blk%zbot(i,j)
              IF (do_wetdry) THEN
                 blk%wsel(i,j) = MAX(blk%wsel(i,j), blk%zbot(i,j))
                 blk%depth(i,j) = MAX(blk%depth(i,j), dry_zero_depth)
              END IF

              IF (.NOT. side) THEN

                                ! copy next cross v value
                 blk%vvel(i,j) = cblk%vvel(coni, conj + jfcells - 1)

                                ! compute u using fluxes
                 IF (i .GT. 2) THEN
                    cflux = uflux(cblk, coni, conj, conj + jfcells - 1)
                 ELSE 
                    cflux = uflux(cblk, coni, conj, conj + jfcells - 1)
                 END IF
                 carea = uarea(blk, i, j, j)
                 IF (carea .GT. 0.0) THEN
                    blk%uvel(i,j) = cflux/carea
                 ELSE 
                    blk%uvel(i,j) = 0.0
                 END IF
                 
              ELSE 
                 blk%uvel(i,j) = cblk%uvel(coni + ifcells - 1, conj)

                 carea = varea(blk, i, i, j)
                 IF (jbeg .GT. 2) THEN
                    cflux = vflux(cblk, coni, coni + ifcells - 1, conj-1)
                 ELSE 
                    cflux = vflux(cblk, coni, coni + ifcells - 1, conj)
                 END IF
                 IF (carea .GT. 0.0) THEN
                    blk%vvel(i,j) = cflux/carea
                 ELSE 
                    blk%vvel(i,j) = 0.0
                 END IF
              END IF

                                ! check wetdry, all neighboring cells
                                ! must be dry for this cell to be dry
              blk%isdry(i,j) = (blk%isdry(i,j) .OR. cblk%isdry(coni, conj))

           END DO
        END DO
     END IF

     IF (.NOT. side) THEN
        blk%uold(ibeg:iend,jbeg:jend) = blk%uvel(ibeg:iend,jbeg:jend)
        blk%ustar(ibeg:iend,jbeg:jend) = blk%uvel(ibeg:iend,jbeg:jend)
        blk%vold(ibeg:iend,jbeg-1:jend) = blk%vvel(ibeg:iend,jbeg-1:jend)
        blk%vstar(ibeg:iend,jbeg-1:jend) = blk%vvel(ibeg:iend,jbeg-1:jend)
        IF (ibeg .EQ. 1) THEN
           blk%cell(ibeg+1,jbeg:jend)%xtype = CELL_NORMAL_TYPE
        ELSE
           blk%cell(ibeg-1,jbeg:jend)%xtype = CELL_NORMAL_TYPE
        END IF
     ELSE 
        blk%uold(ibeg-1:iend,jbeg:jend) = blk%uvel(ibeg-1:iend,jbeg:jend)
        blk%ustar(ibeg-1:iend,jbeg:jend) = blk%uvel(ibeg-1:iend,jbeg:jend)
        blk%vold(ibeg:iend,jbeg:jend) = blk%vvel(ibeg:iend,jbeg:jend)
        blk%vstar(ibeg:iend,jbeg:jend) = blk%vvel(ibeg:iend,jbeg:jend)
        IF (jbeg .EQ. 1) THEN
           blk%cell(ibeg:iend,jbeg+1)%ytype = CELL_NORMAL_TYPE
        ELSE
           blk%cell(ibeg:iend,jbeg-1)%ytype = CELL_NORMAL_TYPE
        END IF
     END IF
     blk%depthold(ibeg:iend,jbeg:jend) = blk%depth(ibeg:iend,jbeg:jend)
     blk%dstar(ibeg:iend,jbeg:jend) = blk%depth(ibeg:iend,jbeg:jend)
     blk%dp(ibeg:iend,jbeg:jend) = 0.0
  END DO


END SUBROUTINE fillghost_hydro

! ----------------------------------------------------------------
! SUBROUTINE fillghost_scalar
! ----------------------------------------------------------------
SUBROUTINE fillghost_scalar(blk, sclr, cblk, csclr, bc)

  IMPLICIT NONE

  TYPE (block_struct), INTENT(IN) :: blk, cblk
  TYPE (scalar_struct), INTENT(INOUT) :: sclr
  TYPE (scalar_struct), INTENT(IN) :: csclr
  TYPE (scalar_bc_spec_struct), INTENT(IN) :: bc

  INTEGER :: n, cells, concells, ifcells, jfcells
  INTEGER :: i, ibeg, iend, coni, conibeg, coniend
  INTEGER :: j, jbeg, jend, conj, conjbeg, conjend
  LOGICAL :: side
  
  DO n = 1, bc%num_cell_pairs

     side = .FALSE.

     SELECT CASE (bc%bc_loc)
     CASE ("US")
        ibeg = 2 - nghost
        iend = ibeg + (nghost - 1)
        coniend = cblk%xmax
        conibeg = coniend - (nghost - 1)
        jbeg = bc%start_cell(n)+1
        jend = bc%end_cell(n)+1
        conjbeg = bc%con_start_cell(n)+1
        conjend = bc%con_end_cell(n)+1
        cells = jend - jbeg + 1
        concells = conjend - conjbeg + 1
     CASE ("DS")
        iend = blk%xmax + nghost
        ibeg = iend - (nghost - 1)
        conibeg = 2
        coniend = conibeg + (nghost - 1)
        jbeg = bc%start_cell(n)+1
        jend = bc%end_cell(n)+1
        conjbeg = bc%con_start_cell(n)+1
        conjend = bc%con_end_cell(n)+1
        cells = jend - jbeg + 1
        concells = conjend - conjbeg + 1
     CASE ("RB")
        ibeg = bc%start_cell(n)+1
        iend = bc%end_cell(n)+1
        conibeg = bc%con_start_cell(n)+1
        coniend = bc%con_end_cell(n)+1
        jbeg = 2 - nghost
        jend = jbeg + (nghost - 1)
        conjend = cblk%ymax
        conjbeg = conjend - (nghost - 1)
        cells = iend - ibeg + 1
        concells = coniend - conibeg + 1
        side = .TRUE.
     CASE ("LB")
        ibeg = bc%start_cell(n)+1
        iend = bc%end_cell(n)+1
        conibeg = bc%con_start_cell(n)+1
        coniend = bc%con_end_cell(n)+1
        jend = blk%ymax + nghost
        jbeg = jend - (nghost - 1)
        conjbeg = 2
        conjend = conjbeg + (nghost - 1)
        cells = iend - ibeg + 1
        concells = coniend - conibeg + 1
        side = .TRUE.
     END SELECT

     SELECT CASE (bc%bc_loc)
     CASE ("US", "DS")
        ifcells = 1
        IF (cells .GE. concells) THEN
           jfcells = cells/concells
        ELSE 
           jfcells = concells/cells
        END IF
     CASE ("LB", "RB")
        IF (cells .GE. concells) THEN
           ifcells = cells/concells
        ELSE 
           ifcells = concells/cells
        END IF
        jfcells = 1
     END SELECT

     IF (cells .EQ. concells) THEN

        ! if the same number of cells are on both sides of the
        ! boundary, just copy the state variables from the connected
        ! cells

        coni = conibeg
        DO i = ibeg, iend
           conj = conjbeg
           DO j = jbeg, jend
              sclr%conc(i, j) = csclr%conc(coni, conj)
              conj = conj + 1
           END DO
           coni = coni + 1
        END DO
           
     ELSE IF (cells .GT. concells) THEN

        ! this block has more cells than the neighboring block, we
        ! need to interpolate the variables from the coarse block

        DO i = ibeg, iend
           coni = conibeg + (i - ibeg - 1)/ifcells
           DO j = jbeg, jend
              conj = conjbeg + (j - jbeg - 1)/jfcells

              sclr%conc(i,j) = scalar_interp(cblk, csclr, &
                   &blk%x(i,j), blk%y(i,j), coni, conj)

           END DO
        END DO

     ELSE IF (cells .LT. concells) THEN

        ! this block is the coarse block

        DO i = ibeg, iend
           coni = conibeg + (i - ibeg)*ifcells
           DO j = jbeg, jend
              conj = conjbeg + (j - jbeg)*jfcells

              sclr%conc(i,j) = scalar_interp(cblk, csclr, &
                   &blk%x(i,j), blk%y(i,j), coni + ifcells/2, conj + jfcells/2)
           END DO
        END DO
     END IF


     sclr%concold(ibeg:iend,jbeg:jend) = sclr%conc(ibeg:iend,jbeg:jend)

     SELECT CASE (bc%bc_loc)
     CASE ("US")
        sclr%cell(2,jbeg:jend)%xtype = SCALAR_NORMAL_TYPE
     CASE ("DS")
        sclr%cell(blk%xmax,jbeg:jend)%xtype = SCALAR_NORMAL_TYPE
     CASE ("RB")
        sclr%cell(ibeg:iend,2)%ytype = SCALAR_NORMAL_TYPE
     CASE ("LB")
        sclr%cell(ibeg:iend,blk%ymax)%ytype = SCALAR_NORMAL_TYPE
     END SELECT
  END DO


END SUBROUTINE fillghost_scalar

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION scalar_interp
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION scalar_interp(blk, sclr, x, y, ihint, jhint)

  IMPLICIT NONE

  DOUBLE PRECISION, EXTERNAL :: distance

  TYPE (block_struct), INTENT(IN) :: blk
  TYPE (scalar_struct), INTENT(IN) :: sclr
  DOUBLE PRECISION, INTENT(IN) :: x, y
  INTEGER, INTENT(IN) :: ihint, jhint

  INTEGER :: i, j, ibeg, iend, jbeg, jend
  DOUBLE PRECISION :: d, wtotal

  jbeg = jhint - 1
  jend = jhint + 1
  IF (jbeg .LT. 2) jbeg = 2
  IF (jend .GT. blk%ymax) jend = blk%ymax
  
  ibeg = ihint - 1
  iend = ihint + 1
  IF (ibeg .LT. 2) ibeg = 2
  IF (iend .GT. blk%xmax) iend = blk%xmax

  wtotal = 0.0
  scalar_interp = 0.0

  j = jhint

  DO i = ibeg, iend
     d = distance(x, y, blk%x(i,j), blk%y(i,j))
     IF (d .LT. 1.0d-10) THEN
        scalar_interp = sclr%conc(i,j)
        RETURN
     END IF
     wtotal = wtotal + 1.0/d
     scalar_interp = scalar_interp + sclr%conc(i,j)/d
  END DO

  i = ihint
  DO j = jbeg, jend
     IF (j .NE. jhint) THEN
        d = distance(x, y, blk%x(i,j), blk%y(i,j))
        IF (d .LT. 1.0d-10) THEN
           scalar_interp = sclr%conc(i,j)
           RETURN
        END IF
        wtotal = wtotal + 1.0/d
        scalar_interp = scalar_interp + sclr%conc(i,j)/d
     END IF
  END DO

  scalar_interp = scalar_interp / wtotal

END FUNCTION scalar_interp


!#################################################################################
!---------------------------------------------------------------------------------
!
! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
!
!---------------------------------------------------------------------------------
SUBROUTINE hydro(status_flag)

  IMPLICIT NONE
  
  INTEGER :: status_flag, x_beg, y_beg, num_bc, i, j
  LOGICAL :: alldry
  DOUBLE PRECISION :: apo

  ! Assign U,V,D BCs for this time
  ! set U boundary conditions for this time

  !----------------------------------------------------------------------------
  ! iteration loop at a fixed time using prescribed U (or Discharge),V,D BCs
  
  DO iteration = 1,number_hydro_iterations

     !**** BLOCK LOOP HERE *****
     DO iblock = 1,max_blocks 
        
        x_beg = 2
        y_beg = 2
        x_end = block(iblock)%xmax
        y_end = block(iblock)%ymax

        ds_flux_given = .FALSE. ! ignore special velocity/flux processing if not needed

        CALL default_hydro_bc(block(iblock))

        ! loop over the total number of bc specifications
        DO num_bc = 1, block_bc(iblock)%num_bc
           CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
                &.FALSE., ds_flux_given)
        END DO
        !-------------------------------------------------------------------------


                                ! Turn off cells that are dry, and
                                ! check to see if the entire block is
                                ! dry.  If it is, there is really no
                                ! point in doing these calculations.

        alldry = .FALSE.
        IF (do_wetdry) THEN
           alldry = .TRUE.
           DO i=1,x_end+1
              DO j=1,y_end+1
                 IF (block(iblock)%isdry(i,j)) THEN
                    block(iblock)%isdead(i  , j  )%p = .TRUE.
                    block(iblock)%isdead(i-1, j  )%u = .TRUE.
                    block(iblock)%isdead(i  , j  )%u = .TRUE.
                    block(iblock)%isdead(i  , j-1)%v = .TRUE.
                    block(iblock)%isdead(i  , j  )%v = .TRUE.
                 ELSE 
                    alldry = .FALSE.
                 END IF
              END DO
           END DO
        END IF
        
        !----------------------------------------------------------------------------
        ! U momentum  solution

        CALL uvel_solve(iblock, block(iblock), delta_t)

        ! end U momentum  solution
        !----------------------------------------------------------------------------
        
        
        !----------------------------------------------------------------------------
        ! V momentum  solution

        CALL vvel_solve(iblock, block(iblock), delta_t)

        ! end V momentum  solution
        !----------------------------------------------------------------------------
        
        ! update ustar, vstar if we have given velocity/flux conditions
        ! IF(ds_flux_given)THEN
        !    ! loop over the total number of bc specifications
        !    DO num_bc = 1, block_bc(iblock)%num_bc
        !       CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
        !            &.TRUE., ds_flux_given)
        !    END DO
        ! END IF
        
        !----------------------------------------------------------------------------
        ! solve depth correction equation
        
        CALL depth_solve(iblock, block(iblock), delta_t)

        !end depth correction solution
        !----------------------------------------------------------------------------
        
        IF(debug)THEN
           WRITE(output_iounit,*)"U* Velocity (before depth correction)"
           DO i=1,block(iblock)%xmax
              WRITE(output_iounit,1000)block(iblock)%ustar(i,:)
              WRITE(output_iounit,*)
           END DO

           WRITE(output_iounit,*)" V* Velocity (before depth correction)"
           DO i=1,block(iblock)%xmax
              WRITE(output_iounit,1000)block(iblock)%vstar(i,:)
              WRITE(output_iounit,*)
           END DO
        ENDIF
1000    FORMAT(50(f12.4,2x))
        
        !----------------------------------------------------------------------------
        ! Apply velocity corrections using the depth correction field

        CALL correct_velocity(block(iblock))
        
        ! end application of velocity correction
        !------------------------------------------------------------------------------------
        
        !-----------------------------------------------------------------------------
        ! apply zero gradient conditions
        
        ! zero gradient conditions on the sides of the block
        !		need to modify for side inflows/block connections
        
!!$        block(iblock)%uvel(1:x_end+1,1) = block(iblock)%ustar(1:x_end+1,2)
!!$        block(iblock)%uvel(1:x_end+1,y_end+1) = block(iblock)%ustar(1:x_end+1,y_end)
!!$        block(iblock)%vvel(1,1:y_end+1) = block(iblock)%vstar(2,1:y_end+1)
!!$        block(iblock)%vvel(x_end+1,1:y_end+1) = block(iblock)%vstar(x_end,1:y_end+1)
!!$        block(iblock)%depth(1:x_end+1,1) = (block(iblock)%depth(1:x_end+1,2) +&
!!$             &block(iblock)%zbot(1:x_end+1,2)) - block(iblock)%zbot(1:x_end+1,1)
!!$        block(iblock)%depth(1:x_end+1,y_end+1) = (block(iblock)%depth(1:x_end+1,y_end) +&
!!$             &block(iblock)%zbot(1:x_end+1,y_end)) - block(iblock)%zbot(1:x_end+1,y_end+1)
!!$        DO i=1,x_end+1
!!$           IF (do_wetdry) THEN
!!$              IF (block(iblock)%depth(i,1) .LT. dry_zero_depth) &
!!$                   &block(iblock)%depth(i,1)  = dry_zero_depth
!!$              IF (block(iblock)%depth(i,y_end+1) .LT. dry_zero_depth) &
!!$                   &block(iblock)%depth(i,y_end+1) = dry_zero_depth
!!$           END IF
!!$        END DO

        
        !----------------------------------------------------------------------------------------------
        ! check for small and negative depth condition and report location
        
        CALL depth_check(iblock, block(iblock), current_time%date_string, current_time%time_string)

        !----------------------------------------------------------------------------------------------
        ! return to momentum equation solution step using updated depth and velocities
        !----------------------------------------------------------------------------------------------

        IF (do_wetdry .AND. iterate_wetdry)&
             &CALL check_wetdry(block(iblock))
        
        ! Calculate bed shear stress here if it is needed for eddy
        ! viscosity calculation
        CALL bedshear(block(iblock))

        ! If specified, compute eddy viscosity
        IF (do_calc_eddy) THEN
           CALL calc_eddy_viscosity(block(iblock))
        END IF

     END DO		! block loop end
     
     !-----------------------------------------------------------------------------------
     ! check to see if mass source has been reduced below tolerance and break out of loop
     maxx_mass = 0
     DO iblock=1,max_blocks
        apo = SUM(ABS(block(iblock)%mass_source(2:block(iblock)%xmax, 2:block(iblock)%ymax)))
        IF(apo >= maxx_mass) maxx_mass = apo
     END DO
     IF(maxx_mass < max_mass_source_sum) EXIT ! break out of internal iteration loop
     
     !------------------------------------------------------------------------

  END DO    ! internal time iteration loop for momentum, depth correction equations
  
  IF (do_wetdry .AND. .NOT. iterate_wetdry) THEN
     DO iblock = 1, max_blocks
        CALL check_wetdry(block(iblock))
     END DO
  END IF

END SUBROUTINE hydro		

! ----------------------------------------------------------------
! SUBROUTINE default_hydro_bc
! ----------------------------------------------------------------
SUBROUTINE default_hydro_bc(blk)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  INTEGER :: x_beg, y_beg, x_end, y_end, i, j, ig

  x_beg = 2
  y_beg = 2
  x_end = blk%xmax
  y_end = blk%ymax

  blk%cell(x_beg,:)%xtype = CELL_BOUNDARY_TYPE
  blk%cell(x_beg,:)%xbctype = FLOWBC_NONE
  blk%cell(x_end,:)%xtype = CELL_BOUNDARY_TYPE
  blk%cell(x_end,:)%xbctype = FLOWBC_NONE
  blk%cell(:,y_beg)%ytype = CELL_BOUNDARY_TYPE
  blk%cell(:,y_beg)%ybctype = FLOWBC_NONE
  blk%cell(:,y_end)%ytype = CELL_BOUNDARY_TYPE
  blk%cell(:,y_end)%ybctype = FLOWBC_NONE

  DO ig = 1, nghost

     i = x_beg - ig
     blk%uvel(i,:) = 0.0
     blk%vvel(i,:) = blk%vvel(x_beg,:)
     CALL extrapolate_udepth(blk, i, y_beg, y_end, level=.FALSE.)
     blk%eddy(i,:) = blk%eddy(x_beg,:)

     i = blk%xmax + ig
     blk%uvel(i,:) = 0.0
     blk%vvel(i,:) = blk%vvel(x_end,:)
     CALL extrapolate_udepth(blk, i, y_beg, y_end, level=.FALSE.)
     blk%eddy(i,:) = blk%eddy(x_end,:)

     j = y_beg - ig
     blk%uvel(:,j) = blk%uvel(:,y_beg)
     blk%vvel(:,j) = 0.0
     CALL extrapolate_vdepth(blk, x_beg, x_end, j, level=.FALSE.)
     blk%eddy(:,j) = blk%eddy(:,y_beg)

     j = blk%ymax + ig
     blk%uvel(:,j) = blk%uvel(:,y_end)
     blk%vvel(:,j) = 0.0
     CALL extrapolate_vdepth(blk, x_beg, x_end, j, level=.FALSE.)
     blk%eddy(:,j) = blk%eddy(:,y_end)

     
  END DO

  blk%isdead(:,:)%u = .FALSE.
  blk%isdead(:,:)%v = .FALSE.
  blk%isdead(:,:)%p = .FALSE.
  blk%xsource = 0.0

END SUBROUTINE default_hydro_bc


! ----------------------------------------------------------------
! SUBROUTINE apply_hydro_bc
! ----------------------------------------------------------------
SUBROUTINE apply_hydro_bc(blk, bc, dsonly, ds_flux_given)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  TYPE (bc_spec_struct) :: bc
  LOGICAL, INTENT(IN) :: dsonly
  LOGICAL, INTENT(INOUT) :: ds_flux_given

  INTEGER :: x_end, y_end, i, j, k, jj, ii
  INTEGER :: i_beg, i_end, j_beg, j_end
  DOUBLE PRECISION :: input_total

  CHARACTER (LEN=1024) :: buf

  x_end = blk%xmax
  y_end = blk%ymax

  ! set default boundary conditions (zero flux)

  IF (.NOT. dsonly) ds_flux_given = .FALSE.


  ! Get boundary condition values from the table. 

  SELECT CASE(bc%bc_type)
  CASE("TABLE")

     SELECT CASE (bc%bc_kind)
     CASE ("ELEVELO")
        CALL table_interp(current_time%time, bc%table_num, table_input, &
             &2*bc%num_cell_pairs)
     CASE DEFAULT
        CALL table_interp(current_time%time, bc%table_num, table_input, &
             &bc%num_cell_pairs)
     END SELECT
  CASE ("SOURCE", "SINK")
     CALL table_interp(current_time%time,&
          & bc%table_num,&
          & table_input, 1)

  CASE ("BLOCK")

     CALL fillghost_hydro(blk, block(bc%con_block), bc)
     RETURN
     
  END SELECT

  ! Assign values to specified boundary

  SELECT CASE(bc%bc_loc)
              
  ! ----------------------------------------------------------------
  ! UPSTREAM (US)
  ! ----------------------------------------------------------------
  CASE("US")
     i=1
     SELECT CASE(bc%bc_type)
     CASE("TABLE")

        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level=.FALSE.)
              CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, input_total)
              DO jj = j_beg, j_end
                 IF (inlet_area(jj) .GT. 0.0) THEN
                    blk%uvel(i,jj) = table_input(j)/input_total
                    IF (blk%uvel(i,jj) .GT. 0) THEN
                       blk%vvel(i, jj-1) = 0.0
                       blk%vvel(i, jj) = 0.0
                    ELSE 
                       blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                       blk%vvel(i, jj) = blk%vvel(i+1, jj)
                    END IF
                 ELSE 
                    blk%uvel(i,jj) = 0.0
                    blk%vvel(i,jj-1) = blk%vvel(i+1, jj-1)
                    blk%vvel(i,jj) = blk%vvel(i+1, jj)
                 END IF
              END DO
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
              blk%vstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
              blk%vold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
              blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_FLOW
              IF (dsonly) blk%lud(i+1,j) = 0.0
           END DO
           
        CASE("VELO")
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level=.FALSE.)
              DO jj = j_beg, j_end
                 blk%uvel(i,jj) = table_input(j)
                 IF (blk%uvel(i,jj) .GT. 0.0) THEN
                    blk%vvel(i, jj-1) = 0.0
                    blk%vvel(i, jj) = 0.0
                 ELSE 
                    blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                    blk%vvel(i, jj) = blk%vvel(i+1, jj)
                 END IF
              END DO
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
              blk%vstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
              blk%vold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
              blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_VEL
              IF (dsonly) blk%lud(i+1,j) = 0.0
           END DO
           
        CASE("ELEV")
           IF (dsonly) RETURN
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%dp(i,jj) = 0.0
                 blk%depth(i,jj) = 2*table_input(j) - &
                      &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                 IF (do_wetdry) blk%depth(i,jj) =  &
                         &MAX(blk%depth(i,jj), dry_zero_depth)
              END DO
              blk%dstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%uvel(i,j_beg:j_end) = blk%uvel(i+1,j_beg:j_end)
              blk%vvel(i,j_beg-1:j_end) = blk%vvel(i+1,j_beg-1:j_end)
              blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_ELEV
           END DO
        CASE ("ELEVELO")
           IF (dsonly) RETURN
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%depth(i,jj) = 2*table_input(j*2-1) - &
                      &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                 blk%uvel(i,jj) = table_input(j*2)
                 IF (blk%uvel(i,jj) .GT. 0.0) THEN
                    blk%vvel(i, jj-1) = 0.0
                    blk%vvel(i,jj) = 0.0
                 ELSE
                    blk%vvel(i, jj-1) = blk%vvel(i+1, jj-1)
                    blk%vvel(i,jj) = blk%vvel(i+1,jj)
                 END IF
              END DO
              blk%dstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
              blk%vstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
              blk%vold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
              blk%cell(i+1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%xbctype = FLOWBC_BOTH
           END DO
        CASE DEFAULT
           GOTO 100
        END SELECT
     CASE ("ZEROG")
        GOTO 100
     CASE DEFAULT
        GOTO 100
     END SELECT
     
  ! ----------------------------------------------------------------
  ! DOWNSTREAM (DS)
  ! ----------------------------------------------------------------
  CASE("DS")
     i = x_end+1
     SELECT CASE(bc%bc_type)
     CASE("TABLE")

        SELECT CASE(bc%bc_kind)
           
           !--------------------------------------------------------------------------
           !*** reusing inlet_area and inlet_flow variables here for outlet conditions
           
        CASE("FLUX") ! can specify the outflow discharge; need to convert to velocity
           ds_flux_given = .TRUE.
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              
              ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
              CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, input_total)

              DO jj=j_beg, j_end
                 IF (inlet_area(jj) .GT. 0.0) THEN
                    blk%uvel(i,jj) =  table_input(j)/input_total
                 ELSE 
                    blk%uvel(i,jj) = 0.0
                 END IF
              END DO
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%vvel(i, j_beg:j_end) = blk%vvel(i-1, j_beg:j_end)
              blk%vstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
              blk%vold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
              blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_FLOW
              IF (dsonly) blk%lud(i-1,j_beg:j_end) = 0.0
           END DO

        CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
           ds_flux_given = .TRUE.
           
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end	 = bc%end_cell(j)+1
              ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
              blk%uvel(i,j_beg:j_end) = table_input(j)
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%vvel(i, j_beg-1:j_end) =  blk%vvel(i-1, j_beg-1:j_end)
              blk%vstar(i,j_beg-1:j_end) = blk%vvel(i,j_beg-1:j_end)
              blk%vold(i,j_beg-1:j_end)  = blk%vvel(i,j_beg-1:j_end)
              blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_VEL
              IF (dsonly) blk%lud(i-1,j_beg:j_end) = 0.0
           END DO
                    
        CASE("ELEV")
           IF (dsonly) RETURN
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end	 = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%dp(i,jj) = 0.0
                 blk%depth(i,jj) = 2*table_input(j) - &
                      &(blk%depth(i-1,jj) + blk%zbot(i-1,jj)) - blk%zbot(i,jj)
                 ! blk%depth(i,jj) = table_input(j) - blk%zbot(i,jj)
                 IF (do_wetdry) blk%depth(i,jj) =  &
                      &MAX(blk%depth(i,jj), dry_zero_depth)
              END DO
              blk%dstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%uvel(i,j_beg:j_end) = blk%uvel(i-1,j_beg:j_end)
              blk%vvel(i, j_beg-1:j_end) = blk%vvel(i-1, j_beg-1:j_end)
              blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_ELEV
           END DO
        CASE ("ELEVELO")
           GOTO 100
        CASE DEFAULT
           GOTO 100
        END SELECT

     CASE ("ZEROG")
        DO j=1,bc%num_cell_pairs
           j_beg = bc%start_cell(j)+1
           j_end = bc%end_cell(j)+1
           ! CALL extrapolate_udepth(blk, i, j_beg, j_end, level = .FALSE.)
           blk%uvel(i,j_beg:j_end) = blk%uvel(i-1,j_beg:j_end)
           blk%vvel(i, j_beg:j_end) = blk%vvel(i-1, j_beg:j_end)
           blk%cell(i-1,j_beg:j_end)%xtype = CELL_BOUNDARY_TYPE
           blk%cell(i-1,j_beg:j_end)%xbctype = FLOWBC_ZEROG
        END DO
     CASE DEFAULT
        GOTO 100
     END SELECT
     
  ! ----------------------------------------------------------------
  ! RIGHT BANK (RB)
  ! ----------------------------------------------------------------
  CASE("RB")
     j = 1
     SELECT CASE(bc%bc_type)
     CASE("TABLE")
        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end = bc%end_cell(k)+1
              ! CALL extrapolate_vdepth(blk, i_beg, i_end, j, level=.FALSE.)
              CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, input_total)
              DO ii = i_beg, i_end
                 IF (inlet_area(ii) .GT. 0.0) THEN
                    blk%vvel(ii,j) = table_input(k)/input_total
                    IF (blk%vvel(ii,j) .GT. 0.0) THEN
                       blk%uvel(ii,j) = 0.0
                    ELSE
                       blk%uvel(ii,j) =  blk%uvel(ii,j+1)
                    END IF
                 ELSE 
                    blk%vvel(ii,j) = 0.0
                    blk%uvel(ii,j) = blk%uvel(ii,j+1)
                 END IF
              END DO
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_FLOW
           END DO
        CASE("VELO")
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end = bc%end_cell(k)+1
              CALL extrapolate_vdepth(blk, i_beg, i_end, j, level = .FALSE.)
              DO ii = i_beg, i_end
                 blk%vvel(ii,j) = table_input(k)
                 IF (blk%vvel(ii,j) .GT. 0.0) THEN
                    blk%uvel(ii,j) = 0.0
                 ELSE 
                    blk%uvel(ii,j) = blk%uvel(ii,j+1)
                 END IF
              END DO
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_VEL
           END DO
        CASE("ELEV")
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end = bc%end_cell(k)+1
              DO ii = i_beg, i_end
                 blk%dp(ii,j) = 0.0
                 blk%depth(ii,j) = 2*table_input(k) - &
                      &(blk%depth(ii,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                 IF (do_wetdry) blk%depth(ii,j) =  &
                      &MAX(blk%depth(ii,j), dry_zero_depth)
              END DO
              blk%dstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j+1)
              blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j+1)
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_ELEV
           END DO
          
        CASE ("ELEVELO")
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end = bc%end_cell(k)+1
              DO ii = i_beg, i_end
                 blk%depth(ii,j) = 2*table_input(k*2-1) - &
                      &(blk%depth(i,j+1) + blk%zbot(ii,j+1)) - blk%zbot(ii,j)
                 blk%vvel(ii,j) = table_input(k*2)
              END DO
              blk%dstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j+1)
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j+1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j+1)%ybctype = FLOWBC_BOTH
           END DO
        CASE DEFAULT
           GOTO 100
        END SELECT

     CASE DEFAULT
        GOTO 100
     END SELECT
     
  ! ----------------------------------------------------------------
  ! LEFT BANK (LB)
  ! ----------------------------------------------------------------
  CASE("LB")
     j = y_end + 1
     SELECT CASE(bc%bc_type)
     CASE("TABLE")
        SELECT CASE(bc%bc_kind)

        CASE("FLUX")

           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end = bc%end_cell(k)+1
              CALL extrapolate_vdepth(blk, i_beg, i_end, j, level=.FALSE.)
              CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, input_total)
              DO ii = i_beg, i_end
                 IF (inlet_area(ii) .GT. 0.0) THEN
                    blk%vvel(ii,j) = table_input(k)/input_total
                    IF (blk%vvel(ii,j) .LT. 0.0) THEN
                       blk%uvel(ii,j) = 0.0
                    ELSE 
                       blk%uvel(ii,j) = blk%uvel(ii,j-1)
                    END IF
                 ELSE 
                    blk%vvel(ii,j) = 0.0
                 END IF
              END DO
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) =  blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_FLOW
           END DO

        CASE("VELO")
           
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end	 = bc%end_cell(k)+1
              CALL extrapolate_vdepth(blk, i_beg, j_end, j, level = .FALSE.)
              DO ii = i_beg, i_end
                 blk%vvel(ii, j) = table_input(k)
                 IF (blk%vvel(ii, j) .LT. 0.0) THEN
                    blk%uvel(ii, j) = 0.0
                 ELSE 
                    blk%uvel(ii, j) = blk%uvel(ii, j-1)
                 END IF
              END DO
              blk%ustar(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%uold(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j)
              blk%vstar(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j)
              blk%vold(i_beg:i_end,j)  = blk%vvel(i_beg:i_end,j)
              blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_VEL
           END DO

        CASE("ELEV")
           DO k=1,bc%num_cell_pairs
              i_beg = bc%start_cell(k)+1
              i_end	 = bc%end_cell(k)+1
              DO ii = i_beg, i_end
                 blk%dp(ii,j) = 0.0
                 blk%depth(ii,j) = 2*table_input(k) - &
                      &(blk%depth(ii,j-1) + blk%zbot(ii,j-1)) - blk%zbot(ii,j)
                 ! blk%depth(ii,j) = table_input(k) - blk%zbot(ii,j)
                 IF (do_wetdry) blk%depth(ii,j) =  &
                      &MAX(blk%depth(ii,j), dry_zero_depth)
              END DO
              blk%dstar(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%depthold(i_beg:i_end,j) = blk%depth(i_beg:i_end,j)
              blk%uvel(i_beg-1:i_end,j) = blk%uvel(i_beg-1:i_end,j-1)
              blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j-1)
              blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
              blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_ELEV
           END DO
        CASE DEFAULT
           GOTO 100
        END SELECT
     CASE ("ZEROG")
        DO k=1,bc%num_cell_pairs
           i_beg = bc%start_cell(j)+1
           i_end = bc%end_cell(j)+1
           CALL extrapolate_vdepth(blk, i_beg, i_end, j, level = .FALSE.)
           blk%uvel(i_beg:i_end,j) = blk%uvel(i_beg:i_end,j-1)
           blk%vvel(i_beg:i_end,j) = blk%vvel(i_beg:i_end,j-1)
           blk%cell(i_beg:i_end,j-1)%ytype = CELL_BOUNDARY_TYPE
           blk%cell(i_beg:i_end,j-1)%ybctype = FLOWBC_ZEROG
        END DO
     CASE DEFAULT
        GOTO 100
     END SELECT
              
  CASE ("IN") 
     IF (dsonly) RETURN
     SELECT CASE(bc%bc_type)
        
     CASE ("SOURCE", "SINK")
        
        ! if this is labeled as a "SINK"
        ! negate whatever is in the table
        
        SELECT  CASE(bc%bc_type)
        CASE ("SINK") 
           table_input = -table_input
        END SELECT
        
        ! an "ALL" extent applies to the
        ! entire block (it would be nice to do
        ! this when the bcspecs are read
        
        SELECT CASE (bc%bc_extent)
        CASE ("ALL")
           bc%start_cell(1) = 1
           bc%end_cell(1) = x_end - 1
           bc%start_cell(2) = 1
           bc%end_cell(2) = y_end - 1
        END SELECT
        
        SELECT CASE (bc%bc_kind)
        CASE("FLUX")
           
           ! if the source is a flux, we find the
           ! total area over which it applies
           
           input_total = 0.0
           DO i = bc%start_cell(1), bc%end_cell(1)
              DO j =  bc%start_cell(2), bc%end_cell(2)
                 IF (.NOT. blk%isdry(i+1,j+1)) &
                      &input_total = input_total + blk%hp1(i+1,j+1)*blk%hp2(i+1,j+1)
              END DO
           END DO
        CASE ("VELO")
           ! if a rate is specified, do not alter
           ! the table value
           input_total = 1.0
        CASE DEFAULT
           GOTO 100
        END SELECT
        
        DO i = bc%start_cell(1), bc%end_cell(1)
           DO j = bc%start_cell(2), bc%end_cell(2)
              IF (.NOT. blk%isdry(i+1,j+1)) THEN
                 blk%xsource(i+1, j+1) = table_input(1)/input_total
              END IF
           END DO
        END DO
        
        
     CASE ("WALL")
        SELECT CASE(bc%bc_kind)
           
           ! a UVEL wall blocks the longitudinal
           ! flow at the upstream edge of the
           ! specified cells.
        CASE ("UVEL")
           
           i = bc%con_block
           DO k = 1, bc%num_cell_pairs
              DO j = bc%start_cell(k),bc%end_cell(k)
                 blk%isdead(i,j+1)%u = .TRUE.
              END DO
           END DO
           
        CASE ("VVEL")
           
           j = bc%con_block + 1
           DO k = 1, bc%num_cell_pairs
              DO i = bc%start_cell(k),bc%end_cell(k)
                 blk%isdead(i+1,j)%v = .TRUE.
              END DO
           END DO
        CASE DEFAULT
           GOTO 100
        END SELECT
        
     CASE ("DEAD")
        
        DO i = bc%start_cell(1), bc%end_cell(1)
           DO j = bc%start_cell(2), bc%end_cell(2)
              blk%isdead(i+1, j+1)%p = .TRUE.
              blk%isdead(i  , j+1)%u = .TRUE.
              blk%isdead(i+1, j+1)%u = .TRUE.
              blk%isdead(i+1, j  )%v = .TRUE.
              blk%isdead(i+1, j+1)%v = .TRUE.
           END DO
        END DO
     CASE DEFAULT
        GOTO 100
     END SELECT
  CASE DEFAULT
     GOTO 100
  END SELECT

  RETURN
100 CONTINUE
  WRITE(buf,*) " apply_hydro_bc: cannot handle: ", &
       &TRIM(bc%bc_loc), " ", TRIM(bc%bc_type), " ", &
       &TRIM(bc%bc_kind), " "
  CALL error_message(buf, fatal=.TRUE.)
END SUBROUTINE apply_hydro_bc

! ----------------------------------------------------------------
! SUBROUTINE compute_uflow_area
! ----------------------------------------------------------------
SUBROUTINE compute_uflow_area(blk, i, jmin, jmax, area, total)

  IMPLICIT NONE
  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: i, jmin, jmax
  DOUBLE PRECISION, INTENT(OUT) :: area(:), total

  INTEGER :: ioff, j
  DOUBLE PRECISION :: d, w
  
  ioff = 1                      ! by default, do the upstream end.
  IF (i .GE. 2) ioff = -1

  area = 0.0
  DO j = jmin, jmax
     d = 0.5*(blk%depth(i,j) + blk%depth(i+ioff,j))
     w = blk%hu2(i,j)
     IF (do_wetdry .AND. blk%depth(i,j) .LE. dry_depth) THEN
        area(j) = 0.0
     ELSE 
        area(j) = d*w
     END IF
  END DO
  total = SUM(area)
END SUBROUTINE compute_uflow_area

! ----------------------------------------------------------------
! SUBROUTINE compute_vflow_area
! ----------------------------------------------------------------
SUBROUTINE compute_vflow_area(blk, imin, imax, j, area, total)

  IMPLICIT NONE
  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: imin, imax, j
  DOUBLE PRECISION, INTENT(OUT) :: area(:), total

  INTEGER :: joff, i
  DOUBLE PRECISION :: d, w
  
  joff = 1                      ! by default, do the right bank
  IF (j .GE. 2) joff = -1

  area = 0.0
  DO i = imin, imax
     w = blk%hv1(i,j)
     d = 0.5*(blk%depth(i,j) + blk%depth(i,j+joff))
     IF (do_wetdry .AND. d .LE. dry_depth) THEN
        area(i) = 0.0
     ELSE 
        area(i) = d*w
     END IF
  END DO
  total = SUM(area)
END SUBROUTINE compute_vflow_area

! ----------------------------------------------------------------
! SUBROUTINE extrapolate_udepth
! ----------------------------------------------------------------
SUBROUTINE extrapolate_udepth(blk, i, jmin, jmax, level)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: i, jmin, jmax
  LOGICAL, INTENT(IN), OPTIONAL :: level

  INTEGER :: ioff
  LOGICAL :: dolvl

  ioff = 1                      ! by default, do the upstream end.
  IF (i .GE. 2) ioff = -1

  dolvl = .TRUE.
  IF (PRESENT(level)) dolvl = level

  IF (dolvl) THEN
                                ! by level w.s. elevation
  
     blk%depth(i,jmin:jmax) = (blk%depth(i+ioff,jmin:jmax) +&
          & blk%zbot(i+ioff,jmin:jmax)) - blk%zbot(i,jmin:jmax)
  ELSE 
                                ! true linear extrapolation of w.s. elevation

     blk%depth(i,jmin:jmax) = (&
          &(blk%depth(i+ioff,jmin:jmax) + blk%zbot(i+ioff,jmin:jmax)) - &
          &(blk%depth(i+2*ioff,jmin:jmax) + blk%zbot(i+2*ioff,jmin:jmax)))*&
          &blk%hu1(i,jmin:jmax)/blk%hu1(i+ioff,jmin:jmax) + &
          &(blk%depth(i+ioff,jmin:jmax) + blk%zbot(i+ioff,jmin:jmax)) - &
          &blk%zbot(i,jmin:jmax)
  END IF

  IF (do_wetdry) THEN
     WHERE (blk%depth(i,jmin:jmax) .LT. dry_zero_depth) &
          &blk%depth(i,jmin:jmax) = dry_zero_depth
  END IF
END SUBROUTINE extrapolate_udepth

! ----------------------------------------------------------------
! SUBROUTINE extrapolate_vdepth
! ----------------------------------------------------------------
SUBROUTINE extrapolate_vdepth(blk, imin, imax, j, level)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: imin, imax, j
  LOGICAL, INTENT(IN), OPTIONAL :: level

  INTEGER :: joff
  LOGICAL :: dolvl

  joff = 1                      ! by default, do the upstream end.
  IF (j .GE. 2) joff = -1

  dolvl = .TRUE.
  IF (PRESENT(level)) dolvl = level

  IF (dolvl) THEN
                                ! by level w.s. elevation
  
     blk%depth(imin:imax,j) = (blk%depth(imin:imax,j+joff) +&
          & blk%zbot(imin:imax,j+joff)) - blk%zbot(imin:imax,j)
  ELSE 
                                ! true linear extrapolation of w.s. elevation

     blk%depth(imin:imax,j) = (&
          &(blk%depth(imin:imax,j+joff) + blk%zbot(imin:imax,j+joff)) - &
          &(blk%depth(imin:imax,j+2*joff) + blk%zbot(imin:imax,j+2*joff)))*&
          &blk%hv2(imin:imax,j)/blk%hv2(imin:imax,j+joff) + &
          &(blk%depth(imin:imax,j+joff) + blk%zbot(imin:imax,j+joff)) - &
          &blk%zbot(imin:imax,j)
  END IF

  IF (do_wetdry) THEN
     WHERE (blk%depth(imin:imax,j) .LT. dry_zero_depth) &
          &blk%depth(imin:imax,j) = dry_zero_depth
  END IF
END SUBROUTINE extrapolate_vdepth

! ----------------------------------------------------------------
! SUBROUTINE depth_check
! ----------------------------------------------------------------
SUBROUTINE depth_check(iblock, blk, date_str, time_str)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock
  TYPE (block_struct), INTENT(INOUT) :: blk
  CHARACTER (LEN=*), INTENT(IN) :: date_str, time_str

  INTEGER :: x_beg, x_end, y_beg, y_end, i, j

  x_beg = 2
  x_end = blk%xmax
  y_beg = 2
  y_end = blk%ymax

  DO i = x_beg, x_end
     DO j = y_beg, y_end

        IF (blk%depth(i,j) <= 0.0) THEN
           IF (do_wetdry) THEN
              WRITE(error_iounit,*)" ERROR: Negative Depth = ",blk%depth(i,j)
              WRITE(error_iounit,*)"     Simulation Time: ", date_str, " ", time_str
              WRITE(error_iounit,*)"     Block Number = ",iblock
              WRITE(error_iounit,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
              
              WRITE(*,*)" ERROR: Negative Depth = ",blk%depth(i,j)
              WRITE(*,*)"     Simulation Time: ", current_time%date_string, " ", current_time%time_string
              WRITE(*,*)"     Block Number = ",iblock
              WRITE(*,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
              
              blk%depth(i,j) = dry_zero_depth
           ELSE
              
              WRITE(error_iounit,*)" FATAL ERROR: Negative Depth = ",MINVAL(blk%depth)
              WRITE(error_iounit,*)"     Block Number = ",iblock
              WRITE(error_iounit,*)"     I,J Location of negative depth = ",MINLOC(blk%depth)
              
              WRITE(*,*)" FATAL ERROR: Negative Depth = ",MINVAL(blk%depth)
              WRITE(*,*)"     Block Number = ",iblock
              WRITE(*,*)"     I,J Location of negative depth = ",MINLOC(blk%depth)
              
              CALL EXIT(1)  ! abort run if you hit a negative depth
              
           END IF
        ELSE IF (.NOT. do_wetdry .AND. blk%depth(i,j) <= 0.20) THEN
           WRITE(error_iounit,*)" WARNING: Small Depth = ", blk%depth(i,j)
           WRITE(error_iounit,*)"     Simulation Time: ", date_str, " ", time_str
           WRITE(error_iounit,*)"     Block Number = ",iblock
           WRITE(error_iounit,*)"     I,J Location of small depth = ",i, j
        END IF
     END DO
  END DO

END SUBROUTINE depth_check


!################################################################################
!--------------------------------------------------------------------------------
! scalar transport solution
!--------------------------------------------------------------------------------
!


!################################################################################
!--------------------------------------------------------------------------------
! scalar transport solution
!--------------------------------------------------------------------------------
!
SUBROUTINE transport(status_flag)

  IMPLICIT NONE

  INTEGER :: status_flag, var, iter
  INTEGER :: ispecies, num_bc

  IF(.NOT. do_flow)THEN
     dum_val = current_time%time + delta_t/86400.0d0 ! velocity and depth are at the NEW time
     CALL hydro_restart_read(dum_val)
     DO iblock=1,max_blocks
        var = 1
        CALL hydro_restart_interp(dum_val, iblock, var, block(iblock)%uvel)
        
        var = 2
        CALL hydro_restart_interp(dum_val, iblock, var, block(iblock)%vvel)
        
        var = 3
        CALL hydro_restart_interp(dum_val, iblock, var, block(iblock)%depth)
     END DO
  END IF
  
  CALL scalar_source_timestep(current_time%time, delta_t)
  IF (source_doing_sed) CALL bed_dist_bedsrc(delta_t)
  DO iblock = 1,max_blocks
     CALL bedshear(block(iblock))
     CALL transport_precalc(block(iblock))
  END DO

  ! INTERNAL ITERATION AT THIS TIME LEVEL LOOP
  DO iter = 1,number_scalar_iterations
     
     ! BLOCK LOOP
     DO iblock = 1,max_blocks

        x_start = 2
        y_start = 2
        x_end = block(iblock)%xmax
        y_end = block(iblock)%ymax
        
        ! SPECIES LOOP - multiple numbers of scalar variables
        DO ispecies = 1, max_species

           CALL default_scalar_bc(block(iblock), species(ispecies)%scalar(iblock))

           ! WRITE(*,*) 'Transport: block = ', iblock, ', species = ', ispecies
           
           ! set boundary conditions for this time
           
           ! loop over the total number of bc specifications
           DO num_bc = 1, scalar_bc(iblock)%num_bc
              IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
                   &CALL apply_scalar_bc(block(iblock), &
                   &species(ispecies)%scalar(iblock), &
                   &scalar_bc(iblock)%bc_spec(num_bc), x_start, y_start)
           END DO ! num bc loop

           CALL apply_scalar_source(iblock, ispecies, x_start, y_start)

           CALL scalar_solve(iblock, block(iblock), species(ispecies)%scalar(iblock), &
                &scalar_source(ispecies)%relax, scalar_source(ispecies)%scheme, &
                &scalar_source(ispecies)%cds_blend, x_start, y_start)

        END DO ! species loop
        
     END DO ! block loop end

  END DO ! internal time loop end for concentration

! end scalar transport soultion
!----------------------------------------------------------------------------
END SUBROUTINE transport 


! ----------------------------------------------------------------
! SUBROUTINE apply_scalar_bc
! ----------------------------------------------------------------
SUBROUTINE apply_scalar_bc(blk, sclr, spec, xstart, ystart)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  TYPE (scalar_struct) :: sclr
  TYPE (scalar_bc_spec_struct) :: spec
  INTEGER, INTENT(INOUT) :: xstart, ystart

  INTEGER :: con_block, k
  INTEGER :: i, ii, i_beg, i_end, x_end
  INTEGER :: j, jj, j_beg, j_end, y_end
  DOUBLE PRECISION :: tmp

  CHARACTER (LEN=1024) :: buf
  
  SELECT CASE(spec%bc_type)
  CASE("BLOCK")
     con_block = spec%con_block
     CALL fillghost_scalar(blk, sclr, block(con_block), &
          &species(spec%species)%scalar(con_block), spec)
     RETURN
  CASE("TABLE")
     CALL scalar_table_interp(current_time%time, spec%table_num,&
          & table_input, spec%num_cell_pairs)
  CASE("SOURCE")
     CALL scalar_table_interp(current_time%time, spec%table_num,&
          & table_input, spec%num_cell_pairs)
  END SELECT

  x_end = blk%xmax
  y_end = blk%ymax

  SELECT CASE(spec%bc_loc)
                    
  CASE("US")
     i=1
     xstart = i + 1
     SELECT CASE(spec%bc_type)
           
     CASE("ZEROG")
        DO j=1,spec%num_cell_pairs
           j_beg = spec%start_cell(j)+1
           j_end = spec%end_cell(j)+1
           sclr%conc(i,j_beg:j_end) = sclr%conc(i+1,j_beg:j_end)
           sclr%cell(i+1,j_beg:j_end)%xtype = SCALAR_BOUNDARY_TYPE
           sclr%cell(i+1,j_beg:j_end)%xbctype = SCALBC_ZG
        END DO
                       
     CASE("TABLE")
        SELECT CASE(spec%bc_kind)
        CASE("FLUX")
           DO j=1,spec%num_cell_pairs
              j_beg = spec%start_cell(j)+1
              j_end = spec%end_cell(j)+1

                                ! compute the inflow cell by cell

              CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, tmp)
              WHERE (blk%uvel(i,j_beg:j_end) .GE. 0.0) 
                 inlet_area(j_beg:j_end) = &
                      &inlet_area(j_beg:j_end)*blk%uvel(i,j_beg:j_end)
              ELSEWHERE
                 inlet_area(j_beg:j_end) = 0.0
              END WHERE
              tmp = SUM(inlet_area(j_beg:j_end))
              DO jj = j_beg, j_end
                 sclr%conc(i,jj) =  table_input(j)/tmp
              END DO
              sclr%concold(i,j_beg:j_end) = sclr%conc(i,j_beg:j_end)
              sclr%cell(xstart,j_beg:j_end)%xtype = SCALAR_BOUNDARY_TYPE
              sclr%cell(xstart,j_beg:j_end)%xbctype = SCALBC_CONC
           END DO
              
        CASE("CONC")
           i = spec%x_start
           xstart = i + 1
           DO j=1,spec%num_cell_pairs
              j_beg = spec%start_cell(j)+1
              j_end = spec%end_cell(j)+1
              sclr%conc(i,j_beg:j_end) = &
                   &table_input(j)*scalar_source(spec%species)%conversion
              sclr%concold(i,j_beg:j_end) = sclr%conc(i,j_beg:j_end)
              sclr%cell(xstart,j_beg:j_end)%xtype = SCALAR_BOUNDARY_TYPE
              sclr%cell(xstart,j_beg:j_end)%xbctype = SCALBC_CONC
           END DO

        CASE DEFAULT
           GOTO 100
        END SELECT
     CASE DEFAULT
        GOTO 100
     END SELECT
        
  CASE("DS")
     i = x_end+1
     SELECT CASE(spec%bc_type)
           
     CASE("ZEROG")
        DO j=1,spec%num_cell_pairs
           j_beg = spec%start_cell(j)+1
           j_end = spec%end_cell(j)+1
           sclr%conc(i, j_beg:j_end) = sclr%conc(i-1, j_beg:j_end)
           sclr%concold(i,j_beg:j_end) = sclr%conc(i,j_beg:j_end)
           sclr%cell(i-1,j_beg:j_end)%xtype = SCALAR_BOUNDARY_TYPE
           sclr%cell(i-1,j_beg:j_end)%xbctype = SCALBC_ZG
        END DO

     CASE DEFAULT
        GOTO 100
     END SELECT

  CASE ("RB")
     j = 1
     ystart = j + 1
     SELECT CASE(spec%bc_type)

     CASE ("ZEROG")
        DO k = 1, spec%num_cell_pairs
           i_beg = spec%start_cell(k)+1
           i_end = spec%end_cell(k)+1
           sclr%conc(i_beg:i_end,j) = sclr%conc(i_beg:i_end,j+1)
           sclr%cell(i_beg:i_end,j+1)%ytype = SCALAR_BOUNDARY_TYPE
           sclr%cell(i_beg:i_end,j+1)%ybctype = SCALBC_ZG
        END DO

     CASE("TABLE")

        SELECT CASE(spec%bc_kind)

        CASE("CONC")
           j = spec%x_start
           ystart = j + 1
           DO k = 1, spec%num_cell_pairs
              i_beg = spec%start_cell(k)+1
              i_end = spec%end_cell(k)+1
              sclr%conc(i_beg:i_end, j) = &
                   &table_input(k)*scalar_source(spec%species)%conversion
              sclr%concold(i_beg:i_end, j) = sclr%conc(i_beg:i_end, j)
              sclr%cell(i_beg:i_end,ystart)%ytype = SCALAR_BOUNDARY_TYPE
              sclr%cell(i_beg:i_end,ystart)%ybctype = SCALBC_CONC
           END DO

        CASE("FLUX")
           DO k = 1, spec%num_cell_pairs
              i_beg = spec%start_cell(k)+1
              i_end = spec%end_cell(k)+1
              CALL compute_vflow_area(blk, i_beg, i_end, j, inlet_area, tmp)
              WHERE (blk%vvel(i_beg:i_end, j) .GT. 0.0)
                 inlet_area(i_beg:i_end) = &
                      &inlet_area(i_beg:i_end)*blk%vvel(i_beg:i_end,j)
              ELSEWHERE
                 inlet_area(i_beg:i_end) = 0.0
              END WHERE
              tmp = SUM(inlet_area(i_beg:i_end))
              DO ii = i_beg, i_end
                 sclr%conc(ii,j) =  table_input(k)/tmp
              END DO
              sclr%concold(i_beg:i_end, j) = sclr%conc(i_beg:i_end, j)
              sclr%cell(i_beg:i_end, j+1)%ytype = SCALAR_BOUNDARY_TYPE
              sclr%cell(i_beg:i_end, j+1)%ybctype = SCALBC_CONC
           END DO
        CASE DEFAULT
           GOTO 100
        END SELECT
     CASE DEFAULT
        GOTO 100
     END SELECT

  CASE ("LB")
     j = y_end+1

     SELECT CASE(spec%bc_type)
           
     CASE("ZEROG")
        DO k=1,spec%num_cell_pairs
           i_beg = spec%start_cell(k)+1
           i_end = spec%end_cell(k)+1
           sclr%conc(i_beg:i_end,j) = sclr%conc(i_beg:i_end,j-1)
           sclr%concold(i_beg:i_end,j) = sclr%conc(i_beg:i_end,j)
           sclr%cell(i_beg:i_end,j-1)%ytype = SCALAR_BOUNDARY_TYPE
           sclr%cell(i_beg:i_end,j-1)%ybctype = SCALBC_ZG
        END DO

     CASE DEFAULT
        GOTO 100
     END SELECT

  CASE ("IN")
     SELECT CASE(spec%bc_type)
     CASE("SOURCE")
        i_beg = spec%start_cell(1)+1
        i_end = spec%end_cell(1)+1
        j_beg = spec%start_cell(2)+1
        j_end = spec%end_cell(2)+1
        sclr%srcconc(i_beg:i_end, j_beg:j_end) = table_input(1)
     CASE DEFAULT
        GOTO 100
     END SELECT

  CASE DEFAULT
     GOTO 100
        
  END SELECT

  RETURN
     
100 CONTINUE
  WRITE(buf,*) " apply_scalar_bc: cannot handle: ", &
       &TRIM(spec%bc_loc), " ", TRIM(spec%bc_type), " ", &
       &TRIM(spec%bc_kind), " for scalar "
  CALL error_message(buf, fatal=.FALSE.)

END SUBROUTINE apply_scalar_bc

! ----------------------------------------------------------------
! SUBROUTINE apply_scalar_source
! This routine computes the scalar source term and stores it for later
! use.  This can be precomputed because all of the scalar source terms
! are explicit, depending only on the previous concentration
! estimates.
! ----------------------------------------------------------------
SUBROUTINE apply_scalar_source(iblock, ispecies, xstart, ystart)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock, ispecies, xstart, ystart

  INTEGER :: xend, yend
  INTEGER :: i, j
  DOUBLE PRECISION :: src, t_water

  xend = block(iblock)%xmax
  yend = block(iblock)%ymax

  species(ispecies)%scalar(iblock)%srcterm = 0.0
  
  DO i = xstart, xend
     DO j = ystart, yend

        IF (source_doing_temp) THEN
           t_water = species(source_temp_idx)%scalar(iblock)%conc(i,j)
        ELSE
           t_water = 0
        END IF

        IF (.NOT. block(iblock)%isdead(i,j)%p) THEN
           src = &
                &scalar_source_term(iblock, i, j, ispecies, &
                &species(ispecies)%scalar(iblock)%conc(i,j),&
                &block(iblock)%depth(i,j), block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j), &
                &t_water, salinity)

           ! Include the affects of any fluid sources.  A positive xsource (ft/s)
           ! indicates an increase in fluid volume. This must include a
           ! scalar concentration or temperature from the scalar bc specifications.  
        
           IF (block(iblock)%xsource(i,j) .GT. 0.0) THEN
              src = src + block(iblock)%xsource(i,j)*&
                   &species(ispecies)%scalar(iblock)%srcconc(i,j)
              !WRITE (*,*) i, j, block(iblock)%xsource(i,j), src
           END IF

           ! If there is condensation (negative evaporation) then give
           ! it air temperature; other scalars would get zero
           IF (block(iblock)%evaporation(i,j) .GT. 0.0) THEN
              SELECT CASE (scalar_source(ispecies)%srctype)
              CASE (TEMP)
                 src = src + block(iblock)%evaporation(i,j)*&
                      &met_zones(1)%current(MET_AIRT)
              END SELECT
           END IF

           src = src*block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j)
        END IF

        species(ispecies)%scalar(iblock)%srcterm(i,j) = src
     END DO
  END DO
END SUBROUTINE apply_scalar_source




!############################################################################
!----------------------------------------------------------------------------
! update old values of dependent variables

SUBROUTINE update(status_flag)

IMPLICIT NONE

INTEGER :: status_flag, iblock, ispecies, i, j
DOUBLE PRECISION :: e


DO iblock=1,max_blocks

  block(iblock)%uoldold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%uold(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%voldold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%vold(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%deptholdold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%depthold(2:block(iblock)%xmax,2:block(iblock)%ymax) 

  block(iblock)%uold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%uvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%vold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%vvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%depthold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%depth(2:block(iblock)%xmax,2:block(iblock)%ymax) 

  block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot
  block(iblock)%dp = 0.0

END DO

IF (do_transport) THEN
   DO ispecies = 1, max_species
      DO iblock = 1, max_blocks
         species(ispecies)%scalar(iblock)%concoldold(2:block(iblock)%xmax,2:block(iblock)%ymax) &
              = species(ispecies)%scalar(iblock)%concold(2:block(iblock)%xmax,2:block(iblock)%ymax)
         species(ispecies)%scalar(iblock)%concold(2:block(iblock)%xmax,2:block(iblock)%ymax) &
              = species(ispecies)%scalar(iblock)%conc(2:block(iblock)%xmax,2:block(iblock)%ymax)
         SELECT CASE (scalar_source(ispecies)%srctype)
         CASE (TEMP)
            IF (scalar_source(ispecies)%temp_param%doexchange) THEN
               DO i = 2, block(iblock)%xmax
                  DO j = 2, block(iblock)%ymax

                     ! compute the evaporation rate in this cell (ft/s)
                     e = met_zone_evaporation_rate(met_zones(1), &
                          &species(ispecies)%scalar(iblock)%conc(i,j))

                     ! set the hydrodynamic evaporation rate (ft/s) if specified
                     IF (scalar_source(ispecies)%temp_param%doevaporate) THEN 
                        block(iblock)%evaporation(i,j) = -e
                     ELSE
                        block(iblock)%evaporation(i,j) = 0.0
                     END IF

                     ! this is used for output and is converted to in/day
                     scalar_source(ispecies)%temp_param%block(iblock)%evaporation(i,j) = &
                          &e*12.0*3600.0*24.0

                  END DO
               END DO
            END IF
         END SELECT
      END DO
   END DO
   CALL scalar_mass_balance(delta_t)
   IF (source_doing_sed) CALL bed_accounting(delta_t)
   
END IF

IF (do_accumulate) CALL accumulate(current_time%time)

END SUBROUTINE update

!#############################################################################
!----------------------------------------------------------------------------
! printout variables over the whole field to ascii file and TECPLOT format file
!
!-----------------------------------------------------------------------------

SUBROUTINE output(status_flag)

IMPLICIT NONE


DOUBLE PRECISION :: depth_e, flux_e, conc_TDG, t_water

INTEGER :: iblock, ispecies, num_bc
INTEGER :: i, j, status_flag
DOUBLE PRECISION :: baro_press

IF (ALLOCATED(met_zones)) THEN
   baro_press = met_zones(1)%current(MET_BARO)
ELSE 
   baro_press = 760.0
END IF

!-----------------------------------------------------------------------------
! format definitions all placed here
2000 FORMAT(a80)
1000 FORMAT(50(f12.4,2x))
1020 FORMAT(2(f12.4,2x),50(f12.6,2x))
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)


IF( (current_time%time >= end_time%time) .OR. (MOD(time_step_count,print_freq) == 0) )THEN
 
	1010 FORMAT(50(f12.6,2x))

                                ! call these again so that ghost cells
                                ! are correctly filled

   DO iblock=1,max_blocks
      DO num_bc = 1, block_bc(iblock)%num_bc
         CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
              &.FALSE., ds_flux_given)
      END DO

      IF (do_transport) THEN
         DO ispecies = 1, max_species
            DO num_bc = 1, scalar_bc(iblock)%num_bc
               IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
                    &CALL apply_scalar_bc(block(iblock), &
                    &species(ispecies)%scalar(iblock), &
                    &scalar_bc(iblock)%bc_spec(num_bc), x_start, y_start)
            END DO
         END DO
      ELSE
         CALL bedshear(block(iblock))
      END IF
   END DO

IF (debug) THEN
   DO iblock=1,max_blocks
      WRITE(output_iounit,*)"------ Values for Date - ",current_time%date_string," Time - ",&
           & current_time%time_string," ------ for block number = ",iblock
	
      WRITE(block_title(12:15),'(i4)')iblock

      title = "U Velocity (located at U staggered node)"
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%uvel)


      title = "V Velocity (located at V staggered node)"
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%vvel)

      title = " Depth "
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%depth)


      title = "Water Surface Elevation "
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%wsel)

      title = "Depth Correction"
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%dp)

      title = "Mass Source"
      title(61:75) = block_title
      CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%mass_source)

      WRITE(output_iounit,*)"Unit Discharge in the 1 direction and total flow (last column) ",block_title
      DO i=1,block(iblock)%xmax
         total_flow = 0.0
         WRITE(output_iounit,1011, advance='no')i
         DO j=2,block(iblock)%ymax
            depth_e = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i+1,j))
            IF(i == 1) depth_e = block(iblock)%depth(i,j)
            IF(i == block(iblock)%xmax) depth_e = block(iblock)%depth(i+1,j)

            flux_e = block(iblock)%hu2(i,j)*block(iblock)%uvel(i,j)*depth_e
            WRITE(output_iounit,1012, advance='no')flux_e
            total_flow = total_flow + flux_e
         END DO
         WRITE(output_iounit,1012, advance='no')total_flow
         WRITE(output_iounit,*)
      END DO
      WRITE(output_iounit,*)
1012  FORMAT(50(f12.4,2x))
1011  FORMAT(i5,2x)

      IF(do_transport)THEN
         DO ispecies = 1, max_species
			title = "Concentration "
			title(61:75) = block_title
			CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,species(ispecies)%scalar(iblock)%conc)

            SELECT CASE (scalar_source(ispecies)%srctype)
            CASE (TDG)
               DO i=1,block(iblock)%xmax+1
                  DO j=1,block(iblock)%ymax+1
                     conc_TDG = species(ispecies)%scalar(iblock)%conc(i,j)
                     t_water = species(source_temp_idx)%scalar(iblock)%conc(i,j)
                     block(iblock)%TDG_stuff(i,j) = TDGasPress(conc_TDG,  t_water,  salinity)
                  END DO
               END DO
               CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%TDG_stuff)
            END SELECT
         END DO
      END IF
   END DO
END IF

DO iblock = 1, max_blocks

   WRITE(output_iounit,*)
   WRITE(output_iounit,*)"*** BLOCK -",iblock," Mass Source Information Summary ***"
   WRITE(output_iounit,*)" Summation of the Absolute Value of the Mass Source array = ",SUM(ABS(block(iblock)%mass_source))
   WRITE(output_iounit,*)" Maximum of the Mass Source array = ",MAXVAL(block(iblock)%mass_source)
   WRITE(output_iounit,*)" Location of Maximum of the Mass Source array = ",MAXLOC(block(iblock)%mass_source)
   WRITE(output_iounit,*)" Minimum of the Mass Source array = ",MINVAL(block(iblock)%mass_source)
   WRITE(output_iounit,*)" Location of Minimum of the Mass Source array = ",MINLOC(block(iblock)%mass_source)
   WRITE(output_iounit,*)

END DO


! end of output to ascii file section
!-------------------------------------------------------------------------------------------------------

CALL diag_plot_print(current_time%date_string, current_time%time_string, delta_t)

!-------------------------------------------------------------------------------------------------------
! print out in tecplot block format
!
IF (.NOT. do_accumulate) CALL accumulate(current_time%time)
CALL plot_print(current_time%date_string, current_time%time_string, &
     &salinity, baro_press)

END IF
! output/plot if-block
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! print out to gage output files
IF(do_gage_print)THEN
	IF(MOD(time_step_count,gage_print_freq) == 0)THEN
       CALL gage_print(current_time%date_string, current_time%time_string,&
            &DBLE((current_time%time - start_time%time)*24), &
            &do_transport, salinity, baro_press)
       CALL mass_print(current_time%date_string, current_time%time_string)
       IF (debug) &
            &CALL block_flux_print(current_time%date_string, current_time%time_string)

! 3011 FORMAT(i5,5x)
! 3005 FORMAT('#date',8x,'time',5x)

	END IF
END IF
status_flag = 99

END SUBROUTINE output

! ----------------------------------------------------------------
! SUBROUTINE read_hotstart
! ----------------------------------------------------------------
SUBROUTINE read_hotstart()

  IMPLICIT NONE

  INTEGER :: iblock, i
  CHARACTER (LEN=1024) :: msg

  ! OPEN(unit=hotstart_iounit,file='hotstart.bin', form='binary')
  ! OPEN(unit=hotstart_iounit,file='hotstart.bin',form='unformatted')
  CALL open_existing('hotstart.bin', hotstart_iounit)
  CALL status_message('reading hotstart file')
  READ(hotstart_iounit,*) do_transport_restart, max_species_in_restart
  DO iblock=1,max_blocks
     READ(hotstart_iounit,*) block(iblock)%uvel
     !WRITE(*,*)'done with uvel read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%uold
     !WRITE(*,*)'done with uold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%uoldold
     !WRITE(*,*)'done with uoldold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%ustar
     !WRITE(*,*)'done with ustar read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%vvel
     !WRITE(*,*)'done with vvel read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%vold
     !WRITE(*,*)'done with vold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%voldold
     !WRITE(*,*)'done with voldold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%vstar
     !WRITE(*,*)'done with vstar read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%depth
     !WRITE(*,*)'done with depth read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%depthold
     !WRITE(*,*)'done with depthold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%deptholdold
     !WRITE(*,*)'done with deptholdold read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%dstar
     !WRITE(*,*)'done with dstar read for block -',iblock
     READ(hotstart_iounit,*) block(iblock)%eddy 
     !WRITE(*,*)'done with eddy read for block -',iblock
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
        DO iblock = 1, max_blocks
           READ(hotstart_iounit,*) species(i)%scalar(iblock)%conc
           WRITE(msg,*)'done with conc read for species -',i,'and block -',iblock
           CALL status_message(msg)
           READ(hotstart_iounit,*) species(i)%scalar(iblock)%concold
           WRITE(msg,*)'done with concold read for species -',i,'and block -',iblock
           CALL status_message(msg)
           READ(hotstart_iounit,*) species(i)%scalar(iblock)%concoldold
           WRITE(msg,*)'done with concoldold read for species -',i,'and block -',iblock
           CALL status_message(msg)
        END DO
     END DO
     
     ! if any sediment species were
     ! specified, we expect to find a bed
     ! in the hotstart file
     
     IF (source_doing_sed) CALL bed_read_hotstart(hotstart_iounit)
     
  ELSE IF (do_transport) THEN
     DO i = 1, max_species
        DO iblock =1, max_blocks
           species(i)%scalar(iblock)%conc = conc_initial
           species(i)%scalar(iblock)%concold = conc_initial
        END DO
     END DO
  END IF
  
  CLOSE(hotstart_iounit)
  WRITE(status_iounit,*)'done reading hotstart file'

END SUBROUTINE read_hotstart



!##########################################################################
!---------------------------------------------------------------------------
! write a  restart file
!---------------------------------------------------------------------------

SUBROUTINE write_restart(status_flag)
IMPLICIT NONE

INTEGER :: i, status_flag, iblock


IF( (current_time%time >= end_time%time) .OR. (MOD(time_step_count,restart_print_freq) == 0) )THEN
	
	restart_filename(1:9) = 'hotstart_'
	restart_filename(10:19) = current_time%date_string
	restart_filename(20:20) = '_'
	restart_filename(21:22) = current_time%time_string(1:2)
	restart_filename(23:24) = current_time%time_string(4:5)
	restart_filename(25:26) = current_time%time_string(7:8)
	restart_filename(27:30) = '.bin'
	
	! OPEN(unit=restart_iounit,file=restart_filename,form='binary')
	! OPEN(unit=restart_iounit,file=restart_filename,form='unformatted')
    CALL open_new(restart_filename, restart_iounit)

	IF(do_transport)THEN
		do_transport_restart = .TRUE.
		WRITE(restart_iounit,*) do_transport_restart, max_species
	ELSE
		do_transport_restart = .FALSE.
		WRITE(restart_iounit,*) do_transport_restart, max_species
	END IF

	DO iblock=1,max_blocks
       block(iblock)%work = block(iblock)%uvel
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work
       
       block(iblock)%work = block(iblock)%uold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%uoldold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%ustar
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vvel
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%voldold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vstar
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%depth
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%depthold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%deptholdold
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%dstar
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%eddy
       WHERE(abs(block(iblock)%work) < tiny) &
            &block(iblock)%work = sign(tiny, block(iblock)%work)
       WRITE(restart_iounit,*)block(iblock)%work

!!$		WRITE(restart_iounit,*) block(iblock)%uvel
!!$		WRITE(restart_iounit,*) block(iblock)%uold
!!$		WRITE(restart_iounit,*) block(iblock)%ustar
!!$		WRITE(restart_iounit,*) block(iblock)%vvel
!!$		WRITE(restart_iounit,*) block(iblock)%vold
!!$		WRITE(restart_iounit,*) block(iblock)%vstar

!!$		WRITE(restart_iounit,*) block(iblock)%depth
!!$		WRITE(restart_iounit,*) block(iblock)%depthold
!!$		WRITE(restart_iounit,*) block(iblock)%dstar	
!!$		WRITE(restart_iounit,*) block(iblock)%eddy
    END DO

	IF(do_transport)THEN
		DO i=1,max_species
			DO iblock = 1, max_blocks
              block(iblock)%work = species(i)%scalar(iblock)%conc
              WHERE(abs(block(iblock)%work) < tiny) &
                   &block(iblock)%work = sign(tiny, block(iblock)%work)
              WRITE(restart_iounit,*)block(iblock)%work
              
              block(iblock)%work = species(i)%scalar(iblock)%concold
              WHERE(abs(block(iblock)%work) < tiny) &
                   &block(iblock)%work = sign(tiny, block(iblock)%work) 
              WRITE(restart_iounit,*)block(iblock)%work

              block(iblock)%work = species(i)%scalar(iblock)%concoldold
              WHERE(abs(block(iblock)%work) < tiny) &
                   &block(iblock)%work = sign(tiny, block(iblock)%work) 
              WRITE(restart_iounit,*)block(iblock)%work

             ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%conc
             ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%concold
			END DO
		END DO

        IF (source_doing_sed) CALL bed_write_hotstart(restart_iounit)
	END IF

	CLOSE(restart_iounit)

ENDIF

END SUBROUTINE write_restart






!#############################################################################
!----------------------------------------------------------------------------
! other internal routines
!-----------------------------------------------------------------------------

! ----------------------------------------------------------------
! SUBROUTINE check_wetdry
! ----------------------------------------------------------------
SUBROUTINE check_wetdry(blk)

  IMPLICIT NONE

  TYPE(block_struct) :: blk
  INTEGER :: i, j, nx, ny
  INTEGER :: i1, j1, n
  LOGICAL :: flag
  DOUBLE PRECISION :: wsavg, wscell, wsn
  LOGICAL :: isdry(i_index_min:blk%xmax + i_index_extra, &
       &j_index_min:blk%ymax + j_index_extra)

  nx = blk%xmax
  ny = blk%ymax

  isdry = blk%isdry

  DO i = 2, nx + 1
     DO j = 2, ny + 1 
                 
        IF (isdry(i,j)) THEN

           wscell = blk%depth(i, j) + blk%zbot(i, j)

                                ! Condition: all wet neighbors must
                                ! have a higher w.s. elevation.

           flag = .FALSE.       ! becomes .TRUE. if *any* wet neighbors are higher
           n = 0                ! count wet neighbors

           DO i1 = i - 1, i + 1, 2
              IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. isdry(i1,j))) THEN
                 wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                 IF (wsn .GE. wscell) flag = .TRUE.
                 n = n + 1
              ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                 SELECT CASE (blk%cell(i,j)%xtype)
                 CASE (CELL_BOUNDARY_TYPE)
                    ! ignore
                 CASE DEFAULT
                    ! should be a ghost cell, use it directly
                    wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                    IF (wsn .GE. wscell) flag = .TRUE.
                    n = n + 1
                 END SELECT
              END IF
           END DO
           DO j1 = j - 1, j + 1, 2
              wsn = blk%depth(i, j1) + blk%zbot(i, j1)
              IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. isdry(i,j1))) THEN
                 IF (wsn .GE. wscell) flag = .TRUE.
                 n = n + 1
              END IF
           END DO

           IF (n .GT. 0 .AND. flag) THEN

                                ! if everything is evened
                                ! out the cell and its neighbors will
                                ! be at the same elevation -- the
                                ! average, compute it
        
              wsavg = wscell
              n = 1

              DO i1 = i - 1, i + 1, 2
                 IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. isdry(i1,j))) THEN
                    wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                    wsavg = wsavg + wsn
                    n = n + 1
                 ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                    SELECT CASE (blk%cell(i,j)%xtype)
                    CASE (CELL_BOUNDARY_TYPE)
                       ! ignore
                    CASE DEFAULT
                       ! should be a ghost cell, use it directly
                       wsn = blk%depth(i1, j) + blk%zbot(i1, j)
                       wsavg = wsavg + wsn
                       n = n + 1
                    END SELECT
                 END IF
              END DO
              DO j1 = j - 1, j + 1, 2
                 wsn = blk%depth(i, j1) + blk%zbot(i, j1)
                 IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. isdry(i,j1))) THEN
                    wsavg = wsavg + wsn
                    n = n + 1
                 END IF
              END DO
              wsavg = wsavg/REAL(n)

!!$                                ! Alternative: A cell becomes wet if
!!$                                ! the average elevation of the cell
!!$                                ! and all its wet neighbors is high
!!$                                ! enough to exceed the dry depth
!!$
!!$           blk%isdry(i,j) = (.NOT. ((wsavg - blk%zbot(i, j)) .GT. dry_depth))

                                ! Alternative: A cell becomes wet if
                                ! the cell and all its wet neighbors
                                ! are high enough to exceed the dry
                                ! depth *and* have all of the wet
                                ! cells remain wet.

              IF (N .GT. 1 .AND. wsavg .GT. blk%zbot(i, j) + dry_rewet_depth ) THEN
                 flag = .TRUE.
                 DO i1 = i - 1, i + 1, 2
                    IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. isdry(i1,j))) THEN
                       flag = flag .AND. ((wsavg - blk%zbot(i1, j)) .GT. dry_depth) 
                    ELSEIF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                       SELECT CASE (blk%cell(i,j)%xtype)
                       CASE (CELL_BOUNDARY_TYPE)
                          ! ignore
                       CASE DEFAULT
                          ! should be a ghost cell, use it directly
                          flag = flag .AND. ((wsavg - blk%zbot(i1, j)) .GT. dry_depth) 
                       END SELECT
                    END IF
                 END DO
                 DO j1 = j - 1, j + 1, 2
                    IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. isdry(i,j1))) THEN
                       flag = flag .AND. ((wsavg - blk%zbot(i, j1)) .GT. dry_depth) 
                    END IF
                 END DO

                                ! if all the neighbors can stay wet,
                                ! then it's ok to be wet again

                                ! we may need a volume check here
                 
                 IF (flag) blk%isdry(i,j) = .FALSE.

              END IF
           END IF
           
        ELSE 

                                ! check the wet cells to see if they
                                ! should be dry

           blk%isdry(i,j) = (blk%depth(i,j) .LE. dry_depth)

        END IF
     END DO
  END DO

                                ! fill out isdry array for plotting

  blk%isdry(:,1) = blk%isdry(:,2)
  blk%isdry(:,ny + 1) = blk%isdry(:,ny+1)
  blk%isdry(1,:) = blk%isdry(2,:)
  blk%isdry(nx + 1,:)  = blk%isdry(nx+1,:)

END SUBROUTINE check_wetdry



END MODULE mass2_main_025

