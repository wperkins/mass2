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
USE scalars
USE scalars_source
USE met_data_module
USE energy_flux
USE gas_functions

USE misc_vars
USE transport_only
USE scalars_source
USE bed_module

!-------------------------------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

DOUBLE PRECISION, PARAMETER :: bigfactor = 1.0d80

!---------------------------------------------------------------------------------
! derived type (structures) declarations

! moved to 
! TYPE(datetime_struct), SAVE :: start_time, end_time, current_time



CONTAINS

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------

SUBROUTINE start_up(status_flag)

IMPLICIT NONE

INTEGER :: status_flag, var
CHARACTER (LEN=1024) :: msg

!-------------------------------------------------------------------------------------------------------
! format definitions all placed here
2000 FORMAT(a80)
1000 FORMAT(50(f12.4,2x))
1020 FORMAT(2(f12.4,2x),50(f12.6,2x))
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)

!-------------------------------------------------------------------------------------------------------
! open io units

CALL open_existing('mass2_v027.cfg', cfg_iounit)
!-----------------------------------------------------------------------------------
! write info to the console
WRITE(*,*)'Pacific Northwest National Laboratory'
WRITE(*,*)
WRITE(*,*)code_version
WRITE(*,*)code_date

!-------------------------------------------------------------------------------------------------------
READ(cfg_iounit,2000)config_file_version
READ(cfg_iounit,2000) !sim_title

!-------------------------------------------------------------------------------------------------------
! allocation of dynamic arrays
READ(cfg_iounit,*)max_blocks
READ(cfg_iounit,*)max_species

! allocate hydrodynamics stuff

CALL allocate_blocks()
ALLOCATE(grid_file_name(max_blocks))

DO iblock=1,max_blocks
	READ(cfg_iounit,*)grid_file_name(iblock)
    CALL open_existing(grid_file_name(iblock), grid_iounit)
	READ(grid_iounit,*)block(iblock)%xmax,block(iblock)%ymax
	CLOSE(grid_iounit)
END DO

DO i=1,max_blocks
	CALL allocate_block_components(i, status_iounit)
END DO

imax = MAXVAL(block(:)%xmax) + 1
jmax = MAXVAL(block(:)%ymax) + 1
CALL allocate_coeff_components(imax, jmax, status_iounit)

! allocate scalar species stuff

CALL allocate_species(error_iounit,status_iounit)

DO i = 1, max_species
	CALL allocate_scalar(max_blocks, i, error_iounit,status_iounit)
END DO

DO i=1, max_species
	DO j =1, max_blocks
		CALL allocate_scalarblock_components(i, j , block(j)%xmax, block(j)%ymax)
	END DO
END DO

ALLOCATE(aa(jmax),bb(jmax),cc(jmax),dd(jmax),tt(jmax),ptemp(0:jmax),qtemp(0:jmax))
ALLOCATE(work(imax,jmax),cos_ij(imax,jmax))
ALLOCATE(inlet_area(jmax), table_input(jmax))
ptemp = 0.0
qtemp = 0.0

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


!---------------------------------------------------------------------------------------------------------
! finish reading cfg file
READ(cfg_iounit,*)do_flow				! on/off switch for hydrodynamics
READ(cfg_iounit,*)do_transport	! on/off switch for transport calculations
baro_press = 760.0              ! in case weather is not read
READ(cfg_iounit,*)do_surface_heatx, do_surface_gasx, weather_filename !*** on/off surface exhange, weather data file
READ(cfg_iounit,*)debug					! extra debug printing

READ(cfg_iounit,*)manning							! switch between manning or chezy bottom friction equation
READ(cfg_iounit,*)write_restart_file, restart_print_freq	! write out a binary restart file "hotstart_date_time.bin"
READ(cfg_iounit,*)read_hotstart_file	! read in a binary hotstart file "hotstart.bin"
READ(cfg_iounit,*)do_gage_print				! do gage print
READ(cfg_iounit,*)given_initial_wsel, read_initial_profile	! if TRUE then wsel_or_depth_initial is the initial water elv.
																						! IF FALSE then wsel_or_depth_initial is the initial depth



READ(cfg_iounit,*)start_time%date_string, start_time%time_string	! model start date & time 
READ(cfg_iounit,*)end_time%date_string, end_time%time_string			! model end date & time
READ(cfg_iounit,*)delta_t 		! time step (seconds)


READ(cfg_iounit,*)number_hydro_iterations, max_mass_source_sum	!*** number of internal iterations at a fixed time level
																																! max summation of mass source at any iteration
READ(cfg_iounit,*)number_scalar_iterations !*** number of internal iterations for scalar solutions	
READ(cfg_iounit,*)scalar_sweep	! internal iterations
READ(cfg_iounit,*)depth_sweep		! depth correction internal iterations
READ(cfg_iounit,*)eddy_default, do_spatial_eddy  ! turb eddy viscosity

! scalar diffusion coeff in xsi (ft^2/sec) ! scalar diffusion coeff in eta (ft^2/sec)
READ(cfg_iounit,*)kx_diff_default,do_spatial_kx
READ(cfg_iounit,*)ky_diff_default,do_spatial_ky 
									
									! chezy constant if chezy-type relation is used (manning=FALSE)
READ(cfg_iounit,*)chezy_con_default, do_spatial_chezy		! manning n value if manning=TRUE
READ(cfg_iounit,*)mann_con		! constant in the bed shear stress  =1.0 if metric units
READ(cfg_iounit,*)relax_dp		! relaxation factor for depth corrections
READ(cfg_iounit,*)inlet_flow	! uniform inlet flow rate in non spill/non gen areas
READ(cfg_iounit,*)ds_elev			! downstream water elevation bc (feet)

READ(cfg_iounit,*)uvel_initial	! initial uniform value of u velocity
READ(cfg_iounit,*)vvel_initial	! initial uniform value of v velocity
READ(cfg_iounit,*)conc_initial	! initial uniform value of concentration
READ(cfg_iounit,*)wsel_or_depth_initial	! initial uniform value of depth OR water surface elv.

READ(cfg_iounit,*)uvel_wind		! initial uniform value of u wind velocity
READ(cfg_iounit,*)vvel_wind		! initial uniform value of v wind velocity

                                ! wetting and drying parameters

READ(cfg_iounit,*)do_wetdry, dry_depth, dry_rewet_depth, dry_zero_depth

                                ! initial bed information

READ(cfg_iounit,*)bed_default_porosity, bed_initial_depth, read_bed_init

READ(cfg_iounit,*) print_freq, do_accumulate ! printout frequency every print_freq time steps
READ(cfg_iounit,*) plot_do_netcdf, do_flow_diag, do_flow_output	! NetCDF output flags
READ(cfg_iounit,*) plot_do_cgns, plot_cgns_docell, plot_cgns_dodesc, plot_cgns_maxtime ! CGNS output flags
READ(cfg_iounit,*) gage_print_freq

CLOSE (cfg_iounit)
!--------------------------------------------------------------------------------------------------------
! do some pre-processing of the config data

mann_con = mann_con**2

start_time%time = date_to_decimal(start_time%date_string,start_time%time_string)
end_time%time = date_to_decimal(end_time%date_string,end_time%time_string)
current_time = start_time

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
	CALL read_met_data(weather_filename)
	CALL update_met_data(current_time%time)
END IF

!------------------------------------------------------------------------------------
! set up the gage print files
IF(do_gage_print) THEN
   CALL gage_file_setup(do_transport,error_iounit, status_iounit)
   CALL mass_file_setup()
END IF


!---------------------------------------------------------------------------------------------------------
! read in the grid files for each block
DO iblock=1,max_blocks
   CALL open_existing(grid_file_name(iblock), grid_iounit)
   READ(grid_iounit,*)junk
   CALL status_message('reading in x,y,z from ' // grid_file_name(iblock))
	 
   ! read in grid x,y, and bottom elevation
   DO i=1,block(iblock)%xmax
      DO j=1,block(iblock)%ymax
         READ(grid_iounit,*)junk,junk,block(iblock)%x_grid(i,j),block(iblock)%y_grid(i,j),block(iblock)%zbot_grid(i,j)
      END DO
   END DO
   CLOSE(grid_iounit)
   WRITE(status_iounit,*)'completed in x,y,z for block n = ',iblock
END DO

DO iblock=1,max_blocks

   CALL buildghost(iblock)


                                ! interpolate x_grid,y_grid,zbot_grid
                                ! onto the c.v. points by simple
                                ! averaging


   DO i = i_index_min + 1, block(iblock)%xmax + i_index_extra - 1
      DO j = j_index_min + 1, block(iblock)%ymax + j_index_extra - 1
         block(iblock)%x(i,j) = 0.25*(block(iblock)%x_grid(i,j)+block(iblock)%x_grid(i-1,j)+&
              & block(iblock)%x_grid(i,j-1)+block(iblock)%x_grid(i-1,j-1))
         block(iblock)%y(i,j) = 0.25*(block(iblock)%y_grid(i,j)+block(iblock)%y_grid(i-1,j)+&
              & block(iblock)%y_grid(i,j-1)+block(iblock)%y_grid(i-1,j-1))
         block(iblock)%zbot(i,j) = 0.25*(block(iblock)%zbot_grid(i,j)+block(iblock)%zbot_grid(i-1,j)+&
              & block(iblock)%zbot_grid(i,j-1)+block(iblock)%zbot_grid(i-1,j-1))
      END DO
   END DO
   ! now take care of the edges of the grid
   ! remember that the c.v.'s have an extra i,j line than the grid
   i=i_index_min
   DO j= j_index_min + 1, block(iblock)%ymax + j_index_extra - 1
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j)+block(iblock)%x_grid(i,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j)+block(iblock)%y_grid(i,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j)+block(iblock)%zbot_grid(i,j-1))
   END DO

   i= block(iblock)%xmax+i_index_extra
   DO j= j_index_min + 1, block(iblock)%ymax + j_index_extra - 1
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i-1,j)+block(iblock)%x_grid(i-1,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i-1,j)+block(iblock)%y_grid(i-1,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i-1,j)+block(iblock)%zbot_grid(i-1,j-1))
   END DO

   j=j_index_min
   DO i = i_index_min + 1, block(iblock)%xmax + i_index_extra - 1
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j)+block(iblock)%x_grid(i-1,j))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j)+block(iblock)%y_grid(i-1,j))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j)+block(iblock)%zbot_grid(i-1,j))
   END DO

   j= block(iblock)%ymax+j_index_extra
   DO i = i_index_min + 1, block(iblock)%xmax + i_index_extra - 1
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j-1)+block(iblock)%x_grid(i-1,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j-1)+block(iblock)%y_grid(i-1,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j-1)+block(iblock)%zbot_grid(i-1,j-1))
   END DO

   ivec = (/i_index_min, i_index_min,&
        &block(iblock)%xmax + i_index_extra, block(iblock)%xmax+i_index_extra/)
   jvec = (/j_index_min, block(iblock)%ymax+j_index_extra,&
        &j_index_min,block(iblock)%ymax+j_index_extra/)
   ivec2 = (/i_index_min,i_index_min,&
        &block(iblock)%xmax+i_index_extra-1,block(iblock)%xmax+i_index_extra-1/)
   jvec2 = (/j_index_min,block(iblock)%ymax+j_index_extra-1,&
        &j_index_min,block(iblock)%ymax+j_index_extra-1/)
   block(iblock)%x(ivec,jvec) = block(iblock)%x_grid(ivec2,jvec2)
   block(iblock)%y(ivec,jvec) = block(iblock)%y_grid(ivec2,jvec2)
   block(iblock)%zbot(ivec,jvec) = block(iblock)%zbot_grid(ivec2,jvec2)
END DO

!-----------------------------------------------------------------------------------------------------------
! assign initial conditions for each block
! using uniform values from cfg file or a restart file

IF(read_hotstart_file)THEN
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
    READ(hotstart_iounit,*) block(iblock)%ustar
!WRITE(*,*)'done with ustar read for block -',iblock
    READ(hotstart_iounit,*) block(iblock)%vvel
!WRITE(*,*)'done with vvel read for block -',iblock
    READ(hotstart_iounit,*) block(iblock)%vold
!WRITE(*,*)'done with vold read for block -',iblock
    READ(hotstart_iounit,*) block(iblock)%vstar
!WRITE(*,*)'done with vstar read for block -',iblock
    READ(hotstart_iounit,*) block(iblock)%depth
!WRITE(*,*)'done with depth read for block -',iblock
    READ(hotstart_iounit,*) block(iblock)%depthold
!WRITE(*,*)'done with depthold read for block -',iblock
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
ELSE
	WRITE(status_iounit,*)'-- setting initial values for all blocks'
	DO iblock=1,max_blocks
    block(iblock)%uvel = uvel_initial
    block(iblock)%uold = block(iblock)%uvel
    block(iblock)%ustar = block(iblock)%uvel
    block(iblock)%vvel = vvel_initial
    block(iblock)%vold = block(iblock)%vvel
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
        block(iblock)%dstar = block(iblock)%depth
    END IF
	END DO
	DO i = 1, max_species
		DO iblock =1, max_blocks
			species(i)%scalar(iblock)%conc = conc_initial
			species(i)%scalar(iblock)%concold = conc_initial
		END DO
	END DO

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
101	CLOSE(50)
ENDIF
IF(do_spatial_kx)THEN
   filename = "kx_coeff.dat"
   CALL open_existing(filename, 50)
	DO WHILE(.TRUE.)
		READ(50,*,END=102)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%kx_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
102	CLOSE(50)
ENDIF
IF(do_spatial_ky)THEN
	filename = "ky_coeff.dat"
    CALL open_existing(filename, 50)
	DO WHILE(.TRUE.)
		READ(50,*,END=103)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%ky_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
103	CLOSE(50)
ENDIF

IF(do_spatial_chezy)THEN
	filename = "roughness_coeff.dat"
    CALL open_existing(filename, 50)
	DO WHILE(.TRUE.)
		READ(50,*,END=100)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%chezy(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
100	CLOSE(50)
ENDIF

IF (read_initial_profile) THEN
   CALL profile_init(given_initial_wsel, manning, SQRT(mann_con), status_iounit)
END IF


!----------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------

DO iblock=1,max_blocks


   CALL metrics(block(iblock))

   IF (do_wetdry) &
        &CALL check_wetdry(iblock, block(iblock)%xmax, block(iblock)%ymax)

 END DO
	


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

!-----------------------------------------------------------------------------------------------------------
! write initial fields to the plot file

OPEN(grid_iounit,file='gridplot1.dat')
WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
	!WRITE(plot_iounit,*)"variables=""x"" ""y"" ""u vel"" ""v vel"" ""u cart"" ""v cart"" ""depth"" ""zbot"" ""wsel"" ""con"""
WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""
DO iblock=1,max_blocks
	WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblock,""""," i=", block(iblock)%xmax + 2, " j= ",block(iblock)%ymax
	WRITE(grid_iounit,'(8G16.8)')block(iblock)%x_grid(0:block(iblock)%xmax+1,1:block(iblock)%ymax)
	WRITE(grid_iounit,'(8G16.8)')block(iblock)%y_grid(0:block(iblock)%xmax+1,1:block(iblock)%ymax)
	WRITE(grid_iounit,'(8G16.8)')block(iblock)%zbot_grid(0:block(iblock)%xmax+1,1:block(iblock)%ymax)
END DO
CLOSE(grid_iounit)

IF ( plot_do_tecplot ) THEN
   OPEN(grid_iounit,file='gridplot-metrics.dat')
   WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid Metrics"""
   WRITE(grid_iounit,*)"variables=""x"" ""y"" ""hp1"" ""hp2"" ""gp12"""
   DO iblock=1,max_blocks
      WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblock,""""," i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
      WRITE(grid_iounit,*)block(iblock)%x
      WRITE(grid_iounit,*)block(iblock)%y
      WRITE(grid_iounit,*)block(iblock)%hp1
      WRITE(grid_iounit,*)block(iblock)%hp2
      WRITE(grid_iounit,*)block(iblock)%gp12
   END DO
   CLOSE(grid_iounit)
END IF

!-----------------------------------------------------------------------
! diagnostic plot; tecplot format

CALL diag_plot_file_setup()

!-----------------------------------------------------------------------

CALL plot_file_setup()
CALL accumulate(start_time%time)
CALL plot_print(start_time%date_string, start_time%time_string, &
     &salinity, baro_press)

IF (do_gage_print) THEN
   CALL gage_print(current_time%date_string, current_time%time_string,&
        &(current_time%time - start_time%time)*24, &
        &do_transport, salinity, baro_press)
END IF

!OPEN(55,file='ds_elev-file2.dat')
!DO j=1,block(1)%ymax+1
! READ(55,*)ds_elev_read(j)
!END DO
!CLOSE(55)

END SUBROUTINE start_up

! ----------------------------------------------------------------
! SUBROUTINE buildghost
! ----------------------------------------------------------------
SUBROUTINE buildghost(iblock)
  
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: iblock
  INTEGER :: i, j, k, con_i, con_j

                                ! extrapolate ghost cells in the grid
                                ! coordinates

   i = 0
   DO j = 1, block(iblock)%ymax
      block(iblock)%x_grid(i,j) = block(iblock)%x_grid(i+1,j) - &
           &(block(iblock)%x_grid(i+2,j) - block(iblock)%x_grid(i+1,j))
      block(iblock)%y_grid(i,j) = block(iblock)%y_grid(i+1,j) - &
           &(block(iblock)%y_grid(i+2,j) - block(iblock)%y_grid(i+1,j))
      block(iblock)%zbot_grid(i,j) = block(iblock)%zbot_grid(i+1,j)
      ! block(iblock)%zbot_grid(i,j) = block(iblock)%zbot_grid(i+1,j) - &
      !      &(block(iblock)%zbot_grid(i+2,j) - block(iblock)%zbot_grid(i+1,j))
   END DO
   i = block(iblock)%xmax + 1
   DO j = 1, block(iblock)%ymax
      block(iblock)%x_grid(i,j) = block(iblock)%x_grid(i-1,j) - &
           &(block(iblock)%x_grid(i-2,j) - block(iblock)%x_grid(i-1,j))
      block(iblock)%y_grid(i,j) = block(iblock)%y_grid(i-1,j) - &
           &(block(iblock)%y_grid(i-2,j) - block(iblock)%y_grid(i-1,j))
      ! block(iblock)%zbot_grid(i,j) = block(iblock)%zbot_grid(i-1,j) - &
      !      &(block(iblock)%zbot_grid(i-2,j) - block(iblock)%zbot_grid(i-1,j))
      block(iblock)%zbot_grid(i,j) = block(iblock)%zbot_grid(i-1,j) 
   END DO

                                ! copy ghost cell coordinates for those
                                ! cells connecting with another block

   
   DO num_bc = 1, block_bc(iblock)%num_bc

      SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type) 
      CASE ("BLOCK")
         con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
         SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
         CASE("US")
            i = 0
            con_i = block(con_block)%xmax - 1
         CASE ("DS")
            i = block(iblock)%xmax + 1
            con_i = 2
         END SELECT
         DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
            con_j = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)
            DO j = block_bc(iblock)%bc_spec(num_bc)%start_cell(k), &
                 &block_bc(iblock)%bc_spec(num_bc)%end_cell(k)
               block(iblock)%x_grid(i,j) = block(con_block)%x_grid(con_i,con_j)
               block(iblock)%y_grid(i,j) = block(con_block)%y_grid(con_i,con_j)
               block(iblock)%zbot_grid(i,j) = block(con_block)%zbot_grid(con_i,con_j)
               block(iblock)%x_grid(i,j+1) = block(con_block)%x_grid(con_i,con_j+1)
               block(iblock)%y_grid(i,j+1) = block(con_block)%y_grid(con_i,con_j+1)
               block(iblock)%zbot_grid(i,j+1) = block(con_block)%zbot_grid(con_i,con_j+1)
               con_j = con_j + 1
            END DO
         END DO
      END SELECT
   END DO

END SUBROUTINE buildghost

! ----------------------------------------------------------------
! SUBROUTINE fillghost
! ----------------------------------------------------------------
SUBROUTINE fillghost(iblock)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock
  INTEGER :: i, j, k, con_i, con_j

                                ! for extrapolated cells, use
                                ! parameters from the neighboring real
                                ! cell (metrics are computed elsewhere)

  block(iblock)%eddy(1,:) = block(iblock)%eddy(2,:)
  block(iblock)%kx_diff(1,:) = block(iblock)%kx_diff(2,:)
  block(iblock)%ky_diff(1,:) = block(iblock)%ky_diff(2,:)
  block(iblock)%chezy(1,:) = block(iblock)%chezy(2,:)
  block(iblock)%eddy(block(iblock)%xmax+1,:) = block(iblock)%eddy(block(iblock)%xmax,:)
  block(iblock)%kx_diff(block(iblock)%xmax+1,:) = block(iblock)%kx_diff(block(iblock)%xmax,:)
  block(iblock)%ky_diff(block(iblock)%xmax+1,:) = block(iblock)%ky_diff(block(iblock)%xmax,:)
  block(iblock)%chezy(block(iblock)%xmax+1,:) = block(iblock)%chezy(block(iblock)%xmax,:)

                                ! copy ghost cell metrics and
                                ! parameters from connecting block

   
   DO num_bc = 1, block_bc(iblock)%num_bc

      SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type) 
      CASE ("BLOCK")
         con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
         SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
         CASE("US")
            i = 1
            con_i = block(con_block)%xmax
         CASE ("DS")
            i = block(iblock)%xmax + 1
            con_i = 2
         END SELECT
         DO k = 1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
            con_j = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+1
            DO j = block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1, &
                 &block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1
               block(iblock)%hp1(i,j) = block(con_block)%hp1(con_i,con_j)
               block(iblock)%hp2(i,j) = block(con_block)%hp2(con_i,con_j)
               block(iblock)%hv1(i,j-1) = block(con_block)%hv1(con_i,con_j-1)
               block(iblock)%hv2(i,j-1) = block(con_block)%hv2(con_i,con_j-1)
               block(iblock)%hv1(i,j) = block(con_block)%hv1(con_i,con_j)
               block(iblock)%hv2(i,j) = block(con_block)%hv2(con_i,con_j)

                                ! do not copy hu1,hu2 - they are calculated

               block(iblock)%gp12(i,j) = block(con_block)%gp12(con_i,con_j)
               block(iblock)%gp12(i,j) = block(con_block)%gp12(con_i,con_j)
               
               block(iblock)%eddy(i,j) = block(con_block)%eddy(con_i,con_j)
               block(iblock)%kx_diff(i,j) = block(con_block)%kx_diff(con_i,con_j)
               block(iblock)%ky_diff(i,j) = block(con_block)%ky_diff(con_i,con_j)
               block(iblock)%chezy(i,j) = block(con_block)%chezy(con_i,con_j)
               con_j = con_j + 1
            END DO
         END DO
      END SELECT
   END DO

END SUBROUTINE fillghost

! ----------------------------------------------------------------
! SUBROUTINE metrics
! Compute the metric coefficients for the block. Depending on the
! metric coeff. and location use either the grid (x,y) or the node
! (x,y)
! ----------------------------------------------------------------
SUBROUTINE metrics(blk)

  IMPLICIT NONE

  TYPE (block_struct) :: blk

  INTEGER :: imin, imax, jmin, jmax, i, j

  imin = i_index_min
  imax = blk%xmax+i_index_extra
  jmin = j_index_min
  jmax = blk%ymax+j_index_extra

   ! metric coeff. 2 on the u face of the c.v.

   DO i=imin, imax-1
      DO j=jmin+1, jmax-1
         blk%hu2(i,j) = & 
              SQRT((blk%x_grid(i,j) - blk%x_grid(i,j-1))**2 + &
              (blk%y_grid(i,j) - blk%y_grid(i,j-1))**2)
      END DO
   END DO

   ! metric coeff 1 on the u face of the c.v.

   DO i=imin+1, imax-i_index_extra
      DO j=jmin, jmax
         blk%hu1(i,j) = &
              SQRT((blk%x(i+1,j) - blk%x(i,j))**2 + &
              (blk%y(i+1,j) - blk%y(i,j))**2)
      END DO
   END DO

   ! on the edge it's only a half-distance

   i=imin
   DO j=jmin, jmax
      blk%hu1(i,j) = &
           SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
           ((blk%y(i+1,j) - blk%y(i,j)))**2)
   END DO
   i=imax-1
   DO j=jmin, jmax
      blk%hu1(i,j) = &
           SQRT(((blk%x(i+1,j) - blk%x(i,j)))**2 + &
           ((blk%y(i+1,j) - blk%y(i,j)))**2)
   END DO
   
   ! metric coeff. 1 on the v face of the c.v.

   DO i=imin+1, imax-1
      DO j=jmin, jmax - 1
         blk%hv1(i,j) = &
              SQRT((blk%x_grid(i,j) - blk%x_grid(i-1,j))**2 + &
              (blk%y_grid(i,j) - blk%y_grid(i-1,j))**2) 
      END DO
   END DO

   ! metric coeff. 2 on the v face of the c.v.

   DO i=imin, imax
      DO j=jmin+1, jmax-j_index_extra-1
         blk%hv2(i,j) = &
              SQRT((blk%x(i,j+1) - blk%x(i,j))**2 + &
              (blk%y(i,j+1) - blk%y(i,j))**2)
      END DO
   END DO

   ! on the edge it's only a half-distance

   j = jmin
   DO i=imin+1, imax-1
      blk%hv2(i,j) = &
           SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
           ((blk%y(i,j+1) - blk%y(i,j)))**2)
   END DO
   j = jmax - 1
   DO i=imin+1, imax-1
      blk%hv2(i,j) = &
           SQRT(((blk%x(i,j+1) - blk%x(i,j)))**2 + &
           ((blk%y(i,j+1) - blk%y(i,j)))**2)
   END DO

   ! compute metric tensor and derivatives at the nodal points hp1, hp2

   DO i = imin+1, imax-1
      DO j=jmin+1, jmax-1
         blk%x_eta(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i-1,j) & 
              - blk%x_grid(i,j-1) - blk%x_grid(i-1,j-1))
         blk%y_eta(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i-1,j) & 
              - blk%y_grid(i,j-1) - blk%y_grid(i-1,j-1))
         blk%x_xsi(i,j) = 0.5*(blk%x_grid(i,j) + blk%x_grid(i,j-1) & 
              - blk%x_grid(i-1,j) - blk%x_grid(i-1,j-1))
         blk%y_xsi(i,j) = 0.5*(blk%y_grid(i,j) + blk%y_grid(i,j-1) & 
              - blk%y_grid(i-1,j) - blk%y_grid(i-1,j-1))
      END DO
   END DO
   i=imin
   DO j=jmin+1, jmax-1
      blk%x_eta(i,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
      blk%y_eta(i,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
   END DO
   i=imax-1
   DO j=jmin+1, jmax-1
      blk%x_eta(i+1,j) = blk%x_grid(i,j) - blk%x_grid(i,j-1)
      blk%y_eta(i+1,j) = blk%y_grid(i,j) - blk%y_grid(i,j-1)
   END DO
   j = jmin
   DO i=imin+1, imax-1
      blk%x_xsi(i,j) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
      blk%y_xsi(i,j) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
   END DO
   j=jmax-1
   DO i=imin+1, imax-1
      blk%x_xsi(i,j+1) = blk%x_grid(i,j) - blk%x_grid(i-1,j)
      blk%y_xsi(i,j+1) = blk%y_grid(i,j) - blk%y_grid(i-1,j)
   END DO

   blk%x_xsi(imin,:) = blk%x_xsi(imin+1,:)
   blk%x_xsi(imax,:) = blk%x_xsi(imax-1,:)

   blk%y_xsi(imin,:) = blk%y_xsi(imin+1,:)
   blk%y_xsi(imax,:) = blk%y_xsi(imax-1,:)

   blk%x_eta(:,jmin) = blk%x_eta(:,jmin+1)
   blk%x_eta(:,jmax) = blk%x_eta(:,jmax-1)

   blk%y_eta(:,jmin) = blk%y_eta(:,jmin+1)
   blk%y_eta(:,jmax) = blk%y_eta(:,jmax-1)


   blk%hp1 = SQRT(blk%x_xsi**2 + blk%y_xsi**2)
   blk%hp2 = SQRT(blk%x_eta**2 + blk%y_eta**2)
  
   ! compute nonorthogonal part of the metric tensor as a check on grid quality

   blk%gp12 = blk%x_xsi*blk%x_eta + blk%y_xsi*blk%y_eta
	


END SUBROUTINE metrics



!#################################################################################
!---------------------------------------------------------------------------------
!
! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
!
!---------------------------------------------------------------------------------
SUBROUTINE hydro(status_flag)

  IMPLICIT NONE
  
  DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
  DOUBLE PRECISION :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
  DOUBLE PRECISION :: zbot_e, zbot_w, zbot_n, zbot_s, dwsdx
  DOUBLE PRECISION :: flux_e,flux_w,flux_n,flux_s					! fluxes
  DOUBLE PRECISION :: diffu_e,diffu_w,diffu_n,diffu_s			! diffusion
  DOUBLE PRECISION :: pec_e,pec_w,pec_n,pec_s	! peclet numbers
  DOUBLE PRECISION :: apo, cpo								! coefficients in discretization eqns
  DOUBLE PRECISION :: u_p, u_e, u_w, u_s, u_n	! u velocities at P and on staggered grid
  DOUBLE PRECISION :: v_p, v_n, v_s, v_e, v_w	! v velocities at P and on staggered grid
  
  INTEGER :: k, status_flag, x_beg, y_beg
  LOGICAL :: alldry

  DOUBLE PRECISION :: barea, maxdrain, sc, sp

  !-------------------------------------------------------------------------------------------------------
  ! format definitions all placed here
2000 FORMAT(a80)
1000 FORMAT(50(f12.4,2x))
1020 FORMAT(2(f12.4,2x),50(f12.6,2x))
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)


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

        block(iblock)%isdead(:,:)%u = .FALSE.
        block(iblock)%isdead(:,:)%v = .FALSE.
        block(iblock)%isdead(:,:)%p = .FALSE.
        block(iblock)%xsource = 0.0

        ds_flux_given = .FALSE. ! ignore special velocity/flux processing if not needed


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
              DO j=2,y_end
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

        ! compute U momentum discretization equation coefficients
        DO i=x_beg, x_end
           DO j=y_beg,y_end
              hp1 = block(iblock)%hu1(i,j) 
              hp2 = block(iblock)%hu2(i,j)
              he1 = block(iblock)%hp1(i+1,j)
              he2 = block(iblock)%hp2(i+1,j)
              hw1 = block(iblock)%hp1(i,j)
              hw2 = block(iblock)%hp2(i,j)
              hs1 = 0.50*(block(iblock)%hv1(i,j-1) + block(iblock)%hv1(i+1,j-1))
              hs2 = 0.50*(block(iblock)%hv2(i,j-1) + block(iblock)%hv2(i+1,j-1))
              hn1 = 0.50*(block(iblock)%hv1(i,j) + block(iblock)%hv1(i+1,j))
              hn2 = 0.50*(block(iblock)%hv2(i,j) + block(iblock)%hv2(i+1,j))
              
              v_p = 0.25*(block(iblock)%vvel(i,j) + block(iblock)%vvel(i+1,j) &
                   + block(iblock)%vvel(i,j-1) + block(iblock)%vvel(i+1,j-1))
              k_e = block(iblock)%eddy(i,j)   ! replace with geometric weighted k's
              k_w = block(iblock)%eddy(i,j)
              k_n = block(iblock)%eddy(i,j)
              k_s = block(iblock)%eddy(i,j)
              k_p = block(iblock)%eddy(i,j)
              
              depth_e = block(iblock)%depth(i+1,j)
              zbot_e = block(iblock)%zbot(i+1,j)
              depth_w = block(iblock)%depth(i,j)
              zbot_w = block(iblock)%zbot(i,j)
              depth_p = 0.5*(depth_e + depth_w)
              depth_n = 0.25*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j+1) &
                   + block(iblock)%depth(i+1,j)+block(iblock)%depth(i+1,j+1))
              depth_s = 0.25*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j-1) &
                   + block(iblock)%depth(i+1,j)+block(iblock)%depth(i+1,j-1))
              
              IF(j == 2)	depth_s = 0.5*(block(iblock)%depth(i,j-1)+block(iblock)%depth(i+1,j-1))
              IF(j == y_end) depth_n = 0.5*(block(iblock)%depth(i,j+1)+block(iblock)%depth(i+1,j+1))
              
              flux_e = he2*0.5*(block(iblock)%uvel(i,j)+ block(iblock)%uvel(i+1,j))*depth_e
              flux_w = hw2*0.5*(block(iblock)%uvel(i,j)+ block(iblock)%uvel(i-1,j))*depth_w
              flux_n = hn1*0.5*(block(iblock)%vvel(i,j)+ block(iblock)%vvel(i+1,j))*depth_n
              flux_s = hs1*0.5*(block(iblock)%vvel(i,j)+ block(iblock)%vvel(i-1,j))*depth_s
              diffu_e =  2.0*k_e*depth_e*he2/he1
              diffu_w =  2.0*k_w*depth_w*hw2/hw1
              diffu_n =  k_n*depth_n*hn1/hn2
              diffu_s =  k_s*depth_s*hs1/hs2
              pec_e = flux_e/diffu_e
              pec_w = flux_w/diffu_w
              pec_n = flux_n/diffu_n
              pec_s = flux_s/diffu_s
              coeff%ae(i,j) = diffu_e*afunc(pec_e) + max(-flux_e,0.0d0)
              coeff%aw(i,j) = diffu_w*afunc(pec_w) + max(flux_w,0.0d0)
              coeff%an(i,j) = diffu_n*afunc(pec_n) + max(-flux_n,0.0d0)
              coeff%as(i,j) = diffu_s*afunc(pec_s) + max(flux_s,0.0d0)
              apo = hp1*hp2*0.5*(block(iblock)%depthold(i,j)+block(iblock)%depthold(i+1,j))/delta_t

              coeff%source(i,j) = 0.0

              !** U source term wind stress ***
              wind_speed = sqrt(uvel_wind**2 + vvel_wind**2)
              wind_drag_coeff = (0.8 + 0.065*wind_speed)*0.001 ! Wu(1982)
              block(iblock)%windshear1(i,j) = density_air*wind_drag_coeff*uvel_wind*wind_speed
              coeff%source(i,j) = coeff%source(i,j) + hp1*hp2*block(iblock)%windshear1(i,j)/density
              
              ! compute the cross term from bousinesq eddy viscosity this term appears
              !	even in cartesian grids
              cross_term = (depth_n*k_n)*(block(iblock)%vvel(i+1,j) - block(iblock)%vvel(i,j)) &
                   - (depth_s*k_s)*(block(iblock)%vvel(i+1,j-1) - block(iblock)%vvel(i,j-1))
              
              coeff%source(i,j) =  coeff%source(i,j) + cross_term
              
              ! compute all the stuff for the curvature terms in a cartesian grid all
              !	these terms should be zero because the gradients of the metric coeff
              !	will be zero
              ! compute derivatives of metric coeff
              h1_eta_p = 0.5*(block(iblock)%hu1(i,j+1) - block(iblock)%hu1(i,j-1))
              
              h2_xsi_p = block(iblock)%hp2(i+1,j) - block(iblock)%hp2(i,j) 
              
              h1_eta_e = (block(iblock)%hv1(i+1,j) - block(iblock)%hv1(i+1,j-1))
              h1_eta_w = (block(iblock)%hv1(i,j) - block(iblock)%hv1(i,j-1))
              
              IF((j/=2).AND.(j/=y_end))THEN
                 h1_eta_n = block(iblock)%hu1(i,j+1) - block(iblock)%hu1(i,j)
                 h1_eta_s = block(iblock)%hu1(i,j) - block(iblock)%hu1(i,j-1)
                 h2_xsi_n = block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i,j)
                 h2_xsi_s = block(iblock)%hv2(i+1,j-1) - block(iblock)%hv2(i,j-1)
              ELSE IF(j==2)THEN
                 
                 h1_eta_s = 2.0*(block(iblock)%hu1(i,j) - block(iblock)%hu1(i,j-1))
              ELSE IF(j == y_end)THEN
                 h1_eta_n = 2.0*(block(iblock)%hu1(i,j+1) - block(iblock)%hu1(i,j))
                 
              END IF
              
              u_p = block(iblock)%uvel(i,j)
              u_n = 0.5*(block(iblock)%uvel(i,j)+block(iblock)%uvel(i,j+1))
              u_s = 0.5*(block(iblock)%uvel(i,j)+block(iblock)%uvel(i,j-1))
              v_e = 0.5*(block(iblock)%vvel(i+1,j)+block(iblock)%vvel(i+1,j-1))
              v_w = 0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j-1))
              v_n = 0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i+1,j))
              v_s = 0.5*(block(iblock)%vvel(i,j-1)+block(iblock)%vvel(i+1,j-1))
              
              IF(j == y_end)THEN
                 h2_xsi_p = 2.0*h2_xsi_p
                 h1_eta_e = h1_eta_p
              END IF
              
              ! compute each part of the U curvature terms
              curve_1 = -depth_p * u_p * v_p * h1_eta_p
              curve_2 = depth_p * v_p * v_p* h2_xsi_p
              curve_3 = (2.0 * k_e * depth_e * v_e/he1) * h1_eta_e &
                   - (2.0 * k_w * depth_w * v_w/hw1) * h1_eta_w
              curve_4 = -(depth_n * k_n * v_n/hn2) * h2_xsi_n &
                   + (depth_s * k_s * v_s/hs2) * h2_xsi_s
              curve_5 = -(depth_n * k_n * u_n/hn2) * h1_eta_n &
                   + (depth_s * k_s * u_s/hs2) * h1_eta_s
              curve_6 = depth_p*k_p*h1_eta_p*((v_e - v_w)/hp1 - (v_p/(hp1*hp2))*(h2_xsi_p) &
                   + (u_n - u_s)/hp2 - (u_p/(hp1*hp2))*(h1_eta_p))
              curve_7 = -2.0*depth_p*k_p*h2_xsi_p*((v_n - v_s)/hp2 + (u_p/(hp1*hp2))*h2_xsi_p)
              
              coeff%source(i,j) =  coeff%source(i,j) + curve_1 + curve_2 + curve_3 &
                   + curve_4 + curve_5 + curve_6 + curve_7
              ! end of U curvature terms ---------------------------------------------
              
              
              coeff%ap(i,j) = coeff%ae(i,j)+coeff%aw(i,j)+coeff%an(i,j)+coeff%as(i,j) + &
                   &apo

              !** Bed Shear Stress Linearization **

              sc = 0.0
              sp = 0.0
              CALL linear_friction(block(iblock)%chezy(i,j), depth_p,&
                   &block(iblock)%uvel(i,j), v_p, hp1*hp2, sc, sp)

!!$              IF (do_wetdry .AND. depth_p .LE. dry_rewet_depth*2.0) THEN
!!$                 dwsdx = ((depth_e + zbot_e) - (depth_w + zbot_w))/hp1
!!$                 CALL shallow_v_nudge(block(iblock)%chezy(i,j), depth_p,&
!!$                      &block(iblock)%uvel(i,j), v_p, dwsdx, sc, sp)
!!$              END IF

              coeff%source(i,j) = coeff%source(i,j) + sc
              coeff%ap(i,j) = coeff%ap(i,j) + sp

              IF (block(iblock)%isdead(i,j)%u) THEN

                 ! force zero velocity when specified

                 coeff%source(i,j) = coeff%source(i,j) + bigfactor*0.0
                 coeff%ap(i,j) = coeff%ap(i,j) + bigfactor
              ELSE IF (i .EQ. x_beg) THEN
                 SELECT CASE (block(iblock)%cell(i,j)%type)
                 CASE (CELL_BOUNDARY_TYPE)

                    ! adjust for upstream boundary conditions

                    SELECT CASE (block(iblock)%cell(i,j)%bctype)
                    CASE (FLOWBC_ELEV)
                       coeff%ap(i,j) = coeff%ap(i,j) - coeff%aw(i,j)
                       coeff%aw(i,j) = 0.0
                    CASE DEFAULT
                       coeff%source(i,j) = coeff%source(i,j) + &
                            &coeff%aw(i,j)*block(iblock)%uvel(i-1,j)
                       coeff%aw(i,j) = 0.0
                    END SELECT
                 CASE DEFAULT
                    coeff%source(i,j) = coeff%source(i,j) + &
                         &coeff%aw(i,j)*block(iblock)%uvel(i-1,j)
                    coeff%aw(i,j) = 0.0
                 END SELECT
              ELSE IF (i .EQ. x_end) THEN
                 SELECT CASE (block(iblock)%cell(i,j)%type)
                 CASE (CELL_BOUNDARY_TYPE)

                    ! adjust for downstream boundary conditions
                    
                    SELECT CASE (block(iblock)%cell(i,j)%bctype)
                    CASE (FLOWBC_FLOW, FLOWBC_VEL)
                       coeff%source(i,j) = coeff%source(i,j) + &
                            &bigfactor*block(iblock)%uvel(i+1,j)
                       coeff%ap(i,j) = coeff%ap(i,j) + bigfactor
                    CASE (FLOWBC_ELEV)
                       coeff%ap(i,j) = coeff%ap(i,j) + coeff%ae(i,j)
                       coeff%ae(i,j) = 0.0
                    END SELECT
                 CASE DEFAULT
                    coeff%source(i,j) = coeff%source(i,j) + &
                         &coeff%ae(i,j)*block(iblock)%uvel(i+1,j)
                    coeff%ae(i,j) = 0.0
                 END SELECT
              END IF

              coeff%bp(i,j) = coeff%source(i,j) + apo*block(iblock)%uold(i,j) &
                   - 0.5*grav*hp2*(depth_e**2 - depth_w**2) &
                   - grav*hp2*depth_p*(block(iblock)%zbot(i+1,j) - block(iblock)%zbot(i,j))
              
              ! compute and store for use in pressure correction equation
              
              coeff%lud(i,j) = 0.5*grav*hp2*(depth_e+depth_w)/coeff%ap(i,j)

           END DO
        END DO

        ! apply zero gradient condition on
        ! left and right sides

        coeff%ap(:,y_beg) = coeff%ap(:,y_beg) - coeff%as(:,y_beg)
        coeff%as(:,y_beg) = 0.0
  
        coeff%ap(:,y_end) = coeff%ap(:,y_end) - coeff%an(:,y_end)
        coeff%an(:,y_end) = 0.0

        CALL solve_tdma(scalar_sweep, x_beg, x_end, y_beg, y_end, &
             &coeff%ap(x_beg:x_end, y_beg:y_end), coeff%aw(x_beg:x_end, y_beg:y_end), &
             &coeff%ae(x_beg:x_end, y_beg:y_end), coeff%as(x_beg:x_end, y_beg:y_end), &
             &coeff%an(x_beg:x_end, y_beg:y_end), coeff%bp(x_beg:x_end, y_beg:y_end), &
             &block(iblock)%ustar(x_beg:x_end,y_beg:y_end))

        ! end U momentum  solution
        !----------------------------------------------------------------------------
        
        
        !----------------------------------------------------------------------------
        ! V momentum  solution
        
        ! compute V Momentum discretization equation coefficients
        DO i=x_beg,x_end
           DO j=y_beg,y_end-1
              hp1 = block(iblock)%hv1(i,j) 
              hp2 = block(iblock)%hv2(i,j)
              he1 = 0.50*(block(iblock)%hu1(i,j) + block(iblock)%hu1(i,j+1))
              he2 = 0.50*(block(iblock)%hu2(i,j) + block(iblock)%hu2(i,j+1))
              hw1 = 0.50*(block(iblock)%hu1(i-1,j) + block(iblock)%hu1(i-1,j+1))
              hw2 = 0.50*(block(iblock)%hu2(i-1,j) + block(iblock)%hu2(i-1,j+1))
              hs1 = block(iblock)%hp1(i,j)
              hs2 = block(iblock)%hp2(i,j)
              hn1 = block(iblock)%hp1(i,j+1)
              hn2 = block(iblock)%hp2(i,j+1)
              
              u_p = 0.25*(block(iblock)%uvel(i,j) + block(iblock)%uvel(i-1,j) &
                   + block(iblock)%uvel(i,j+1) + block(iblock)%uvel(i-1,j+1))
              
              k_e = block(iblock)%eddy(i,j)   ! replace with geometric weighted k's
              k_w = block(iblock)%eddy(i,j)
              k_n = block(iblock)%eddy(i,j)
              k_s = block(iblock)%eddy(i,j)
              k_p = block(iblock)%eddy(i,j)
              
              depth_n = block(iblock)%depth(i,j+1)
              zbot_n = block(iblock)%zbot(i,j+1)
              depth_s = block(iblock)%depth(i,j)
              zbot_s = block(iblock)%zbot(i,j)
              depth_p = 0.5*(depth_n + depth_s)
              depth_e = 0.25*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j+1) &
                   + block(iblock)%depth(i+1,j)+block(iblock)%depth(i+1,j+1))
              depth_w = 0.25*(block(iblock)%depth(i,j)+block(iblock)%depth(i-1,j) &
                   + block(iblock)%depth(i-1,j)+block(iblock)%depth(i-1,j+1))
              
              IF(i == 2) depth_w = 0.5*(block(iblock)%depth(i-1,j)+block(iblock)%depth(i-1,j+1))
              IF(i == x_end) depth_e = 0.5*(block(iblock)%depth(i+1,j)+block(iblock)%depth(i+1,j+1))
              
              flux_e = he2*0.5*(block(iblock)%uvel(i,j)+ block(iblock)%uvel(i,j+1))*depth_e
              flux_w = hw2*0.5*(block(iblock)%uvel(i-1,j)+ block(iblock)%uvel(i-1,j+1))*depth_w
              flux_n = hn1*0.5*(block(iblock)%vvel(i,j)+ block(iblock)%vvel(i,j+1))*depth_n
              flux_s = hs1*0.5*(block(iblock)%vvel(i,j)+ block(iblock)%vvel(i,j-1))*depth_s
              diffu_e =  2.0*k_e*depth_e*he2/he1
              diffu_w =  2.0*k_w*depth_w*hw2/hw1
              diffu_n =  k_n*depth_n*hn1/hn2
              diffu_s =  k_s*depth_s*hs1/hs2
              pec_e = flux_e/diffu_e
              pec_w = flux_w/diffu_w
              pec_n = flux_n/diffu_n
              pec_s = flux_s/diffu_s
              coeff%ae(i,j) = diffu_e*afunc(pec_e) + max(-flux_e,0.0d0)
              coeff%aw(i,j) = diffu_w*afunc(pec_w) + max(flux_w,0.0d0)
              coeff%an(i,j) = diffu_n*afunc(pec_n) + max(-flux_n,0.0d0)
              coeff%as(i,j) = diffu_s*afunc(pec_s) + max(flux_s,0.0d0)
              apo = hp1*hp2*0.5*(block(iblock)%depthold(i,j)+block(iblock)%depthold(i,j+1))/delta_t
              
              coeff%source(i,j) = 0.0
              
              !** note V source term  wind stress ***
              wind_speed = sqrt(uvel_wind**2 + vvel_wind**2)
              wind_drag_coeff = (0.8 + 0.065*wind_speed)*0.001 ! Wu(1982)
              block(iblock)%windshear2(i,j) = density_air*wind_drag_coeff*vvel_wind*wind_speed
              coeff%source(i,j) = coeff%source(i,j) + hp1*hp2*block(iblock)%windshear2(i,j)/density
              
              
              cross_term = (depth_e*k_e)*(block(iblock)%uvel(i,j+1) - block(iblock)%uvel(i,j)) &
                   - (depth_w*k_w)*(block(iblock)%uvel(i-1,j+1) - block(iblock)%uvel(i-1,j))
              
              coeff%source(i,j) =  coeff%source(i,j) + cross_term
              
              ! compute all the stuff for the curvature terms; in a cartesian grid all
              !	these terms should be zero because the gradients of the metric coeff
              !	will be zero
              ! compute derivatives of metric coeff
              h1_eta_p = block(iblock)%hp1(i,j+1) - block(iblock)%hp1(i,j)
              h2_xsi_p = 0.5*(block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i-1,j))
              
              h2_xsi_n = block(iblock)%hu2(i,j+1) - block(iblock)%hu2(i-1,j+1)
              h2_xsi_s = block(iblock)%hu2(i,j) - block(iblock)%hu2(i-1,j)
!!$              IF((i /=2).AND.(i/=x_end))THEN
                 h2_xsi_e = block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i,j)
                 h2_xsi_w = block(iblock)%hv2(i,j) - block(iblock)%hv2(i-1,j)
                 h1_eta_e = block(iblock)%hu1(i,j+1) - block(iblock)%hu1(i,j)
                 h1_eta_w = block(iblock)%hu1(i-1,j+1) - block(iblock)%hu1(i-1,j)
!!$              ELSE IF(i==2)THEN
!!$                 h2_xsi_w = 2.0*(block(iblock)%hv2(i,j) - block(iblock)%hv2(i-1,j))
!!$              ELSE IF(i==x_end)THEN
!!$                 h2_xsi_e = 2.0*(block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i,j))
!!$                 
!!$              END IF
              
              v_p = block(iblock)%vvel(i,j)
              u_n = 0.5*(block(iblock)%uvel(i-1,j+1)+block(iblock)%uvel(i,j+1))
              u_s = 0.5*(block(iblock)%uvel(i,j)+block(iblock)%uvel(i-1,j))
              u_e = 0.5*(block(iblock)%uvel(i,j)+block(iblock)%uvel(i,j+1))
              u_w = 0.5*(block(iblock)%uvel(i-1,j)+block(iblock)%uvel(i-1,j+1))
              v_e = 0.5*(block(iblock)%vvel(i+1,j)+block(iblock)%vvel(i,j))
              v_w = 0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i-1,j))
              v_n = 0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j+1))
              v_s = 0.5*(block(iblock)%vvel(i,j)+block(iblock)%vvel(i,j-1))
              
              ! compute each part of the curvature terms
              curve_1 = -depth_p * u_p * v_p * h2_xsi_p
              curve_2 = depth_p * u_p * u_p* h1_eta_p
              curve_3 = (2.0 * k_n * depth_n * u_n/hn2) * h2_xsi_n &
                   - (2.0 * k_s * depth_s * u_s/hs1) * h2_xsi_s
              curve_4 = -(depth_e * k_e * v_e/he1) * h2_xsi_e &
                   + (depth_w * k_w * v_w/hw1) * h2_xsi_w
              curve_5 = -(depth_e * k_e * u_e/he1) * h1_eta_e &
                   + (depth_w * k_w * u_w/hw1) * h1_eta_w
              curve_6 = depth_p*k_p*h2_xsi_p*((v_e - v_w)/hp1 - (v_p/(hp1*hp2))*(h2_xsi_p) &
                   + (u_n - u_s)/hp2 - (u_p/(hp1*hp2))*(h1_eta_p))
              curve_7 = -2.0*depth_p*k_p*h1_eta_p*((u_e - u_w)/hp1 + (v_p/(hp1*hp2))*h1_eta_p)
              
              coeff%source(i,j) =  coeff%source(i,j) + curve_1 + curve_2 + curve_3 &
                   + curve_4 + curve_5 + curve_6 + curve_7
              
              ! end of V curvature terms ---------------------------------------------

              
              coeff%ap(i,j) = coeff%ae(i,j)+coeff%aw(i,j)+coeff%an(i,j)+coeff%as(i,j) &
                   + apo


              !** Bed Shear Stress Linearization **

              sc = 0.0
              sp = 0.0
              CALL linear_friction(block(iblock)%chezy(i,j), depth_p, &
                   &u_p, block(iblock)%vvel(i,j), hp1*hp2, sc, sp)

!!$              IF (do_wetdry .AND. depth_p .LE. dry_rewet_depth*2.0) THEN
!!$                 dwsdx = ABS(((depth_n + zbot_n) - (depth_s + zbot_s))/hp2)
!!$                 CALL shallow_v_nudge(block(iblock)%chezy(i,j), depth_p,&
!!$                      &block(iblock)%vvel(i,j), u_p, dwsdx, sc, sp)
!!$              END IF

              coeff%source(i,j) = coeff%source(i,j) + sc
              coeff%ap(i,j) = coeff%ap(i,j) + sp

                                ! force zero velocity when specified

              IF (block(iblock)%isdead(i,j)%v) THEN
                 coeff%source(i,j) = coeff%source(i,j) + bigfactor*0.0
                 coeff%ap(i,j) = coeff%ap(i,j) + bigfactor
              ELSE IF (i .EQ. x_beg) THEN
                 IF (block(iblock)%cell(i,j)%type .EQ. CELL_BOUNDARY_TYPE .OR.&
                      &block(iblock)%cell(i,j+1)%type .EQ. CELL_BOUNDARY_TYPE) THEN

                    ! adjust for upstream boundary conditions (always zero)

                    coeff%ap(i,j) = coeff%ap(i,j) + coeff%aw(i,j)
                    coeff%aw(i,j) = 0.0
                 ELSE 
                    coeff%source(i,j) = coeff%source(i,j) + &
                         &coeff%aw(i,j)*block(iblock)%vvel(i-1,j)
                    coeff%aw(i,j) = 0.0
                 END IF
              ELSE IF (i .EQ. x_end) THEN
                 IF (block(iblock)%cell(i,j)%type .EQ. CELL_BOUNDARY_TYPE .OR.&
                      &block(iblock)%cell(i,j+1)%type .EQ. CELL_BOUNDARY_TYPE) THEN

                    ! adjust for downstream boundary conditions

                    SELECT CASE (block(iblock)%cell(i,j)%bctype)
                    CASE (FLOWBC_FLOW, FLOWBC_VEL, FLOWBC_ELEV)
                       coeff%ap(i,j) = coeff%ap(i,j) - coeff%ae(i,j)
                       coeff%ae(i,j) = 0.0
                    END SELECT
                 ELSE
                    coeff%source(i,j) = coeff%source(i,j) + &
                         &coeff%ae(i,j)*block(iblock)%vvel(i+1,j)
                    coeff%ae(i,j) = 0.0
                 END IF
              END IF

              coeff%bp(i,j) = coeff%source(i,j) + apo*block(iblock)%vold(i,j) &
                   - 0.5*grav*hp1*(depth_n**2 - depth_s**2) &
                   - grav*hp1*depth_p*(block(iblock)%zbot(i,j+1) - block(iblock)%zbot(i,j))

              ! compute and store for use in pressure correction equation
              
              coeff%lvd(i,j) = 0.5*grav*hp1*(depth_n+depth_s)/coeff%ap(i,j)
           END DO
        END DO
        
        ! apply zero flow conditions on sides

        coeff%bp(:,y_beg) = coeff%bp(:,y_beg) + &
             &coeff%as(:,y_beg)*block(iblock)%vvel(:,1)
        coeff%as(:,y_beg) = 0

        coeff%bp(:,y_end-1) = coeff%bp(:,y_end-1) + &
             &coeff%an(:,y_end-1)*block(iblock)%vvel(:,y_end)
        coeff%an(:,y_end) = 0.0

        CALL solve_tdma(scalar_sweep, x_beg, x_end, y_beg, y_end-1, &
             &coeff%ap(x_beg:x_end, y_beg:y_end), coeff%aw(x_beg:x_end, y_beg:y_end), &
             &coeff%ae(x_beg:x_end, y_beg:y_end), coeff%as(x_beg:x_end, y_beg:y_end), &
             &coeff%an(x_beg:x_end, y_beg:y_end), coeff%bp(x_beg:x_end, y_beg:y_end), &
             &block(iblock)%vstar(x_beg:x_end,y_beg:y_end-1))
        
        ! end V momentum  solution
        !----------------------------------------------------------------------------
        
        ! update ustar, vstar if we have given velocity/flux conditions
        IF(ds_flux_given)THEN
           ! loop over the total number of bc specifications
           DO num_bc = 1, block_bc(iblock)%num_bc
              CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
                   &.TRUE., ds_flux_given)
           END DO
        END IF
        
        !----------------------------------------------------------------------------
        ! solve depth correction equation
        
        ! compute coefficients in the depth correction equation
        coeff%ce = 0.0
        coeff%cw = 0.0
        coeff%cn = 0.0
        coeff%cs = 0.0
        coeff%cp = 0.0
        DO i=2,x_end
           DO j=2,y_end
              hp1 = block(iblock)%hp1(i,j)
              hp2 = block(iblock)%hp2(i,j)
              he1 = block(iblock)%hu1(i,j)
              he2 = block(iblock)%hu2(i,j)
              hw1 = block(iblock)%hu1(i-1,j)
              hw2 = block(iblock)%hu2(i-1,j)
              hs1 = block(iblock)%hv1(i,j-1)
              hs2 = block(iblock)%hv2(i,j-1)
              hn1 = block(iblock)%hv1(i,j)
              hn2 = block(iblock)%hv2(i,j)
              
              depth_e = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i+1,j))
              depth_w = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i-1,j))
              depth_n = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j+1))
              depth_s = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j-1))
              
!!$              IF(i == 2)			depth_w = block(iblock)%depth(i-1,j)
!!$              IF(i == x_end)	depth_e = block(iblock)%depth(i+1,j)
              IF(j == 2)			depth_s = block(iblock)%depth(i,j-1)
              IF(j == y_end)	depth_n = block(iblock)%depth(i,j+1)
              
              flux_e = he2*block(iblock)%ustar(i,j)*depth_e
              flux_w = hw2*block(iblock)%ustar(i-1,j)*depth_w
              flux_n = hn1*block(iblock)%vstar(i,j)*depth_n
              flux_s = hs1*block(iblock)%vstar(i,j-1)*depth_s
              
              cpo = hp1*hp2/delta_t
              block(iblock)%mass_source(i,j) = &
                   &cpo*(block(iblock)%depthold(i,j) - block(iblock)%depth(i,j)) + &
                   &flux_w - flux_e + flux_s - flux_n 
              
              coeff%ce(i,j) = he2*depth_e*coeff%lud(i,j)
              coeff%cw(i,j) = hw2*depth_w*coeff%lud(i-1,j)
              coeff%cn(i,j) = hn1*depth_n*coeff%lvd(i,j)
              coeff%cs(i,j) = hs1*depth_s*coeff%lvd(i,j-1)
              coeff%cp(i,j) = coeff%ce(i,j) + coeff%cw(i,j) + coeff%cn(i,j) + coeff%cs(i,j) &
                   + cpo

              coeff%bp(i,j) = block(iblock)%mass_source(i,j) + &
                   &block(iblock)%xsource(i,j)*hp1*hp2
              ! block(iblock)%mass_source(i,j) = block(iblock)%mass_source(i,j) + &
              !      &block(iblock)%xsource(i,j)*hp1*hp2

              IF (block(iblock)%isdead(i,j)%p) THEN
                 coeff%bp(i,j) = coeff%bp(i,j) + bigfactor*0.0
                 coeff%cp(i,j) = coeff%cp(i,j) + bigfactor
              ELSE IF (i .EQ. x_beg) THEN
                 SELECT CASE (block(iblock)%cell(i,j)%type)
                 CASE (CELL_BOUNDARY_TYPE)

                    ! adjust for upstream boundary conditions (always zero)

                    SELECT CASE (block(iblock)%cell(i,j)%bctype)
                    CASE (FLOWBC_FLOW, FLOWBC_VEL)
                       coeff%cp(i,j) = coeff%cp(i,j) - coeff%cw(i,j)
                       coeff%cw(i,j) = 0.0
                    CASE (FLOWBC_ELEV)
                       coeff%cp(i,j) = coeff%cp(i,j) + coeff%cw(i,j)
                       coeff%cw(i,j) = 0.0
                    END SELECT
                 CASE DEFAULT
                    coeff%bp(i,j) = coeff%bp(i,j) +&
                         &coeff%cw(i,j)*block(iblock)%dp(i-1,j)
                    coeff%cw(i,j) = 0.0
                 END SELECT
              ELSE IF (i .EQ. x_end) THEN
                 SELECT CASE (block(iblock)%cell(i,j)%type)
                 CASE (CELL_BOUNDARY_TYPE)

                    ! adjust for downstream boundary conditions

                    SELECT CASE (block(iblock)%cell(i,j)%bctype)
                    CASE (FLOWBC_FLOW, FLOWBC_VEL)
                       coeff%ce(i,j) = 0.0
                    END SELECT
                 CASE DEFAULT
                    coeff%bp(i,j) = coeff%bp(i,j) +&
                         &coeff%ce(i,j)*block(iblock)%dp(i+1,j)
                    coeff%ce(i,j) = 0.0
                 END SELECT
              END IF
           END DO
        END DO

                                ! apply zero gradient on the sides
  
        coeff%cp(:,y_beg) = coeff%cp(:,y_beg) - coeff%cs(:,y_beg)
        coeff%cs(:,y_beg) = 0.0
        coeff%cp(:,y_end) = coeff%cp(:,y_end) - coeff%cn(:,y_end)
        coeff%cn(:,y_end) = 0.0

        CALL solve_tdma(depth_sweep, x_beg, x_end, y_beg, y_end, &
             &coeff%cp(x_beg:x_end,y_beg:y_end), coeff%cw(x_beg:x_end,y_beg:y_end), &
             &coeff%ce(x_beg:x_end,y_beg:y_end), coeff%cs(x_beg:x_end,y_beg:y_end), &
             &coeff%cn(x_beg:x_end,y_beg:y_end), &
             &coeff%bp(x_beg:x_end,y_beg:y_end), &
             &block(iblock)%dp(x_beg:x_end,y_beg:y_end))

        ! compute updated depth with some underrelaxation
        ! depth = rel*depth_new + (1 - rel)*depth_old
        IF(update_depth)THEN
           DO i=2,x_end
              DO j=2,y_end
                 block(iblock)%depth(i,j) = block(iblock)%depth(i,j) + relax_dp*block(iblock)%dp(i,j)
              END DO
           END DO
        ENDIF
        

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
        
        !----------------------------------------------------------------------------
        ! Apply velocity corrections using the depth correction field
        DO i=2,x_end
           DO j=2,y_end
              block(iblock)%uvel(i,j) = block(iblock)%ustar(i,j) &
                   + coeff%lud(i,j)*(block(iblock)%dp(i,j)-block(iblock)%dp(i+1,j))
              
              block(iblock)%ustar(i,j) = block(iblock)%uvel(i,j)
           END DO
        END DO
        
        DO i=2,x_end
           DO j=2,y_end-1
              block(iblock)%vvel(i,j) = block(iblock)%vstar(i,j) &
                   + coeff%lvd(i,j)*(block(iblock)%dp(i,j)-block(iblock)%dp(i,j+1))
              
              block(iblock)%vstar(i,j) = block(iblock)%vvel(i,j)
           END DO
        END DO
        
        ! end application of velocity correction
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! update depth if we have given velocity/flux conditions
!!$        IF(ds_flux_given)THEN
!!$           ! loop over the total number of bc specifications
!!$           DO num_bc = 1, block_bc(iblock)%num_bc
!!$              
!!$              
!!$              IF (block_bc(iblock)%bc_spec(num_bc)%bc_loc=="DS") THEN
!!$                 SELECT CASE (block_bc(iblock)%bc_spec(num_bc)%bc_type)
!!$                 CASE ("TABLE")
!!$                 
!!$                    CALL table_interp(current_time%time,&
!!$                         & block_bc(iblock)%bc_spec(num_bc)%table_num,&
!!$                         & table_input, block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
!!$                    SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
!!$                    CASE("VELO","FLUX") 
!!$                       
!!$                       DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
!!$                          j_dsflux_start = block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
!!$                          j_dsflux_end	 = block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
!!$                          CALL extrapolate_depth(block(iblock), x_end+1, &
!!$                               &j_dsflux_start, j_dsflux_end, level = .FALSE.)
!!$                       END DO
!!$                    END SELECT
!!$                 END SELECT
!!$              END IF
!!$           END DO
!!$        END IF
        
        !----------------------------------------------------------------------------
        ! extrapolate water surface elevation to upstream *** NEED TO IMPLEMENT LINEAR EXTRAPOLATION ****
        ! equal water surface elevation - better for bathymetry; not great for uniform slope channel
        
!!$        block(iblock)%depth(1,2:y_end) = (block(iblock)%depth(2,2:y_end) + block(iblock)%zbot(2,2:y_end)) &
!!$             + 0.5*((block(iblock)%depth(2,2:y_end)+block(iblock)%zbot(2,2:y_end)) -&
!!$             & (block(iblock)%depth(3,2:y_end)+block(iblock)%zbot(3,2:y_end))) - block(iblock)%zbot(1,2:y_end)

!!$        i = 1
!!$        DO j = 2, y_end
!!$           CALL extrapolate_depth(block(iblock), i, j, j, level = .FALSE.)
!!$           IF (do_wetdry) block(iblock)%depth(i,j) = MAX(dry_zero_depth, block(iblock)%depth(i,j))
!!$        END DO
        
        !-----------------------------------------------------------------------------
        ! apply zero gradient conditions
        
        !IF((iblock == 2).OR.(max_blocks == 1))THEN
        !   block(iblock)%uvel(x_end+1,2:y_end) = block(iblock)%ustar(x_end,2:y_end)
        !ELSE
        !	block(iblock)%uvel(x_end+1,2:y_end) = block(iblock+1)%uvel(1,2:y_end)
        !ENDIF
        
        ! zero gradient conditions on the sides of the block
        !		need to modify for side inflows/block connections
        
        block(iblock)%uvel(1:x_end+1,1) = block(iblock)%ustar(1:x_end+1,2)
        block(iblock)%uvel(1:x_end+1,y_end+1) = block(iblock)%ustar(1:x_end+1,y_end)
        block(iblock)%depth(1:x_end+1,1) = (block(iblock)%depth(1:x_end+1,2) +&
             &block(iblock)%zbot(1:x_end+1,2)) - block(iblock)%zbot(1:x_end+1,1)
        block(iblock)%depth(1:x_end+1,y_end+1) = (block(iblock)%depth(1:x_end+1,y_end) +&
             &block(iblock)%zbot(1:x_end+1,y_end)) - block(iblock)%zbot(1:x_end+1,y_end+1)
        DO i=1,x_end+1
           IF (do_wetdry) THEN
              IF (block(iblock)%depth(i,1) .LT. dry_zero_depth) &
                   &block(iblock)%depth(i,1)  = dry_zero_depth
              IF (block(iblock)%depth(i,y_end+1) .LT. dry_zero_depth) &
                   &block(iblock)%depth(i,y_end+1) = dry_zero_depth
           END IF
        END DO

        
        !----------------------------------------------------------------------------------------------
        ! check for small and negative depth condition and report location
        
        DO i = 1, x_end+1
           DO j = 1, y_end+1
              IF (.NOT. do_wetdry .AND. block(iblock)%depth(i,j) <= 0.20) THEN
                 WRITE(error_iounit,*)" WARNING: Small Depth = ", block(iblock)%depth(i,j)
                 WRITE(error_iounit,*)"     Block Number = ",iblock
                 WRITE(error_iounit,*)"     I,J Location of small depth = ",i, j
              ELSE IF (block(iblock)%depth(i,j) <= 0.0) THEN
                 IF (do_wetdry) THEN
                    WRITE(error_iounit,*)" ERROR: Negative Depth = ",block(iblock)%depth(i,j)
                    WRITE(error_iounit,*)"     Simulation Time: ", current_time%date_string, " ", current_time%time_string
                    WRITE(error_iounit,*)"     Block Number = ",iblock
                    WRITE(error_iounit,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
                    
                    WRITE(*,*)" ERROR: Negative Depth = ",block(iblock)%depth(i,j)
                    WRITE(*,*)"     Simulation Time: ", current_time%date_string, " ", current_time%time_string
                    WRITE(*,*)"     Block Number = ",iblock
                    WRITE(*,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
                       
                    block(iblock)%depth(i,j) = dry_zero_depth
                 ELSE

                    WRITE(error_iounit,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
                    WRITE(error_iounit,*)"     Block Number = ",iblock
                    WRITE(error_iounit,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)
           
                    WRITE(*,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
                    WRITE(*,*)"     Block Number = ",iblock
                    WRITE(*,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)
           
                    CALL EXIT(1)  ! abort run if you hit a negative depth

                 END IF
              END IF
           END DO
        END DO
!!$        IF(.NOT. do_wetdry .AND. MINVAL(block(iblock)%depth) <= 0.20)THEN
!!$           WRITE(error_iounit,*)" WARNING: Small Depth = ",MINVAL(block(iblock)%depth)
!!$           WRITE(error_iounit,*)"     Block Number = ",iblock
!!$           WRITE(error_iounit,*)"     I,J Location of small depth = ",MINLOC(block(iblock)%depth)
!!$        END IF
!!$
!!$        IF(MINVAL(block(iblock)%depth) <= 0.0)THEN
!!$           IF (do_wetdry) THEN
!!$              DO i = 1,x_end+1
!!$                 DO j = 1,y_end+1
!!$                    IF (block(iblock)%depth(i,j) .LT. 0.0) THEN
!!$                       WRITE(error_iounit,*)" ERROR: Negative Depth = ",block(iblock)%depth(i,j)
!!$                       WRITE(error_iounit,*)"     Simulation Time: ", current_time%date_string, " ", current_time%time_string
!!$                       WRITE(error_iounit,*)"     Block Number = ",iblock
!!$                       WRITE(error_iounit,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
!!$           
!!$                       WRITE(*,*)" ERROR: Negative Depth = ",block(iblock)%depth(i,j)
!!$                       WRITE(*,*)"     Simulation Time: ", current_time%date_string, " ", current_time%time_string
!!$                       WRITE(*,*)"     Block Number = ",iblock
!!$                       WRITE(*,*)"     I,J Location of negative depth = (", i, ", ", j, ")"
!!$                       
!!$                       block(iblock)%depth(i,j) = dry_zero_depth
!!$                    END IF
!!$                 END DO
!!$              END DO
!!$           ELSE
!!$              WRITE(error_iounit,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
!!$              WRITE(error_iounit,*)"     Block Number = ",iblock
!!$              WRITE(error_iounit,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)
!!$           
!!$              WRITE(*,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
!!$              WRITE(*,*)"     Block Number = ",iblock
!!$              WRITE(*,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)
!!$           
!!$              CALL EXIT(1)  ! abort run if you hit a negative depth
!!$           END IF
!!$        END IF
        
        !----------------------------------------------------------------------------------------------
        ! return to momentum equation solution step using updated depth and velocities
        !----------------------------------------------------------------------------------------------

        IF (do_wetdry .AND. iterate_wetdry)&
             &CALL check_wetdry(iblock, block(iblock)%xmax, block(iblock)%ymax)

     END DO		! block loop end
     
     !-----------------------------------------------------------------------------------
     ! check to see if mass source has been reduced below tolerance and break out of loop
     maxx_mass = 0
     DO iblock=1,max_blocks
        IF(SUM(ABS(block(iblock)%mass_source)) >= maxx_mass) maxx_mass = SUM(ABS(block(iblock)%mass_source))
     END DO
     IF(maxx_mass < max_mass_source_sum) EXIT ! break out of internal iteration loop
     
     !------------------------------------------------------------------------
     
  END DO    ! internal time iteration loop for momentum, depth correction equations
  
  IF (do_wetdry .AND. .NOT. iterate_wetdry) THEN
     DO iblock = 1, max_blocks
        CALL check_wetdry(iblock, block(iblock)%xmax + 1, block(iblock)%ymax + 1)
     END DO
  END IF

  CALL bedshear()

END SUBROUTINE hydro		

! ----------------------------------------------------------------
! SUBROUTINE apply_hydro_bc
! ----------------------------------------------------------------
SUBROUTINE apply_hydro_bc(blk, bc, dsonly, ds_flux_given)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  TYPE (bc_spec_struct) :: bc
  LOGICAL, INTENT(IN) :: dsonly
  LOGICAL, INTENT(INOUT) :: ds_flux_given

  INTEGER :: x_end, y_end, i, j, k, jj, con_i, con_j, j_beg, j_end
  DOUBLE PRECISION :: input_total

  x_end = blk%xmax
  y_end = blk%ymax

  IF (.NOT. dsonly) ds_flux_given = .FALSE.

  SELECT CASE(bc%bc_loc)
              
  CASE("US")
     i=1
     SELECT CASE(bc%bc_type)
     CASE("BLOCK")
        IF (dsonly) RETURN
        SELECT CASE(bc%bc_kind)
        CASE("VELO")
           con_block = bc%con_block
           DO j=1,bc%num_cell_pairs
              con_j = bc%con_start_cell(j) + 1
              con_i = block(con_block)%xmax
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%uvel(i,jj) = block(con_block)%uvel(con_i, con_j)
                 blk%uold(i,jj) = block(con_block)%uold(con_i, con_j)
                 blk%ustar(i,jj) = block(con_block)%ustar(con_i, con_j)
                 blk%vvel(i,jj) = block(con_block)%vvel(con_i, con_j)
                 blk%vold(i,jj) = block(con_block)%vold(con_i, con_j)
                 blk%vstar(i,jj) = block(con_block)%vstar(con_i, con_j)
                 blk%depth(i,jj) = block(con_block)%depth(con_i, con_j)
                 blk%depthold(i,jj) = block(con_block)%depthold(con_i, con_j)
                 blk%dstar(i,jj) = block(con_block)%dstar(con_i, con_j)
                 blk%isdry(i,jj) = block(con_block)%isdry(con_i, con_j)
                 blk%dp(i,jj) = 0.0 ! depth in other block needs no correction
                 con_j = con_j + 1
              END DO
              ! blk%cell(i+1,j_beg:j_end)%type = CELL_NORMAL_TYPE
           END DO
        END SELECT
        
     CASE("TABLE")
        CALL table_interp(current_time%time, bc%table_num, table_input, &
             &bc%num_cell_pairs)
        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              CALL extrapolate_depth(blk, i, j_beg, j_end, level=.FALSE.)
              CALL compute_uflow_area(blk, i, j_beg, j_end, inlet_area, input_total)
              DO jj = j_beg, j_end
                 IF (inlet_area(jj) .GT. 0.0) THEN
                    blk%uvel(i,jj) = table_input(j)/input_total
                 ELSE 
                    blk%uvel(i,jj) = 0.0
                 END IF
              END DO
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
              blk%vvel(i, j_beg:j_end) = 0.0
              blk%vstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
              blk%vold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
              blk%cell(i+1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%bctype = FLOWBC_FLOW
              IF (dsonly) coeff%lud(i+1,j) = 0.0
           END DO
           
        CASE("VELO")
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              CALL extrapolate_depth(blk, x_end+1, j_beg, j_end, level = .FALSE.)
              blk%uvel(i,j_beg:j_end) = table_input(j)
              blk%vvel(i, j_beg:j_end) = 0.0
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) =  blk%uvel(i,j_beg:j_end)
              blk%vstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
              blk%vold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
              blk%cell(i+1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%bctype = FLOWBC_VEL
              IF (dsonly) coeff%lud(i+1,j) = 0.0
           END DO
           
        CASE("ELEV")
           IF (dsonly) RETURN
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%depth(i,jj) = 2*table_input(j) - &
                      &(blk%depth(i+1,jj) + blk%zbot(i+1,jj)) - blk%zbot(i,jj)
                 IF (do_wetdry) blk%depth(i,jj) =  &
                         &MAX(blk%depth(i,jj), dry_zero_depth)
              END DO
              blk%dstar(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%depthold(i,j_beg:j_end) = blk%depth(i,j_beg:j_end)
              blk%uvel(i,j_beg:j_end) = blk%uvel(i+1,j_beg:j_end)
              blk%vvel(i,j_beg:j_end) = 0.0
              blk%cell(i+1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i+1,j_beg:j_end)%bctype = FLOWBC_ELEV
           END DO
        END SELECT
     END SELECT
     
  CASE("DS")
     i = x_end+1
     SELECT CASE(bc%bc_type)
     CASE("BLOCK")
        IF (dsonly) RETURN
        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
        CASE("ELEV")
           con_block = bc%con_block
           DO j=1,bc%num_cell_pairs
              con_i = 2
              con_j = bc%con_start_cell(j) + 1
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%uvel(i,jj) = block(con_block)%uvel(con_i, con_j)
                 blk%uold(i,jj) = block(con_block)%uold(con_i, con_j)
                 blk%ustar(i,jj) = block(con_block)%ustar(con_i, con_j)
                 blk%vvel(i,jj) = block(con_block)%vvel(con_i, con_j)
                 blk%vold(i,jj) = block(con_block)%vold(con_i, con_j)
                 blk%vstar(i,jj) = block(con_block)%vstar(con_i, con_j)
                 blk%depth(i, jj) = block(con_block)%depth(con_i, con_j)
                 blk%dstar(i, jj) = block(con_block)%dstar(con_i, con_j)
                 blk%depthold(i, jj) = block(con_block)%depthold(con_i, con_j)
                 blk%isdry(i, jj) =  block(con_block)%isdry(con_i, con_j)
                 blk%dp(i,jj) = 0.0 ! depth in other block needs no correction
                 con_j = con_j + 1
              END DO
              ! Xblk%cell(i-1,j_beg:j_end)%type = CELL_NORMAL_TYPE
           END DO
        END SELECT
        
     CASE("TABLE")
        CALL table_interp(current_time%time, bc%table_num, table_input, &
             &bc%num_cell_pairs)
        SELECT CASE(bc%bc_kind)
           
           !--------------------------------------------------------------------------
           !*** reusing inlet_area and inlet_flow variables here for outlet conditions
           
        CASE("FLUX") ! can specify the outflow discharge; need to convert to velocity
           ds_flux_given = .TRUE.
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end = bc%end_cell(j)+1
              
              CALL extrapolate_depth(blk, i, j_beg, j_end, level = .FALSE.)
              CALL compute_uflow_area(blk, x_end, j_beg, j_end, inlet_area, input_total)
              DO jj=j_beg, j_end
                 IF (inlet_area(jj) .GT. 0.0) THEN
                    blk%uvel(i,jj) =  table_input(j)/input_total
                 ELSE 
                    blk%uvel(i,jj) = 0.0
                 END IF
              END DO
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%vvel(i, j_beg:j_end) = 0.0
              blk%vstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
              blk%vold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
              blk%cell(i-1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%bctype = FLOWBC_FLOW
              IF (dsonly) coeff%lud(i-1,j) = 0.0
           END DO

        CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
           ds_flux_given = .TRUE.
           
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end	 = bc%end_cell(j)+1
              CALL extrapolate_depth(blk, i, j_beg, j_end, level = .FALSE.)
              blk%uvel(i,j_beg:j_end) = table_input(j)
              blk%ustar(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%uold(i,j_beg:j_end) = blk%uvel(i,j_beg:j_end)
              blk%vvel(i, j_beg:j_end) = 0.0
              blk%vstar(i,j_beg:j_end) = blk%vvel(i,j_beg:j_end)
              blk%vold(i,j_beg:j_end)  = blk%vvel(i,j_beg:j_end)
              blk%cell(i-1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%bctype = FLOWBC_VEL
              IF (dsonly) coeff%lud(i-1,j) = 0.0
           END DO
                    
        CASE("ELEV")
           IF (dsonly) RETURN
           DO j=1,bc%num_cell_pairs
              j_beg = bc%start_cell(j)+1
              j_end	 = bc%end_cell(j)+1
              DO jj = j_beg, j_end
                 blk%depth(i,jj) = table_input(j) - blk%zbot(i,jj)
                 IF (do_wetdry) blk%depth(i,jj) =  &
                      &MAX(blk%depth(i,jj), dry_zero_depth)
              END DO
              blk%dstar(i,jj) = blk%depth(i,jj)
              blk%depthold(i,jj) = blk%depth(i,jj)
              blk%uvel(i,j_beg:j_end) = blk%uvel(i-1,j_beg:j_end)
              blk%vvel(i, j_beg:j_end) = blk%vvel(i-1, j_beg:j_end)
              blk%cell(i-1,j_beg:j_end)%type = CELL_BOUNDARY_TYPE
              blk%cell(i-1,j_beg:j_end)%bctype = FLOWBC_ELEV
           END DO
        END SELECT
        
     END SELECT
     
     ! these are dummied in for later implementation
  CASE("RIGHT")
     IF (dsonly) RETURN
     SELECT CASE(bc%bc_type)
     CASE("BLOCK")
        SELECT CASE(bc%bc_kind)
        CASE("MATCH")
           
        END SELECT
        
     CASE("TABLE")
        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
        CASE("VELO")
        CASE("ELEV")
           
        END SELECT
        
     END SELECT
     
  CASE("LEFT")
     IF (dsonly) RETURN
     SELECT CASE(bc%bc_type)
     CASE("BLOCK")
        SELECT CASE(bc%bc_kind)
        CASE("MATCH")
           
        END SELECT
        
     CASE("TABLE")
        SELECT CASE(bc%bc_kind)
        CASE("FLUX")
        CASE("VELO")
        CASE("ELEV")
        END SELECT
        
     END SELECT
              
  CASE ("IN") 
     IF (dsonly) RETURN
     SELECT CASE(bc%bc_type)
        
     CASE ("SOURCE", "SINK")
        
        CALL table_interp(current_time%time,&
             & bc%table_num,&
             & table_input, 1)
        
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
                 IF (do_wetdry .AND. .NOT. blk%isdry(i+1,j+1)) &
                      &input_total = input_total + blk%hp1(i+1,j+1)*blk%hp2(i+1,j+1)
              END DO
           END DO
        CASE ("VELO")
           ! if a rate is specified, do not alter
           ! the table value
           input_total = 1.0
        END SELECT
        
        DO i = bc%start_cell(1), bc%end_cell(1)
           DO j = bc%start_cell(2), bc%end_cell(2)
              IF (do_wetdry .AND. .NOT. blk%isdry(i+1,j+1)) &
                   &blk%xsource(i+1, j+1) = table_input(1)/input_total
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
        
     END SELECT
  END SELECT
END SUBROUTINE apply_hydro_bc
! ----------------------------------------------------------------
! SUBROUTINE compute_uflow_area
! ----------------------------------------------------------------
SUBROUTINE compute_uflow_area(blk, i, jmin, jmax, area, total)

  IMPLICIT NONE
  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: i, jmin, jmax
  DOUBLE PRECISION, INTENT(OUT) :: area(:), total

  INTEGER :: j
  DOUBLE PRECISION :: d, w
  
  area = 0.0
  DO j = jmin, jmax
     IF (do_wetdry .AND. blk%depth(i,j) .LE. dry_depth) THEN
        area(j) = 0.0
     ELSE 
        area(j) = blk%depth(i,j)*blk%hu2(i,j)
     END IF
  END DO
  total = SUM(area)
END SUBROUTINE compute_uflow_area

! ----------------------------------------------------------------
! SUBROUTINE extrapolate_depth
! ----------------------------------------------------------------
SUBROUTINE extrapolate_depth(blk, i, jmin, jmax, level)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  INTEGER, INTENT(IN) :: i, jmin, jmax
  LOGICAL, INTENT(IN), OPTIONAL :: level

  INTEGER :: ioff, j
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
END SUBROUTINE extrapolate_depth

! ----------------------------------------------------------------
! SUBROUTINE linear_friction Linearization of the friction term.
! ustar is the (previous) velocity in the direction we are interested
! in; vstar is the (previous) velocity in the other direction 
!
! WARNING: values of sc and sp are intentionally incremented.
! ----------------------------------------------------------------
SUBROUTINE linear_friction(c, depth, ustar, vstar, harea, sc, sp)

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: c, depth, ustar, vstar, harea
  DOUBLE PRECISION, INTENT(INOUT) :: sc, sp

  DOUBLE PRECISION :: roughness, vmagstar, cterm, pterm

  ! Also uses: grav, density

  IF(manning)THEN
     IF (do_wetdry) THEN
        roughness = (grav*c**2)/(mann_con*MAX(depth, dry_depth)**0.3333333)
     ELSE
        roughness = (grav*c**2)/(mann_con*depth**0.3333333)
     END IF
  ELSE
     roughness = c
  END IF
  vmagstar = sqrt(ustar*ustar + vstar*vstar)

  ! Alternative: linearize by making the source term all constant:

  ! cterm = - harea*roughness*ustar*vmagstar
  ! pterm = 0.0
  
  ! Alternative: linearize by using current ustar and previous
  ! estimate of velocity magnitude
  
  cterm = 0.0
  pterm = - harea*roughness*vmagstar
  
  ! Alternative: Taylor Series expansion -- only good for vmag > 0.0,
  
  ! IF (vmagstar > 1.0d-20) THEN
  !    cterm = harea*roughness*(ustar**3.0)/vmagstar
  !    pterm = - harea*roughness*(ustar*ustar/vmagstar + vmagstar)
  ! ELSE 
  !    cterm = 0.0
  !    pterm = harea*roughness*vmagstar
  ! END IF
              
  sc = sc + cterm
  sp = sp - pterm

END SUBROUTINE linear_friction

! ----------------------------------------------------------------
! SUBROUTINE shallow_v_nudge
! ----------------------------------------------------------------
SUBROUTINE shallow_v_nudge(c, depth, ustar, vstar, dwsdx, sc, sp)

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: c, depth, ustar, vstar, dwsdx
  DOUBLE PRECISION, INTENT(INOUT) :: sc, sp

  DOUBLE PRECISION :: vmagstar, sf, utilde, cterm, pterm, sstar

  DOUBLE PRECISION, PARAMETER :: mfactor = 2.0

  sstar = sp*ustar + sc
  vmagstar = sqrt(ustar*ustar + vstar*vstar)

  IF (manning) THEN
     sf = c**2.0/mann_con*(ustar*vmagstar/(depth**(4/3)))
  ELSE
     sf = (ustar*vmagstar)/depth/c**2.0
  END IF

  sf = ABS(sf)

  IF (sf .GE. mfactor*ABS(dwsdx) .AND. sf .GT. 0.0) THEN
     utilde = ustar/vmagstar*SQRT(ABS(dwsdx))
     ! utilde = ustar/ABS(ustar)*SQRT(ABS(dwsdx))
     IF (manning) THEN
        utilde = utilde * c*depth**(2/3)/SQRT(mann_con)
     ELSE
        utilde = utilde * c*SQRT(depth)
     END IF
     
     IF (ABS(utilde - ustar) .GT. 0.0) THEN 
        cterm = (sstar*utilde)/(utilde - ustar)
        pterm = sstar/(utilde - ustar)

        sc = cterm
        sp = -pterm
     END IF
  END IF

END SUBROUTINE shallow_v_nudge


! ----------------------------------------------------------------
! SUBROUTINE transport_precalc
! This routine precalculates various hydrodynamic values needed
! ----------------------------------------------------------------
SUBROUTINE transport_precalc()

  IMPLICIT NONE

  DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
  DOUBLE PRECISION :: xdiffp, ydiffp

  DO iblock = 1, max_blocks
     x_end = block(iblock)%xmax
     y_end = block(iblock)%ymax

     i = 1
     block(iblock)%inlet_area(2:y_end) = block(iblock)%depth(i,2:y_end)*block(iblock)%hu2(i,2:y_end)

     

     block(iblock)%k_e = 0.0
     block(iblock)%k_w = 0.0
     block(iblock)%k_n = 0.0
     block(iblock)%k_s = 0.0
     DO i= 2,x_end
        DO j=2,y_end

           hp1 = block(iblock)%hp1(i,j)
           hp2 = block(iblock)%hp2(i,j)
           he1 = block(iblock)%hu1(i,j)
           he2 = block(iblock)%hu2(i,j)
           hw1 = block(iblock)%hu1(i-1,j)
           hw2 = block(iblock)%hu2(i-1,j)
           hs1 = block(iblock)%hv1(i,j-1)
           hs2 = block(iblock)%hv2(i,j-1)
           hn1 = block(iblock)%hv1(i,j)
           hn2 = block(iblock)%hv2(i,j)
           
                                ! do not allow diffusion in dead cells 

           IF (block(iblock)%isdead(i,j)%p) THEN
              xdiffp = 0.0
              ydiffp = 0.0
           ELSE 
              xdiffp = block(iblock)%kx_diff(i,j)
              ydiffp = block(iblock)%ky_diff(i,j)
           END IF

                                ! use the harmonic, rather than
                                ! arithmatic, mean of diffusivity

           IF (xdiffp .GT. 0.0) THEN
              IF (.NOT. block(iblock)%isdead(i,j)%u) THEN
                 block(iblock)%k_e(i,j) = &
                      &2.0*xdiffp*block(iblock)%kx_diff(i+1,j)/&
                      &(xdiffp + block(iblock)%kx_diff(i+1,j))
              END IF
              IF (.NOT. block(iblock)%isdead(i-1,j)%u) THEN
                 block(iblock)%k_w(i,j) = &
                      &2.0*xdiffp*block(iblock)%kx_diff(i-1,j)/&
                      &(xdiffp + block(iblock)%kx_diff(i-1,j))
              END IF
           END IF

           IF (ydiffp .GT. 0.0) THEN
              IF (.NOT. block(iblock)%isdead(i,j)%v) THEN
                 block(iblock)%k_n(i,j) = &
                      &2.0*ydiffp*block(iblock)%ky_diff(i,j+1)/&
                      &(ydiffp + block(iblock)%ky_diff(i,j+1))
              END IF
              IF (.NOT. block(iblock)%isdead(i,j-1)%v) THEN
                 block(iblock)%k_s(i,j) = &
                      &2.0*ydiffp*block(iblock)%ky_diff(i,j-1)/&
                      &(ydiffp + block(iblock)%ky_diff(i,j-1))
              END IF
           END IF

           ! block(iblock)%k_e(i,j) = 0.5*(block(iblock)%kx_diff(i,j)+block(iblock)%kx_diff(i+1,j))
           ! block(iblock)%k_w(i,j) = 0.5*(block(iblock)%kx_diff(i,j)+block(iblock)%kx_diff(i-1,j))
           ! block(iblock)%k_n(i,j) = 0.5*(block(iblock)%ky_diff(i,j)+block(iblock)%ky_diff(i,j+1))
           ! block(iblock)%k_s(i,j) = 0.5*(block(iblock)%ky_diff(i,j)+block(iblock)%ky_diff(i,j-1))

           block(iblock)%depth_e(i,j) = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i+1,j))
           block(iblock)%depth_w(i,j) = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i-1,j))
           block(iblock)%depth_n(i,j) = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j+1))
           block(iblock)%depth_s(i,j) = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j-1))

           IF(i == 2) block(iblock)%depth_w(i,j) = block(iblock)%depth(i-1,j)
           IF(i == x_end) block(iblock)%depth_e(i,j) = block(iblock)%depth(i+1,j)
           IF(j == 2) block(iblock)%depth_s(i,j) = block(iblock)%depth(i,j-1)
           IF(j == y_end) block(iblock)%depth_n(i,j) = block(iblock)%depth(i,j+1)

           block(iblock)%flux_e(i,j) = he2*block(iblock)%uvel(i,j)*block(iblock)%depth_e(i,j)
           block(iblock)%flux_w(i,j) = hw2*block(iblock)%uvel(i-1,j)*block(iblock)%depth_w(i,j)
           block(iblock)%flux_n(i,j) = hn1*block(iblock)%vvel(i,j)*block(iblock)%depth_n(i,j)
           block(iblock)%flux_s(i,j) = hs1*block(iblock)%vvel(i,j-1)*block(iblock)%depth_s(i,j)
           block(iblock)%diffu_e(i,j) = block(iblock)%k_e(i,j)*block(iblock)%depth_e(i,j)*he2/he1
           block(iblock)%diffu_w(i,j) = block(iblock)%k_w(i,j)*block(iblock)%depth_w(i,j)*hw2/hw1
           block(iblock)%diffu_n(i,j) = block(iblock)%k_n(i,j)*block(iblock)%depth_n(i,j)*hn1/hn2
           block(iblock)%diffu_s(i,j) = block(iblock)%k_s(i,j)*block(iblock)%depth_s(i,j)*hs1/hs2

!!$                                ! only needed when the power law scheme is used
!!$           block(iblock)%pec_e(i,j) = block(iblock)%flux_e(i,j)/block(iblock)%diffu_e(i,j)
!!$           block(iblock)%pec_w(i,j) = block(iblock)%flux_w(i,j)/block(iblock)%diffu_w(i,j)
!!$           block(iblock)%pec_n(i,j) = block(iblock)%flux_n(i,j)/block(iblock)%diffu_n(i,j)
!!$           block(iblock)%pec_s(i,j) = block(iblock)%flux_s(i,j)/block(iblock)%diffu_s(i,j)
           
        END DO
     END DO

     block(iblock)%apo(2:x_end,2:y_end) = &
          &block(iblock)%hp1(2:x_end,2:y_end)*&
          &block(iblock)%hp2(2:x_end,2:y_end)*&
          &block(iblock)%depthold(2:x_end,2:y_end)/delta_t

  END DO

END SUBROUTINE transport_precalc


!################################################################################
!--------------------------------------------------------------------------------
! scalar transport solution
!--------------------------------------------------------------------------------
!

SUBROUTINE transport(status_flag)

  IMPLICIT NONE

  INTEGER :: status_flag, var, iter

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
     CALL bedshear()
  END IF
  
  CALL scalar_source_timestep(current_time%time, delta_t)
  IF (source_doing_sed) CALL bed_dist_bedsrc(delta_t)
  CALL transport_precalc()

  ! INTERNAL ITERATION AT THIS TIME LEVEL LOOP
  DO iter = 1,number_scalar_iterations
     
     ! BLOCK LOOP
     DO iblock = 1,max_blocks
        
        x_end = block(iblock)%xmax
        y_end = block(iblock)%ymax
        
        ! SPECIES LOOP - multiple numbers of scalar variables
        DO ispecies = 1, max_species
           
           ! set boundary conditions for this time
           
           ! loop over the total number of bc specifications
           DO num_bc = 1, scalar_bc(iblock)%num_bc
              IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
                   &CALL apply_scalar_bc(block(iblock), &
                   &species(ispecies)%scalar(iblock), &
                   &scalar_bc(iblock)%bc_spec(num_bc), x_start)
           END DO ! num bc loop

           !-------------------------------------------------------------------------
           ! compute scalar transport discretization equation coefficients
           
           ! block(iblock)%apo(x_start:x_end,2:y_end) = &
           !      &block(iblock)%hp1(x_start:x_end,2:y_end)*&
           !      &block(iblock)%hp2(x_start:x_end,2:y_end)*&
           !      &block(iblock)%depthold(x_start:x_end,2:y_end)/delta_t

           coeff%ae(x_start:x_end,2:y_end) = &
                &max(0d+00, -block(iblock)%flux_e(x_start:x_end,2:y_end), &
                &block(iblock)%diffu_e(x_start:x_end,2:y_end) - &
                &block(iblock)%flux_e(x_start:x_end,2:y_end)/2.0)
           coeff%aw(x_start:x_end,2:y_end) = &
                &max(0d+00, block(iblock)%flux_w(x_start:x_end,2:y_end), &
                &block(iblock)%diffu_w(x_start:x_end,2:y_end) + &
                &block(iblock)%flux_w(x_start:x_end,2:y_end)/2.0)
           coeff%an(x_start:x_end,2:y_end) = &
                &max(0d+00, -block(iblock)%flux_n(x_start:x_end,2:y_end), &
                &block(iblock)%diffu_n(x_start:x_end,2:y_end) - &
                &block(iblock)%flux_n(x_start:x_end,2:y_end)/2.0)
           coeff%as(x_start:x_end,2:y_end) = &
                &max(0d+00, block(iblock)%flux_s(x_start:x_end,2:y_end), &
                &block(iblock)%diffu_s(x_start:x_end,2:y_end) + &
                &block(iblock)%flux_s(x_start:x_end,2:y_end)/2.0)
           coeff%bp = 0.0

           DO i= x_start,x_end
              DO j=2,y_end

              IF (source_doing_temp) THEN
                 t_water = species(source_temp_idx)%scalar(iblock)%conc(i,j)
              ELSE
                 t_water = 0
              END IF

              IF (block(iblock)%isdead(i,j)%p) THEN
                 coeff%bp(i,j) = 0.0
              ELSE 
                 coeff%bp(i,j) = &
                      &scalar_source_term(iblock, i, j, ispecies, &
                      &species(ispecies)%scalar(iblock)%concold(i,j),&
                      &block(iblock)%depth(i,j), block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j), &
                      &t_water, salinity)
                 coeff%bp(i,j) = coeff%bp(i,j)*&
                      &block(iblock)%hp1(i,j)*block(iblock)%hp2(i,j)
              END IF

              coeff%ap(i,j) = &
                   &coeff%ae(i,j) + coeff%aw(i,j) + coeff%an(i,j) + coeff%as(i,j) + &
                   &block(iblock)%apo(i,j) 

              coeff%bp(i,j) = coeff%bp(i,j) + &
                   &block(iblock)%apo(i,j)*&
                   &species(ispecies)%scalar(iblock)%concold(i,j)

              SELECT CASE (species(ispecies)%scalar(iblock)%cell(i,j)%type)
              CASE (SCALAR_BOUNDARY_TYPE)
                 SELECT CASE (species(ispecies)%scalar(iblock)%cell(i,j)%bctype)
                 CASE (SCALBC_CONC)
                    IF (i .EQ. x_start) THEN
                       coeff%bp(i,j) = coeff%bp(i,j) + 2.0*coeff%aw(i,j)*&
                            &species(ispecies)%scalar(iblock)%conc(i-1,j)
                       coeff%ap(i,j) = coeff%ap(i,j) + coeff%aw(i,j)
                       coeff%aw(i,j) = 0.0
                    END IF
                 CASE (SCALBC_ZG)
                    IF (i .EQ. x_start) THEN
                       coeff%ap(i,j) = coeff%ap(i,j) - coeff%aw(i,j)
                       coeff%aw(i,j) = 0.0
                    ELSE IF (i .EQ. x_end) THEN
                       coeff%ap(i,j) = coeff%ap(i,j) - coeff%ae(i,j)
                       coeff%ae(i,j) = 0.0
                    END IF
                 END SELECT
              CASE DEFAULT
                 IF (i .EQ. x_start) THEN
                    coeff%bp(i,j) = coeff%bp(i,j) + coeff%aw(i,j)*&
                         &species(ispecies)%scalar(iblock)%conc(i-1,j)
                    coeff%aw(i,j) = 0.0
                 ELSE IF (i .EQ. x_end) THEN
                    coeff%bp(i,j) = coeff%bp(i,j) + coeff%ae(i,j)*&
                         &species(ispecies)%scalar(iblock)%conc(i+1,j)
                    coeff%ae(i,j) = 0.0
                 END IF
              END SELECT
              END DO
           END DO

                                ! apply zero gradient to sides

           coeff%ap(:,2) = coeff%ap(:,2) - coeff%as(:,2)
           coeff%as(:,2) = 0.0

           coeff%ap(:,y_end) = coeff%ap(:,y_end) - coeff%an(:,y_end)
           coeff%an(:,y_end) = 0.0

           CALL solve_tdma(scalar_sweep, x_start, x_end, 2, y_end,&
                &coeff%ap(x_start:x_end,2:y_end), coeff%aw(x_start:x_end,2:y_end), &
                &coeff%ae(x_start:x_end,2:y_end), coeff%as(x_start:x_end,2:y_end), &
                &coeff%an(x_start:x_end,2:y_end), coeff%bp(x_start:x_end,2:y_end), &
                &species(ispecies)%scalar(iblock)%conc(x_start:x_end,2:y_end))

           ! set zero gradient at shoreline
           species(ispecies)%scalar(iblock)%conc(:,1) = &
                &species(ispecies)%scalar(iblock)%conc(:,2)
           species(ispecies)%scalar(iblock)%conc(:,y_end+1) = &
                &species(ispecies)%scalar(iblock)%conc(:,y_end)


           ! fill in the unused corner nodes so that the plots look ok
           species(ispecies)%scalar(iblock)%conc(x_start-1,1) = species(ispecies)%scalar(iblock)%conc(x_start-1,2)
           species(ispecies)%scalar(iblock)%conc(x_start-1,block(iblock)%ymax+1) =&
                & species(ispecies)%scalar(iblock)%conc(x_start-1,block(iblock)%ymax)
           species(ispecies)%scalar(iblock)%conc(block(iblock)%xmax+1,1) =&
                & species(ispecies)%scalar(iblock)%conc(block(iblock)%xmax,2)
           species(ispecies)%scalar(iblock)%conc(block(iblock)%xmax+1,block(iblock)%ymax+1) =&
                & species(ispecies)%scalar(iblock)%conc(block(iblock)%xmax,block(iblock)%ymax)
           
           
        END DO ! species loop
        
     END DO ! block loop end

  END DO ! internal time loop end for concentration

! end scalar transport soultion
!----------------------------------------------------------------------------
END SUBROUTINE transport 

! ----------------------------------------------------------------
! SUBROUTINE apply_scalar_bc
! ----------------------------------------------------------------
SUBROUTINE apply_scalar_bc(blk, sclr, spec, xstart)

  IMPLICIT NONE

  TYPE (block_struct) :: blk
  TYPE (scalar_struct) :: sclr
  TYPE (scalar_bc_spec_struct) :: spec
  INTEGER, INTENT(INOUT) :: xstart

  INTEGER :: i, j, jj, con_i, con_j, j_beg, j_end, con_block, x_end
  DOUBLE PRECISION :: tmp

  x_end = blk%xmax

  SELECT CASE(spec%bc_loc)
                    
  CASE("US")
     i=1
     x_start = i + 1
     SELECT CASE(spec%bc_type)
           
     CASE("ZEROG")
        DO j=1,spec%num_cell_pairs
           j_beg = spec%start_cell(j)+1
           j_end = spec%end_cell(j)+1
           sclr%conc(i,j_beg:j_end) = sclr%conc(i+1,j_beg:j_end)
           sclr%cell(i+1,j_beg:j_end)%type = SCALAR_BOUNDARY_TYPE
           sclr%cell(i+1,j_beg:j_end)%bctype = SCALBC_ZG
        END DO
                       
     CASE("BLOCK")
        SELECT CASE(spec%bc_kind)
        CASE("CONC")
           con_block = spec%con_block
           con_i = block(con_block)%xmax
           DO j=1,spec%num_cell_pairs
              j_beg = spec%start_cell(j)+1
              j_end = spec%end_cell(j)+1
              con_j = spec%con_start_cell(j)+1
              sclr%conc(i,j_beg:j_end) = &
                   &species(spec%species)%scalar(con_block)%&
                   &conc(con_i, spec%con_start_cell(j)+1:spec%con_end_cell(j)+1)
              sclr%concold(i,j_beg:j_end) = &
                   &species(spec%species)%scalar(con_block)%&
                   &concold(con_i, spec%con_start_cell(j)+1:spec%con_end_cell(j)+1)
              sclr%cell(i,j_beg:j_end)%type = SCALAR_NORMAL_TYPE
           END DO
              
        END SELECT
           
     CASE("TABLE")
        CALL scalar_table_interp(current_time%time, spec%table_num,&
                & table_input, spec%num_cell_pairs)
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
              sclr%cell(x_start,j_beg:j_end)%type = SCALAR_BOUNDARY_TYPE
              sclr%cell(x_start,j_beg:j_end)%bctype = SCALBC_CONC
           END DO
              
        CASE("CONC")
           i = spec%x_start
           x_start = i + 1
           DO j=1,spec%num_cell_pairs
              j_beg = spec%start_cell(j)+1
              j_end = spec%end_cell(j)+1
              sclr%conc(i,j_beg:j_end) = &
                   &table_input(j)*scalar_source(spec%species)%conversion
              sclr%concold(i,j_beg:j_end) = sclr%conc(i,j_beg:j_end)
              sclr%cell(x_start,j_beg:j_end)%type = SCALAR_BOUNDARY_TYPE
              sclr%cell(x_start,j_beg:j_end)%bctype = SCALBC_CONC
           END DO
              
        END SELECT
           
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
           sclr%cell(i-1,j_beg:j_end)%type = SCALAR_BOUNDARY_TYPE
           sclr%cell(i-1,j_beg:j_end)%bctype = SCALBC_ZG
        END DO

     CASE("BLOCK")
        SELECT CASE(spec%bc_kind)
        CASE("CONC")
           con_block = spec%con_block
           con_i = 2
           DO j=1,spec%num_cell_pairs
              j_beg = spec%start_cell(j)+1
              j_end = spec%end_cell(j)+1
              con_j = spec%con_start_cell(j)+1
              sclr%conc(i,j_beg:j_end) = &
                   &species(spec%species)%scalar(con_block)%&
                   &conc(con_i, spec%con_start_cell(j)+1:spec%con_end_cell(j)+1)
              sclr%concold(i,j_beg:j_end) = &
                   &species(spec%species)%scalar(con_block)%&
                   &concold(con_i, spec%con_start_cell(j)+1:spec%con_end_cell(j)+1)
              sclr%cell(i,j_beg:j_end)%type = SCALAR_NORMAL_TYPE
           END DO
              
        END SELECT
           
     END SELECT
  END SELECT
     
END SUBROUTINE apply_scalar_bc


!############################################################################
!----------------------------------------------------------------------------
! update old values of dependent variables

SUBROUTINE update(status_flag)

IMPLICIT NONE

INTEGER :: status_flag


DO iblock=1,max_blocks

  block(iblock)%uold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%uvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%vold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%vvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%depthold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%depth(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot

END DO

IF (do_transport) THEN
   DO ispecies = 1, max_species
      DO iblock = 1, max_blocks
         species(ispecies)%scalar(iblock)%concold(2:block(iblock)%xmax,2:block(iblock)%ymax) &
              = species(ispecies)%scalar(iblock)%conc(2:block(iblock)%xmax,2:block(iblock)%ymax)
      END DO
   END DO
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


DOUBLE PRECISION :: depth_e, flux_e

INTEGER :: status_flag

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
                    &scalar_bc(iblock)%bc_spec(num_bc), x_start)
            END DO
         END DO
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
! 3011 FORMAT(i5,5x)
! 3005 FORMAT('#date',8x,'time',5x)

	END IF
END IF
status_flag = 99

END SUBROUTINE output


!##########################################################################
!---------------------------------------------------------------------------
! write a  restart file
!---------------------------------------------------------------------------

SUBROUTINE write_restart(status_flag)
IMPLICIT NONE

INTEGER :: status_flag


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
!
! Tridiangonal Matrix Solution
SUBROUTINE tridag(start, finish, a, b, c, d,sol, ptemp, qtemp)

IMPLICIT NONE

INTEGER :: i,last,ifp1,k,start,finish
DOUBLE PRECISION, DIMENSION(:) :: a,b,c,d,sol
DOUBLE PRECISION :: ptemp(0:), qtemp(0:)

DO i=start,finish
ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
END DO
!!$FORALL (i=start:finish)
!!$   ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
!!$   qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
!!$END FORALL

sol(finish) = qtemp(finish)

DO i=finish-1,start,-1
sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
END DO
!!$FORALL (i=finish-1:start:-1)
!!$   sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
!!$END FORALL


END SUBROUTINE tridag
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
! Advection scheme function for advection-diffusion equations
!
!

DOUBLE PRECISION FUNCTION afunc(peclet_num)

IMPLICIT NONE

DOUBLE PRECISION :: peclet_num

peclet_num = abs(peclet_num)

afunc = max(0.0d0,(1.0-0.1*peclet_num)**5)  !power-law
!afunc = 1.0                              !upwind-difference

END FUNCTION afunc
!-----------------------------------------------------------------------------

! ----------------------------------------------------------------
! SUBROUTINE bedshear
! computes the shear used by biota/sediment scalars
! ----------------------------------------------------------------
SUBROUTINE bedshear()

  IMPLICIT NONE

  INTEGER :: iblk, i, j
  DOUBLE PRECISION :: roughness, u, v

  DO iblk = 1, max_blocks
     block(iblk)%shear = 0 
     DO i = 1, block(iblk)%xmax + 1
        DO j = 2, block(iblk)%ymax
           IF (i .EQ. 1) THEN
              u = block(iblk)%uvel(i,j)
              v = 0.0
           ELSE IF (i .EQ. block(iblk)%xmax + 1) THEN
              u = block(iblk)%uvel(i-1,j)
              v = 0.0
           ELSE 
              u = 0.5*(block(iblk)%uvel(i-1,j) + block(iblk)%uvel(i,j))
              v = 0.5*(block(iblk)%vvel(i-1,j) + block(iblk)%vvel(i,j))
           END IF
           IF(manning)THEN
              roughness = (grav*block(iblk)%chezy(i,j)**2)/&
                   &(mann_con*block(iblk)%depth(i,j)**0.3333333)
           ELSE
              roughness = block(iblk)%chezy(i,j)
           ENDIF
           block(iblk)%shear(i,j) = roughness*density*sqrt(u*u + v*v)
        END DO
     END DO
  END DO
END SUBROUTINE bedshear

! ----------------------------------------------------------------
! SUBROUTINE check_wetdry
! ----------------------------------------------------------------
SUBROUTINE check_wetdry(iblock, nx, ny)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock, nx, ny
  INTEGER :: i1, j1, n
  LOGICAL :: flag
  DOUBLE PRECISION :: wsavg, wscell, wsn
  LOGICAL :: isdry(i_index_min:nx + i_index_extra, j_index_min:ny + j_index_extra)

  isdry = block(iblock)%isdry

  DO i = 2, nx + 1
     DO j = 2, ny + 1 
                 
        IF (isdry(i,j)) THEN

           wscell = block(iblock)%depth(i, j) + block(iblock)%zbot(i, j)

                                ! Condition: all wet neighbors must
                                ! have a higher w.s. elevation.

           flag = .FALSE.       ! becomes .TRUE. if *any* wet neighbors are higher
           n = 0                ! count wet neighbors

           DO i1 = i - 1, i + 1, 2
              IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. isdry(i1,j))) THEN
                 wsn = block(iblock)%depth(i1, j) + block(iblock)%zbot(i1, j)
                 IF (wsn .GE. wscell) flag = .TRUE.
                 n = n + 1
              ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                 wsn = wselev_block_neighbor(iblock, i1, j)
                 IF (wsn .GT. -9990.0) THEN
                    IF (wsn .GE. wscell) flag = .TRUE. 
                    n = n + 1
                 END IF
              END IF
           END DO
           DO j1 = j - 1, j + 1, 2
              wsn = block(iblock)%depth(i, j1) + block(iblock)%zbot(i, j1)
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
                    wsn = block(iblock)%depth(i1, j) + block(iblock)%zbot(i1, j)
                    wsavg = wsavg + wsn
                    n = n + 1
                 ELSE IF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                    wsn = wselev_block_neighbor(iblock, i1, j)
                    IF (wsn .GT. -9990.0) THEN
                       wsavg = wsavg + wsn
                       n = n + 1
                    END IF
                 END IF
              END DO
              DO j1 = j - 1, j + 1, 2
                 wsn = block(iblock)%depth(i, j1) + block(iblock)%zbot(i, j1)
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
!!$           block(iblock)%isdry(i,j) = (.NOT. ((wsavg - block(iblock)%zbot(i, j)) .GT. dry_depth))

                                ! Alternative: A cell becomes wet if
                                ! the cell and all its wet neighbors
                                ! are high enough to exceed the dry
                                ! depth *and* have all of the wet
                                ! cells remain wet.

              IF (N .GT. 1 .AND. wsavg .GT. block(iblock)%zbot(i, j) + dry_rewet_depth ) THEN
                 flag = .TRUE.
                 DO i1 = i - 1, i + 1, 2
                    IF (i1 .GT. 1 .AND. i1 .LT. nx + 1 .AND. (.NOT. isdry(i1,j))) THEN
                       flag = flag .AND. ((wsavg - block(iblock)%zbot(i1, j)) .GT. dry_depth) 
                    ELSEIF (i1 .EQ. 1 .OR. i1 .EQ. nx + 1) THEN
                       flag = flag .AND. ((wsavg - zbot_block_neighbor(iblock, i1, j)) .GT. dry_depth)
                    END IF
                 END DO
                 DO j1 = j - 1, j + 1, 2
                    IF (j1 .GT. 1 .AND. j1 .LT. ny + 1 .AND. (.NOT. isdry(i,j1))) THEN
                       flag = flag .AND. ((wsavg - block(iblock)%zbot(i, j1)) .GT. dry_depth) 
                    END IF
                 END DO

                                ! if all the neighbors can stay wet,
                                ! then it's ok to be wet again

                                ! we may need a volume check here
                 
                 IF (flag) block(iblock)%isdry(i,j) = .FALSE.

              END IF
           END IF
           
        ELSE 

                                ! check the wet cells to see if they
                                ! should be dry

           block(iblock)%isdry(i,j) = (block(iblock)%depth(i,j) .LE. dry_depth)

        END IF
     END DO
  END DO

                                ! fill out isdry array for plotting

  block(iblock)%isdry(:,1) = block(iblock)%isdry(:,2)
  block(iblock)%isdry(:,ny + 1) = block(iblock)%isdry(:,ny+1)
  block(iblock)%isdry(1,:) = block(iblock)%isdry(2,:)
  block(iblock)%isdry(nx + 1,:)  = block(iblock)%isdry(nx+1,:)

END SUBROUTINE check_wetdry

! ----------------------------------------------------------------
! DOUBLE PRECISION FUNCTION wselev_block_neighbor
! Determines the water surface elevation of the cell neighboring the
! specified cell if there is a block connected there.  A bogus value
! of -9999.0 is returned if there is no neighboring block, or if the
! neighboring cell is dry.
! This is really a hack.  Ghost cells would be a better approach.
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION wselev_block_neighbor(iblock, i, j) RESULT (z)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iblock, i, j
  INTEGER :: num_bc, icon, jcon, con_block, k
  LOGICAL :: doelev = .FALSE.

  doelev = .TRUE.

  GOTO 100

  ENTRY zbot_block_neighbor(iblock, i, j) RESULT (z)

  doelev = .FALSE.

100 CONTINUE

  z = -9999.0

  DO num_bc = 1, block_bc(iblock)%num_bc

     con_block = 0
     icon = 0
     jcon = 0

     IF (i .EQ. 1) THEN 
        SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
        CASE("US")
           SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)
           CASE("BLOCK")
              con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
              icon = block(con_block)%xmax
           END SELECT
        END SELECT
     ELSE
        SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)
        CASE("DS")
           SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)
           CASE("BLOCK")
              con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
              icon = 2
           END SELECT
        END SELECT
     END IF
     IF (con_block .NE. 0 .AND. icon .NE. 0) THEN
        DO k=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
           IF (j .GE. block_bc(iblock)%bc_spec(num_bc)%start_cell(k)+1 .AND. &
                &j .LE. block_bc(iblock)%bc_spec(num_bc)%end_cell(k)+1) THEN
              jcon = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(k)+ j - &
                   &(block_bc(iblock)%bc_spec(num_bc)%start_cell(k))
              IF (active_cell(con_block, icon, jcon)) THEN
                 z = block(con_block)%zbot(icon, jcon)
                 IF (doelev) z = z + block(con_block)%depth(icon, jcon)
              END IF
              RETURN
           END IF
        END DO
     END IF
  END DO

END FUNCTION wselev_block_neighbor


! ----------------------------------------------------------------
! LOGICAL FUNCTION active_cell
! ----------------------------------------------------------------
LOGICAL FUNCTION active_cell(iblock, i, j)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: iblock, i, j

  active_cell = .NOT. &
       &(block(iblock)%isdry(i,j) .OR. &
       &block(iblock)%isdead(i,j)%p)
  active_cell = .NOT. &
       &(block(iblock)%isdry(i,j))

END FUNCTION active_cell


END MODULE mass2_main_025

