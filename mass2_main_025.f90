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
! MOD HISTORY:	4-28-98 0.23 version fully implements multiblock, dynamic arrays
!
!***************************************************************
!

PROGRAM mass2_main

!-------------------------------------------------------------------------------------------------------

USE globals
USE io_routines_module
USE block_boundary_conditions
USE table_boundary_conditions
USE date_time
USE gage_output
USE plot_output
USE scalars
USE met_data_module
USE energy_flux
USE gas_functions


!-------------------------------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: n, i, j, jj, junk, icell, jcell
INTEGER :: imax,jmax,ivec(4),jvec(4),ivec2(4),jvec2(4)
INTEGER :: system_time(8)
INTEGER :: iblock, con_block, num_bc, ispecies
INTEGER :: cfg_iounit=10, output_iounit=11, error_iounit=13, status_iounit=14
INTEGER :: grid_iounit=15, hotstart_iounit=16, restart_iounit=17, bcspec_iounit=18
INTEGER :: mass_source_iounit=19, diag_plot_iounit=20

INTEGER :: i_start_cell, i_end_cell, j_start_cell , j_end_cell
INTEGER :: max_species_in_restart

DOUBLE PRECISION :: roughness, species_io_vec(100)

DOUBLE PRECISION, ALLOCATABLE :: aa(:),bb(:),cc(:),dd(:),tt(:),ptemp(:),qtemp(:)
DOUBLE PRECISION, ALLOCATABLE :: work(:,:),cos_ij(:,:),inlet_area(:),table_input(:)

!---------------------------------------------------------------------------------
! derived type (structures) declarations
TYPE(datetime_struct) :: start_time, end_time, current_time


!--- v013 defs ---------------------------------------------------

INTEGER :: x_end,y_end
DOUBLE PRECISION :: delta_x,delta_y
DOUBLE PRECISION :: eddy_default, kx_diff_default, ky_diff_default ! default diffusion coeff for scalar transport
DOUBLE PRECISION :: k_p,k_e,k_w,k_n,k_s 

DOUBLE PRECISION :: delta_t
INTEGER :: time_step_count = 0, print_freq = 1, gage_print_freq = 1, restart_print_freq = 1



DOUBLE PRECISION :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
DOUBLE PRECISION :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
DOUBLE PRECISION :: flux_e,flux_w,flux_n,flux_s					! fluxes
DOUBLE PRECISION :: diffu_e,diffu_w,diffu_n,diffu_s			! diffusion
DOUBLE PRECISION :: pec_e,pec_w,pec_n,pec_s	! peclet numbers
DOUBLE PRECISION :: apo, cpo								! coefficients in discretization eqns
DOUBLE PRECISION :: u_p, u_e, u_w, u_s, u_n	! u velocities at P and on staggered grid
DOUBLE PRECISION :: v_p, v_n, v_s, v_e, v_w	! v velocities at P and on staggered grid



DOUBLE PRECISION :: cross_term				! eddy viscosity cross term in momement equations
DOUBLE PRECISION :: curve_1,curve_2,curve_3,curve_4,curve_5,curve_6,curve_7	! curvature terms

DOUBLE PRECISION :: h1_eta_p, h2_xsi_p						! derivatives of metric coeff
DOUBLE PRECISION :: h1_eta_e, h1_eta_w, h1_eta_n, h1_eta_s	! e.g., h1_eta_p is the partial deriv
DOUBLE PRECISION :: h2_xsi_e, h2_xsi_w, h2_xsi_n, h2_xsi_s	! of h1 in eta direction at point p

DOUBLE PRECISION :: chezy_con_default, slope,ds_elev,z_step
DOUBLE PRECISION :: relax_dp,mann_con

INTEGER :: jspill_start   ! y node line to start spill at
INTEGER :: jspill_end			! y node line to stop spill at
INTEGER :: jgen_start  		! y node line to start generation at
INTEGER :: jgen_end				! y node line to stop generation ** max is block%ymax

INTEGER :: j_dsflux_start, j_dsflux_end

INTEGER :: x_start			! x line to start scalar calculations (usually = 2)

DOUBLE PRECISION :: spill_flow	! unit spill flow (cfs)
DOUBLE PRECISION :: gen_flow   	! unit generation flow (cfs)
DOUBLE PRECISION :: gen_conc    ! unit generation TDG %Saturation
DOUBLE PRECISION :: spill_conc  ! unit spill TDG %Saturation

DOUBLE PRECISION :: uvel_initial, vvel_initial, conc_initial, wsel_or_depth_initial ! intial values to assign over field

DOUBLE PRECISION :: uvel_wind, vvel_wind, wind_speed, wind_drag_coeff

DOUBLE PRECISION :: max_mass_source_sum, maxx_mass
DOUBLE PRECISION :: salinity = 0.0, ccstar, conc_TDG

INTEGER :: iteration, number_hydro_iterations, number_scalar_iterations
INTEGER :: sweep,scalar_sweep, depth_sweep
!LOGICAL :: x_sweep, y_sweep, x_sweep_dp, y_sweep_dp
LOGICAL :: debug, manning, read_grid
LOGICAL :: write_restart_file, read_hotstart_file
LOGICAL :: ds_flux_given, given_initial_wsel, read_initial_profile
LOGICAL :: update_depth = .TRUE.
LOGICAL :: do_flow = .TRUE. , do_transport = .FALSE. , do_gage_print = .FALSE.
LOGICAL :: do_transport_restart = .FALSE.
LOGICAL :: do_spatial_eddy, do_spatial_kx, do_spatial_ky, do_spatial_chezy
LOGICAL :: do_surface_heatx = .FALSE. , do_surface_gasx = .FALSE.

DOUBLE PRECISION :: inlet_flow
DOUBLE PRECISION :: total_flow

DOUBLE PRECISION :: ds_elev_read(27)

DOUBLE PRECISION :: transfer_coeff, gasx_a, gasx_b, gasx_c, gasx_d

!--- v013 defs ----------------------------------------------------

CHARACTER (LEN=80), ALLOCATABLE :: grid_file_name(:)
CHARACTER (LEN=80) :: weather_filename
CHARACTER (LEN=80) :: config_file_version, filename
CHARACTER*80 :: sim_title
CHARACTER*75 :: title
CHARACTER (LEN=80) :: code_version = "mass2 code version 0.251"
CHARACTER (LEN=80) :: code_date = "release date: December 19, 1998"
CHARACTER*15 :: block_title
CHARACTER*30 :: restart_filename
CHARACTER*26 :: zone_name

!-------------------------------------------------------------------------------------------------------
! debugging temps
INTEGER :: dum_start, dum_end
DOUBLE PRECISION :: dum_vel(100), dum_depth(100), dum_val
CHARACTER (LEN=80) :: dum_char
!-------------------------------------------------------------------------------------------------------
! format definitions all placed here
2000 FORMAT(a80)
1000 FORMAT(50(f12.4,2x))
1020 FORMAT(2(f12.4,2x),50(f12.6,2x))
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)

!-------------------------------------------------------------------------------------------------------
! open io units

OPEN(cfg_iounit,file='mass2_v025.cfg')
OPEN(output_iounit,file='output.out', access='sequential', RECL=1024)
OPEN(error_iounit,file='error-warning.out')
OPEN(status_iounit,file='status.out')
OPEN(mass_source_iounit,file='mass_source_monitor.out')
OPEN(diag_plot_iounit,file='diagnostic_plot.dat')
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

CALL allocate_blocks(error_iounit,status_iounit)
ALLOCATE(grid_file_name(max_blocks))

DO iblock=1,max_blocks
	READ(cfg_iounit,*)grid_file_name(iblock)
	OPEN(grid_iounit,file=grid_file_name(iblock))
	READ(grid_iounit,*)block(iblock)%xmax,block(iblock)%ymax
	CLOSE(grid_iounit)
END DO

DO i=1,max_blocks
	CALL allocate_block_components(i, block(i)%xmax+1, block(i)%ymax+1, status_iounit)
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
		CALL allocate_scalarblock_components(i, j , block(j)%xmax+1, block(j)%ymax+1, status_iounit, error_iounit)
	END DO
END DO





ALLOCATE(aa(jmax),bb(jmax),cc(jmax),dd(jmax),tt(jmax),ptemp(jmax),qtemp(jmax))
ALLOCATE(work(imax,jmax),cos_ij(imax,jmax))
ALLOCATE(inlet_area(jmax), table_input(jmax))

!-------------------------------------------------------------------------------
! read, allocate, and set up block and table boundary conditions

! read hydrodynamics related stuff
CALL allocate_block_bc(max_blocks, error_iounit, status_iounit)

CALL read_bcspecs(bcspec_iounit, error_iounit, status_iounit, block%xmax, block%ymax)

CALL read_bc_tables

! now decipher which cells talk to each other in the block connections
IF(max_blocks > 1)THEN
	CALL set_block_connections(max_blocks, error_iounit, status_iounit)
END IF


!---------------------------------------------------------------------------------------------------------
! finish reading cfg file
READ(cfg_iounit,*)do_flow				! on/off switch for hydrodynamics
READ(cfg_iounit,*)do_transport	! on/off switch for transport calculations
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

READ(cfg_iounit,*) print_freq		! printout frequency every print_freq time steps
READ(cfg_iounit,*) gage_print_freq
!--------------------------------------------------------------------------------------------------------
! do some pre-processing of the config data

mann_con = mann_con**2

start_time%time = date_to_decimal(start_time%date_string,start_time%time_string)
end_time%time = date_to_decimal(end_time%date_string,end_time%time_string)
current_time = start_time

!---------------------------------------------------------------------------------
IF(do_transport)THEN
! read species related stuff
	CALL allocate_scalar_block_bc(max_blocks, error_iounit, status_iounit)
	CALL read_scalar_bcspecs(bcspec_iounit, error_iounit, status_iounit, block%xmax, block%ymax)
	CALL read_scalar_bc_tables
	CALL set_scalar_block_connections(max_blocks, max_species, error_iounit, status_iounit)
END IF

! read in met data from a file
IF(do_surface_heatx .OR. do_surface_gasx)THEN
	CALL read_met_data(weather_filename)
	CALL update_met_data(current_time%time)
END IF

! read in surface gas exchange coefficients
!
IF(do_surface_gasx)THEN
	OPEN(grid_iounit,file='gas_exchange_coeff.dat')
	READ(grid_iounit,*)gasx_a, gasx_b, gasx_c, gasx_d
	CLOSE(grid_iounit)
	WRITE(status_iounit,*)'completed reading surface gas exchange coefficients'
END IF

!------------------------------------------------------------------------------------
! set up the gage print files
IF(do_gage_print) CALL gage_file_setup(error_iounit, status_iounit)


!---------------------------------------------------------------------------------------------------------
! read in the grid files for each block
DO iblock=1,max_blocks
   OPEN(grid_iounit,file=grid_file_name(iblock))	
   READ(grid_iounit,*)junk
   WRITE(status_iounit,*)'reading in x,y,z for block n = ',iblock
	 
   ! read in grid x,y, and bottom elevation
   DO i=1,block(iblock)%xmax
      DO j=1,block(iblock)%ymax
         READ(grid_iounit,*)junk,junk,block(iblock)%x_grid(i,j),block(iblock)%y_grid(i,j),block(iblock)%zbot_grid(i,j)
      END DO
   END DO
   CLOSE(grid_iounit)
   WRITE(status_iounit,*)'completed in x,y,z for block n = ',iblock
END DO

! interpolate x_grid,y_grid,zbot_grid onto the c.v. points by simple averaging
DO iblock=1,max_blocks
   DO i = 2, block(iblock)%xmax
      DO j = 2, block(iblock)%ymax
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
   i=1
   DO j= 2, block(iblock)%ymax
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j)+block(iblock)%x_grid(i,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j)+block(iblock)%y_grid(i,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j)+block(iblock)%zbot_grid(i,j-1))
   END DO

   i= block(iblock)%xmax+1
   DO j= 2, block(iblock)%ymax
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i-1,j)+block(iblock)%x_grid(i-1,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i-1,j)+block(iblock)%y_grid(i-1,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i-1,j)+block(iblock)%zbot_grid(i-1,j-1))
   END DO

   j=1
   DO i=2, block(iblock)%xmax
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j)+block(iblock)%x_grid(i-1,j))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j)+block(iblock)%y_grid(i-1,j))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j)+block(iblock)%zbot_grid(i-1,j))
   END DO

   j= block(iblock)%ymax+1
   DO i=2, block(iblock)%xmax
      block(iblock)%x(i,j) = 0.5*(block(iblock)%x_grid(i,j-1)+block(iblock)%x_grid(i-1,j-1))
      block(iblock)%y(i,j) = 0.5*(block(iblock)%y_grid(i,j-1)+block(iblock)%y_grid(i-1,j-1))
      block(iblock)%zbot(i,j) = 0.5*(block(iblock)%zbot_grid(i,j-1)+block(iblock)%zbot_grid(i-1,j-1))
   END DO

   ivec = (/1,1,block(iblock)%xmax+1,block(iblock)%xmax+1/)
   jvec = (/1,block(iblock)%ymax+1,1,block(iblock)%ymax+1/)
   ivec2 = (/1,1,block(iblock)%xmax,block(iblock)%xmax/)
   jvec2 = (/1,block(iblock)%ymax,1,block(iblock)%ymax/)
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
	OPEN(unit=hotstart_iounit,file='hotstart.bin',form='formatted')
	WRITE(status_iounit,*)'-- reading hotstart file'
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
		IF(max_species_in_restart > max_species) max_species_in_restart = max_species
		DO i=1,max_species_in_restart
			DO iblock = 1, max_blocks
				READ(hotstart_iounit,*) species(i)%scalar(iblock)%conc
			WRITE(*,*)'done with conc read for species -',i,'and block -',iblock
				READ(hotstart_iounit,*) species(i)%scalar(iblock)%concold
			WRITE(*,*)'done with concold read for species -',i,'and block -',iblock
			END DO
		END DO
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
			block(iblock)%depthold = block(iblock)%depth
			block(iblock)%dstar = block(iblock)%depth
		ELSE
			block(iblock)%depth = wsel_or_depth_initial
			block(iblock)%depthold = block(iblock)%depth
			block(iblock)%dstar = block(iblock)%depth
		ENDIF
    END IF
	END DO
	DO i = 1, max_species
		DO iblock =1, max_blocks
			species(i)%scalar(iblock)%conc = conc_initial
			species(i)%scalar(iblock)%concold = conc_initial
		END DO
	END DO

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
  OPEN(50,file=filename)
	DO WHILE(.TRUE.)
		READ(50,*,END=101)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%eddy(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
101	CLOSE(50)
ENDIF
IF(do_spatial_kx)THEN
	filename = "kx_coeff.dat"
  OPEN(50,file=filename)
	DO WHILE(.TRUE.)
		READ(50,*,END=102)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%kx_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
102	CLOSE(50)
ENDIF
IF(do_spatial_ky)THEN
	filename = "ky_coeff.dat"
  OPEN(50,file=filename)
	DO WHILE(.TRUE.)
		READ(50,*,END=103)iblock, dum_val,i_start_cell, i_end_cell, j_start_cell , j_end_cell
		block(iblock)%ky_diff(i_start_cell+1:i_end_cell+1,j_start_cell+1:j_end_cell+1) = dum_val
	END DO
103	CLOSE(50)
ENDIF

IF(do_spatial_chezy)THEN
	filename = "roughness_coeff.dat"
	OPEN(50,file=filename)
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
! compute the metric coefficients for each block
! depending on the metric coeff. and location use either the grid (x,y) or the node (x,y)

DO iblock=1,max_blocks

   ! metric coeff. 1 on the v face of the c.v.
   DO i=2, block(iblock)%xmax
      DO j=1, block(iblock)%ymax
         block(iblock)%hv1(i,j) = &
              SQRT((block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i-1,j))**2 + &
              (block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i-1,j))**2) 
      END DO
   END DO

   ! metric coeff. 2 on the u face of the c.v.
   DO i=1, block(iblock)%xmax
      DO j=2, block(iblock)%ymax
         block(iblock)%hu2(i,j) = & 
              SQRT((block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i,j-1))**2 + &
              (block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i,j-1))**2)
      END DO
   END DO

   ! metric coeff 1 on the u face of the c.v.
   DO i=2, block(iblock)%xmax-1
      DO j=1, block(iblock)%ymax+1
         block(iblock)%hu1(i,j) = &
              SQRT((block(iblock)%x(i+1,j) - block(iblock)%x(i,j))**2 + &
              (block(iblock)%y(i+1,j) - block(iblock)%y(i,j))**2)
      END DO
   END DO

   ! on the edge it's only a half-distance
   DO i=1, 1
      DO j=1, block(iblock)%ymax+1
         block(iblock)%hu1(i,j) = &
              SQRT(((block(iblock)%x(i+1,j) - block(iblock)%x(i,j)))**2 + &
              ((block(iblock)%y(i+1,j) - block(iblock)%y(i,j)))**2)
      END DO
   END DO
   DO i=block(iblock)%xmax, block(iblock)%xmax
      DO j=1, block(iblock)%ymax+1
         block(iblock)%hu1(i,j) = &
              SQRT(((block(iblock)%x(i+1,j) - block(iblock)%x(i,j)))**2 + &
              ((block(iblock)%y(i+1,j) - block(iblock)%y(i,j)))**2)
      END DO
   END DO
   
   ! metric coeff. 2 on the v face of the c.v.
   DO i=1, block(iblock)%xmax+1
      DO j=2, block(iblock)%ymax-1
         block(iblock)%hv2(i,j) = &
              SQRT((block(iblock)%x(i,j+1) - block(iblock)%x(i,j))**2 + &
              (block(iblock)%y(i,j+1) - block(iblock)%y(i,j))**2)
      END DO
   END DO
   ! on the edge it's only a half-distance
   DO i=2, block(iblock)%xmax
      DO j = 1, 1
         block(iblock)%hv2(i,j) = &
              SQRT(((block(iblock)%x(i,j+1) - block(iblock)%x(i,j)))**2 + &
              ((block(iblock)%y(i,j+1) - block(iblock)%y(i,j)))**2)
      END DO
   END DO
   DO i=2, block(iblock)%xmax
      DO j= block(iblock)%ymax, block(iblock)%ymax
         block(iblock)%hv2(i,j) = &
              SQRT(((block(iblock)%x(i,j+1) - block(iblock)%x(i,j)))**2 + &
              ((block(iblock)%y(i,j+1) - block(iblock)%y(i,j)))**2)
      END DO
   END DO

   !-------------------------------------------------------------------------
   ! compute metric tensor and derivatives at the nodal points hp1, hp2

   DO i=2, block(iblock)%xmax
      DO j=2, block(iblock)%ymax
         block(iblock)%x_eta(i,j) = 0.5*(block(iblock)%x_grid(i,j) + block(iblock)%x_grid(i-1,j) & 
              - block(iblock)%x_grid(i,j-1) - block(iblock)%x_grid(i-1,j-1))
         block(iblock)%y_eta(i,j) = 0.5*(block(iblock)%y_grid(i,j) + block(iblock)%y_grid(i-1,j) & 
              - block(iblock)%y_grid(i,j-1) - block(iblock)%y_grid(i-1,j-1))
         block(iblock)%x_xsi(i,j) = 0.5*(block(iblock)%x_grid(i,j) + block(iblock)%x_grid(i,j-1) & 
              - block(iblock)%x_grid(i-1,j) - block(iblock)%x_grid(i-1,j-1))
         block(iblock)%y_xsi(i,j) = 0.5*(block(iblock)%y_grid(i,j) + block(iblock)%y_grid(i,j-1) & 
              - block(iblock)%y_grid(i-1,j) - block(iblock)%y_grid(i-1,j-1))
      END DO
   END DO
   DO i=1, 1
      DO j=2, block(iblock)%ymax
         block(iblock)%x_eta(i,j) = block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i,j-1)
         block(iblock)%y_eta(i,j) = block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i,j-1)
      END DO
   END DO
   DO i=block(iblock)%xmax, block(iblock)%xmax
      DO j=2, block(iblock)%ymax
         block(iblock)%x_eta(i+1,j) = block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i,j-1)
         block(iblock)%y_eta(i+1,j) = block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i,j-1)
      END DO
   END DO
   DO i=2, block(iblock)%xmax
      DO j=1, 1
         block(iblock)%x_xsi(i,j) = block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i-1,j)
         block(iblock)%y_xsi(i,j) = block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i-1,j)
      END DO
   END DO
   DO i=2, block(iblock)%xmax
      DO j=block(iblock)%ymax, block(iblock)%ymax
         block(iblock)%x_xsi(i,j+1) = block(iblock)%x_grid(i,j) - block(iblock)%x_grid(i-1,j)
         block(iblock)%y_xsi(i,j+1) = block(iblock)%y_grid(i,j) - block(iblock)%y_grid(i-1,j)
      END DO
   END DO

   block(iblock)%x_xsi(1,:) = block(iblock)%x_xsi(2,:)
   block(iblock)%x_xsi(block(iblock)%xmax+1,:) = block(iblock)%x_xsi(block(iblock)%xmax,:)

   block(iblock)%y_xsi(1,:) = block(iblock)%y_xsi(2,:)
   block(iblock)%y_xsi(block(iblock)%xmax+1,:) = block(iblock)%y_xsi(block(iblock)%xmax,:)

   block(iblock)%x_eta(:,1) = block(iblock)%x_eta(:,2)
   block(iblock)%x_eta(:,block(iblock)%ymax+1) = block(iblock)%x_eta(:,block(iblock)%ymax)

   block(iblock)%y_eta(:,1) = block(iblock)%y_eta(:,2)
   block(iblock)%y_eta(:,block(iblock)%ymax+1) = block(iblock)%y_eta(:,block(iblock)%ymax)


   block(iblock)%hp1 = SQRT(block(iblock)%x_xsi**2 + block(iblock)%y_xsi**2)
   block(iblock)%hp2 = SQRT(block(iblock)%x_eta**2 + block(iblock)%y_eta**2)

	!---------------------------------------------------------------------------
  ! compute nonorthogonal part of the metric tensor as a check on grid quality

	block(iblock)%gp12 = block(iblock)%x_xsi*block(iblock)%x_eta + block(iblock)%y_xsi*block(iblock)%y_eta
	
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

DO iblock=1,max_blocks

	WRITE(block_title(12:15),'(i4)')iblock
	WRITE(output_iounit,*)'initial values for block number - ',iblock
	
	title = 'X grid values - state plane coord' 
	title(61:75) = block_title
	
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax,1,block(iblock)%ymax,block(iblock)%x_grid)

	title = 'Y grid values - state plane coord'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax,1,block(iblock)%ymax,block(iblock)%y_grid)

	title = 'Bottom Elevation grid values - mean sea level'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax,1,block(iblock)%ymax,block(iblock)%zbot_grid)

	title = 'Water Surface Elevation - mean sea level'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%wsel)

        title = "U Velocity (located at U staggered node)"
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%uvel)


        title = "V Velocity (located at V staggered node)"
        title(61:75) = block_title
        CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%vvel)

	title = 'Depth'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%depth)

	title = 'X c.v. node values - state plane coord'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%x)

	title = 'Y c.v. node values - state plane coord'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%y)

	title = 'Bottom Elevation c.v node values - mean sea level'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%zbot)

	title = 'h1 metric coeff at U location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hu1)

	title = 'h2 metric coeff at U location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hu2)

	title = 'h1 metric coeff at V location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hv1)

	title = 'h2 metric coeff at V location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hv2)

	title = 'h1 metric coeff at P location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hp1)

	title = 'h2 metric coeff at P location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%hp2)

	title = 'g12 nonorthogonal component of the metric tensor at P location'
	title(61:75) = block_title
	CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%gp12)


END DO

!-----------------------------------------------------------------------------------------------------------
! write initial fields to the plot file

OPEN(grid_iounit,file='gridplot1.dat')
WRITE(grid_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code - Grid"""
	!WRITE(plot_iounit,*)"variables=""x"" ""y"" ""u vel"" ""v vel"" ""u cart"" ""v cart"" ""depth"" ""zbot"" ""wsel"" ""con"""
WRITE(grid_iounit,*)"variables=""x"" ""y"" ""zbot"""
DO iblock=1,max_blocks
	WRITE(grid_iounit,*)"zone f=block"," t=""block ",iblock,""""," i=", block(iblock)%xmax, " j= ",block(iblock)%ymax
	WRITE(grid_iounit,*)block(iblock)%x_grid(1:block(iblock)%xmax,1:block(iblock)%ymax)
	WRITE(grid_iounit,*)block(iblock)%y_grid(1:block(iblock)%xmax,1:block(iblock)%ymax)
	WRITE(grid_iounit,*)block(iblock)%zbot_grid(1:block(iblock)%xmax,1:block(iblock)%ymax)
END DO
CLOSE(grid_iounit)

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

!-----------------------------------------------------------------------
! diagnostic plot; tecplot format

WRITE(diag_plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
WRITE(diag_plot_iounit,2022)
2022 FORMAT("variables=""x"" ""y"" ""Froude No."" ""Courant No.""")

!-----------------------------------------------------------------------

CALL plot_file_setup_tecplot()
CALL plot_print_tecplot(start_time%date_string, start_time%time_string,&
     &salinity, baro_press)


!OPEN(55,file='ds_elev-file2.dat')
!DO j=1,block(1)%ymax+1
! READ(55,*)ds_elev_read(j)
!END DO
!CLOSE(55)

!----------------------------------------------------------------------------------------------------------
! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
!----------------------------------------------------------------------------------
! Time Marching Loop

DO WHILE( current_time%time <= end_time%time ) 



IF(do_flow)THEN

! Assign U,V,D BCs for this time
! set U boundary conditions for this time

!----------------------------------------------------------------------------
! iteration loop at a fixed time using prescribed U (or Discharge),V,D BCs

DO iteration = 1,number_hydro_iterations

!**** BLOCK LOOP HERE *****
DO iblock = 1,max_blocks 

x_end = block(iblock)%xmax
y_end = block(iblock)%ymax
ds_flux_given = .FALSE. ! ignore special velocity/flux processing if not needed


! set upstream U velocity boundary conditions; either discharge or velocity 
i=1
inlet_area(2:y_end) = block(iblock)%depth(i,2:y_end)*block(iblock)%hu2(i,2:y_end)

! loop over the total number of bc specifications
DO num_bc = 1, block_bc(iblock)%num_bc
	

	SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_loc)

	CASE("US")
	i=1
	inlet_area(2:y_end) = block(iblock)%depth(i,2:y_end)*block(iblock)%hu2(i,2:y_end)
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)

		CASE("BLOCK")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("VELO")
				con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
                                   block(iblock)%uvel(i,block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                        & block(con_block)%uvel(block(con_block)%xmax,&
                                        & block_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)+1:&
                                        & block_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)+1)
				
                                   block(iblock)%vvel(i,block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                        & block(con_block)%vvel(block(con_block)%xmax,&
                                        & block_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)+1:&
                                        & block_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)+1)
	
				dum_start = block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
				dum_end = block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
				dum_start = block_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)
				dum_end = block_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)
				dum_vel(1:y_end) = block(iblock)%uvel(i,block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                     & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) 

				END DO

			END SELECT

		CASE("TABLE")
			CALL table_interp(current_time%time,&
                             & block_bc(iblock)%bc_spec(num_bc)%table_num,&
                             & table_input, block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
					inlet_flow = table_input(j)/(1 + block_bc(iblock)%bc_spec(num_bc)%end_cell(j) -&
                                             & block_bc(iblock)%bc_spec(num_bc)%start_cell(j))
					DO jj=block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1,&
                                             & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
                                           block(iblock)%uvel(i,jj) =  inlet_flow/inlet_area(jj)

					END DO
				END DO

			CASE("VELO")
				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				block(iblock)%uvel(i,block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                     & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) &
                                     = table_input(j)

				END DO

			CASE("ELEV")

			END SELECT
			block(iblock)%vvel(i,1:y_end)  = 0.0
			

		END SELECT
		block(iblock)%ustar(i,:) = block(iblock)%uvel(i,:)
		block(iblock)%uold(i,:) =  block(iblock)%uvel(i,:)
		block(iblock)%vstar(i,1:y_end) = block(iblock)%vvel(i,1:y_end)
		block(iblock)%vold(i,1:y_end)  = block(iblock)%vvel(i,1:y_end)

	CASE("DS")
		i = x_end+1
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)
		CASE("BLOCK")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("ELEV")
				con_block = block_bc(iblock)%bc_spec(num_bc)%con_block
				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				block(iblock)%depth(i,block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                     & block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                     & block(con_block)%depth(1, block_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)+1:&
                                     & block_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)+1)

				END DO

			END SELECT

		CASE("TABLE")
			CALL table_interp(current_time%time,&
                             & block_bc(iblock)%bc_spec(num_bc)%table_num,&
                             & table_input, block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
				ds_flux_given = .TRUE.

				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
					j_dsflux_start = block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
					j_dsflux_end	 = block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
					block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end) = table_input(j)
					block(iblock)%ustar(x_end,j_dsflux_start:j_dsflux_end) =&
                                             & block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
					block(iblock)%uold(x_end,j_dsflux_start:j_dsflux_end) =&
                                             & block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
					block(iblock)%depth(x_end+1,j_dsflux_start:j_dsflux_end) =&
                                             & (block(iblock)%depth(x_end,j_dsflux_start:j_dsflux_end) +&
                                             & block(iblock)%zbot(x_end,j_dsflux_start:j_dsflux_end)) -&
                                             & block(iblock)%zbot(x_end+1,j_dsflux_start:j_dsflux_end)
				END DO

			CASE("ELEV")
				DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
					DO jj=block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1,block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
					block(iblock)%depth(i,jj) =  table_input(j) - block(iblock)%zbot(i,jj)
					END DO
				END DO

			END SELECT

		END SELECT
		block(iblock)%dstar(i,2:y_end)		=	block(iblock)%depth(i,2:y_end)
		block(iblock)%depthold(i,2:y_end) = block(iblock)%depth(i,2:y_end)

! these are dummied in for later implementation
	CASE("RIGHT")
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)
		CASE("BLOCK")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("MATCH")

			END SELECT

		CASE("TABLE")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("VELO")
			CASE("ELEV")
			
			END SELECT

		END SELECT

	CASE("LEFT")
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_type)
		CASE("BLOCK")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("MATCH")

			END SELECT

		CASE("TABLE")
			SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("VELO")
			CASE("ELEV")
			END SELECT

		END SELECT


	END SELECT


END DO
!-------------------------------------------------------------------------





!----------------------------------------------------------------------------
! U momentum  solution


! compute U momentum discretization equation coefficients
 DO i=2,x_end
    DO j=2,y_end
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
      depth_w = block(iblock)%depth(i,j)
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
      
			IF(manning)THEN
        roughness = (grav*block(iblock)%chezy(i,j)**2)/(mann_con*depth_p**0.3333333)
      ELSE
				roughness = block(iblock)%chezy(i,j)
			ENDIF
      block(iblock)%bedshear1(i,j) = roughness*density*block(iblock)%uvel(i,j) &
      *sqrt(block(iblock)%uvel(i,j)*block(iblock)%uvel(i,j) + v_p*v_p)

      coeff%source(i,j) = -hp1*hp2*block(iblock)%bedshear1(i,j)/density 

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


      coeff%ap(i,j) = coeff%ae(i,j)+coeff%aw(i,j)+coeff%an(i,j)+coeff%as(i,j) + apo 
      coeff%bp(i,j) = coeff%source(i,j) + apo*block(iblock)%uold(i,j) &
											- 0.5*grav*hp2*(depth_e**2 - depth_w**2) &
											- grav*hp2*depth_p*(block(iblock)%zbot(i+1,j) - block(iblock)%zbot(i,j))

      ! compute and store for use in pressure correction equation

      coeff%lud(i,j) = 0.5*grav*hp2*(depth_e+depth_w)/coeff%ap(i,j)
    END DO
 END DO

 

 ! solve for U* using line-by-line TDMA
 !  solves a set of linear equations of the form:
 !  a(i)C(i) = b(i)C(i+1) + c(i)C(i-1) + d(i)
 !
 DO sweep=1,scalar_sweep
 !if(x_sweep)THEN
 !DO j=2,y_end
 !   DO i=2,x_end
 !   cc(i) = coeff%aw(i,j)
 !   aa(i) = coeff%ap(i,j)
 !   bb(i) = coeff%ae(i,j)
 !   dd(i) = coeff%an(i,j)*block(iblock)%ustar(i,j+1) + coeff%as(i,j)*block(iblock)%ustar(i,j-1) &
 !           + coeff%bp(i,j)
 !   END DO

 ! set the special boundary coefficients

 ! aa(1) = 1.0
 ! bb(1) = 0.0
 ! cc(1) = 0.0
 ! dd(1) = block(iblock)%ustar(1,j) 
 ! aa(x_end+1) = 1.0
 ! bb(x_end+1) = 0.0
 ! cc(x_end+1) = 1.0
 ! dd(x_end+1) = 0.0
 ! CALL tridag(1,x_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
 ! DO i=1,x_end+1
 !   block(iblock)%ustar(i,j) = tt(i)
 ! END DO

 ! END DO
 ! ENDIF


  ! U* solution LBL sweep in the y direction
  !
!  if(y_sweep)THEN
  DO i=2,x_end
    DO j=2,y_end
    cc(j) = coeff%as(i,j)
    aa(j) = coeff%ap(i,j)
    bb(j) = coeff%an(i,j)
    dd(j) = coeff%ae(i,j)*block(iblock)%ustar(i+1,j) + coeff%aw(i,j)*block(iblock)%ustar(i-1,j) &
            + coeff%bp(i,j)
    END DO
  aa(1) = 1.0
  bb(1) = 1.0
  cc(1) = 0.0
  dd(1) = 0.0
  aa(y_end+1) = 1.0
  bb(y_end+1) = 0.0
  cc(y_end+1) = 1.0
  dd(y_end+1) = 0.0
  CALL tridag(1,y_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
  DO j=1,y_end+1
    block(iblock)%ustar(i,j) = tt(j)
  END DO

  END DO

 ! ENDIF  !y-sweep

 END DO

! end U momentum  solution
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
! V momentum  solution

! compute V Momentum discretization equation coefficients
 DO i=2,x_end
    DO j=2,y_end-1
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
      depth_s = block(iblock)%depth(i,j)
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

			IF(manning)THEN
        roughness = (grav*block(iblock)%chezy(i,j)**2)/(mann_con*depth_p**0.3333333)
      ELSE
				roughness = block(iblock)%chezy(i,j)
			ENDIF
      block(iblock)%bedshear2(i,j) = roughness*density*block(iblock)%vvel(i,j) &
      *sqrt(block(iblock)%vvel(i,j)*block(iblock)%vvel(i,j) + u_p*u_p)

      coeff%source(i,j) = -hp1*hp2*block(iblock)%bedshear2(i,j)/density 
      
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
	IF((i /=2).AND.(i/=x_end))THEN
      h2_xsi_e = block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i,j)
      h2_xsi_w = block(iblock)%hv2(i,j) - block(iblock)%hv2(i-1,j)
      h1_eta_e = block(iblock)%hu1(i,j+1) - block(iblock)%hu1(i,j)
      h1_eta_w = block(iblock)%hu1(i-1,j+1) - block(iblock)%hu1(i-1,j)
	ELSE IF(i==2)THEN
			h2_xsi_w = 2.0*(block(iblock)%hv2(i,j) - block(iblock)%hv2(i-1,j))
	ELSE IF(i==x_end)THEN
			h2_xsi_e = 2.0*(block(iblock)%hv2(i+1,j) - block(iblock)%hv2(i,j))

	END IF

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
      coeff%bp(i,j) = coeff%source(i,j) + apo*block(iblock)%vold(i,j) &
											- 0.5*grav*hp1*(depth_n**2 - depth_s**2) &
											- grav*hp1*depth_p*(block(iblock)%zbot(i,j+1) - block(iblock)%zbot(i,j))

      ! compute and store for use in pressure correction equation

      coeff%lvd(i,j) = 0.5*grav*hp1*(depth_n+depth_s)/coeff%ap(i,j)
    END DO
 END DO

 ! solve for V* using line-by-line TDMA
 !  solves a set of linear equations of the form:
 !  a(i)C(i) = b(i)C(i+1) + c(i)C(i-1) + d(i)
 !
 DO sweep=1,scalar_sweep
! if(x_sweep)THEN
! DO j=2,y_end-1
!    DO i=2,x_end
!    cc(i) = coeff%aw(i,j)
!    aa(i) = coeff%ap(i,j)
!    bb(i) = coeff%ae(i,j)
!    dd(i) = coeff%an(i,j)*block(iblock)%vstar(i,j+1) + coeff%as(i,j)*block(iblock)%vstar(i,j-1) &
!            + coeff%bp(i,j)
!    END DO

 ! set the special boundary coefficients

!  aa(1) = 1.0
!  bb(1) = 0.0
!  cc(1) = 0.0
!  dd(1) = block(iblock)%vstar(1,j) 
!  aa(x_end+1) = 1.0
!  bb(x_end+1) = 0.0
!  cc(x_end+1) = 1.0
!  dd(x_end+1) = 0.0
!  CALL tridag(1,x_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
!  DO i=1,x_end+1
!    block(iblock)%vstar(i,j) = tt(i)
!  END DO
!  END DO
!  ENDIF

  ! V* solution LBL sweep in the y direction
  !
!  if(y_sweep)THEN
  DO i=2,x_end
    DO j=2,y_end-1
    cc(j) = coeff%as(i,j)
    aa(j) = coeff%ap(i,j)
    bb(j) = coeff%an(i,j)
    dd(j) = coeff%ae(i,j)*block(iblock)%vstar(i+1,j) + coeff%aw(i,j)*block(iblock)%vstar(i-1,j) &
            + coeff%bp(i,j)
    END DO
  aa(1) = 1.0
  bb(1) = 0.0
  cc(1) = 0.0
  dd(1) = block(iblock)%vvel(i,1)
  aa(y_end) = 1.0
  bb(y_end) = 0.0
  cc(y_end) = 0.0
  dd(y_end) = block(iblock)%vvel(i,y_end)
  CALL tridag(1,y_end,aa,bb,cc,dd,tt,ptemp,qtemp)
  DO j=1,y_end
    block(iblock)%vstar(i,j) = tt(j)
  END DO

  END DO

! END IF  ! y-sweep
 END DO
! end V momentum  solution
!----------------------------------------------------------------------------

! update ustar, vstar if we have given velocity/flux conditions
IF(ds_flux_given)THEN
! loop over the total number of bc specifications
	DO num_bc = 1, block_bc(iblock)%num_bc
	

	IF((block_bc(iblock)%bc_spec(num_bc)%bc_loc=="DS").AND.(block_bc(iblock)%bc_spec(num_bc)%bc_type=="TABLE"))THEN
	
		CALL table_interp(current_time%time,&
                     & block_bc(iblock)%bc_spec(num_bc)%table_num,&
                     & table_input, block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
		CASE("FLUX")
		CASE("VELO") ! can specifiy the velocity (e.g, zero flow)
			

			DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				j_dsflux_start = block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
				j_dsflux_end	 = block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
				block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end) = table_input(j)
				block(iblock)%ustar(x_end,j_dsflux_start:j_dsflux_end) =&
                                     & block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
				block(iblock)%uold(x_end,j_dsflux_start:j_dsflux_end) =&
                                     & block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
				block(iblock)%depth(x_end+1,j_dsflux_start:j_dsflux_end) =&
                                     & (block(iblock)%depth(x_end,j_dsflux_start:j_dsflux_end) +&
                                     & block(iblock)%zbot(x_end,j_dsflux_start:j_dsflux_end)) -&
                                     & block(iblock)%zbot(x_end+1,j_dsflux_start:j_dsflux_end)

				coeff%lud(x_end,j_dsflux_start:j_dsflux_end) =  0.0
 
			END DO
		END SELECT
	END IF

	END DO
END IF

 !----------------------------------------------------------------------------
 ! solve depth correction equation

 ! compute coefficients in the depth correction equation
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

			IF(i == 2)			depth_w = block(iblock)%depth(i-1,j)
			IF(i == x_end)	depth_e = block(iblock)%depth(i+1,j)
			IF(j == 2)			depth_s = block(iblock)%depth(i,j-1)
			IF(j == y_end)	depth_n = block(iblock)%depth(i,j+1)

      flux_e = he2*block(iblock)%ustar(i,j)*depth_e
      flux_w = hw2*block(iblock)%ustar(i-1,j)*depth_w
      flux_n = hn1*block(iblock)%vstar(i,j)*depth_n
      flux_s = hs1*block(iblock)%vstar(i,j-1)*depth_s

      cpo = hp1*hp2/delta_t
      block(iblock)%mass_source(i,j) = cpo*(block(iblock)%depthold(i,j) - block(iblock)%depth(i,j)) &
                                + flux_w - flux_e + flux_s - flux_n

  coeff%ce(i,j) = he2*depth_e*coeff%lud(i,j)
  coeff%cw(i,j) = hw2*depth_w*coeff%lud(i-1,j)
  coeff%cn(i,j) = hn1*depth_n*coeff%lvd(i,j)
  coeff%cs(i,j) = hs1*depth_s*coeff%lvd(i,j-1)
  coeff%cp(i,j) = coeff%ce(i,j) + coeff%cw(i,j) + coeff%cn(i,j) + coeff%cs(i,j) &
                   + cpo
  END DO
 END DO
 ! solve for d' using line-by-line TDMA
 !  solves a set of linear equations of the form:
 !  a(i)C(i) = b(i)C(i+1) + c(i)C(i-1) + d(i)
 !
 DO sweep=1,depth_sweep
! if(x_sweep_dp)THEN
! DO j=2,y_end
!    DO i=2,x_end
!   cc(i) = coeff%cw(i,j)
!    aa(i) = coeff%cp(i,j)
!    bb(i) = coeff%ce(i,j)
!    dd(i) = coeff%cn(i,j)*block(iblock)%dp(i,j+1) + coeff%cs(i,j)*block(iblock)%dp(i,j-1) &
!            + block(iblock)%mass_source(i,j)
!    END DO

 ! set the special boundary coefficients
 ! if depth is specified then d'=0 since no depth correction is required
 ! if velocity is specified then that d' coeff will be zero

!  aa(2) = coeff%cp(2,j) - coeff%cw(2,j)
  !!bb(1) = 0.0
!  cc(2) = 0.0
  !dd(1) = block(iblock)%conc(0,j) 
!  aa(x_end+1) = 1.0
!  bb(x_end+1) = 0.0
!  cc(x_end+1) = 0.0
!  dd(x_end+1) = 0.0 ! d' is zero at given depth
!  CALL tridag(2,x_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
!  DO i=2,x_end+1
!    block(iblock)%dp(i,j) = tt(i)
!  END DO
!  END DO
!  END IF


  ! LBL sweep in the y direction
  ! d' depth correction equation
!  if(y_sweep_dp)THEN
  DO i=2,x_end
    DO j=2,y_end
    cc(j) = coeff%cs(i,j)
    aa(j) = coeff%cp(i,j)
    bb(j) = coeff%cn(i,j)
    dd(j) = coeff%ce(i,j)*block(iblock)%dp(i+1,j) + coeff%cw(i,j)*block(iblock)%dp(i-1,j) &
            + block(iblock)%mass_source(i,j)
    END DO
  aa(2) = coeff%cp(i,2) - coeff%cs(i,2)
  !bb(1) = 1.0
  cc(2) = 0.0
  !dd(1) = 0.0
  aa(y_end) = coeff%cp(i,y_end) - coeff%cn(i,y_end)
  bb(y_end) = 0.0
  !cc(y_end) = 1.0
  !dd(y_end) = 0.0
  CALL tridag(2,y_end,aa,bb,cc,dd,tt,ptemp,qtemp)
  DO j=2,y_end
    block(iblock)%dp(i,j) = tt(j)
  END DO

 END DO
! ENDIF  ! y-sweep

 END DO
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
IF(ds_flux_given)THEN
! loop over the total number of bc specifications
	DO num_bc = 1, block_bc(iblock)%num_bc
	

	IF((block_bc(iblock)%bc_spec(num_bc)%bc_loc=="DS").AND.(block_bc(iblock)%bc_spec(num_bc)%bc_type=="TABLE"))THEN
	
		CALL table_interp(current_time%time,&
                     & block_bc(iblock)%bc_spec(num_bc)%table_num,&
                     & table_input, block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
		SELECT CASE(block_bc(iblock)%bc_spec(num_bc)%bc_kind)
		CASE("FLUX")
		CASE("VELO") ! can specifiy the velocity (e.g, zero flow)

			DO j=1,block_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				j_dsflux_start = block_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
				j_dsflux_end	 = block_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
				block(iblock)%depth(x_end+1,j_dsflux_start:j_dsflux_end) =&
                                     & (block(iblock)%depth(x_end,j_dsflux_start:j_dsflux_end) +&
                                     & block(iblock)%zbot(x_end,j_dsflux_start:j_dsflux_end)) -&
                                     & block(iblock)%zbot(x_end+1,j_dsflux_start:j_dsflux_end)
			END DO
		END SELECT
	END IF

	END DO
END IF

!----------------------------------------------------------------------------
! extrapolate water surface elevation to upstream *** NEED TO IMPLEMENT LINEAR EXTRAPOLATION ****
! equal water surface elevation - better for bathymetry; not great for uniform slope channel

block(iblock)%depth(1,2:y_end) = (block(iblock)%depth(2,2:y_end) + block(iblock)%zbot(2,2:y_end)) &
     + 0.5*((block(iblock)%depth(2,2:y_end)+block(iblock)%zbot(2,2:y_end)) -&
     & (block(iblock)%depth(3,2:y_end)+block(iblock)%zbot(3,2:y_end))) - block(iblock)%zbot(1,2:y_end)

!-----------------------------------------------------------------------------
! apply zero gradient conditions

!IF((iblock == 2).OR.(max_blocks == 1))THEN
!   block(iblock)%uvel(x_end+1,2:y_end) = block(iblock)%ustar(x_end,2:y_end)
!ELSE
!	block(iblock)%uvel(x_end+1,2:y_end) = block(iblock+1)%uvel(1,2:y_end)
!ENDIF

! zero gradient conditions on the sides of the block
!		need to modify for side inflows/block connections
DO i=1,x_end+1
   block(iblock)%uvel(i,1) = block(iblock)%ustar(i,2)
   block(iblock)%uvel(i,y_end+1) = block(iblock)%ustar(i,y_end)
   block(iblock)%depth(i,1) = (block(iblock)%depth(i,2)+block(iblock)%zbot(i,2)) - block(iblock)%zbot(i,1)
   block(iblock)%depth(i,y_end+1) = (block(iblock)%depth(i,y_end)+block(iblock)%zbot(i,y_end)) - block(iblock)%zbot(i,y_end+1)
END DO

!----------------------------------------------------------------------------------------------
! check for small and negative depth condition and report location

IF(MINVAL(block(iblock)%depth) <= 0.20)THEN
	WRITE(error_iounit,*)" WARNING: Small Depth = ",MINVAL(block(iblock)%depth)
	WRITE(error_iounit,*)"     Block Number = ",iblock
	WRITE(error_iounit,*)"     I,J Location of small depth = ",MINLOC(block(iblock)%depth)
END IF

IF(MINVAL(block(iblock)%depth) <= 0.0)THEN
	WRITE(error_iounit,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
	WRITE(error_iounit,*)"     Block Number = ",iblock
	WRITE(error_iounit,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)
	
	WRITE(*,*)" FATAL ERROR: Negative Depth = ",MINVAL(block(iblock)%depth)
	WRITE(*,*)"     Block Number = ",iblock
	WRITE(*,*)"     I,J Location of negative depth = ",MINLOC(block(iblock)%depth)

	CALL EXIT  ! abort run if you hit a negative depth
END IF

!----------------------------------------------------------------------------------------------
! return to momentum equation solution step using updated depth and velocities
!----------------------------------------------------------------------------------------------

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

END IF		! if-block for on/off hydrodynamics


!********************************************************************************************************
!--------------------------------------------------------------------------------------------------------
! scalar transport solution
!--------------------------------------------------------------------------------------------------------
!********************************************************************************************************

IF(do_transport)THEN

IF(do_surface_heatx .OR. do_surface_gasx)THEN
	CALL update_met_data(current_time%time)
END IF

! INTERNAL ITERATION AT THIS TIME LEVEL LOOP
DO iteration = 1,number_scalar_iterations

! BLOCK LOOP
DO iblock = 1,max_blocks

x_end = block(iblock)%xmax
y_end = block(iblock)%ymax

! SPECIES LOOP - multiple numbers of scalar variables
DO ispecies = 1, max_species

! set boundary conditions for this time
!IF(iblock == 1)THEN
!		block(iblock)%conc(i,2:y_end) = 100.0
!		block(iblock)%concold(i,2:y_end) = 100.0
!	
!		block(iblock)%conc(i,jspill_start:jspill_end) = spill_conc
!		block(iblock)%concold(i,jspill_start:jspill_end) = spill_conc
!	
!		block(iblock)%conc(i,jgen_start:jgen_end) = gen_conc
!		block(iblock)%concold(i,jgen_start:jgen_end) = gen_conc
!ELSE
!		block(iblock)%conc(i,2:y_end) = block(iblock-1)%conc(block(iblock-1)%xmax,2:block(iblock-1)%ymax)
!		block(iblock)%concold(i,2:y_end) = block(iblock-1)%concold(block(iblock-1)%xmax,2:block(iblock-1)%ymax)
!END IF

! loop over the total number of bc specifications
DO num_bc = 1, scalar_bc(iblock)%num_bc

IF(scalar_bc(iblock)%bc_spec(num_bc)%species == ispecies)THEN

SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_loc)

	CASE("US")
	i=1
	x_start = i + 1
	inlet_area(2:y_end) = block(iblock)%depth(i,2:y_end)*block(iblock)%hu2(i,2:y_end)
		SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_type)

		CASE("ZEROG")
			DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				species(ispecies)%scalar(iblock)%conc(i,scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                     & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                     & species(ispecies)%scalar(iblock)%conc(i+1,&
                                     & scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                     & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1)

			END DO

		CASE("BLOCK")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("CONC")
				con_block = scalar_bc(iblock)%bc_spec(num_bc)%con_block
				DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
                                   species(ispecies)%scalar(iblock)%conc(i,scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                        & 0.5*(species(ispecies)%scalar(iblock)%&
                                        & conc(2,scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) +&
                                        & species(ispecies)%scalar(con_block)%conc(block(con_block)%xmax,&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)+1))
				
				END DO

			END SELECT

		CASE("TABLE")
			CALL scalar_table_interp(current_time%time,&
                             & scalar_bc(iblock)%bc_spec(num_bc)%table_num,&
                             & table_input, scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
				DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
					inlet_flow = table_input(j)/(1 + scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j) -&
                                             & scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j))
					DO jj=scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1,&
                                             & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
                                           species(ispecies)%scalar(iblock)%conc(i,jj) =  inlet_flow/inlet_area(jj)

					END DO
				END DO

			CASE("CONC")
				i = scalar_bc(iblock)%bc_spec(num_bc)%x_start
				x_start = i + 1
				DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
          species(ispecies)%scalar(iblock)%conc(i,scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                           & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) = table_input(j)

				END DO
			
			END SELECT

	END SELECT
	species(ispecies)%scalar(iblock)%concold(i,:) =  species(ispecies)%scalar(iblock)%conc(i,:)
	
	CASE("DS")
		i = x_end+1
		SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_type)

		CASE("ZEROG")
			DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
                           species(ispecies)%scalar(iblock)%conc(x_end+1,&
                                & scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                & species(ispecies)%scalar(iblock)%conc(x_end,&
                                & scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1)

			END DO
		CASE("BLOCK")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("CONC")
				con_block = scalar_bc(iblock)%bc_spec(num_bc)%con_block
				DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
                                   species(ispecies)%scalar(iblock)%conc(i,scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) =&
                                        & 0.5*(species(ispecies)%scalar(iblock)%conc(x_end,&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1) +&
                                        & species(ispecies)%scalar(con_block)%conc(2,&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%con_start_cell(j)+1:&
                                        & scalar_bc(iblock)%bc_spec(num_bc)%con_end_cell(j)+1) )
				
				END DO

			END SELECT

		CASE("TABLE")
			CALL scalar_table_interp(current_time%time,&
                             & scalar_bc(iblock)%bc_spec(num_bc)%table_num,&
                             & table_input, scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs)
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("CONC") ! can specifiy the concentration

				!DO j=1,scalar_bc(iblock)%bc_spec(num_bc)%num_cell_pairs
				!	j_dsflux_start = scalar_bc(iblock)%bc_spec(num_bc)%start_cell(j)+1
				!	j_dsflux_end	 = scalar_bc(iblock)%bc_spec(num_bc)%end_cell(j)+1
				!	block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end) = table_input(j)
				!	block(iblock)%ustar(x_end,j_dsflux_start:j_dsflux_end) = block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
				!	block(iblock)%uold(x_end,j_dsflux_start:j_dsflux_end) =  block(iblock)%uvel(x_end,j_dsflux_start:j_dsflux_end)
				!	block(iblock)%depth(x_end+1,j_dsflux_start:j_dsflux_end) = (block(iblock)%depth(x_end,j_dsflux_start:j_dsflux_end) + block(iblock)%zbot(x_end,j_dsflux_start:j_dsflux_end)) &
	      ! - block(iblock)%zbot(x_end+1,j_dsflux_start:j_dsflux_end)
				!END DO



			END SELECT

		END SELECT
		
		species(ispecies)%scalar(iblock)%concold(i,1:y_end) = species(ispecies)%scalar(iblock)%conc(i,1:y_end)

! these are dummied in for later implementation
	CASE("RIGHT")
		SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_type)
		CASE("BLOCK")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("MATCH")

			END SELECT

		CASE("TABLE")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("VELO")
			
			END SELECT
		END SELECT

	CASE("LEFT")
		SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_type)
		CASE("BLOCK")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("MATCH")

			END SELECT

		CASE("TABLE")
			SELECT CASE(scalar_bc(iblock)%bc_spec(num_bc)%bc_kind)
			CASE("FLUX")
			CASE("VELO")
			
			END SELECT

		END SELECT


END SELECT

END IF 

END DO ! num bc loop


!-------------------------------------------------------------------------
! compute scalar transport discretization equation coefficients

 DO i= x_start,x_end
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
  
			k_e = 0.5*(block(iblock)%kx_diff(i,j)+block(iblock)%kx_diff(i+1,j))
      k_w = 0.5*(block(iblock)%kx_diff(i,j)+block(iblock)%kx_diff(i-1,j))
      k_n = 0.5*(block(iblock)%ky_diff(i,j)+block(iblock)%ky_diff(i,j+1))
      k_s = 0.5*(block(iblock)%ky_diff(i,j)+block(iblock)%ky_diff(i,j-1))

      depth_e = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i+1,j))
      depth_w = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i-1,j))
      depth_n = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j+1))
      depth_s = 0.5*(block(iblock)%depth(i,j)+block(iblock)%depth(i,j-1))

			IF(i == 2)			depth_w = block(iblock)%depth(i-1,j)
			IF(i == x_end)	depth_e = block(iblock)%depth(i+1,j)
			IF(j == 2)			depth_s = block(iblock)%depth(i,j-1)
			IF(j == y_end)	depth_n = block(iblock)%depth(i,j+1)

      flux_e = he2*block(iblock)%uvel(i,j)*depth_e
      flux_w = hw2*block(iblock)%uvel(i-1,j)*depth_w
      flux_n = hn1*block(iblock)%vvel(i,j)*depth_n
      flux_s = hs1*block(iblock)%vvel(i,j-1)*depth_s
      diffu_e =  k_e*depth_e*he2/he1
      diffu_w =  k_w*depth_w*hw2/hw1
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

      apo = hp1*hp2*block(iblock)%depthold(i,j)/delta_t

      coeff%source(i,j) = 0.0 
			IF( (ispecies == 2) .AND. do_surface_heatx )THEN
				t_water = species(ispecies)%scalar(iblock)%conc(i,j)
				coeff%source(i,j) = net_heat_flux(net_solar, t_water, t_air, t_dew, windspeed) &
															/(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet
				
				!WRITE(*,*)"net heat flux = ",net_heat_flux(net_solar, t_water, t_air, t_dew, windspeed)
				!WRITE(*,*)"source term = ", coeff%source(i,j)
				!WRITE(*,*)"net_solar = ", net_solar
				!WRITE(*,*)"t_water = ", t_water
				!WRITE(*,*)"t_air = ",t_air
				!WRITE(*,*)"t_dew = ",t_dew
				!WRITE(*,*)"windspeed =",windspeed
				!READ(*,*)
			
			END IF

			IF((ispecies == 1) .AND. do_surface_gasx )THEN
				conc_TDG = species(ispecies)%scalar(iblock)%conc(i,j)
				t_water = species(2)%scalar(iblock)%conc(i,j)
				ccstar = TDGasConc( baro_press, t_water,  salinity ) ! c* will be the conc at Barometric Press.
				
				transfer_coeff = gasx_a + gasx_b*windspeed + gasx_c*windspeed**2 + gasx_d*windspeed**3
				transfer_coeff = transfer_coeff*3.2808/84600.0 ! convert from meters/day to feet/sec

				coeff%source(i,j) = transfer_coeff*( ccstar - conc_TDG )

			END IF

      coeff%ap(i,j) = coeff%ae(i,j)+coeff%aw(i,j)+coeff%an(i,j)+coeff%as(i,j) &
       + apo 
      coeff%bp(i,j) = coeff%source(i,j)*hp1*hp2 + apo*species(ispecies)%scalar(iblock)%concold(i,j)
    
		END DO
 END DO

 ! solve using line-by-line TDMA
 !  solves a set of linear equations of the form:
 !  a(i)C(i) = b(i)C(i+1) + c(i)C(i-1) + d(i)
 !
DO sweep=1,scalar_sweep
! if(x_sweep)THEN
! DO j=2,y_end
!    DO i=2,x_end
!    cc(i) = coeff%aw(i,j)
!    aa(i) = coeff%ap(i,j)
!    bb(i) = coeff%ae(i,j)
!    dd(i) = coeff%an(i,j)*block(iblock)%conc(i,j+1) + coeff%as(i,j)*block(iblock)%conc(i,j-1) &
!            + coeff%bp(i,j)
!    END DO

 ! set the special boundary coefficients

!  aa(1) = 1.0
!  bb(1) = 0.0
!  cc(1) = 0.0
!  dd(1) = block(iblock)%conc(1,j) 
!  aa(x_end+1) = 1.0
!  bb(x_end+1) = 0.0
!  cc(x_end+1) = 1.0
!  dd(x_end+1) = 0.0
!  CALL tridag(1,x_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
!  DO i=1,x_end+1
!    block(iblock)%conc(i,j) = tt(i)
!    END DO
!   END DO
!  END IF
  ! LBL sweep in the y direction
  !
!  if(y_sweep)THEN
DO i= x_start,x_end
	DO j=2,y_end
    cc(j) = coeff%as(i,j)
    aa(j) = coeff%ap(i,j)
    bb(j) = coeff%an(i,j)
    dd(j) = coeff%ae(i,j)*species(ispecies)%scalar(iblock)%conc(i+1,j) +&
         & coeff%aw(i,j)*species(ispecies)%scalar(iblock)%conc(i-1,j) &
            + coeff%bp(i,j)
	END DO
  aa(1) = 1.0
  bb(1) = 1.0
  cc(1) = 0.0
  dd(1) = 0.0 !block(iblock)%conc(i,0) 
  aa(y_end+1) = 1.0
  bb(y_end+1) = 0.0
  cc(y_end+1) = 1.0
  dd(y_end+1) = 0.0 !block(iblock)%conc(i,y_end+1) 
  CALL tridag(1,y_end+1,aa,bb,cc,dd,tt,ptemp,qtemp)
  DO j=1,y_end+1
    species(ispecies)%scalar(iblock)%conc(i,j) = tt(j)
  END DO
  ! set zero gradient at shoreline
  !block(iblock)%conc(i,0) = block(iblock)%conc(i,1)
  !block(iblock)%conc(i,y_end+1) = block(iblock)%conc(i,y_end)
END DO
! ENDIF ! y-sweep

! set zero downstream gradient at exit if last block
!IF(max_blocks == 1)THEN
!	block(iblock)%conc(x_end+1,1:y_end+1) = block(iblock)%conc(x_end,1:y_end+1)
!	block(iblock)%concold(x_end+1,1:y_end+1) = block(iblock)%concold(x_end,1:y_end+1)
!ELSE
!	IF(iblock == 1)THEN
!		block(iblock)%conc(x_end+1,1:y_end+1) = block(iblock+1)%conc(2,1:y_end+1)
!		block(iblock)%concold(x_end+1,1:y_end+1) = block(iblock+1)%concold(2,1:y_end+1)
!	ELSE
!		block(iblock)%conc(x_end+1,1:y_end+1) = block(iblock)%conc(x_end,1:y_end+1)
!		block(iblock)%concold(x_end+1,1:y_end+1) = block(iblock)%concold(x_end,1:y_end+1)
!	END IF
!END IF

END DO ! conc scalar sweep


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

END IF	! if-block for transport equation solution

! end scalar transport soultion
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
! update old values of dependent variables
DO iblock=1,max_blocks

  block(iblock)%uold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%uvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%vold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%vvel(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%depthold(2:block(iblock)%xmax,2:block(iblock)%ymax) =&
       & block(iblock)%depth(2:block(iblock)%xmax,2:block(iblock)%ymax) 
  block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot

END DO

DO ispecies = 1, max_species
	DO iblock = 1, max_blocks
		species(ispecies)%scalar(iblock)%concold(2:block(iblock)%xmax,2:block(iblock)%ymax) &
				= species(ispecies)%scalar(iblock)%conc(2:block(iblock)%xmax,2:block(iblock)%ymax)
	END DO
END DO
!---------------------------------------------------------------------------
!update model time
 time_step_count = time_step_count + 1

! update decimal julian day time
current_time%time = current_time%time + delta_t/86400.0d0 ! remember that the delta is in SECONDS

CALL decimal_to_date(current_time%time, current_time%date_string, current_time%time_string)



!----------------------------------------------------------------------------
! printout variables over the whole field to ascii file and TECPLOT format file
IF( (current_time%time >= end_time%time) .OR. (MOD(time_step_count,print_freq) == 0) )THEN
 
	1010 FORMAT(50(f12.6,2x))

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
1012 FORMAT(50(f12.4,2x))
1011 FORMAT(i5,2x)

	IF(do_transport)THEN
		DO ispecies = 1, max_species
			title = "Concentration "
			title(61:75) = block_title
			CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,species(ispecies)%scalar(iblock)%conc)
		END DO

		DO i=1,block(iblock)%xmax+1
		DO j=1,block(iblock)%ymax+1
		conc_TDG = species(1)%scalar(iblock)%conc(i,j)
		t_water = species(2)%scalar(iblock)%conc(i,j)
		block(iblock)%TDG_stuff(i,j) = TDGasPress(conc_TDG,  t_water,  salinity)
		END DO
		END DO
		CALL output_2d_array(output_iounit,title,1,block(iblock)%xmax+1,1,block(iblock)%ymax+1,block(iblock)%TDG_stuff)

	END IF

END DO 

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

DO iblock=1,max_blocks

!---------------------------------------------------------------
! diagnostic plot file output; tecplot format
   WRITE(diag_plot_iounit,*)"zone f=block"," t=""",zone_name,iblock,""""," i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
   WRITE(diag_plot_iounit,*)block(iblock)%x
   WRITE(diag_plot_iounit,*)block(iblock)%y

   block(iblock)%froude_num = SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
        SQRT(grav*block(iblock)%depth)
   WRITE(diag_plot_iounit,*)block(iblock)%froude_num

   block(iblock)%courant_num = delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
         SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
         SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)
   WRITE(diag_plot_iounit,*)block(iblock)%courant_num
   

END DO

!-------------------------------------------------------------------------------------------------------
! print out in tecplot block format
!
CALL plot_print_tecplot(current_time%date_string, current_time%time_string, &
     &salinity, baro_press)

ENDIF
! output/plot if-block
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! print out to gage output files
IF(do_gage_print)THEN
	IF(MOD(time_step_count,gage_print_freq) == 0)THEN
		DO i=1,num_gages
			OPEN(50,file=gage_specs(i)%filename, POSITION="APPEND")
			iblock = gage_specs(i)%block
			icell = gage_specs(i)%i_cell + 1 ! convert from cell to i,j
			jcell = gage_specs(i)%j_cell + 1

			IF(do_transport)THEN
				DO ispecies=1,max_species
					species_io_vec(ispecies) = species(ispecies)%scalar(iblock)%conc(icell,jcell)
				END DO
			END IF
			conc_TDG = species(1)%scalar(iblock)%conc(icell,jcell)
			t_water = species(2)%scalar(iblock)%conc(icell,jcell)
			WRITE(50,2020)current_time%date_string,current_time%time_string, &
				(current_time%time - start_time%time)*24, &
				block(iblock)%wsel(icell,jcell),block(iblock)%depth(icell,jcell), &
				SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2), &
				block(iblock)%uvel(icell,jcell), block(iblock)%vvel(icell,jcell), &
				SUM(ABS(block(iblock)%mass_source)), &
				species_io_vec(1:max_species), &
				TDGasPress(conc_TDG,  t_water,  salinity), &
				TDGasDP(conc_TDG, t_water,  salinity,  baro_press), &
				TDGasSaturation( conc_TDG, t_water,  salinity, baro_press)

			CLOSE(50)
		END DO

		! print out mass source monitoring information
		IF(time_step_count == 1)THEN
			WRITE(mass_source_iounit,*)"# mass source history - summation of the mass source in each block "
			WRITE(mass_source_iounit,*)"#      total mass imbalance for each block in ft3/sec"
			WRITE(mass_source_iounit,3005,advance='no')
			DO iblock = 1, max_blocks 
				WRITE(mass_source_iounit,3011, advance='no')iblock
			END DO
			WRITE(mass_source_iounit,*)
		END IF
        j = 1
        DO iblock = 1, max_blocks
           IF (j == 1) &
                &WRITE(mass_source_iounit,3013, advance='no')current_time%date_string,current_time%time_string
           WRITE(mass_source_iounit,3012, advance='no')SUM(ABS(block(iblock)%mass_source))
           IF (j >= 20) THEN
              j = 1
              IF (iblock .ne. max_blocks) WRITE(mass_source_iounit,*)
           ELSE
              j = j + 1
           END IF
        END DO
        WRITE(mass_source_iounit,*)

3013 FORMAT(a10,2x,a8,2x)
3012 FORMAT((f12.2,1x))
3011 FORMAT(i5,5x)
3005 FORMAT('#date',8x,'time',5x)

	END IF
END IF

2020 FORMAT(a10,2x,a8,2x,50(f12.2,2x))
!---------------------------------------------------------------------------
! write a binary restart file
IF(write_restart_file)THEN
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
	OPEN(unit=restart_iounit,file=restart_filename,form='formatted')

	IF(do_transport)THEN
		do_transport_restart = .TRUE.
		WRITE(restart_iounit,*) do_transport_restart, max_species
	ELSE
		do_transport_restart = .FALSE.
		WRITE(restart_iounit,*) do_transport_restart, max_species
	END IF

	DO iblock=1,max_blocks
       block(iblock)%work = block(iblock)%uvel
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work
       
       block(iblock)%work = block(iblock)%uold
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%ustar
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vvel
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vold
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%vstar
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%depth
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%depthold
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%dstar
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
       WRITE(restart_iounit,*)block(iblock)%work

       block(iblock)%work = block(iblock)%eddy
       WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
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
              WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
              WRITE(restart_iounit,*)block(iblock)%work
              
              block(iblock)%work = species(i)%scalar(iblock)%concold
              WHERE(block(iblock)%work < tiny) block(iblock)%work = tiny
              WRITE(restart_iounit,*)block(iblock)%work

             ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%conc
             ! WRITE(restart_iounit,*) species(i)%scalar(iblock)%concold
			END DO
		END DO
	END IF

	CLOSE(restart_iounit)

ENDIF
ENDIF


!***************************************************************************
END DO
! end time loop
!***************************************************************************




!--------------------------------------------------------------------------
! end of flow_solver **************
!---------------------------------------------------------------------------

CLOSE(cfg_iounit)
CLOSE(output_iounit)
CLOSE(plot_iounit)
CLOSE(error_iounit)
CLOSE(status_iounit)
CLOSE(mass_source_iounit)
CLOSE(diag_plot_iounit)

! WRITE(*,*)'*** ALL DONE - PRESS RETURN ***'
! READ(*,*)


CONTAINS
!-------------------
! internal routines
!-----------------------------------------------------------------------------
!
! Tridiangonal Matrix Solution
SUBROUTINE tridag(start, finish, a, b, c, d,sol, ptemp, qtemp)

IMPLICIT NONE

INTEGER :: i,last,ifp1,k,start,finish
DOUBLE PRECISION, DIMENSION(:) :: a,b,c,d,sol,ptemp,qtemp

DO i=start,finish
ptemp(i) = b(i)/(a(i) - c(i)*ptemp(i-1))
qtemp(i) = (d(i) + c(i)*qtemp(i-1))/(a(i)-c(i)*ptemp(i-1))
END DO

sol(finish) = qtemp(finish)

DO i=finish-1,start,-1
sol(i) = ptemp(i)*sol(i+1) + qtemp(i)
END DO


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

!-----------------------------------------------------------------------------
END PROGRAM mass2_main
