!#####################################################################################
! module to hold all the junk variables from the v025 monolithic source
!
!-------------------------------------------------------------------------------------

MODULE misc_vars

USE date_time

IMPLICIT NONE

CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

TYPE(datetime_struct), SAVE :: start_time, end_time, current_time

CHARACTER (LEN=80), SAVE :: code_version = "mass2 code version 0.27"
CHARACTER (LEN=80), SAVE :: code_date = "release: $Date$"


! INTEGER, SAVE :: n, i, j, jj, junk, icell, jcell
! INTEGER, SAVE :: imax,jmax ! ,ivec(4),jvec(4),ivec2(4),jvec2(4)
! INTEGER, SAVE :: system_time(8)
INTEGER, SAVE :: iblock !, con_block! , num_bc! , ispecies! , con_j, con_i
INTEGER, SAVE :: output_iounit=11, error_iounit=13, status_iounit=14
INTEGER, SAVE :: grid_iounit=15, hotstart_iounit=16, restart_iounit=17, bcspec_iounit=18
! INTEGER :: mass_source_iounit=19

INTEGER, SAVE :: i_start_cell, i_end_cell, j_start_cell , j_end_cell
INTEGER, SAVE :: max_species_in_restart

! DOUBLE PRECISION, SAVE :: species_io_vec(100) ! roughness, 

! DOUBLE PRECISION, ALLOCATABLE, SAVE :: aa(:),bb(:),cc(:),dd(:),tt(:),ptemp(:),qtemp(:)
DOUBLE PRECISION, ALLOCATABLE, SAVE :: inlet_area(:),table_input(:) ! work(:,:),cos_ij(:,:)



!--- v013 defs ---------------------------------------------------

INTEGER, SAVE :: x_end,y_end
! DOUBLE PRECISION, SAVE :: delta_x,delta_y
DOUBLE PRECISION, SAVE :: eddy_default, kx_diff_default, ky_diff_default ! default diffusion coeff for scalar transport
! DOUBLE PRECISION, SAVE :: k_p,k_e,k_w,k_n,k_s 

DOUBLE PRECISION, SAVE :: delta_t
INTEGER, SAVE :: time_step_count = 0, print_freq = 1, gage_print_freq = 1, restart_print_freq = 1



! DOUBLE PRECISION, SAVE :: hp1,hp2,he1,he2,hw1,hw2,hn1,hn2,hs1,hs2	! metric coefficients at p,e,w,n,s
! DOUBLE PRECISION, SAVE :: depth_e,depth_w,depth_n,depth_s,depth_p	! depths at p,e,w,n,s
! DOUBLE PRECISION, SAVE :: flux_e,flux_w,flux_n,flux_s					! fluxes
! DOUBLE PRECISION, SAVE :: diffu_e,diffu_w,diffu_n,diffu_s			! diffusion
! DOUBLE PRECISION, SAVE :: pec_e,pec_w,pec_n,pec_s	! peclet numbers
! DOUBLE PRECISION, SAVE :: apo, cpo								! coefficients in discretization eqns
! DOUBLE PRECISION, SAVE :: u_p, u_e, u_w, u_s, u_n	! u velocities at P and on staggered grid
! DOUBLE PRECISION, SAVE :: v_p, v_n, v_s, v_e, v_w	! v velocities at P and on staggered grid



! DOUBLE PRECISION, SAVE :: cross_term				! eddy viscosity cross term in momement equations
! DOUBLE PRECISION, SAVE :: curve_1,curve_2,curve_3,curve_4,curve_5,curve_6,curve_7	! curvature terms

! DOUBLE PRECISION, SAVE :: h1_eta_p, h2_xsi_p						! derivatives of metric coeff
! DOUBLE PRECISION, SAVE :: h1_eta_e, h1_eta_w, h1_eta_n, h1_eta_s	! e.g., h1_eta_p is the partial deriv
! DOUBLE PRECISION, SAVE :: h2_xsi_e, h2_xsi_w, h2_xsi_n, h2_xsi_s	! of h1 in eta direction at point p

DOUBLE PRECISION, SAVE :: chezy_con_default ! ,z_step !slope, ds_elev
DOUBLE PRECISION, SAVE :: relax_dp, relax_uv, blend_time, mann_con

! INTEGER, SAVE :: jspill_start   ! y node line to start spill at
! INTEGER, SAVE :: jspill_end			! y node line to stop spill at
! INTEGER, SAVE :: jgen_start  		! y node line to start generation at
! INTEGER, SAVE :: jgen_end				! y node line to stop generation ** max is block%ymax

! INTEGER, SAVE :: j_dsflux_start, j_dsflux_end

INTEGER, SAVE :: x_start, y_start			! x line to start scalar calculations (usually = 2)

! DOUBLE PRECISION, SAVE :: spill_flow	! unit spill flow (cfs)
! DOUBLE PRECISION, SAVE :: gen_flow   	! unit generation flow (cfs)
! DOUBLE PRECISION, SAVE :: gen_conc    ! unit generation TDG %Saturation
! DOUBLE PRECISION, SAVE :: spill_conc  ! unit spill TDG %Saturation

DOUBLE PRECISION, SAVE :: uvel_initial, vvel_initial, conc_initial, wsel_or_depth_initial ! intial values to assign over field

DOUBLE PRECISION, SAVE :: uvel_wind, vvel_wind, wind_speed, wind_drag_coeff

DOUBLE PRECISION, SAVE :: max_mass_source_sum, maxx_mass
DOUBLE PRECISION, SAVE :: salinity = 0.0 ! , ccstar ! , conc_TDG

INTEGER, SAVE :: iteration, number_hydro_iterations, number_scalar_iterations
! INTEGER, SAVE :: sweep, scalar_sweep, depth_sweep
!LOGICAL, SAVE :: x_sweep, y_sweep, x_sweep_dp, y_sweep_dp
LOGICAL, SAVE :: debug, manning
LOGICAL, SAVE :: write_restart_file, read_hotstart_file
LOGICAL, SAVE :: ds_flux_given, given_initial_wsel, read_initial_profile
LOGICAL, SAVE :: update_depth = .TRUE.
LOGICAL, SAVE :: do_flow = .TRUE. , do_transport = .FALSE. , do_gage_print = .FALSE.
LOGICAL, SAVE :: do_flow_output = .FALSE.
LOGICAL, SAVE :: do_flow_diag = .TRUE. !.FALSE.
LOGICAL, SAVE :: do_transport_restart = .FALSE.
LOGICAL, SAVE :: do_spatial_eddy, do_spatial_kx, do_spatial_ky, do_spatial_chezy
LOGICAL, SAVE :: do_surface_heatx = .FALSE. , do_surface_gasx = .FALSE.
LOGICAL, SAVE :: do_accumulate = .FALSE.
LOGICAL, SAVE :: do_rptdead = .FALSE.

DOUBLE PRECISION, SAVE :: inlet_flow
DOUBLE PRECISION, SAVE :: total_flow

! DOUBLE PRECISION, SAVE :: ds_elev_read(27)

DOUBLE PRECISION, SAVE :: transfer_coeff, gasx_a, gasx_b, gasx_c, gasx_d

!--- v013 defs ----------------------------------------------------

CHARACTER (LEN=1024), ALLOCATABLE, SAVE :: grid_file_name(:)
CHARACTER (LEN=1024), SAVE :: weather_filename
CHARACTER (LEN=1024), SAVE :: config_file_version, filename
CHARACTER*80, SAVE :: sim_title
CHARACTER*75, SAVE :: title

CHARACTER*15, SAVE :: block_title
CHARACTER*30, SAVE :: restart_filename
CHARACTER*26, SAVE :: zone_name

!-------------------------------------------------------------------------------------------------------
! debugging temps
INTEGER, SAVE :: dum_start, dum_end
DOUBLE PRECISION, SAVE :: dum_vel(100), dum_depth(100), dum_val
CHARACTER (LEN=80), SAVE :: dum_char


! ----------------------------------------------------------------
! global parameters for wetting and drying
! ----------------------------------------------------------------

                                ! flag to turn wetting and drying on

LOGICAL, SAVE :: do_wetdry = .TRUE.

                                ! flag to do wet/dry checking every
                                ! hydro iteration, rather than every
                                ! time step

LOGICAL, SAVE :: iterate_wetdry = .TRUE.

                                ! the depth that defines "dry"

DOUBLE PRECISION, SAVE :: dry_depth = 0.1

                                ! the minimum depth at which cells
                                ! are initialized (in cold starts)

DOUBLE PRECISION, SAVE :: dry_zero_depth = 0.05

                                ! the indicator depth at which
                                ! rewetting is allowed

DOUBLE PRECISION, SAVE :: dry_rewet_depth = 0.12

! ----------------------------------------------------------------
! global parameters used to control cell overlap at block boundaries,
! and ghost cells
! ----------------------------------------------------------------

INTEGER, PARAMETER :: nghost = 2
INTEGER, PARAMETER :: i_ghost = nghost, j_ghost = nghost
! INTEGER, PARAMETER :: i_index_min = 1, i_index_extra = 1
INTEGER, PARAMETER :: i_index_min = 1-i_ghost, i_index_extra = 1+i_ghost
INTEGER, PARAMETER :: j_index_min = 1-j_ghost, j_index_extra = 1+j_ghost



END MODULE misc_vars
