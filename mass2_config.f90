! ----------------------------------------------------------------
! MODULE config
! ----------------------------------------------------------------
MODULE config

  USE utility
  USE julian
  USE time_series
  USE differencing
  USE solver_common
  USE block_variable_base, ONLY: nghost

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PRIVATE, PARAMETER :: cfg_iounit=10

  CHARACTER (LEN=80), PUBLIC, SAVE :: config_file_name = "mass2.cfg"

  CHARACTER (LEN=1024), PUBLIC :: config_file_version
  INTEGER, PUBLIC :: max_blocks
  INTEGER, PUBLIC :: max_species
  CHARACTER (LEN=1024), ALLOCATABLE :: grid_file_name(:)
  LOGICAL, PUBLIC :: do_flow
  LOGICAL, PUBLIC :: do_transport
  LOGICAL, PUBLIC :: do_surface_heatx
  LOGICAL, PUBLIC :: do_surface_gasx
  CHARACTER (LEN=1024), PUBLIC :: weather_filename
  LOGICAL, PUBLIC :: debug
  LOGICAL, PUBLIC :: manning

  LOGICAL, PUBLIC :: write_restart_file
  INTEGER, PUBLIC :: restart_print_freq

  LOGICAL, PUBLIC :: read_hotstart_file

  LOGICAL, PUBLIC :: do_gage_print

  TYPE(datetime_struct), PUBLIC :: start_time

  TYPE(datetime_struct), PUBLIC :: end_time

  LOGICAL, PUBLIC :: given_initial_wsel
  LOGICAL, PUBLIC :: read_initial_profile

  DOUBLE PRECISION, PUBLIC :: delta_t
  INTEGER, SAVE :: number_hydro_iterations
  DOUBLE PRECISION, SAVE :: max_mass_source_sum
  INTEGER, SAVE :: number_scalar_iterations

  DOUBLE PRECISION, PUBLIC ::eddy_default
  LOGICAL, PUBLIC :: do_spatial_eddy
  LOGICAL, PUBLIC :: do_calc_eddy
  DOUBLE PRECISION, PUBLIC :: relax_eddy

  DOUBLE PRECISION, SAVE :: kx_diff_default
  LOGICAL, PUBLIC :: do_spatial_kx

  DOUBLE PRECISION, SAVE :: ky_diff_default
  LOGICAL, PUBLIC :: do_spatial_ky

  DOUBLE PRECISION, PUBLIC :: chezy_con_default
  LOGICAL, PUBLIC :: do_spatial_chezy

  DOUBLE PRECISION, PUBLIC :: mann_con

  DOUBLE PRECISION, PUBLIC :: relax_dp

  DOUBLE PRECISION, PUBLIC :: relax_uv

  DOUBLE PRECISION, PUBLIC :: blend_time

  DOUBLE PRECISION, SAVE :: uvel_initial
  DOUBLE PRECISION, PUBLIC, SAVE :: vvel_initial
  DOUBLE PRECISION, PUBLIC, SAVE :: conc_initial
  DOUBLE PRECISION, PUBLIC, SAVE :: wsel_or_depth_initial

  DOUBLE PRECISION, PUBLIC, SAVE :: uvel_wind
  DOUBLE PRECISION, PUBLIC, SAVE :: vvel_wind

  ! ----------------------------------------------------------------
  ! global parameters for wetting and drying
  ! ----------------------------------------------------------------

                                ! flag to turn wetting and drying on

  LOGICAL, PUBLIC, SAVE :: do_wetdry = .TRUE.

                                ! flag to do wet/dry checking every
                                ! hydro iteration, rather than every
                                ! time step

  LOGICAL, PUBLIC, SAVE :: iterate_wetdry = .TRUE.

                                ! the depth that defines "dry"

  DOUBLE PRECISION, PUBLIC, SAVE :: dry_depth = 0.1

                                ! the minimum depth at which cells
                                ! are initialized (in cold starts)

  DOUBLE PRECISION, PUBLIC, SAVE :: dry_zero_depth = 0.05

                                ! the indicator depth at which
                                ! rewetting is allowed

  DOUBLE PRECISION, PUBLIC, SAVE :: dry_rewet_depth = 0.12

  DOUBLE PRECISION, PUBLIC, SAVE :: bed_default_porosity
  DOUBLE PRECISION, PUBLIC, SAVE ::bed_initial_depth
  LOGICAL, PUBLIC :: read_bed_init = .FALSE.

  INTEGER, PUBLIC :: print_freq
  LOGICAL, PUBLIC :: do_accumulate

  LOGICAL, PUBLIC :: plot_do_netcdf, do_flow_diag, do_flow_output

  LOGICAL, PUBLIC :: plot_do_cgns

                                ! this flag is used to determine the
                                ! kind of grid stored in CGNS. If
                                ! .FALSE., the CGNS output mimics that
                                ! to netcdf. Otherwise, the CGNS
                                ! output uses the grid read in by
                                ! mass2 and all values are cell
                                ! centered.

  LOGICAL, PUBLIC :: plot_cgns_docell = .FALSE.

                                ! this flag is used to determine if
                                ! descriptor nodes are written along
                                ! with the data.

  LOGICAL, PUBLIC :: plot_cgns_dodesc = .TRUE.

                                ! since the CGNS format becomes very
                                ! unwieldy with large numbers of
                                ! nodes, the plot output can be split
                                ! into several files containing a
                                ! fixed number of output time steps

  INTEGER, PUBLIC :: plot_cgns_maxtime = 10
  
  INTEGER, PUBLIC :: gage_print_freq

  ! ----------------------------------------------------------------
  ! some derived configuration parameters that are not in the file
  ! ----------------------------------------------------------------
  
  LOGICAL, PUBLIC, SAVE :: do_rptdead = .FALSE.

CONTAINS

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


    IMPLICIT NONE

    INTEGER :: iblock, i, line
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



END MODULE config
