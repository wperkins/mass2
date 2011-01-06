! ----------------------------------------------------------------
! file: mass2_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 14, 2003 by William A. Perkins
! Last Change: Thu Jan  6 09:00:02 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL



PROGRAM mass2_parallel

  USE mpi
  USE utility
  USE time_series
  USE block_grid
  USE block_initialization
  USE config
  USE plot_output

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  CHARACTER (LEN=1024) :: buffer
  INTEGER :: ierr
  INTEGER :: mpi_rank

  CALL time_series_module_init()
  CALL date_time_flags()

  CALL mpi_init( ierr )
  IF (ierr .NE. 0) CALL error_message("MPI: initialization failure", fatal=.TRUE.)
  CALL ga_initialize()

  CALL mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, ierr)
  IF (ierr .NE. 0) CALL error_message("MPI: cannot get rank", fatal=.TRUE.)

  WRITE(buffer, '("status.", I0.3, ".out")') mpi_rank
  CALL open_new(buffer, utility_status_iounit)
  WRITE(buffer, '("error-warning.", I0.3, ".out")') mpi_rank
  CALL open_new(buffer, utility_error_iounit)

  IF (mpi_rank .EQ. 0) THEN
     CALL banner()
  END IF

  CALL read_config()
  CALL read_grid()
  CALL bc_init()
  CALL initialize()

  CALL ga_sync()

  CALL output_init(mpi_rank)

  CALL ga_sync()
  CALL ga_terminate()
  CALL mpi_finalize(ierr)   

END PROGRAM mass2_parallel

! ----------------------------------------------------------------
! SUBROUTINE read_grid
! ----------------------------------------------------------------
SUBROUTINE read_grid()

  USE utility
  USE config
  USE block_grid

  IMPLICIT NONE

  INTEGER :: b
  INTEGER :: imax, jmax

  IF (max_blocks .GT. 1) THEN
     CALL error_message("Only one block allowed right now", fatal=.TRUE.)
  END IF

  ALLOCATE(block(max_blocks))

  DO b = 1, max_blocks

     CALL block_read_grid(block(b), b, grid_file_name(b))

     CALL block_build_ghost(block(b))

     CALL block_interp_grid(block(b))

     CALL block_metrics(block(b))

     CALL block_plot_geometry(block(b))

  END DO

  CALL block_distribution_report()

  imax = MAXVAL(block(:)%xmax) + 1
  jmax = MAXVAL(block(:)%ymax) + 1

  ALLOCATE(inlet_area(MAX(imax,jmax)), table_input(MAX(imax,jmax)))

END SUBROUTINE read_grid

! ----------------------------------------------------------------
! SUBROUTINE bc_init
! ----------------------------------------------------------------
SUBROUTINE bc_init()

  USE utility
  USE config
  USE block_hydro_bc

  IMPLICIT NONE

  INTEGER :: iblock, i

  !-------------------------------------------------------------------------------
  ! read, allocate, and set up block and table boundary conditions

  ! read hydrodynamics related stuff
  CALL allocate_block_bc(max_blocks)

  CALL read_bcspecs(max_blocks, block%xmax, block%ymax)

  CALL read_bc_tables

  ! now decipher which cells talk to each other in the block connections
  IF(max_blocks > 1)THEN
     CALL set_block_connections(max_blocks, utility_error_iounit, utility_status_iounit)
  END IF

  IF(do_transport)THEN
     CALL error_message('transport not implemented', fatal=.TRUE.)
  !    ! read species related stuff
  !    CALL allocate_scalar_block_bc(max_blocks)
  !    CALL read_scalar_bcspecs(bcspec_iounit, max_blocks, max_species, &
  !         &block%xmax, block%ymax)
  !    CALL read_scalar_bc_tables()
  !    CALL set_scalar_block_connections(max_blocks, max_species)
  !    CALL scalar_source_read()
  !    IF (source_doing_sed) CALL bed_initialize()
  !    CALL scalar_mass_init()

  !    ! transport only mode
  !    IF(.NOT. do_flow)THEN
  !       CALL allocate_hydro_interp_blocks()
  !       DO i=1,max_blocks
  !          CALL allocate_hydro_interp_comp(i, status_iounit)
  !       END DO
  !       CALL read_transport_only_dat()
  !       CALL check_transport_only_dat(start_time%time,end_time%time)
  !    END IF

  END IF

  ! FIXME
  ! ! read in met data from a file
  ! IF (source_need_met) THEN
  !    CALL read_met_data(weather_filename)
  !    CALL update_met_data(current_time%time)
  ! END IF

END SUBROUTINE bc_init

! ----------------------------------------------------------------
! SUBROUTINE output_init
! ----------------------------------------------------------------
SUBROUTINE output_init(mpi_rank)

  USE config
  USE globals
  USE plot_output

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: mpi_rank
  INTEGER :: system_time(8)

!!$  !------------------------------------------------------------------------------------
!!$  ! set up the gage print files
!!$  IF(do_gage_print) THEN
!!$     CALL gage_file_setup(do_transport,error_iounit, status_iounit)
!!$     CALL mass_file_setup()
!!$     IF (debug) CALL block_flux_setup()
!!$  END IF
!!$
!!$
!!$  !-----------------------------------------------------------------------------------------------------------
!!$  ! print problem configuration information and initial variable values
!!$
!!$  WRITE(output_iounit,*)"2D Depth-Averaged Flow Solver "
!!$  WRITE(output_iounit,*)code_version
!!$  WRITE(output_iounit,*)code_date
!!$
!!$  CALL DATE_AND_TIME(VALUES = system_time)
!!$  WRITE(output_iounit,2010)system_time(2),system_time(3),system_time(1),system_time(5),system_time(6),system_time(7)
!!$
!!$  WRITE(output_iounit,*)"Total Number of Blocks = ",max_blocks
!!$  WRITE(output_iounit,*)"Grids read from these files: "
!!$  DO iblock=1,max_blocks
!!$     WRITE(output_iounit,2000)grid_file_name(iblock)
!!$  END DO
!!$  WRITE(output_iounit,*)
!!$
!!$  block_title(1:11) = ' for block '
!!$
!!$  IF (debug) THEN
!!$     DO iblock=1,max_blocks
!!$
!!$        WRITE(block_title(12:15),'(i4)')iblock
!!$        WRITE(output_iounit,*)'initial values for block number - ',iblock
!!$
!!$        title = 'X grid values - state plane coord' 
!!$        title(61:75) = block_title
!!$
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%x_grid)
!!$
!!$        title = 'Y grid values - state plane coord'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%y_grid)
!!$
!!$        title = 'Bottom Elevation grid values - mean sea level'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%zbot_grid)
!!$
!!$        title = 'Water Surface Elevation - mean sea level'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%wsel)
!!$
!!$        title = "U Velocity (located at U staggered node)"
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%uvel)
!!$
!!$
!!$        title = "V Velocity (located at V staggered node)"
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%vvel)
!!$
!!$        title = 'Depth'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%depth)
!!$
!!$        title = 'X c.v. node values - state plane coord'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%x)
!!$
!!$        title = 'Y c.v. node values - state plane coord'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%y)
!!$
!!$        title = 'Bottom Elevation c.v node values - mean sea level'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%zbot)
!!$
!!$        title = 'h1 metric coeff at U location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hu1)
!!$
!!$        title = 'h2 metric coeff at U location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hu2)
!!$
!!$        title = 'h1 metric coeff at V location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hv1)
!!$
!!$        title = 'h2 metric coeff at V location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hv2)
!!$
!!$        title = 'h1 metric coeff at P location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hp1)
!!$
!!$        title = 'h2 metric coeff at P location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%hp2)
!!$
!!$        title = 'g12 nonorthogonal component of the metric tensor at P location'
!!$        title(61:75) = block_title
!!$        CALL output_2d_array(output_iounit,title,&
!!$             &i_index_min,block(iblock)%xmax + i_index_extra,&
!!$             &j_index_min,block(iblock)%ymax + j_index_extra,&
!!$             &block(iblock)%gp12)
!!$
!!$     END DO
!!$  END IF

  IF (mpi_rank .EQ. 0) THEN
     CALL block_gridplot('gridplot1.dat', DOGHOST=debug)
  END IF

  ! ----------------------------------------------------------------
  ! write initial fields to the plot file
  ! ----------------------------------------------------------------
  
  CALL plot_file_setup()
!!$  CALL accumulate(start_time%time)
  CALL plot_print(start_time%date_string, start_time%time_string, &
       &salinity, baro_press)

!!$  IF (do_gage_print) THEN
!!$     CALL gage_print(start_time%date_string, start_time%time_string,&
!!$          &(start_time%time - start_time%time)*24, &
!!$          &do_transport, salinity, baro_press)
!!$  END IF

2000 FORMAT(a80)
2010 FORMAT('Simulation Run on Date - ',i2,'-',i2,'-',i4,' at time ',i2,':',i2,':',i2/)

END SUBROUTINE output_init



! ----------------------------------------------------------------
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()
  
  USE globals

  IMPLICIT NONE

#include "global.fh"

  WRITE(*,*)'                             ___   '
  WRITE(*,*)'                            /__ \  '
  WRITE(*,*)'    __  ______   __________ __/ /  '
  WRITE(*,*)'   /  |/  /   | / ___/ ___// __/   '
  WRITE(*,*)'  / /|_/ / /| | \__ \\__ \/____/   '
  WRITE(*,*)' / /  / / ___ |___/ /__/ /         '
  WRITE(*,*)'/_/  /_/_/  |_/____/____/          '
  WRITE(*,*)
  WRITE(*,*) code_version
  WRITE(*,*) code_date
  WRITE(*,*)
  WRITE(*,*)'Developed and Maintained by'
  WRITE(*,*)'Pacific Northwest National Laboratory'
  WRITE(*,*)
  WRITE(*,*)'Contact: '
  WRITE(*,*)'    Dr. Marshall C. Richmond <marshall.richmond@pnl.gov>'
  WRITE(*,*)'    William A. Perkins <william.perkins@pnl.gov>'
  WRITE(*,*)
  WRITE(*,'(" Running on ", I3, " processors")') ga_nnodes()
  WRITE(*,'(" Using Global Arrays ", I1, ".", I1)') GA_VERSION_MAJOR, GA_VERSION_MINOR
  
END SUBROUTINE banner




