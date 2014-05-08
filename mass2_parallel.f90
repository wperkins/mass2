! ----------------------------------------------------------------
! file: mass2_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 14, 2003 by William A. Perkins
! Last Change: 2014-05-08 09:55:32 d3g096
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL



PROGRAM mass2_parallel

  USE utility
  USE time_series
  USE config
  USE block_initialization
  USE hydro_solve
  USE scalar_solve_module
  USE gage_output
  USE mass_source_output
  USE plot_output
  USE hotstart

  IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  CHARACTER (LEN=1024) :: buffer, fmt, fmt2
  INTEGER :: ierr
  INTEGER :: mpi_rank, mpi_size, digits
  INTEGER :: maxsteps
  INTEGER :: junk, heap, stack, iblk, i, j
  LOGICAL :: ok
  INTEGER :: imax, jmax

  CALL time_series_module_init()
  CALL date_time_flags()

  CALL mpi_init( ierr )
  IF (ierr .NE. 0) CALL error_message("MPI: initialization failure", fatal=.TRUE.)
  CALL ga_initialize()

  heap = 10000
  stack= 10000
  ok = ma_init(MT_DBL, heap, stack)


  CALL mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, ierr)
  IF (ierr .NE. 0) CALL error_message("MPI: cannot get rank", fatal=.TRUE.)

  CALL mpi_comm_size(MPI_COMM_WORLD, mpi_size, ierr)
  IF (ierr .NE. 0) CALL error_message("MPI: cannot get size", fatal=.TRUE.)

  digits = int(log10(real(mpi_size)) + 1)

  WRITE(fmt, '("I0.", I1)') digits

  fmt2 = '("status.", ' // TRIM(fmt) // ', ".out")'
  WRITE(buffer, fmt2) mpi_rank
  CALL open_new(buffer, utility_status_iounit)

  fmt2 = '("error-warning.", ' // TRIM(fmt) // ', ".out")'
  WRITE(buffer, fmt2) mpi_rank
  CALL open_new(buffer, utility_error_iounit)

  IF (mpi_rank .EQ. 0) THEN
     CALL banner()
  END IF

  CALL read_config()
  current_time = start_time

  ALLOCATE(block(max_blocks))

  DO iblk = 1, max_blocks

     CALL block_read_grid(block(iblk), iblk, grid_file_name(iblk))

     CALL block_extrap_ghost(block(iblk))

     CALL block_extrap_ghost_corners(block(iblk))

  END DO

  CALL bc_init()

  DO iblk = 1, max_blocks

     CALL block_build_ghost(iblk)

     CALL block_extrap_ghost_corners(block(iblk))

     CALL block_interp_grid(block(iblk))

     CALL block_metrics(block(iblk))

     CALL block_plot_geometry(block(iblk))

  END DO

  CALL block_distribution_report()

  imax = MAXVAL(block(:)%xmax) + 1
  jmax = MAXVAL(block(:)%ymax) + 1

  ALLOCATE(inlet_area(MAX(imax,jmax)), table_input(MAX(imax,jmax)))

  IF (do_transport) THEN
     CALL allocate_species()
     DO i=1, max_species
        DO j =1, max_blocks
           CALL allocate_scalarblock_components(j, block(j)%varbase)
        END DO
     END DO
  END IF

  CALL initialize()
  DO j =1, max_blocks
     CALL fillghost(j)
  END DO
  
  IF (do_flow .OR. do_transport) CALL solver_setup()

  CALL ga_sync()

  CALL output_init(mpi_rank)

  ! Time Marching Loop
  
  maxsteps = INT((end_time%time - current_time%time)/(delta_t/86400.0d0))
  time_step_count = 0
  DO WHILE(current_time%time .LT. end_time%time) 

     IF(do_flow)THEN
        CALL hydro()
     ENDIF

     IF(do_transport)THEN
        CALL transport()
     ENDIF

     ! update the old time level values of the independent variables
     CALL update()

     !---------------------------------------------------------------------------
     !update model time prior to output 
     !   since we are advancing in time the dependent variables are now at the next
     !   time level

     time_step_count = time_step_count + 1

     ! update decimal julian day time
     current_time%time = start_time%time + DBLE(time_step_count)*delta_t/86400.000000d0 ! remember that the delta is in SECONDS

     CALL decimal_to_date(current_time%time, current_time%date_string, current_time%time_string)

     CALL output(mpi_rank)

     IF(write_restart_file)THEN
        CALL write_restart()
     END IF

  END DO
  ! end time loop

  IF (do_flow .OR. do_transport) junk = solver_finalize()

  CALL plot_file_close()
  CALL gage_file_close()
  CALL mass_file_close(mpi_rank)
!!$  IF (debug) CALL block_flux_close()
  CALL time_series_module_done()

  DO iblk = 1, max_blocks
     CALL block_deallocate(block(iblk))
  END DO

  CLOSE(utility_error_iounit)
  CLOSE(utility_status_iounit)

  CALL ga_sync()
  CALL ga_terminate()
  CALL mpi_finalize(ierr)   

END PROGRAM mass2_parallel

! ----------------------------------------------------------------
! SUBROUTINE bc_init
! ----------------------------------------------------------------
SUBROUTINE bc_init()

  USE utility
  USE config
  USE block_hydro_bc
  USE scalars
  USE scalar_bc_module
  USE scalars_source
  USE met_data_module
  USE transport_only

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
     CALL allocate_scalar_block_bc(max_blocks)
     CALL read_scalar_bcspecs(max_blocks, max_species, block(:)%xmax, block(:)%ymax)
     CALL read_scalar_bc_tables()
     CALL set_scalar_block_connections()
     CALL scalar_source_read()
  !    IF (source_doing_sed) CALL bed_initialize()
  !    CALL scalar_mass_init()

     ! transport only mode
     IF(.NOT. do_flow)THEN
        CALL transport_only_allocate()
        CALL transport_only_read()
        CALL transport_only_check(start_time%time, end_time%time)
     END IF

  END IF

  ! FIXME
  ! read in met data from a file
  IF (source_need_met) THEN
     CALL read_met_data(weather_filename)
     CALL update_met_data(current_time%time)
  END IF

END SUBROUTINE bc_init

! ----------------------------------------------------------------
! SUBROUTINE output_init
! ----------------------------------------------------------------
SUBROUTINE output_init(mpi_rank)

  USE config
  USE globals
  USE gage_output
  USE mass_source_output
  USE plot_output

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: mpi_rank
  INTEGER :: system_time(8)

  IF (mpi_rank .EQ. 0) THEN
     CALL block_gridplot('gridplot1.dat', DOGHOST=debug)
  END IF

  !------------------------------------------------------------------------------------
  ! set up the gage print files
  ! FIXME
  IF(do_gage_print) THEN
     CALL gage_file_setup()
     CALL mass_file_setup(mpi_rank)
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
! SUBROUTINE update
! update old values of dependent variables
! ----------------------------------------------------------------
SUBROUTINE update()

  USE config
  USE block_module
  USE scalars

  IMPLICIT NONE

  INTEGER :: iblock, ispecies

  DO iblock=1,max_blocks

     CALL block_var_timestep(block(iblock)%bv_uvel)
     CALL block_var_timestep(block(iblock)%bv_vvel)
     CALL block_var_timestep(block(iblock)%bv_depth)

     block(iblock)%wsel = block(iblock)%depth + block(iblock)%zbot
     CALL block_var_put(block(iblock)%bv_wsel)
     CALL block_var_sync()
     CALL block_var_get(block(iblock)%bv_wsel)
     block(iblock)%dp = 0.0

  END DO

  IF (do_transport) THEN
     DO ispecies = 1, max_species
        DO iblock = 1, max_blocks
           CALL block_var_timestep(species(ispecies)%scalar(iblock)%concvar)
        END DO
     END DO
!!$     CALL scalar_mass_balance(delta_t)
!!$     IF (source_doing_sed) CALL bed_accounting(delta_t)
  END IF
!!$
!!$  IF (do_accumulate) CALL accumulate(current_time%time)

END SUBROUTINE update


SUBROUTINE output(mpi_rank)

  USE config
  USE globals
  USE block_module
  USE block_hydro_bc
  USE gage_output
  USE mass_source_output
  USE plot_output


  IMPLICIT NONE

  INTEGER, INTENT(IN) :: mpi_rank

  DOUBLE PRECISION :: depth_e, flux_e, conc_TDG

  INTEGER :: iblock, ispecies, num_bc
  INTEGER :: i, j
  LOGICAL :: ds_flux_given

  IF( (current_time%time >= end_time%time) .OR. (MOD(time_step_count,print_freq) == 0) )THEN

     ! call these again so that ghost cells
     ! are correctly filled

     DO iblock=1,max_blocks
        IF (.NOT. do_flow) THEN
           CALL block_compute_bc_flux(block(iblock), block_bc(iblock))
        END IF
        DO num_bc = 1, block_bc(iblock)%num_bc
           CALL apply_hydro_bc(block(iblock), block_bc(iblock)%bc_spec(num_bc), &
                &.FALSE., ds_flux_given)

        END DO

        ! just put this for output

        IF (do_rptdead) THEN
           CALL block_var_put_logical(block(iblock)%bv_dead, block(iblock)%isdead%p)
        END IF
        
        IF (do_transport) THEN
!!$           DO ispecies = 1, max_species
!!$              DO num_bc = 1, scalar_bc(iblock)%num_bc
!!$                 IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
!!$                      &CALL apply_scalar_bc(block(iblock), &
!!$                      &species(ispecies)%scalar(iblock), &
!!$                      &scalar_bc(iblock)%bc_spec(num_bc), x_start, y_start)
!!$              END DO
!!$           END DO
        ELSE
           CALL bedshear(block(iblock))
        END IF
     END DO

     

!!$     IF (.NOT. do_accumulate) CALL accumulate(current_time%time)
     CALL plot_print(current_time%date_string, current_time%time_string, &
          &salinity, baro_press)

  END IF

  IF(do_gage_print)THEN
     IF((current_time%time >= end_time%time) .OR. (MOD(time_step_count,gage_print_freq) == 0)) THEN
        CALL gage_print(current_time%date_string, current_time%time_string,&
             &DBLE((current_time%time - start_time%time)*24), &
             &do_transport, salinity, baro_press)
        CALL mass_print(mpi_rank, current_time%date_string, current_time%time_string)
!!$        IF (debug) &
!!$             &CALL block_flux_print(current_time%date_string, current_time%time_string)
!!$
     END IF
  END IF

END SUBROUTINE output

! ----------------------------------------------------------------
! SUBROUTINE solver_setup
! ----------------------------------------------------------------
SUBROUTINE solver_setup()

  USE config
  USE block_module
  USE solver_module

  IMPLICIT NONE

  INTEGER :: iblk
  INTEGER :: junk
  INTEGER :: x_beg, x_end, y_beg, y_end
  INTEGER :: imin, imax, jmin, jmax

  CALL solver_initialize(max_blocks)

  DO iblk = 1, max_blocks
     x_beg = 2
     x_end = block(iblk)%xmax
     y_beg = 2
     y_end =  block(iblk)%ymax
     CALL block_owned_window(block(iblk), imin, imax, jmin, jmax)
     imin = MAX(imin, x_beg)
     imax = MIN(imax, x_end)
     jmin = MAX(jmin, y_beg)
     jmax = MIN(jmax, y_end)

     junk = solver_initialize_block(iblk, &
          &x_beg, x_end, y_beg, y_end, &
          &imin, imax, jmin, jmax, do_flow, do_transport)
  
  END DO
END SUBROUTINE solver_setup


! ----------------------------------------------------------------
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()
  
  USE globals

  IMPLICIT NONE

#include "global.fh"
#include "finclude/petscsys.h"

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
  WRITE(*,'(" Using PETSc ",I1,".",I1,".",I1,"-p",I1," (", A, ")")') &
       &PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR, PETSC_VERSION_PATCH,&
       &PETSC_VERSION_DATE
  WRITE(*,*)
  
END SUBROUTINE banner





