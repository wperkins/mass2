! ----------------------------------------------------------------
! file: mass2_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 14, 2003 by William A. Perkins
! Last Change: Thu Apr 15 13:42:24 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL



PROGRAM mass2_parallel

  USE utility
  USE fptrap
  USE mass2_main_025
  USE parallel

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"


  INTEGER :: maxsteps
  INTEGER :: junk
  INTEGER :: status_flag = 1, ierr, i

                                ! initialize MPI

  CALL mpi_init_mass2()

  CALL time_series_module_init()
  CALL date_time_flags()


                                ! only the root process does I/O
  IF (mpi_rank .EQ. 0) THEN 

     CALL open_new('status.out', utility_status_iounit)
     CALL open_new('error-warning.out', utility_error_iounit)
     CALL open_new('output.out', output_iounit)

     CALL banner()
     WRITE(*,*)'Running Parallel MASS2 with ', mpi_procs, ' processors'

  ELSE

     ! if there is any output from the other processes, send it to
     ! standard output.

     ! utility_status_iounit = 6
     ! utility_error_iounit = 6
  END IF

  CALL fptrap_common()

  CALL start_up_parallel()

  junk = solver_initialize(max_blocks, block(:)%xmax, block(:)%ymax, &
       &do_flow, do_transport)

  ! DO iblock = 1, max_blocks
  !    IF (pdebug) WRITE(*,*) mpi_rank, ': Transferring block ', iblock
  !    CALL mpi_transfer_block_hydro(block(iblock), 0, block_map(iblock)%pno)
  ! END DO
  ! DO iblock = 1, max_blocks
  !    IF (pdebug) WRITE(*,*) mpi_rank, ': Transferring block ', iblock
  !    CALL mpi_transfer_block_hydro(block(iblock), block_map(iblock)%pno, 0)
  ! END DO

  !----------------------------------------------------------------------------------
  ! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
  !----------------------------------------------------------------------------------
  ! Time Marching Loop

  maxsteps = INT((end_time%time - current_time%time)/(delta_t/86400.0d0))
  time_step_count = 0

  IF (pdebug) WRITE(*, *) mpi_rank, ": Total Time Steps: ", maxsteps



  DO WHILE(current_time%time .LT. end_time%time) 

     ! CALL mpi_barrier(MPI_COMM_WORLD, ierr)

     IF (pdebug) WRITE(*, *) mpi_rank, ": Time step ", time_step_count

     IF(do_flow)THEN
        IF (pdebug) WRITE(*, *) mpi_rank, ": ", time_step_count, ": calling hydro_parallel"
        CALL hydro_parallel(status_flag)
     ENDIF

     IF(do_transport)THEN
        CALL transport_parallel()
     ENDIF

     !---------------------------------------------------------------------------
     !update model time prior to output 
     !   since we are advancing in time the dependent variables are now at the next
     !   time level

     time_step_count = time_step_count + 1

     ! update decimal julian day time
     current_time%time = start_time%time + DBLE(time_step_count)*delta_t/86400.000000d0 ! remember that the delta is in SECONDS

     IF (mpi_rank .EQ. 0) THEN

        ! update the old time level values of the independent variables
        CALL update(status_flag)

        CALL decimal_to_date(current_time%time, current_time%date_string, current_time%time_string)
   
        CALL output(status_flag)

        IF(write_restart_file)THEN
           CALL write_restart(status_flag)
        END IF
     END IF

     CALL mpi_barrier(MPI_COMM_WORLD, ierr)

  END DO


  junk = solver_finalize()

  IF (mpi_rank .EQ. 0) THEN

     WRITE(*,*)'completion with status=',status_flag

     CLOSE(output_iounit)
     CALL plot_file_close()
     CALL gage_file_close()
     CALL diag_plot_file_close()
     CALL mass_file_close()
     CALL time_series_module_done()
     CLOSE(utility_error_iounit)
     CLOSE(utility_status_iounit)
     
  END IF

  CALL MPI_FINALIZE(ierr)   
  IF (ierr .NE. 0) CALL error_message("MPI: finalization failure", fatal=.TRUE.)

END PROGRAM mass2_parallel

! ----------------------------------------------------------------
! SUBROUTINE start_up_parallel
! ----------------------------------------------------------------
SUBROUTINE start_up_parallel()

  USE mass2_main_025
  USE parallel

  IMPLICIT NONE

  INTEGER :: var, imax, jmax
  INTEGER :: i, j, junk
  CHARACTER (LEN=1024) :: msg

  IF (mpi_rank .EQ. 0) THEN
     CALL read_config()
     current_time = start_time
  END IF

                                ! broadcast required parts of the
                                ! configuration
  CALL mpi_bcast_config()

                                ! all processes need to have blocks
  CALL allocate_blocks()


  baro_press = 760.0              ! in case weather is not read

  IF (mpi_rank .EQ. 0) THEN
     CALL read_grid(max_blocks, grid_file_name, readpts=.FALSE.)
  END IF

  CALL mpi_bcast_block_size()

  ! Assign each block to a process. 

  ALLOCATE(block_map(max_blocks))
  CALL mpi_map_blocks()

  DO i=1,max_blocks
     CALL allocate_block_components(i, status_iounit)
  END DO

  imax = MAXVAL(block(:)%xmax) + 1
  jmax = MAXVAL(block(:)%ymax) + 1

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

  ALLOCATE(inlet_area(jmax), table_input(jmax))

  IF (mpi_rank .EQ. 0) THEN
     CALL read_grid(max_blocks, grid_file_name, readpts=.TRUE.)
     CALL bc_init()
     CALL initialize()
     CALL output_init()
  END IF

  ! transfer the constant parts of the blocks to the appropriate
  ! process.  The blocks have all of the metrics computed, parameters
  ! (diffusivity, e.g.)

  DO iblock = 1, max_blocks
     CALL mpi_transfer_block_grid(block(iblock), 0, block_map(iblock)%pno)
  END DO
  
END SUBROUTINE start_up_parallel


! ----------------------------------------------------------------
! SUBROUTINE hydro_parallel
! ----------------------------------------------------------------
!---------------------------------------------------------------------------------
!
! SOLUTION OF THE MOMENTUM, DEPTH CORRECTION, AND SCALAR TRANSPORT EQUATIONS
!
!---------------------------------------------------------------------------------
SUBROUTINE hydro_parallel(status_flag)

  USE globals, ONLY: block, max_blocks
  USE misc_vars, ONLY: number_hydro_iterations
  USE block_boundary_conditions, ONLY: block_bc
  USE mass2_main_025, ONLY: default_hydro_bc, apply_hydro_bc, check_wetdry, depth_check
  USE parallel

  IMPLICIT NONE

  INTEGER :: k, status_flag, x_beg, y_beg, num_bc, i, j, junk, ierr
  LOGICAL :: alldry, done

  DOUBLE PRECISION :: apo

  !----------------------------------------------------------------------------
  ! iteration loop at a fixed time using prescribed U (or Discharge),V,D BCs

  DO iteration = 1,number_hydro_iterations

     IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration

     IF (mpi_rank .EQ. 0) THEN
        DO iblock = 1, max_blocks

           CALL default_hydro_bc(block(iblock))

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
        END DO

        IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
             &": BC's complete"

     END IF

     ! CALL mpi_barrier(MPI_COMM_WORLD, ierr)

     ! distribute the blocks to the appropriate processes.

     DO iblock = 1, max_blocks
        IF (pdebug) WRITE(*,*) mpi_rank, ': Transferring block ', iblock
        CALL mpi_transfer_block_hydro(block(iblock), 0, block_map(iblock)%pno, &
             &results_only = .FALSE.)
     END DO

     IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
          &": block distribution complete"


     CALL mpi_barrier(MPI_COMM_WORLD, ierr)

     ! At this point, all the processes go off and do their own blocks.

     DO iblock = 1,max_blocks 

        x_end = block(iblock)%xmax
        y_end = block(iblock)%ymax

        IF (mpi_rank .EQ. block_map(iblock)%pno) THEN

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
        
           IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
                &": block initialization complete"

           CALL uvel_solve(iblock, block(iblock), delta_t)

           IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
                &": U solution complete"

           CALL vvel_solve(iblock, block(iblock), delta_t)
        
           IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
                &": V solution complete"

           CALL depth_solve(iblock, block(iblock), delta_t)

           IF (pdebug) WRITE (*, *) mpi_rank, ": Hydro iteration ", iteration, &
                &": depth correction solution complete"

           CALL correct_velocity(block(iblock))
        
           IF (do_wetdry .AND. iterate_wetdry) CALL check_wetdry(block(iblock))

        END IF

     END DO		! block loop end


     ! all the processes meet back here when their respective blocks
     ! are done

     CALL mpi_barrier(MPI_COMM_WORLD, ierr)

     ! get all of the blocks back from their processes

     DO iblock = 1, max_blocks
        IF (pdebug) WRITE(*,*) mpi_rank, ': Transferring block ', iblock
        CALL mpi_transfer_block_hydro(block(iblock), block_map(iblock)%pno, 0, &
             &results_only = .TRUE.)
     END DO

     ! Check to see if we should exit the loop, we need to be careful
     ! to make sure all processes know whether or not to stop
     ! iterating

     done = .FALSE.
     
     IF ( mpi_rank .EQ. 0 ) THEN


        ! check for small and negative depth condition and report
        ! location
        
        CALL depth_check(iblock, block(iblock), current_time%date_string, current_time%time_string)


        ! check to see if mass source has been reduced below tolerance
        ! and break out of loop
        maxx_mass = 0
        DO iblock=1,max_blocks
           apo = SUM(ABS(block(iblock)%mass_source(2:block(iblock)%xmax, 2:block(iblock)%ymax)))
           IF(apo >= maxx_mass) maxx_mass = apo
        END DO
        done = (maxx_mass < max_mass_source_sum)
     
     END IF

     CALL MPI_BCAST(done, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     IF (done) EXIT
     
  END DO    ! internal time iteration loop for momentum, depth correction equations
  
  IF (mpi_rank .EQ. 0) THEN
     IF (do_wetdry .AND. .NOT. iterate_wetdry) THEN
        DO iblock = 1, max_blocks
           CALL check_wetdry(block(iblock))
        END DO
     END IF
  END IF

END SUBROUTINE hydro_parallel

! ----------------------------------------------------------------
! SUBROUTINE transport_parallel
! ----------------------------------------------------------------
SUBROUTINE transport_parallel()

  USE globals, ONLY: block, max_blocks
  USE scalars, ONLY: species, max_species
  USE block_boundary_conditions, ONLY: scalar_bc
  USE mass2_main_025, ONLY: apply_scalar_bc, apply_scalar_source
  USE transport_only, ONLY: hydro_restart_read, hydro_restart_interp
  USE parallel

  IMPLICIT NONE

  INTEGER :: var, iter
  INTEGER :: i, j, ispecies, num_bc, ierr

  ! If we're doing transport only the master process needs to
  ! interpolate and distribute the new hydrodynamics

  IF(.NOT. do_flow) THEN
     IF (mpi_rank .EQ. 0) THEN
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
     DO iblock=1,max_blocks
        CALL mpi_transfer_block_hydro(block(iblock), 0, block_map(iblock)%pno, .FALSE.)
     END DO
  END IF
  
  IF (mpi_rank .EQ. 0) THEN
     CALL scalar_source_timestep(current_time%time, delta_t)
     IF (source_doing_sed) CALL bed_dist_bedsrc(delta_t)
  END IF

  ! Each process calculates their own values for transport

  DO iblock = 1,max_blocks
     IF (mpi_rank .EQ. block_map(iblock)%pno) THEN
        CALL bedshear(block(iblock))
        CALL transport_precalc(block(iblock))
     END IF
  END DO

  ! INTERNAL ITERATION AT THIS TIME LEVEL LOOP
  DO iter = 1,number_scalar_iterations


     ! SPECIES LOOP - multiple numbers of scalar variables
     DO ispecies = 1, max_species

        ! The master process handles the boundary conditions.

        IF (mpi_rank .EQ. 0) THEN
           DO iblock = 1,max_blocks
              CALL default_scalar_bc(block(iblock), species(ispecies)%scalar(iblock))
              DO num_bc = 1, scalar_bc(iblock)%num_bc
                 IF(scalar_bc(iblock)%bc_spec(num_bc)%species .EQ. ispecies)&
                      &CALL apply_scalar_bc(block(iblock), &
                      &species(ispecies)%scalar(iblock), &
                      &scalar_bc(iblock)%bc_spec(num_bc), x_start, y_start)
              END DO ! num bc loop
              CALL apply_scalar_source(iblock, ispecies, x_start, y_start)
           END DO
        END IF

        ! Distribute the blocks after BC's have been applied

        DO iblock=1,max_blocks
           CALL mpi_transfer_block_scalar(block(iblock), &
                &species(ispecies)%scalar(iblock), &
                &0, block_map(iblock)%pno, .FALSE.)
        END DO

        ! Each process works on its own set of blocks.  Note that this
        ! assumes an x_start of 2 for all blocks.

        DO iblock = 1,max_blocks

           IF (mpi_rank .EQ. block_map(iblock)%pno) THEN
              CALL scalar_solve(iblock, block(iblock), &
                   &species(ispecies)%scalar(iblock), 2, 2)
           END IF

        END DO

        CALL mpi_barrier(MPI_COMM_WORLD, ierr)

        ! Gather up the blocks after they've been solved

        DO iblock=1,max_blocks
           CALL mpi_transfer_block_scalar(block(iblock), &
                &species(ispecies)%scalar(iblock), &
                &block_map(iblock)%pno, 0, .TRUE.)
        END DO
     END DO
  END DO ! internal time loop end for concentration
END SUBROUTINE transport_parallel


! ----------------------------------------------------------------
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()

  USE misc_vars

  IMPLICIT NONE

  WRITE(*,*)'#     #    #     #####   #####   #####'
  WRITE(*,*)'##   ##   # #   #     # #     # #     #'
  WRITE(*,*)'# # # #  #   #  #       #             #'
  WRITE(*,*)'#  #  # #     #  #####   #####   #####'
  WRITE(*,*)'#     # #######       #       # #'
  WRITE(*,*)'#     # #     # #     # #     # #'
  WRITE(*,*)'#     # #     #  #####   #####  #######'
  WRITE(*,*)
  WRITE(*,*)'Developed and Maintained by'
  WRITE(*,*)'Pacific Northwest National Laboratory'
  WRITE(*,*)
  WRITE(*,*)code_version
  WRITE(*,*)code_date
  
END SUBROUTINE banner




