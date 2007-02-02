! ----------------------------------------------------------------
! file: global_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 13, 2003 by William A. Perkins
! Last Change: Tue Apr 13 07:07:24 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE parallel
! ----------------------------------------------------------------
MODULE parallel

  USE mpi
  USE globals
  USE misc_vars  
  USE scalars
  USE bed_module
  USE utility
  USE solver_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PUBLIC :: mpi_rank, mpi_procs

  TYPE block_map_struct
     INTEGER :: size
     INTEGER :: pno
  END TYPE block_map_struct
  TYPE (block_map_struct), ALLOCATABLE :: block_map(:)

  LOGICAL, PUBLIC :: pdebug = .FALSE.

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION block_size
  ! ----------------------------------------------------------------
  INTEGER FUNCTION block_size(blk)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
  
    INTEGER :: imin, imax, jmin, jmax ! index limits
    INTEGER :: isize, jsize
  
    imin = i_index_min
    imax = blk%xmax + i_index_extra
    jmin = j_index_min
    jmax = blk%ymax + j_index_extra

    isize = imax - imin + 1
    jsize = jmax - jmin + 1
    
    block_size = isize*jsize
  END FUNCTION block_size


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_init_mass2
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_init_mass2()

    IMPLICIT NONE

    TYPE (cell_type_struct) :: c
    TYPE (isdead_struct) :: d
    TYPE (scalar_cell_type_struct) :: s

    INTEGER :: type(5), length(5), disp(5)
    INTEGER :: ierr
                                ! start MPI
    CALL MPI_INIT( ierr )
    IF (ierr .NE. 0) CALL error_message("MPI: initialization failure", fatal=.TRUE.)

                                ! Find out process rank
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, ierr)
    IF (ierr .NE. 0) CALL error_message("MPI: cannot get rank", fatal=.TRUE.)

                                ! find out the number of processes
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_procs, ierr)
    IF (ierr .NE. 0) CALL error_message("MPI: cannot get size", fatal=.TRUE.)

  END SUBROUTINE mpi_init_mass2

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_map_blocks
  ! This routine determines which process is used to compute each
  ! block.  Blocks are distributed according to size.
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_map_blocks()
    
    IMPLICIT NONE
    
    INTEGER :: iblock, size, total_cells, i, j, mapped, p
    INTEGER, ALLOCATABLE :: proc_size(:), proc_blk(:), sorted(:)

    CHARACTER (LEN=1024) :: msg

    IF (mpi_procs .GT. max_blocks .AND. mpi_rank .EQ. 0) THEN
       WRITE(msg, *) 'More processes than blocks (', mpi_procs, ' > ', &
            &max_blocks, '), processors will be idle'
       CALL error_message(msg, fatal=.FALSE.)
    END IF
    
    ALLOCATE(proc_size(0:mpi_procs-1), proc_blk(0:mpi_procs-1), sorted(max_blocks))
    
    proc_size = 0
    proc_blk = 0
    
    total_cells = 0
    DO iblock = 1, max_blocks
       sorted(iblock) = iblock
       size = block_size(block(iblock))
       total_cells = total_cells + size
       block_map(iblock)%size = size
    END DO

    ! sort the blocks by the total number of cells
    DO i = 1, max_blocks - 1
       DO j = i + 1, max_blocks
          IF (block_map(sorted(j))%size .GT. block_map(sorted(i))%size) THEN
             iblock = sorted(i)
             sorted(i) = sorted(j)
             sorted(j) = iblock
          END IF
       END DO
    END DO

    DO i = 1, max_blocks
       iblock = sorted(i)
       IF (i .LE. mpi_procs) THEN
          p = i - 1
          p = mpi_procs - p - 1
       ELSE 
          p = MINLOC(proc_size, DIM=1)
          p = p - 1
       END IF
       IF (mpi_rank .EQ. 0) THEN
          WRITE(msg, 100) iblock, block_map(iblock)%size, p
          CALL status_message(msg)
       END IF
       block_map(iblock)%pno = p
       proc_size(p) = proc_size(p) + block_map(iblock)%size
       proc_blk(p) = proc_blk(p) + 1
    END DO

    IF (mpi_rank .EQ. 0) THEN
       WRITE (msg, *) 'Final distribution of cells to processes:'
       DO p = 0, mpi_procs-1
          WRITE (msg, 200) p, proc_blk(p), proc_size(p), 100.0*DBLE(proc_size(p))/DBLE(total_cells)
          CALL status_message(msg)
       END DO
    END IF

    DEALLOCATE(proc_size, sorted)
          
100 FORMAT("Assigning block ", I2, " (", I7, " cells) to process ", I2)
200 FORMAT('Process ', I2, ': ', I2, ' blocks, ', I7, ' cells (', F5.1, '%)')
  END SUBROUTINE mpi_map_blocks

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_bcast_block_size
  ! This sends the block dimensions to all processes
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_bcast_block_size()

    IMPLICIT NONE

    INTEGER :: iblock, ierr

    DO iblock = 1, max_blocks
       CALL MPI_BCAST(block(iblock)%xmax, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
       CALL MPI_BCAST(block(iblock)%ymax, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    END DO

  END SUBROUTINE mpi_bcast_block_size

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_bcast_config
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_bcast_config()


    IMPLICIT NONE

    INTEGER :: ierr

                                ! main sizes
    CALL MPI_BCAST(max_blocks, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(max_species, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

                                ! main operation flags
    CALL MPI_BCAST(do_flow, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(do_transport, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(do_surface_heatx, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(do_surface_gasx, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(do_wetdry, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(manning, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)

                                ! time limits
    CALL MPI_BCAST(start_time%time, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(end_time%time, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(current_time%time, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(delta_t, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    ! wet/dry parameters

    CALL MPI_BCAST(dry_depth, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(dry_rewet_depth, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(dry_zero_depth, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    ! other things

    CALL MPI_BCAST(uvel_wind, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(vvel_wind, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(wind_speed, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(number_hydro_iterations, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(number_scalar_iterations, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(relax_dp, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(mann_con, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    ! solver configuration (TDMA only for now)
    CALL MPI_BCAST(scalar_sweep, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(depth_sweep, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(use_tdma_for_scalar, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(use_tdma_for_depth, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(scalar_rtol, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(scalar_atol, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(depth_rtol, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(depth_atol, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

  END SUBROUTINE mpi_bcast_config

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_bcast_blksize
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_bcast_blksize(nblocks)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nblocks
    INTEGER :: xmax(nblocks), ymax(nblocks)
    INTEGER :: iblock, ierr

    IF (mpi_rank .EQ. 0) THEN
       xmax = block(1:nblocks)%xmax
       ymax = block(1:nblocks)%ymax
    END IF
    CALL MPI_BCAST(xmax, nblocks, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    CALL MPI_BCAST(ymax, nblocks, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    IF (mpi_rank .NE. 0) THEN
       block(1:nblocks)%xmax = xmax
       block(1:nblocks)%ymax = ymax
    END IF
  
  END SUBROUTINE mpi_bcast_blksize


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_send_block_grid
  ! Sends blk's grid locations and metrics to process dest
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_block_grid(blk, src, dest)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: dest, src
    INTEGER :: tag = 0
    INTEGER :: size
    INTEGER :: ierr, status(MPI_STATUS_SIZE)

    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       size = block_size(blk)
       CALL MPI_SEND(blk%x, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%y, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%x_grid, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%y_grid, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%x_out, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%y_out, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%x_xsi, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%y_xsi, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%x_eta, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%y_eta, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hp1, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hp2, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%gp12, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hv1, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hv2, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hu1, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%hu2, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%zbot, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%zbot_grid, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%zbot_out, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%eddy, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%kx_diff, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%ky_diff, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%chezy, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
    ELSE IF (mpi_rank .EQ. dest) THEN
       size = block_size(blk)
       CALL MPI_RECV(blk%x, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%y, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%x_grid, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%y_grid, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%x_out, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%y_out, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%x_xsi, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%y_xsi, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%x_eta, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%y_eta, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hp1, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hp2, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%gp12, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hv1, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hv2, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hu1, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%hu2, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%zbot, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%zbot_grid, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%zbot_out, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%eddy, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%kx_diff, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%ky_diff, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%chezy, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
    ELSE
       RETURN
    END IF
  END SUBROUTINE mpi_transfer_block_grid

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_block_hydro
  !
  ! Transfers necessary information for hydrodynamic solution.  If
  ! results_only is true, stuff that does not change during solution
  ! is not transferred.
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_block_hydro(blk, src, dest, results_only)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: dest, src
    LOGICAL, INTENT(IN) :: results_only

    INTEGER :: tag = 0
    INTEGER :: size, ierr, status(MPI_STATUS_SIZE)

    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       size = block_size(blk)
       IF (pdebug) WRITE(*, *) mpi_rank, ": Sending block hydro "
       CALL MPI_SEND(blk%uvel, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%vvel, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%ustar, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%vstar, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%depth, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%dstar, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%dp, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%wsel, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%isdry, size, MPI_LOGICAL, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(blk%mass_source, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       ! CALL MPI_SEND(blk%windshear1, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       ! CALL MPI_SEND(blk%windshear2, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       IF (.NOT. results_only) THEN
          CALL MPI_SEND(blk%uold, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%vold, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%depthold, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%xsource, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          ! CALL MPI_SEND(blk%lud, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          ! CALL MPI_SEND(blk%lvd, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%cell(:,:)%xtype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%cell(:,:)%xbctype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%cell(:,:)%ytype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%cell(:,:)%ybctype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%isdead(:,:)%u, size, MPI_LOGICAL, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%isdead(:,:)%v, size, MPI_LOGICAL, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(blk%isdead(:,:)%p, size, MPI_LOGICAL, dest, tag, MPI_COMM_WORLD, ierr)
       END IF
    ELSE IF (mpi_rank .EQ. dest) THEN
       size = block_size(blk)
       IF (pdebug) WRITE(*, *) mpi_rank, ": Receiving block hydro "
       CALL MPI_RECV(blk%uvel, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%vvel, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%ustar, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%vstar, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%depth, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%dstar, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%dp, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       ! CALL MPI_RECV(blk%windshear1, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       ! CALL MPI_RECV(blk%windshear2, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%wsel, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%isdry, size, MPI_LOGICAL, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(blk%mass_source, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       IF (.NOT. results_only) THEN
          CALL MPI_RECV(blk%uold, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%vold, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%depthold, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%xsource, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          ! CALL MPI_RECV(blk%lud, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          ! CALL MPI_RECV(blk%lvd, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%cell(:,:)%xtype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%cell(:,:)%xbctype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%cell(:,:)%ytype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%cell(:,:)%ybctype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%isdead(:,:)%u, size, MPI_LOGICAL, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%isdead(:,:)%v, size, MPI_LOGICAL, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(blk%isdead(:,:)%p, size, MPI_LOGICAL, src, tag, MPI_COMM_WORLD, status, ierr)
       END IF
   END IF
  END SUBROUTINE mpi_transfer_block_hydro

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_block_hydrobc
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_block_hydrobc(blk, src, dest)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: dest, src

    
 

  END SUBROUTINE mpi_transfer_block_hydrobc


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_iarray
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_iarray(iarray, isize, jsize, src, dest)

    IMPLICIT NONE
  
    INTEGER, INTENT(INOUT) :: iarray(isize, jsize)
    INTEGER, INTENT(IN) :: isize, jsize, src, dest
    INTEGER :: tag = 12, status(MPI_STATUS_SIZE)
    INTEGER :: ierr
  
    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       CALL MPI_SEND(iarray, isize*jsize, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
    ELSE IF (mpi_rank .EQ. dest) THEN
       CALL MPI_RECV(iarray, isize*jsize, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
    END IF
  END SUBROUTINE mpi_transfer_iarray

  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_larray
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_larray(larray, isize, jsize, src, dest)

    IMPLICIT NONE
  
    LOGICAL, INTENT(INOUT) :: larray(isize, jsize)
    INTEGER, INTENT(IN) :: isize, jsize, src, dest
    INTEGER :: tag = 12, status(MPI_STATUS_SIZE)
    INTEGER :: ierr
  
    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       CALL MPI_SEND(larray, isize*jsize, MPI_LOGICAL, dest, tag, MPI_COMM_WORLD, ierr)
    ELSE IF (mpi_rank .EQ. dest) THEN
       CALL MPI_RECV(larray, isize*jsize, MPI_LOGICAL, src, tag, MPI_COMM_WORLD, status, ierr)
    END IF
  END SUBROUTINE mpi_transfer_larray


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_hydro_cell_flags
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_hydro_cell_flags(blk, src, dest)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(INOUT) :: blk
    INTEGER, INTENT(IN) :: dest, src

    INTEGER :: ival(i_index_min:blk%xmax + i_index_extra, &
         &j_index_min:blk%ymax + j_index_extra)
    LOGICAL :: lval(i_index_min:blk%xmax + i_index_extra, &
         &j_index_min:blk%ymax + j_index_extra)

    INTEGER :: isize, jsize

    isize = blk%xmax + i_index_extra - i_index_min + 1
    jsize = blk%ymax + j_index_extra - j_index_min + 1

    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       ival(:, :) = blk%cell(:,:)%xtype
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       ival(:, :) = blk%cell(:,:)%xbctype
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       ival(:, :) = blk%cell(:,:)%ytype
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       ival(:, :) = blk%cell(:,:)%ybctype
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
    ELSE IF (mpi_rank .EQ. dest) THEN
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       blk%cell(:,:)%xtype = ival(:, :)
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       blk%cell(:,:)%xbctype = ival(:, :)
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       blk%cell(:,:)%ytype = ival(:, :)
       CALL mpi_transfer_iarray(ival, isize, jsize, src, dest)
       blk%cell(:,:)%ybctype = ival(:, :)
    END IF
  END SUBROUTINE mpi_transfer_hydro_cell_flags


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_block_scalar
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_block_scalar(blk, sca, src, dest, results_only)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (scalar_struct), INTENT(INOUT) :: sca
    LOGICAL :: results_only

    INTEGER, INTENT(IN) :: dest, src
    INTEGER :: tag = 0
    INTEGER :: size, ierr, status(MPI_STATUS_SIZE)

    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       size = block_size(blk)
       CALL MPI_SEND(sca%conc, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       IF (.NOT. results_only) THEN
          CALL MPI_SEND(sca%concold, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(sca%srcterm, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(sca%cell(:,:)%xtype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          CALL MPI_SEND(sca%cell(:,:)%xbctype, size, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
       END IF
    ELSE IF (mpi_rank .EQ. dest) THEN
       size = block_size(blk)
       CALL MPI_RECV(sca%conc, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       IF (.NOT. results_only) THEN
          CALL MPI_RECV(sca%concold, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(sca%srcterm, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(sca%cell(:,:)%xtype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
          CALL MPI_RECV(sca%cell(:,:)%xbctype, size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, status, ierr)
       END IF
    END IF
  END SUBROUTINE mpi_transfer_block_scalar


  ! ----------------------------------------------------------------
  ! SUBROUTINE mpi_transfer_block_bed
  ! ----------------------------------------------------------------
  SUBROUTINE mpi_transfer_block_bed(blk, bed, src, dest)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (bed_block_rec), INTENT(INOUT) :: bed

    INTEGER, INTENT(IN) :: dest, src
    INTEGER :: tag = 0
    INTEGER :: size, spsize, ierr, status(MPI_STATUS_SIZE)
    
    IF (src .EQ. dest) THEN
       RETURN
    ELSE IF (mpi_rank .EQ. src) THEN
       size = block_size(blk)
       spsize = size*max_species
       CALL MPI_SEND(bed%pore(1, i_index_min, j_index_min), spsize, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(bed%particulate(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(bed%sediment(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(bed%poreflux(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(bed%porosity, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
       CALL MPI_SEND(bed%depth, size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
   ELSE IF (mpi_rank .EQ. dest) THEN
       size = block_size(blk)
       spsize = size*max_species
       CALL MPI_RECV(bed%pore(1, i_index_min, j_index_min), spsize, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(bed%particulate(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(bed%sediment(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(bed%poreflux(1, i_index_min, j_index_min), spsize, &
            &MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(bed%porosity, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
       CALL MPI_RECV(bed%depth, size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, status, ierr)
    END IF

  END SUBROUTINE mpi_transfer_block_bed


END MODULE parallel



