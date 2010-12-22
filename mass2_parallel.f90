! ----------------------------------------------------------------
! file: mass2_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 14, 2003 by William A. Perkins
! Last Change: Wed Dec 22 14:48:33 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL



PROGRAM mass2_parallel

  USE mpi
  USE utility
  USE time_series
  USE block_parallel_grid
  USE config

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
  
  max_blocks = 1
  ALLOCATE(grid_file_name(max_blocks))
  grid_file_name(1) = "grid.out"
  CALL read_grid()
  

  CALL ga_sync()

  IF (mpi_rank .EQ. 0) THEN
     CALL block_gridplot('gridplot1.dat', .TRUE.)
  END IF

  CALL ga_sync()

  DEALLOCATE(block)

  CALL ga_terminate()
  CALL mpi_finalize(ierr)   

END PROGRAM mass2_parallel

! ----------------------------------------------------------------
! SUBROUTINE read_grid
! ----------------------------------------------------------------
SUBROUTINE read_grid()

  USE utility
  USE config
  USE block_parallel_grid

  IMPLICIT NONE

  INTEGER :: b

  IF (max_blocks .GT. 1) THEN
     CALL error_message("Only one block allowed right now", fatal=.TRUE.)
  END IF

  ALLOCATE(block(max_blocks))

  DO b = 1, max_blocks

     CALL block_read_grid(block(b), b, grid_file_name(b))

     CALL block_build_ghost(block(b))

     CALL block_interp_grid(block(b))

     CALL block_metrics(block(b))

  END DO

  CALL block_distribution_report()
  

END SUBROUTINE read_grid

! ----------------------------------------------------------------
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()

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




