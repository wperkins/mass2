! ----------------------------------------------------------------
! file: mass2_parallel.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 14, 2003 by William A. Perkins
! Last Change: Wed Dec 22 06:30:31 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL



PROGRAM mass2_parallel

  USE mpi
  USE utility
  USE block_parallel_grid

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  INTEGER :: ierr
  INTEGER :: mpi_rank

  CALL mpi_init( ierr )
  IF (ierr .NE. 0) CALL error_message("MPI: initialization failure", fatal=.TRUE.)
  CALL ga_initialize()

  CALL mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, ierr)
  IF (ierr .NE. 0) CALL error_message("MPI: cannot get rank", fatal=.TRUE.)

  IF (mpi_rank .EQ. 0) THEN
     CALL banner()
  END IF

  ALLOCATE(block(1))

  CALL block_read_grid(block(1), 1, 'grid.out')

  CALL block_distribution_report()

  CALL block_build_ghost(block(1))

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
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()

  IMPLICIT NONE

#include "global.fh"

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
  WRITE(*,*)'Contact: '
  WRITE(*,*)'    Dr. Marshall C. Richmond <marshall.richmond@pnl.gov>'
  WRITE(*,*)'    William A. Perkins <william.perkins@pnl.gov>'
  WRITE(*,*)
  WRITE(*,'(" Running on ", I3, " processors")') ga_nnodes()
  WRITE(*,'(" Using Global Arrays ", I1, ".", I1)') GA_VERSION_MAJOR, GA_VERSION_MINOR
  
END SUBROUTINE banner




