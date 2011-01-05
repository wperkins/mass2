! ----------------------------------------------------------------
! file: solver_common.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June  5, 2003 by William A. Perkins
! Last Change: Tue Jan  4 07:52:20 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE solver_common
! ----------------------------------------------------------------
MODULE solver_common

  IMPLICIT NONE


  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"


                                ! these denote the equation to be
                                ! solved, and are used to signal the
                                ! reuse of solver information if
                                ! applicable
  INTEGER, PARAMETER, PUBLIC :: NUM_SOLVE = 4
  INTEGER, PARAMETER, PUBLIC :: &
       & SOLVE_U = 1, SOLVE_V = 2, SOLVE_DP = 3, SOLVE_SCALAR = 4

                                ! the flags are used to bail out of
                                ! the special solver and resort to the
                                ! tried and true TDMA solver. This can
                                ! be set separately, but globally, for
                                ! scalar quantities and depth
                                ! correction.

  LOGICAL, PUBLIC :: use_tdma_for_scalar, use_tdma_for_depth

                                ! TDMA sweeps, or maximum solver
                                ! iterations, used when solving the
                                ! scalar and depth correction
                                ! equations
  INTEGER, PUBLIC :: scalar_sweep, depth_sweep

                                ! Tolerances used for non-TDMA
                                ! solvers: rsol = relative tolerance,
                                ! asol = absolute tolerance
  DOUBLE PRECISION, PUBLIC :: scalar_rtol = 1.0d-03, scalar_atol = 1.0d-12
  DOUBLE PRECISION, PUBLIC :: depth_rtol = 1.0d-02, depth_atol = 1.0d-12
  
END MODULE solver_common

