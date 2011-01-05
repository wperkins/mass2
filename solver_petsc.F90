! -*- mode: f90 -*------------------------------------------------
! file: solver_petsc.F90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 10, 2003 by William A. Perkins
! Last Change: Wed Jan  5 09:20:15 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE solver
! ----------------------------------------------------------------
MODULE solver_module

  USE solver_common

  IMPLICIT NONE


  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

#include "finclude/petscsys.h"
#include "finclude/petscdef.h"
#include "finclude/petscvec.h"
#include "finclude/petscvec.h90"
#include "finclude/petscmat.h"
#include "finclude/petscmat.h90"
#include "finclude/petscksp.h"
#include "finclude/petscksp.h90"

                                ! individual blocks need to have save
                                ! matrices, vectors, and solver
                                ! contexts for each equation solved
  TYPE petsc_save_rec
     LOGICAL :: built
     Vec :: x,b,u
     Mat :: A
     KSP :: ksp
  END TYPE petsc_save_rec

  TYPE petsc_blk_rec
     TYPE (petsc_save_rec) :: eq(NUM_SOLVE)
  END TYPE petsc_blk_rec

  TYPE (petsc_blk_rec), ALLOCATABLE, PRIVATE :: pinfo(:)

                                ! remember the number of blocks for
                                ! finalization
  INTEGER, PRIVATE :: myblocks

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_initialize
  ! Returns 0 if all is well
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_initialize(blocks, xmax, ymax, do_flow, do_transport)
    
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blocks, xmax(blocks), ymax(blocks)
    LOGICAL, INTENT(IN) :: do_flow, do_transport
    INTEGER :: ierr
    INTEGER :: iblock, imax, jmax, ieq, its
    CHARACTER (LEN=10) :: prefix
    CHARACTER (LEN=1024) :: buf
    LOGICAL :: build

    PetscReal :: rtol, atol, dtol

    dtol = 1e+04

    myblocks = blocks
    ALLOCATE(pinfo(myblocks))

    CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
    CHKERRQ(ierr)

    CALL PetscPopSignalHandler(ierr)
    CHKERRQ(ierr)

    DO iblock = 1, myblocks
       DO ieq = 1, 4

          imax = xmax(iblock) - 2 + 1
          jmax = ymax(iblock) - 2 + 1

                                ! determine the size of the linear
                                ! system based on which equation is
                                ! being solved
          SELECT CASE (ieq)
          CASE (SOLVE_V)
             jmax = jmax - 1
             build = do_flow
          CASE (SOLVE_U, SOLVE_DP)
             build = do_flow
          CASE (SOLVE_SCALAR)
             build = do_transport
          END SELECT

          pinfo(iblock)%eq(ieq)%built = build
    
          IF (build) THEN 
             WRITE (*,*) 'Solver Initialize, Block ', iblock, ', equation ', ieq
             WRITE (*,*) '     imax = ', imax, ', jmax = ', jmax, ', size = ', imax*jmax

          

                                ! create a coefficient matrix of
                                ! appropriate size for this block and
                                ! equation and another matrix that may
                                ! store a preconditioner

             CALL MatCreate(PETSC_COMM_WORLD, pinfo(iblock)%eq(ieq)%A, ierr)
             CHKERRQ(ierr)
             CALL MatSetType(pinfo(iblock)%eq(ieq)%A, MATSEQAIJ, ierr)
             CHKERRQ(ierr)
             CALL MatSetSizes(pinfo(iblock)%eq(ieq)%A, imax*jmax, imax*jmax, &
                  &PETSC_DETERMINE, PETSC_DETERMINE, ierr)
             CALL MatSetFromOptions(pinfo(iblock)%eq(ieq)%A, ierr)
             CHKERRQ(ierr)
    
                                ! create vectors to hold the
                                ! right-hand side and estimate
             
             CALL VecCreate(PETSC_COMM_WORLD, pinfo(iblock)%eq(ieq)%x,ierr)
             CHKERRQ(ierr)
             CALL VecSetType(pinfo(iblock)%eq(ieq)%x, VECSEQ, ierr)
             CHKERRQ(ierr)

             ! FIXME: wrong sizes
             CALL VecSetSizes(pinfo(iblock)%eq(ieq)%x, imax*jmax, PETSC_DETERMINE, ierr)
             CHKERRQ(ierr)
             
             CALL VecSetFromOptions(pinfo(iblock)%eq(ieq)%x,ierr)
             CHKERRQ(ierr)
             CALL VecDuplicate(pinfo(iblock)%eq(ieq)%x,pinfo(iblock)%eq(ieq)%b,ierr)
             CHKERRQ(ierr)
             CALL VecDuplicate(pinfo(iblock)%eq(ieq)%x,pinfo(iblock)%eq(ieq)%u,ierr)
             CHKERRQ(ierr)

                                ! create a solver context for this
                                ! block and equation. The options are
                                ! renamed with a prefix of either
                                ! 'scalar' or 'depth'.  

             SELECT CASE (ieq)
             CASE (SOLVE_DP)
                prefix = 'depth_'
                its = depth_sweep
                rtol = depth_rtol
                atol = depth_atol
             CASE DEFAULT
                prefix = 'scalar_'
                its = scalar_sweep
                rtol = scalar_rtol
                atol = scalar_atol
             END SELECT

             
             CALL KSPCreate(PETSC_COMM_WORLD, pinfo(iblock)%eq(ieq)%ksp, ierr)
             CHKERRQ(ierr)
             CALL KSPAppendOptionsPrefix(pinfo(iblock)%eq(ieq)%ksp, prefix, ierr)
             CHKERRQ(ierr)
             CALL KSPSetInitialGuessNonzero(pinfo(iblock)%eq(ieq)%ksp, PETSC_TRUE, ierr)
             CHKERRQ(ierr)
             CALL KSPSetTolerances(pinfo(iblock)%eq(ieq)%ksp, rtol, atol, dtol, its, ierr)
             CHKERRQ(ierr)
             CALL KSPDefaultConvergedSetUIRNorm(pinfo(iblock)%eq(ieq)%ksp, ierr) 
             CHKERRQ(ierr)
             CALL KSPMonitorSet(pinfo(iblock)%eq(ieq)%ksp, KSPMonitorDefault, &
                  &PETSC_NULL, PETSC_NULL, ierr);
             CHKERRQ(ierr)

                                ! set options from the command line
                                ! (does PC and KSP too)
             CALL KSPSetFromOptions(pinfo(iblock)%eq(ieq)%ksp, ierr)
             CHKERRQ(ierr)
          END IF
       END DO
    END DO
    solver_initialize =  ierr
  END FUNCTION solver_initialize

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver(iblock, ieq, x_start, x_end, y_start, y_end, its, &
       &ap, aw, ae, as, an, bp, x)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock, ieq, x_start, x_end, y_start, y_end, its
    DOUBLE PRECISION, INTENT(IN), &
         &DIMENSION(x_start:x_end,y_start:y_end) :: &
         &ap, aw, ae, as, an, bp
    DOUBLE PRECISION, INTENT(INOUT), &
         &DIMENSION(x_start:x_end,y_start:y_end) :: x
    INTEGER :: imax, jmax, i, j, ip, ie, iw, in, is, itmp, jtmp
    INTEGER :: ierr
    
    PetscScalar v, tout(1)
    PetscOffset i_t

    imax = x_end - x_start + 1
    jmax = y_end - y_start + 1

    ! WRITE(*, *) 'Solver: starting: ', x_start, x_end, y_start, y_end, imax, jmax

    DO i = 1, imax
       DO j = 1, jmax
          ip = (i-1)*jmax + j - 1
          ie = i*jmax + j - 1
          iw = (i-2)*jmax + j - 1
          in = (i-1)*jmax + (j+1) - 1
          is = (i-1)*jmax + (j-1) - 1

          itmp = x_start + (i - 1)
          jtmp = y_start + (j - 1)
          
          v = ap(itmp,jtmp)
          call MatSetValues(pinfo(iblock)%eq(ieq)%A, 1, ip, 1, ip, v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)

          IF (j .LT. jmax) THEN
             v = -an(itmp,jtmp)
             call MatSetValues(pinfo(iblock)%eq(ieq)%A, 1, ip, 1, in, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (j .GT. 1) THEN
             v = -as(itmp,jtmp)
             call MatSetValues(pinfo(iblock)%eq(ieq)%A, 1, ip, 1, is, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (i .LT. imax) THEN
             v = -ae(itmp,jtmp)
             call MatSetValues(pinfo(iblock)%eq(ieq)%A, 1, ip, 1, ie, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (i .GT. 1) THEN
             v = -aw(itmp,jtmp)
             call MatSetValues(pinfo(iblock)%eq(ieq)%A, 1, ip, 1, iw, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          v = bp(itmp,jtmp)
          call VecSetValues(pinfo(iblock)%eq(ieq)%b, 1, ip, v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)
        
          v = x(itmp,jtmp)
          call VecSetValues(pinfo(iblock)%eq(ieq)%x, 1, ip, v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)

          ! WRITE(*,*) "Solver: inserted row ", ip
       END DO
    END DO

    ! WRITE(*,*) "Solver: values assembled"

    CALL MatAssemblyBegin(pinfo(iblock)%eq(ieq)%A,MAT_FINAL_ASSEMBLY,ierr)
    CHKERRQ(ierr)
    CALL MatAssemblyEnd(pinfo(iblock)%eq(ieq)%A,MAT_FINAL_ASSEMBLY,ierr)
    CHKERRQ(ierr)
    CALL VecAssemblyBegin(pinfo(iblock)%eq(ieq)%x, ierr)
    CHKERRQ(ierr)
    CALL VecAssemblyEnd(pinfo(iblock)%eq(ieq)%x, ierr)
    CHKERRQ(ierr)
    CALL VecAssemblyBegin(pinfo(iblock)%eq(ieq)%b, ierr)
    CHKERRQ(ierr)
    CALL VecAssemblyEnd(pinfo(iblock)%eq(ieq)%b, ierr)
    CHKERRQ(ierr)

    ! WRITE(*,*) "Solver: Matrix assembled"

    call KSPSetOperators(pinfo(iblock)%eq(ieq)%ksp,&
         &pinfo(iblock)%eq(ieq)%A,pinfo(iblock)%eq(ieq)%A,&
         &SAME_NONZERO_PATTERN, ierr)
    CHKERRQ(ierr)
    call KSPSolve(pinfo(iblock)%eq(ieq)%ksp, &
         &pinfo(iblock)%eq(ieq)%b, pinfo(iblock)%eq(ieq)%x, ierr)
    CHKERRQ(ierr)

    ! WRITE(*,*) "Solver: Solution complete"

    call VecGetArray(pinfo(iblock)%eq(ieq)%x, tout, i_t, ierr)
    CHKERRQ(ierr)
  
    DO i = 1, imax
       DO j = 1, jmax
          ip = (i-1)*jmax + j
          itmp = x_start + (i - 1)
          jtmp = y_start + (j - 1)
          x(itmp,jtmp) = tout(i_t + 1 + (ip - 1))
       END DO
    END DO
    CALL VecRestoreArray(pinfo(iblock)%eq(ieq)%x, tout, i_t, ierr)
    CHKERRQ(ierr)

    ! WRITE(*,*) "Solver: Solution written to array"

    solver = ierr
  END FUNCTION solver
  

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_finalize
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_finalize()

    IMPLICIT NONE

    INTEGER :: iblock, ieq, ierr

    DO iblock = 1, myblocks
       DO ieq = 1, 4
          IF (pinfo(iblock)%eq(ieq)%built) THEN
             CALL KSPDestroy(pinfo(iblock)%eq(ieq)%ksp, ierr)
             CHKERRQ(ierr)
             CALL MatDestroy(pinfo(iblock)%eq(ieq)%A, ierr)
             CHKERRQ(ierr)
             CALL VecDestroy(pinfo(iblock)%eq(ieq)%x, ierr)
             CHKERRQ(ierr)
             CALL VecDestroy(pinfo(iblock)%eq(ieq)%b, ierr)
             CHKERRQ(ierr)
             CALL VecDestroy(pinfo(iblock)%eq(ieq)%u, ierr)
             CHKERRQ(ierr)
          END IF
       END DO
    END DO
    DEALLOCATE(pinfo)
    CALL PetscFinalize(ierr)
    CHKERRQ(ierr)
    solver_finalize = ierr
  END FUNCTION solver_finalize

END MODULE solver_module
