! -*- mode: f90 -*------------------------------------------------
! file: solver_petsc.F90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 10, 2003 by William A. Perkins
! Last Change: Wed Jul 20 13:14:28 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
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
#include "finclude/petscpc.h"
#include "finclude/petscpc.h90"
#include "finclude/petscksp.h"
#include "finclude/petscksp.h90"
#include "finclude/petscis.h"
#include "finclude/petscis.h90"

                                ! individual blocks need to have save
                                ! matrices, vectors, and solver
                                ! contexts for each equation solved
  TYPE petsc_save_rec
     LOGICAL :: built
     Vec :: x,b,lx
     Mat :: A
     KSP :: ksp
     VecScatter :: vscat
  END TYPE petsc_save_rec

  TYPE petsc_blk_rec
     INTEGER :: gimin, gimax, gjmin, gjmax
     INTEGER :: limin, limax, ljmin, ljmax
     INTEGER :: nlocal, nglobal
     TYPE (petsc_save_rec) :: eq(NUM_SOLVE)
  END TYPE petsc_blk_rec

  TYPE (petsc_blk_rec), ALLOCATABLE, PRIVATE :: pinfo(:)

                                ! remember the number of blocks for
                                ! finalization
  INTEGER, PRIVATE :: myblocks
  
  CHARACTER (LEN=80), PRIVATE :: petscoptfile = "mass2.petscrc"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE solver_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE solver_initialize(blocks)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: blocks

    INTEGER :: b, ieq, ierr
    INTEGER :: status
    LOGICAL :: exists
    
    ALLOCATE(pinfo(blocks))

    ! make sure there is a PETSc configuration file

    INQUIRE(FILE=petscoptfile, EXIST=exists) 
    IF (.NOT. exists) THEN
       OPEN(UNIT=16, FILE=petscoptfile, ACTION='WRITE', IOSTAT=status)
       IF (status .EQ. 0) THEN
          WRITE (16,*)
          CLOSE(16)
       END IF
    END IF

    CALL PetscInitialize(petscoptfile, ierr)
    CHKERRQ(ierr)

    ! CALL PetscPopSignalHandler(ierr)
    CHKERRQ(ierr)

    myblocks = blocks

    DO b = 1, blocks
       DO ieq = 1,  4
          pinfo(b)%eq(ieq)%built = .FALSE.
       END DO
    END DO

  END SUBROUTINE solver_initialize

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_global_index
  !
  ! the indexes i and j are in the global window.  
  !
  ! the result is a global, 0-based index into a global vector
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_global_index(pinfo, i, j) RESULT (ip)

    IMPLICIT NONE
    TYPE (petsc_blk_rec), INTENT(IN) :: pinfo
    INTEGER, INTENT(IN) :: i, j
    INTEGER :: itmp, jtmp, jsize

    ! 0-based indexes in global window

    itmp = i - pinfo%gimin
    jtmp = j - pinfo%gjmin

    ! size of global window
    jsize = pinfo%gjmax - pinfo%gjmin + 1

    ! 0-based global vector index
    ip = itmp*jsize + jtmp

  END FUNCTION solver_global_index


  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver_initialize_block
  ! Returns 0 if all is well
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_initialize_block(iblock, gimin, gimax, gjmin, gjmax, &
       &limin, limax, ljmin, ljmax, do_flow, do_transport)
    
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock
    INTEGER, INTENT(IN) :: gimin, gimax, gjmin, gjmax
    INTEGER, INTENT(IN) :: limin, ljmin, limax, ljmax
    LOGICAL, INTENT(IN) :: do_flow, do_transport
    INTEGER :: ierr
    INTEGER :: ieq, its
    INTEGER :: i, j, idx
    CHARACTER (LEN=10) :: prefix
    CHARACTER (LEN=1024) :: buf, buf1
    LOGICAL :: build

    KSP :: subksp
    PC :: pc, subpc
    PetscReal :: rtol, atol, dtol
    PetscInt, ALLOCATABLE :: lidx(:), gidx(:)
    PetscInt :: mylo, myhi
    IS :: lset, gset

    INTEGER :: nproc

    CALL mpi_comm_size(MPI_COMM_WORLD, nproc, ierr)

    dtol = 1e+04

    pinfo(iblock)%gimin = gimin
    pinfo(iblock)%gimax = gimax
    pinfo(iblock)%gjmin = gjmin
    pinfo(iblock)%gjmax = gjmax
    pinfo(iblock)%nglobal = (gimax - gimin + 1)*(gjmax - gjmin + 1)

    pinfo(iblock)%limin = limin
    pinfo(iblock)%limax = limax
    pinfo(iblock)%ljmin = ljmin
    pinfo(iblock)%ljmax = ljmax
    pinfo(iblock)%nlocal = (limax - limin + 1)*(ljmax - ljmin + 1)

    DO ieq = 1, 4

       ! determine the size of the linear
       ! system based on which equation is
       ! being solved
       SELECT CASE (ieq)
       CASE (SOLVE_U, SOLVE_V, SOLVE_DP)
          build = do_flow
       CASE (SOLVE_SCALAR)
          build = do_transport
       END SELECT

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

       IF (build) THEN 

          ! create a coefficient matrix of
          ! appropriate size for this block and
          ! equation and another matrix that may
          ! store a preconditioner

          CALL MatCreate(PETSC_COMM_WORLD, pinfo(iblock)%eq(ieq)%A, ierr)
          CHKERRQ(ierr)
          CALL MatSetType(pinfo(iblock)%eq(ieq)%A, MATMPIAIJ, ierr)
          CHKERRQ(ierr)
          CALL MatSetSizes(pinfo(iblock)%eq(ieq)%A, &
               &pinfo(iblock)%nlocal, pinfo(iblock)%nlocal, &
               &PETSC_DETERMINE, PETSC_DETERMINE, ierr)
          CALL MatSetFromOptions(pinfo(iblock)%eq(ieq)%A, ierr)
          CHKERRQ(ierr)

          ! create vectors to hold the
          ! right-hand side and estimate

          CALL VecCreate(PETSC_COMM_WORLD, pinfo(iblock)%eq(ieq)%x,ierr)
          CHKERRQ(ierr)
          CALL VecSetType(pinfo(iblock)%eq(ieq)%x, VECMPI, ierr)
          CHKERRQ(ierr)

          ! FIXME: wrong sizes
          CALL VecSetSizes(pinfo(iblock)%eq(ieq)%x, &
               &pinfo(iblock)%nlocal, PETSC_DETERMINE, ierr)
          CHKERRQ(ierr)

          CALL VecSetFromOptions(pinfo(iblock)%eq(ieq)%x,ierr)
          CHKERRQ(ierr)
          CALL VecDuplicate(pinfo(iblock)%eq(ieq)%x, pinfo(iblock)%eq(ieq)%b,ierr)
          CHKERRQ(ierr)
          CALL VecDuplicate(pinfo(iblock)%eq(ieq)%x, pinfo(iblock)%eq(ieq)%lx,ierr)
          CHKERRQ(ierr)

          ! build an index set for global indexes on the local processor
          
          ALLOCATE(lidx(pinfo(iblock)%nlocal))
          ALLOCATE(gidx(pinfo(iblock)%nlocal))

          CALL VecGetOwnershipRange(pinfo(iblock)%eq(ieq)%x, mylo, myhi, ierr)
          CHKERRQ(ierr)

          idx = 1
          DO i = limin, limax
             DO j = ljmin, ljmax
                lidx(idx) = idx + mylo - 1
                gidx(idx) = solver_global_index(pinfo(iblock), i, j)
                idx = idx + 1
             END DO
          END DO
          CALL ISCreateGeneral(PETSC_COMM_WORLD, pinfo(iblock)%nlocal, lidx, lset, ierr)
          
          ! CALL ISView(lset, PETSC_VIEWER_STDOUT_WORLD, ierr)
          
          CALL ISCreateGeneral(PETSC_COMM_WORLD, pinfo(iblock)%nlocal, gidx, gset, ierr)
          
          ! CALL ISView(gset, PETSC_VIEWER_STDOUT_WORLD, ierr)

          DEALLOCATE(lidx)
          DEALLOCATE(gidx)

          CALL VecScatterCreate(pinfo(iblock)%eq(ieq)%x, gset, &
               &pinfo(iblock)%eq(ieq)%lx, lset, &
               &pinfo(iblock)%eq(ieq)%vscat, ierr)
          CHKERRQ(ierr)

          ! CALL VecScatterView(pinfo(iblock)%eq(ieq)%vscat, PETSC_VIEWER_STDOUT_WORLD, ierr)

          CALL ISDestroy(lset, ierr)
          CHKERRQ(ierr)
          CALL ISDestroy(gset, ierr)
          CHKERRQ(ierr)


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
          ! CALL KSPMonitorSet(pinfo(iblock)%eq(ieq)%ksp, KSPMonitorDefault, &
          !      &PETSC_NULL, PETSC_NULL, ierr);
          ! CHKERRQ(ierr)

          ! need to change the preconditioner, since the default
          ! (ILU) does not work with MATMPIAIJ matrixes

          CALL KSPGetPC(pinfo(iblock)%eq(ieq)%ksp, pc, ierr)
          CHKERRQ(ierr)
          CALL PCSetType(pc, PCASM, ierr)
          CHKERRQ(ierr)

          ! set options from the command line
          ! (does PC and KSP too)
          CALL KSPSetFromOptions(pinfo(iblock)%eq(ieq)%ksp, ierr)
          CHKERRQ(ierr)

          ! report what methods were actually used
          
          CALL KSPGetType(pinfo(iblock)%eq(ieq)%ksp, buf, ierr)
          CHKERRQ(ierr)
          CALL PCGetType(pc, buf1, ierr)
          CHKERRQ(ierr)

          WRITE (*,*) 'Solver Initialize, Block ', iblock, &
               &', equation ', ieq, '(', TRIM(prefix), &
               &'), local size = ', pinfo(iblock)%nlocal, &
               &', KSP = ', TRIM(buf), ', PC = ', TRIM(buf1)

          pinfo(iblock)%eq(ieq)%built = .TRUE.

       END IF
    END DO
    solver_initialize_block =  ierr
  END FUNCTION solver_initialize_block

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION solver
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver(iblock, ieq, imin, imax, jmin, jmax, its, &
       &ap, aw, ae, as, an, bp, x)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock, ieq, imin, imax, jmin, jmax, its
    DOUBLE PRECISION, INTENT(IN), &
         &DIMENSION(imin:imax, jmin:jmax) :: &
         &ap, aw, ae, as, an, bp
    DOUBLE PRECISION, INTENT(INOUT), &
         &DIMENSION(imin:imax, jmin:jmax) :: x
    INTEGER :: i, j, ip, ie, iw, in, is
    DOUBLE PRECISION :: dtmp
    INTEGER :: lidx, gimin, gimax, gjmin, gjmax
    INTEGER :: ierr
    
    PetscScalar v
    PetscScalar, pointer :: x_vv(:)

    gimin = pinfo(iblock)%gimin
    gimax = pinfo(iblock)%gimax
    gjmin = pinfo(iblock)%gjmin
    gjmax = pinfo(iblock)%gjmax

    DO i = imin, imax
       DO j = jmin, jmax
          ip = solver_global_index(pinfo(iblock), i  , j  )
          ie = solver_global_index(pinfo(iblock), i+1, j  )
          iw = solver_global_index(pinfo(iblock), i-1, j  )
          in = solver_global_index(pinfo(iblock), i  , j+1)
          is = solver_global_index(pinfo(iblock), i  , j-1)

          v = ap(i,j)
          call MatSetValue(pinfo(iblock)%eq(ieq)%A, ip, ip, v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)

          IF (j .LT. gjmax) THEN
             v = -an(i,j)
             call MatSetValue(pinfo(iblock)%eq(ieq)%A, ip, in, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (j .GT. gjmin) THEN
             v = -as(i,j)
             call MatSetValue(pinfo(iblock)%eq(ieq)%A, ip, is, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (i .LT. gimax) THEN
             v = -ae(i,j)
             call MatSetValue(pinfo(iblock)%eq(ieq)%A, ip, ie, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          IF (i .GT. gimin) THEN
             v = -aw(i,j)
             call MatSetValue(pinfo(iblock)%eq(ieq)%A, ip, iw, v, INSERT_VALUES,ierr)
             CHKERRQ(ierr)
          END IF

          v = bp(i,j)
          call VecSetValue(pinfo(iblock)%eq(ieq)%b, ip, v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)
        
          v = x(i,j)
          call VecSetValue(pinfo(iblock)%eq(ieq)%x, ip, v, INSERT_VALUES, ierr)
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

    ! CALL MatView(pinfo(iblock)%eq(ieq)%A, PETSC_VIEWER_STDOUT_WORLD, ierr)

    ! CALL VecView(pinfo(iblock)%eq(ieq)%b, PETSC_VIEWER_STDOUT_WORLD, ierr)

    ! WRITE(*,*) "Solver: Matrix assembled"

    call KSPSetOperators(pinfo(iblock)%eq(ieq)%ksp,&
         &pinfo(iblock)%eq(ieq)%A,pinfo(iblock)%eq(ieq)%A,&
         &SAME_NONZERO_PATTERN, ierr)
    CHKERRQ(ierr)
    call KSPSolve(pinfo(iblock)%eq(ieq)%ksp, &
         &pinfo(iblock)%eq(ieq)%b, pinfo(iblock)%eq(ieq)%x, ierr)
    CHKERRQ(ierr)

    ! FIXME: Report divergence problems

    ! WRITE(*,*) "Solver: Solution complete"

    ! CALL VecView(pinfo(iblock)%eq(ieq)%x, PETSC_VIEWER_STDOUT_WORLD, ierr)

    CALL VecScatterBegin(pinfo(iblock)%eq(ieq)%vscat,&
         &pinfo(iblock)%eq(ieq)%x, pinfo(iblock)%eq(ieq)%lx, &
         &INSERT_VALUES, SCATTER_FORWARD, ierr)
    CALL VecScatterEnd(pinfo(iblock)%eq(ieq)%vscat,&
         &pinfo(iblock)%eq(ieq)%x, pinfo(iblock)%eq(ieq)%lx, &
         &INSERT_VALUES, SCATTER_FORWARD, ierr)

    ! CALL VecView(pinfo(iblock)%eq(ieq)%lx, PETSC_VIEWER_STDOUT_WORLD, ierr)

    call VecGetArrayF90(pinfo(iblock)%eq(ieq)%lx, x_vv, ierr)
    CHKERRQ(ierr)
  
    lidx = 1
    DO i = imin, imax
       DO j = jmin, jmax
          dtmp = x_vv(lidx)
          x(i, j) = dtmp
          lidx = lidx + 1
       END DO
    END DO
    CALL VecRestoreArrayF90(pinfo(iblock)%eq(ieq)%lx, x_vv, ierr)
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
             CALL VecDestroy(pinfo(iblock)%eq(ieq)%lx, ierr)
             CHKERRQ(ierr)
             CALL VecDestroy(pinfo(iblock)%eq(ieq)%b, ierr)
             CHKERRQ(ierr)
             ! CALL VecScatterDestroy(pinfo(iblock)%eq(ieq)%vscat, ierr)
             ! CHKERRQ(ierr)
          END IF
       END DO
    END DO
    DEALLOCATE(pinfo)
    CALL PetscFinalize(ierr)
    CHKERRQ(ierr)
    solver_finalize = ierr
  END FUNCTION solver_finalize

END MODULE solver_module
