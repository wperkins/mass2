! -*- mode: f90 -*------------------------------------------------
! file: solver_petsc.F90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 10, 2003 by William A. Perkins
! Last Change: 2017-08-10 08:54:28 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE solver
! ----------------------------------------------------------------
MODULE solver_module

  USE solver_common

  IMPLICIT NONE


  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

#include "finclude/petscsys.h"
#include "finclude/petsclog.h"
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
     INTEGER :: pidxmin, pidxmax
     INTEGER, POINTER :: pidx(:,:)
     TYPE (petsc_save_rec) :: eq(NUM_SOLVE)
  END TYPE petsc_blk_rec

  TYPE (petsc_blk_rec), ALLOCATABLE, PRIVATE :: pinfo(:)

                                ! remember the number of blocks for
                                ! finalization
  INTEGER, PRIVATE :: myblocks

  DOUBLE PRECISION, PRIVATE :: tmp1_time, tmp2_time
  DOUBLE PRECISION, PRIVATE :: total_time, depth_time, scalar_time
  CHARACTER (LEN=1028), PRIVATE :: msg
  
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

    CALL PetscPopSignalHandler(ierr)
    CHKERRQ(ierr)

    CALL PetscTime(total_time, ierr);
    CHKERRQ(ierr)

    scalar_time = 0.0;
    depth_time = 0.0;

    myblocks = blocks

    DO b = 1, blocks
       pinfo(b)%pidx => NULL()
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
  ! 
  ! the result is the index used by MASS2, it should not be used as a
  ! global index by PETSc
  ! ----------------------------------------------------------------
  INTEGER FUNCTION solver_global_index(pinfo, i, j) RESULT (ip)

    IMPLICIT NONE
    TYPE (petsc_blk_rec), INTENT(IN) :: pinfo
    INTEGER, INTENT(IN) :: i, j
    INTEGER :: itmp, jtmp, jsize

    ! 0-based indexes in global window

    ! if the i and j are outside the global window return -1

    ip = -1
    IF ((pinfo%gimin .LE. i .AND. i .LE. pinfo%gimax) .AND. &
         &(pinfo%gjmin .LE. j .AND. j .LE. pinfo%gjmax)) THEN

       itmp = i - pinfo%gimin
       jtmp = j - pinfo%gjmin

       ! size of global window
       jsize = pinfo%gjmax - pinfo%gjmin + 1
       
       ! 0-based global vector index
       ip = itmp*jsize + jtmp
    END IF

  END FUNCTION solver_global_index

  ! ----------------------------------------------------------------
  ! SUBROUTINE solver_map_indexes
  ! Collective
  ! ----------------------------------------------------------------
  SUBROUTINE solver_map_indexes(brec)

    IMPLICIT NONE

    TYPE (petsc_blk_rec), INTENT(INOUT) :: brec
    INTEGER :: i, j, n
    INTEGER :: imin, imax, jmin, jmax
    
    AO :: theorder
    PetscInt, ALLOCATABLE :: mass2gidx(:), petscgidx(:)
    PetscInt :: thesize
    PetscErrorCode :: ierr

    ! create a mapping between the indexes and the indexes used by
    ! petsc by using the locally owned indexes

    thesize = (brec%limax - brec%limin + 3)*(brec%ljmax - brec%ljmin + 3)

    ALLOCATE(mass2gidx(thesize), petscgidx(thesize))
    
    n = 1
    DO i = brec%limin, brec%limax
       DO j = brec%ljmin, brec%ljmax
          mass2gidx(n) = solver_global_index(brec, i, j)
          petscgidx(n) = brec%pidxmin + n - 1
          n = n + 1
       END DO
    END DO

    CALL AOCreateBasic(MPI_COMM_WORLD, brec%nlocal, mass2gidx, petscgidx, theorder, ierr)
    CHKERRQ(ierr)

    ! get the petsc indexes for a window one index larger than what is
    ! owned by this process

    mass2gidx = -1

    n = 1
    DO i = brec%limin - 1, brec%limax + 1
       DO j = brec%ljmin - 1, brec%ljmax + 1
          mass2gidx(n) = solver_global_index(brec, i, j)
          n = n + 1
       END DO
    END DO

    CALL AOApplicationToPetsc(theorder, thesize, mass2gidx, ierr)
    CHKERRQ(ierr)

    ! CALL AOView(theorder, PETSC_VIEWER_STDOUT_WORLD, ierr)
    ! CHKERRQ(ierr)

    ALLOCATE(brec%pidx(brec%limin - 1:brec%limax + 1, brec%ljmin - 1:brec%ljmax + 1))

    n = 1
    DO i = brec%limin - 1, brec%limax + 1
       DO j = brec%ljmin - 1, brec%ljmax + 1
          brec%pidx(i, j) = mass2gidx(n)
          n = n + 1
       END DO
    END DO

    CALL AODestroy(theorder, ierr)
    CHKERRQ(ierr)

    DEALLOCATE(mass2gidx, petscgidx)

    
  END SUBROUTINE solver_map_indexes



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
    PetscErrorCode :: ierr

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

          CALL MatCreate(MPI_COMM_WORLD, pinfo(iblock)%eq(ieq)%A, ierr)
          CHKERRQ(ierr)
          CALL MatSetType(pinfo(iblock)%eq(ieq)%A, MATMPIAIJ, ierr)
          CHKERRQ(ierr)
          CALL MatSetSizes(pinfo(iblock)%eq(ieq)%A, &
               &pinfo(iblock)%nlocal, pinfo(iblock)%nlocal, &
               &PETSC_DETERMINE, PETSC_DETERMINE, ierr)
          CALL MatSetFromOptions(pinfo(iblock)%eq(ieq)%A, ierr)
          CHKERRQ(ierr)
          CALL MatSetUp(pinfo(iblock)%eq(ieq)%A, ierr)
          CHKERRQ(ierr)

          ! create vectors to hold the
          ! right-hand side and estimate

          CALL VecCreate(MPI_COMM_WORLD, pinfo(iblock)%eq(ieq)%x,ierr)
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

          CALL VecGetOwnershipRange(pinfo(iblock)%eq(ieq)%x, mylo, myhi, ierr)
          CHKERRQ(ierr)

          IF (.NOT. ASSOCIATED(pinfo(iblock)%pidx)) THEN
             ! this only needs to be done once per block
             pinfo(iblock)%pidxmin = mylo
             pinfo(iblock)%pidxmax = myhi
             CALL solver_map_indexes(pinfo(iblock))
          END IF

          CALL KSPCreate(MPI_COMM_WORLD, pinfo(iblock)%eq(ieq)%ksp, ierr)
          CHKERRQ(ierr)
          CALL KSPAppendOptionsPrefix(pinfo(iblock)%eq(ieq)%ksp, prefix, ierr)
          CHKERRQ(ierr)
          CALL KSPSetInitialGuessNonzero(pinfo(iblock)%eq(ieq)%ksp, PETSC_TRUE, ierr)
          CHKERRQ(ierr)
          CALL KSPSetTolerances(pinfo(iblock)%eq(ieq)%ksp, rtol, atol, dtol, its, ierr)
          CHKERRQ(ierr)
#if PETSC_VERSION_LT(3,5,0)
          CALL KSPDefaultConvergedSetUIRNorm(pinfo(iblock)%eq(ieq)%ksp, ierr) 
          CHKERRQ(ierr)
#else
          CALL KSPConvergedDefaultSetUIRNorm(pinfo(iblock)%eq(ieq)%ksp, ierr) 
          CHKERRQ(ierr)
#endif

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

          WRITE (msg,*) 'Solver Initialize, Block ', iblock, &
               &', equation ', ieq, '(', TRIM(prefix), &
               &'), local size = ', pinfo(iblock)%nlocal, &
               &', KSP = ', TRIM(buf), ', PC = ', TRIM(buf1), '\n'
          CALL PetscSynchronizedPrintf(MPI_COMM_WORLD, msg, ierr)
          CHKERRQ(ierr)

          pinfo(iblock)%eq(ieq)%built = .TRUE.

       END IF
    END DO
    ! CALL PetscSynchronizedFlush(MPI_COMM_WORLD, ierr)
    ! CHKERRQ(ierr)
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
    INTEGER :: i, j, ip, ie, iw, in, is, idx
    DOUBLE PRECISION :: dtmp
    INTEGER :: lidx, gimin, gimax, gjmin, gjmax
    INTEGER :: ierr
    
    PetscScalar :: v(5)
    PetscInt :: ridx(5), cidx(5)
    PetscScalar, pointer :: x_vv(:)

    CALL PetscTime(tmp1_time, ierr);
    CHKERRQ(ierr)

    gimin = pinfo(iblock)%gimin
    gimax = pinfo(iblock)%gimax
    gjmin = pinfo(iblock)%gjmin
    gjmax = pinfo(iblock)%gjmax

    DO i = imin, imax
       DO j = jmin, jmax
          ip = pinfo(iblock)%pidx(i  , j  )
          ie = pinfo(iblock)%pidx(i+1, j  )
          iw = pinfo(iblock)%pidx(i-1, j  )
          in = pinfo(iblock)%pidx(i  , j+1)
          is = pinfo(iblock)%pidx(i  , j-1)

          idx = 1
          v = 0.0;
          ridx(1) = ip

          cidx(idx) = ip
          v(idx) = ap(i,j)
          idx = idx + 1

          IF (ie .GE. 0) THEN
             cidx(idx) = ie
             v(idx) = -ae(i,j)
             idx = idx + 1
          END IF

          IF (iw .GE. 0) THEN
             cidx(idx) = iw
             v(idx) = -aw(i,j)
             idx = idx + 1
          END IF

          IF (in .GE. 0) THEN
             cidx(idx) = in
             v(idx) = -an(i,j)
             idx = idx + 1
          END IF

          IF (is .GE. 0) THEN
             cidx(idx) = is
             v(idx) = -as(i,j)
             idx = idx + 1
          END IF
          idx = idx - 1

          call MatSetValues(pinfo(iblock)%eq(ieq)%A, &
               &1, ridx, idx, cidx, &
               &v, INSERT_VALUES, ierr)
          CHKERRQ(ierr)

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

    call KSPSetOperators(pinfo(iblock)%eq(ieq)%ksp,&
         &pinfo(iblock)%eq(ieq)%A,pinfo(iblock)%eq(ieq)%A,&
         &ierr)
    CHKERRQ(ierr)
    call KSPSolve(pinfo(iblock)%eq(ieq)%ksp, &
         &pinfo(iblock)%eq(ieq)%b, pinfo(iblock)%eq(ieq)%x, ierr)
    CHKERRQ(ierr)

    call VecGetArrayF90(pinfo(iblock)%eq(ieq)%x, x_vv, ierr)
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

    CALL PetscTime(tmp2_time, ierr);
    CHKERRQ(ierr)


    SELECT CASE (ieq)
    CASE (SOLVE_U, SOLVE_V, SOLVE_SCALAR)
       scalar_time = scalar_time + (tmp2_time - tmp1_time)
    CASE (SOLVE_DP)
       depth_time = depth_time + (tmp2_time - tmp1_time)
    END SELECT

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
       DEALLOCATE(pinfo(iblock)%pidx)
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

    CALL PetscTime(tmp2_time, ierr);
    CHKERRQ(ierr)
    total_time = tmp2_time - total_time

    WRITE(msg, 100) total_time,  &
         &scalar_time, scalar_time/total_time*100.0, &
         &depth_time, depth_time/total_time*100.0
    CALL PetscSynchronizedPrintf(MPI_COMM_WORLD, msg, ierr)
    CHKERRQ(ierr)
    CALL PetscSynchronizedFlush(MPI_COMM_WORLD, ierr)
    CHKERRQ(ierr)
    
    DEALLOCATE(pinfo)
    CALL PetscFinalize(ierr)
    CHKERRQ(ierr)
    solver_finalize = ierr

100 FORMAT('Total =', F10.2, &
         &', Scalar = ', F10.2, ' (', F4.1, '%)', &
         &', Depth = ', F10.2, ' (', F4.1, '%)', '\n')
  END FUNCTION solver_finalize

END MODULE solver_module
