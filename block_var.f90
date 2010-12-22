! ----------------------------------------------------------------
! file: block_var.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: Wed Dec 22 11:26:20 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL


! ----------------------------------------------------------------
! MODULE block_var
!
! This module is build around the block_variable type.  It is intended
! to encapsulate GA operations for a single variable and maintain
! local arrays for that variable.  All indexing uses the GA indexing
! (1-based).
! ----------------------------------------------------------------
MODULE block_variable

  USE utility
  USE block_variable_base

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PUBLIC, PARAMETER :: &
       &BLK_VAR_CURRENT = 1, &
       &BLK_VAR_STAR = 2, &
       &BLK_VAR_OLD = 3, &
       &BLK_VAR_OLDOLD = 4

  ! ----------------------------------------------------------------
  ! TYPE block_var
  ! ----------------------------------------------------------------
  TYPE block_var

     TYPE (block_var_base), POINTER :: base

     ! handle to the Global Array for this variable
     INTEGER :: ga_handle

     ! local copies of global array; includes both owned and ghost;
     ! these use MASS2 array indexes; these may not all be required
     DOUBLE PRECISION, ALLOCATABLE :: current(:,:)
     DOUBLE PRECISION, POINTER :: star(:,:)
     DOUBLE PRECISION, POINTER :: old(:,:)
     DOUBLE PRECISION, POINTER :: oldold(:,:)
  END type block_var


CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE (block_var) FUNCTION block_var_allocate
  ! ----------------------------------------------------------------
  TYPE (block_var) FUNCTION block_var_allocate(name, base, const) &
       &RESULT(v)
  
    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: name
    TYPE (block_var_base), POINTER, INTENT(IN) :: base
    LOGICAL, INTENT(IN), OPTIONAL :: const

    POINTER v
    
    LOGICAL :: ga_ok
    INTEGER :: ierr
    INTEGER :: imin, imax, jmin, jmax
    CHARACTER (LEN=1024) :: msg
    LOGICAL :: doconst

    doconst = .FALSE.
    IF (PRESENT(const)) doconst = const

    ierr = 0

    ALLOCATE(v, STAT=ierr)
    IF (ierr .NE. 0) GOTO 100

    v%base => base
    ga_ok = ga_duplicate(v%base%ga_handle, v%ga_handle, name)

    IF (.NOT. ga_ok) GOTO 100

    ! CALL ga_print(v%ga_handle)

    imin = v%base%imin_used
    imax = v%base%imax_used
    jmin = v%base%jmin_used
    jmax = v%base%jmax_used

    ALLOCATE(v%current(imin:imax, jmin:jmax), STAT=ierr)
    IF (ierr .NE. 0) GOTO 100
    v%current = 0.0

    IF (.NOT. doconst) THEN
       ALLOCATE(v%star(imin:imax, jmin:jmax), &
            &v%old(imin:imax, jmin:jmax), &
            &v%oldold(imin:imax, jmin:jmax), STAT=ierr)
       IF (ierr .NE. 0) GOTO 100

       v%star = 0.0
       v%old = 0.0
       v%oldold = 0.0

    END IF

    RETURN

100 CONTINUE

    DEALLOCATE(v)
    NULLIFY(v)

    WRITE(msg, *) "Process ", ga_nodeid(), &
         &": allocation error for block variable ", &
         &TRIM(name)
    CALL error_message(msg, fatal=.FALSE.)
    RETURN

  END FUNCTION block_var_allocate


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_deallocate
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_deallocate(v)

    IMPLICIT NONE

    TYPE (block_var), POINTER, INTENT(INOUT) :: v
    LOGICAL :: ok

    DEALLOCATE(v%current)
    IF (ASSOCIATED(v%star)) DEALLOCATE(v%star)
    IF (ASSOCIATED(v%old)) DEALLOCATE(v%old)
    IF (ASSOCIATED(v%oldold)) DEALLOCATE(v%oldold)

    ok = ga_destroy(v%ga_handle)

    DEALLOCATE(v)
    NULLIFY(v)

  END SUBROUTINE block_var_deallocate

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_get
  ! 
  ! This fills the local copies of the global array with whatever is
  ! in the global array.  Ghost cell values are included in the get.
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_get(var, index)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax

    INTEGER :: myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    lo = var%base%lo_used
    hi = var%base%hi_used
    ld = var%base%ld_used

    lo(3) = myindex
    hi(3) = myindex

    imin = var%base%imin_used
    imax = var%base%imax_used
    jmin = var%base%jmin_used
    jmax = var%base%jmax_used

    SELECT CASE (myindex)
    CASE (BLK_VAR_CURRENT)
       CALL nga_get(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_STAR)
       IF (ASSOCIATED(var%star)) &
            &CALL nga_get(var%ga_handle, lo, hi, var%star(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_OLD)
       IF (ASSOCIATED(var%old))&
            &CALL nga_get(var%ga_handle, lo, hi, var%old(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_OLDOLD)
       IF (ASSOCIATED(var%oldold))&
            &CALL nga_get(var%ga_handle, lo, hi, var%oldold(imin:imax, jmin:jmax), ld)
    END SELECT

  END SUBROUTINE block_var_get


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_put
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_put(var, index)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax
    DOUBLE PRECISION, ALLOCATABLE :: tmp(:,:)

    INTEGER :: myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    lo = var%base%lo_owned
    hi = var%base%hi_owned
    ld = var%base%ld_owned

    lo(3) = myindex
    hi(3) = myindex

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned

    SELECT CASE (myindex)
    CASE (BLK_VAR_CURRENT)
       CALL nga_put(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_STAR)
       IF (ASSOCIATED(var%star)) &
            &CALL nga_put(var%ga_handle, lo, hi, var%star(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_OLD)
       IF (ASSOCIATED(var%old))&
            &CALL nga_put(var%ga_handle, lo, hi, var%old(imin:imax, jmin:jmax), ld)
    CASE (BLK_VAR_OLDOLD)
       IF (ASSOCIATED(var%oldold))&
            &CALL nga_put(var%ga_handle, lo, hi, var%oldold(imin:imax, jmin:jmax), ld)
    END SELECT

    ! ALLOCATE(tmp(ld(1), ld(2)))

    ! CALL nga_put(var%ga_handle, lo, hi, tmp, ld)

    ! SELECT CASE (myindex)
    ! CASE (BLK_VAR_CURRENT)
    !     var%current(imin:imax, jmin:jmax) = tmp(:,:)
    ! CASE (BLK_VAR_STAR)
    !    IF (ASSOCIATED(var%star)) var%star(imin:imax, jmin:jmax) = tmp(:,:)
    ! CASE (BLK_VAR_OLD)
    !    IF (ASSOCIATED(var%old)) var%old(imin:imax, jmin:jmax) = tmp(:,:)
    ! CASE (BLK_VAR_OLDOLD)
    !    IF (ASSOCIATED(var%oldold)) var%oldold(imin:imax, jmin:jmax) = tmp(:,:)
    ! END SELECT

    ! DEALLOCATE(tmp)

  END SUBROUTINE block_var_put

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_iterate
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_iterate(var)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var

    INTEGER :: imin, imax, jmin, jmax

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned
    
    IF (ASSOCIATED(var%star)) THEN
       var%star(imin:imax, jmin:jmax) = var%current(imin:imax, jmin:jmax)
    END IF


  END SUBROUTINE block_var_iterate
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_timestep
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_timestep(var)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var

    INTEGER :: imin, imax, jmin, jmax

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned
    

    IF (ASSOCIATED(var%oldold) .AND. ASSOCIATED(var%old)) THEN
       var%oldold(imin:imax, jmin:jmax) = var%old(imin:imax, jmin:jmax)
       var%old(imin:imax, jmin:jmax) = var%current(imin:imax, jmin:jmax)
    END IF

  END SUBROUTINE block_var_timestep

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_all
  !
  ! This fills buffer with the entire global array of the specified
  ! variable (and index).
  !
  ! It is assumed that buffer has the correct dimensions.  The caller
  ! should get the buffer using function block_buffer()
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_all(var, buffer, index)

    IMPLICIT NONE

    TYPE (block_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex = BLK_VAR_CURRENT

    INTEGER :: junk, lo(ndim), hi(ndim), ld(ndim)

    IF (PRESENT(index)) myindex = index

    CALL nga_inquire(var%ga_handle, junk, junk, hi)
    lo = 1
    ld = hi - lo + 1
    lo(3) = myindex
    hi(3) = myindex
    CALL nga_get(var%ga_handle, lo, hi, buffer, ld)

  END SUBROUTINE block_var_all


END MODULE block_variable
