! ----------------------------------------------------------------
! file: block_var.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 17, 2010 by William A. Perkins
! Last Change: Sat Jan 15 19:30:09 2011 by William A. Perkins <d3g096@PE10588.local>
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
     DOUBLE PRECISION, POINTER :: current(:,:)
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

#include "mafdecls.fh"
#include "global.fh"

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

    ELSE 
       NULLIFY(v%star)
       NULLIFY(v%old)
       NULLIFY(v%oldold)
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

#include "mafdecls.fh"
#include "global.fh"

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

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax

    INTEGER :: myindex

    DOUBLE PRECISION, POINTER :: tmp(:,:)

    myindex = BLK_VAR_CURRENT

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

    NULLIFY(tmp)

    SELECT CASE (myindex)
    CASE (BLK_VAR_CURRENT)
       tmp => var%current
    CASE (BLK_VAR_STAR)
       IF (ASSOCIATED(var%star)) &
            &tmp => var%star
    CASE (BLK_VAR_OLD)
       IF (ASSOCIATED(var%old))&
            &tmp => var%old
    CASE (BLK_VAR_OLDOLD)
       IF (ASSOCIATED(var%oldold))&
            &tmp => var%oldold
    END SELECT
    
    CALL nga_get(var%ga_handle, lo, hi, tmp(imin:imax, jmin:jmax), ld)
  END SUBROUTINE block_var_get


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_put
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_put(var, index)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax
    DOUBLE PRECISION, POINTER :: tmp(:,:)

    INTEGER :: myindex

    myindex = BLK_VAR_CURRENT

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

    NULLIFY(tmp)
    SELECT CASE (myindex)
    CASE (BLK_VAR_CURRENT)
       tmp => var%current
    CASE (BLK_VAR_STAR)
       IF (ASSOCIATED(var%star)) &
            &tmp => var%star
    CASE (BLK_VAR_OLD)
       IF (ASSOCIATED(var%old))&
            &tmp => var%old
    CASE (BLK_VAR_OLDOLD)
       IF (ASSOCIATED(var%oldold))&
            &tmp => var%oldold
    END SELECT

    CALL nga_put(var%ga_handle, lo, hi, tmp(imin:imax, jmin:jmax), ld)

  END SUBROUTINE block_var_put

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_get_logical
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_get_logical(var, larray)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var
    LOGICAL, INTENT(INOUT) :: larray(&
         &var%base%imin_used:var%base%imax_used,&
         &var%base%jmin_used:var%base%jmax_used)

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax, i, j

    lo = var%base%lo_used
    hi = var%base%hi_used
    ld = var%base%ld_used

    lo(3) = 1
    hi(3) = 1

    imin = var%base%imin_used
    imax = var%base%imax_used
    jmin = var%base%jmin_used
    jmax = var%base%jmax_used

    CALL nga_get(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax), ld)

    DO i = imin, imax
       DO j = jmin, jmax
          larray(i, j) = (var%current(i,j) .GT. 0.5)
       END DO
    END DO

  END SUBROUTINE block_var_get_logical

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_put_logical
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_put_logical(var, larray)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var
    LOGICAL, INTENT(IN) :: larray(&
         &var%base%imin_used:var%base%imax_used,&
         &var%base%jmin_used:var%base%jmax_used)

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax, i, j

    lo = var%base%lo_owned
    hi = var%base%hi_owned
    ld = var%base%ld_owned

    lo(3) = 1
    hi(3) = 1

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned

    var%current = 0.0

    DO i = imin, imax
       DO j = jmin, jmax
          IF (larray(i, j)) var%current(i,j) = 1.0
       END DO
    END DO

    CALL nga_put(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax), ld)

  END SUBROUTINE block_var_put_logical


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_initialize(var, val)

    IMPLICIT NONE

    TYPE (block_var), INTENT(INOUT) :: var
    DOUBLE PRECISION, INTENT(IN) :: val

    var%current = val
    IF (ASSOCIATED(var%star))  CALL block_var_iterate(var)
    IF (ASSOCIATED(var%old)) THEN 
       CALL block_var_timestep(var)
       CALL block_var_timestep(var)
    END IF

  END SUBROUTINE block_var_initialize

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
    
    var%star(imin:imax, jmin:jmax) = var%current(imin:imax, jmin:jmax)
    CALL block_var_put(var, BLK_VAR_STAR)

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
    

    var%oldold(imin:imax, jmin:jmax) = var%old(imin:imax, jmin:jmax)
    var%old(imin:imax, jmin:jmax) = var%current(imin:imax, jmin:jmax)
    CALL block_var_put(var, BLK_VAR_OLD)
    CALL block_var_put(var, BLK_VAR_OLDOLD)

    ! remote old values are never needed on the local processor, so no
    ! need to get

  END SUBROUTINE block_var_timestep

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_get_all
  !
  ! This fills buffer with the entire global array of the specified
  ! variable (and index).
  !
  ! It is assumed that buffer has the correct dimensions.  The caller
  ! should get the buffer using function block_buffer()
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_get_all(var, buffer, index)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    INTEGER :: junk, lo(ndim), hi(ndim), ld(ndim)

    myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    CALL nga_inquire(var%ga_handle, junk, junk, hi)
    lo = 1
    ld = hi - lo + 1
    lo(3) = myindex
    hi(3) = myindex
    CALL nga_get(var%ga_handle, lo, hi, buffer, ld)

  END SUBROUTINE block_var_get_all


  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_put_all
  !
  ! This fills buffer with the entire global array of the specified
  ! variable (and index).
  !
  ! It is assumed that buffer has the correct dimensions.  The caller
  ! should fill the buffer using function block_buffer()
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_put_all(var, buffer, index)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex

    INTEGER :: junk, lo(ndim), hi(ndim), ld(ndim)

    myindex = BLK_VAR_CURRENT

    IF (PRESENT(index)) myindex = index

    CALL nga_inquire(var%ga_handle, junk, junk, hi)
    lo = 1
    ld = hi - lo + 1
    lo(3) = myindex
    hi(3) = myindex
    CALL nga_put(var%ga_handle, lo, hi, buffer, ld)

  END SUBROUTINE block_var_put_all

END MODULE block_variable
