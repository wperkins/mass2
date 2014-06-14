! ----------------------------------------------------------------
! file: bed_var.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 27, 2014 by William A. Perkins
! Last Change: 2014-06-13 13:37:28 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE bed_variable
! ----------------------------------------------------------------
MODULE bed_variable

  USE utility
  USE block_variable_base

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! ----------------------------------------------------------------
  ! TYPE bed_var
  ! ----------------------------------------------------------------
  TYPE bed_var
     TYPE (block_var_base), POINTER :: base

     ! handle to the Global Array for this variable
     INTEGER :: ga_handle

     ! the number of slices (first array index) in this var
     INTEGER :: slices

     ! The local part of this var
     DOUBLE PRECISION, POINTER :: current(:,:,:)
  END type bed_var

CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE(BED_VAR) FUNCTION bed_var_allocate
  ! ----------------------------------------------------------------
  TYPE(BED_VAR) FUNCTION bed_var_allocate(name, xmax, ymax, nslices) RESULT(v)

    IMPLICIT NONE

#include "global.fh"

    CHARACTER (LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: xmax, ymax, nslices

    POINTER v
    
    LOGICAL :: ga_ok
    INTEGER :: ierr
    INTEGER :: imin, imax, jmin, jmax
    CHARACTER (LEN=1024) :: msg
    
    ierr = 0

    ALLOCATE(v, STAT=ierr)
    IF (ierr .NE. 0) GOTO 100

    v%base => block_var_base_allocate(xmax, ymax, nslices);
    v%ga_handle = v%base%ga_handle;
    v%slices = nslices

    imin = v%base%imin_owned
    imax = v%base%imax_owned
    jmin = v%base%jmin_owned
    jmax = v%base%jmax_owned

    ALLOCATE(v%current(imin:imax, jmin:jmax, nslices), STAT=ierr)
    IF (ierr .NE. 0) GOTO 100
    v%current = 0.0

    RETURN

100 CONTINUE

    DEALLOCATE(v)
    NULLIFY(v)

    WRITE(msg, *) "Process ", ga_nodeid(), &
         &": allocation error for bed variable ", &
         &TRIM(name)
    CALL error_message(msg, fatal=.FALSE.)
    RETURN

  END FUNCTION bed_var_allocate

  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_var_deallocate
  ! ----------------------------------------------------------------
  SUBROUTINE bed_var_deallocate(v)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (bed_var), POINTER, INTENT(INOUT) :: v
    LOGICAL :: ok

    DEALLOCATE(v%current)

    ok = ga_destroy(v%ga_handle)

    DEALLOCATE(v)
    NULLIFY(v)

  END SUBROUTINE bed_var_deallocate

  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_var_get
  ! 
  ! This fills the local copies of the global array with whatever is
  ! in the global array.  
  ! ----------------------------------------------------------------
  SUBROUTINE bed_var_get(var, index)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (bed_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax, kmin, kmax

    lo = var%base%lo_owned
    hi = var%base%hi_owned
    ld = var%base%ld_owned

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned
    kmin = 1
    kmax = var%slices

    IF (PRESENT(index)) THEN
       lo(3) = index
       hi(3) = index
       kmin = index
       kmax = index
    END IF
       
    CALL nga_get(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax, kmin:kmax), ld)
  END SUBROUTINE bed_var_get


  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_var_put
  ! ----------------------------------------------------------------
  SUBROUTINE bed_var_put(var, index)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (bed_var), INTENT(INOUT) :: var
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: lo(ndim), hi(ndim), ld(ndim-1)
    INTEGER :: imin, imax, jmin, jmax, kmin, kmax

    lo = var%base%lo_owned
    hi = var%base%hi_owned
    ld = var%base%ld_owned

    imin = var%base%imin_owned
    imax = var%base%imax_owned
    jmin = var%base%jmin_owned
    jmax = var%base%jmax_owned
    kmin = 1
    kmax = var%slices

    IF (PRESENT(index)) THEN
       lo(3) = index
       hi(3) = index
       kmin = index
       kmax = index
    END IF

    CALL nga_put(var%ga_handle, lo, hi, var%current(imin:imax, jmin:jmax, kmin:kmax), ld)

  END SUBROUTINE bed_var_put

  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_var_get_all
  ! ----------------------------------------------------------------
  SUBROUTINE bed_var_get_all(var, buffer, index)

    IMPLICIT NONE

    TYPE (bed_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex
    
    INTEGER :: junk, lo(ndim), hi(ndim), ld(ndim)

    myindex = 1
    IF (PRESENT(index)) myindex = index

    CALL nga_inquire(var%ga_handle, junk, junk, hi)
    lo = 1
    ld = hi - lo + 1
    lo(3) = myindex
    hi(3) = myindex
    CALL nga_get(var%ga_handle, lo, hi, buffer, ld)

  END SUBROUTINE bed_var_get_all

  ! ----------------------------------------------------------------
  ! SUBROUTINE bed_var_put_all
  ! ----------------------------------------------------------------
  SUBROUTINE bed_var_put_all(var, buffer, index)

    IMPLICIT NONE

    TYPE (bed_var), INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(OUT) :: buffer(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: index

    INTEGER :: myindex
    
    INTEGER :: junk, lo(ndim), hi(ndim), ld(ndim)

    myindex = 1
    IF (PRESENT(index)) myindex = index

    CALL nga_inquire(var%ga_handle, junk, junk, hi)
    lo = 1
    ld = hi - lo + 1
    lo(3) = myindex
    hi(3) = myindex
    CALL nga_put(var%ga_handle, lo, hi, buffer, ld)

  END SUBROUTINE bed_var_put_all




END MODULE bed_variable
  
