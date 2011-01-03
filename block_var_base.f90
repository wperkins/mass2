! ----------------------------------------------------------------
! file: block_var_base.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 20, 2010 by William A. Perkins
! Last Change: Mon Jan  3 09:02:58 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE block_variable_base
! ----------------------------------------------------------------
MODULE block_variable_base

  USE globals

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! dimensions of the global array
  INTEGER, PUBLIC, PARAMETER :: ndim = 3
  INTEGER, PUBLIC, PARAMETER :: nslice = 4

  ! ----------------------------------------------------------------
  ! TYPE block_var_base
  ! ----------------------------------------------------------------
  TYPE block_var_base

     ! a (unused) global array that provides the basis for others
     INTEGER :: ga_handle

     ! local ranges of (global array) indexes for this varable
     INTEGER :: lo_owned(ndim), hi_owned(ndim), ld_owned(ndim-1)
     INTEGER :: lo_used(ndim), hi_used(ndim), ld_used(ndim-1)

     ! global ranges of (MASS2) indexes
     INTEGER :: imin_global, imax_global, jmin_global, jmax_global

     ! local ranges of (MASS2) indexes for this variable
     INTEGER :: imin_owned, imax_owned, jmin_owned, jmax_owned
     INTEGER :: imin_used, imax_used, jmin_used, jmax_used

  END type block_var_base


CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE (block_var_base) FUNCTION block_var_base_allocate
  ! ----------------------------------------------------------------
  TYPE (block_var_base) FUNCTION block_var_base_allocate(xmax, ymax) &
       &RESULT (base)
    
    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    INTEGER, INTENT(IN) :: xmax, ymax
    INTEGER :: dims(ndim), chunk(ndim), lo(ndim), hi(ndim)
    INTEGER :: me, imin, imax, jmin, jmax
    LOGICAL :: ga_ok

    POINTER base

    ALLOCATE (base)

    me = ga_nodeid()

    ! set the global index limits

    base%imin_global = i_index_min
    base%imax_global = xmax + i_index_extra
    base%jmin_global = j_index_min
    base%jmax_global = ymax + j_index_extra

    ! create the model global array

    dims(1) = base%imax_global - base%imin_global + 1
    dims(2) = base%jmax_global - base%jmin_global + 1
    dims(3) = nslice

    base%ga_handle = ga_create_handle()
    call ga_set_data(base%ga_handle, ndim, dims, MT_DBL)
    call ga_set_array_name(base%ga_handle, "base")

    chunk(1) = 1
    chunk(2) = 1
    chunk(3) = nslice

    call ga_set_chunk(base%ga_handle, chunk)
    
    ga_ok = ga_allocate(base%ga_handle)

    ! get this processor's share

    CALL nga_distribution(base%ga_handle, me, base%lo_owned, base%hi_owned)

    base%ld_owned(1) = base%hi_owned(1) - base%lo_owned(1) + 1
    base%ld_owned(2) = base%hi_owned(2) - base%lo_owned(1) + 1

    CALL ga_zero(base%ga_handle)

    base%hi_used(1) = MIN(base%hi_owned(1) + nghost, dims(1))
    base%hi_used(2) = MIN(base%hi_owned(2) + nghost, dims(2))
    base%hi_used(3) = nslice
    base%lo_used(1) = MAX(base%lo_owned(1) - nghost, 1)
    base%lo_used(2) = MAX(base%lo_owned(2) - nghost, 1)
    base%lo_used(3) = 1
    base%ld_used(1) = base%hi_used(1) - base%lo_used(1) + 1
    base%ld_used(2) = base%hi_used(2) - base%lo_used(2) + 1

    base%imin_owned = base%lo_owned(1) - 1 + i_index_min
    base%imax_owned = base%hi_owned(1) - 1 + i_index_min
    base%jmin_owned = base%lo_owned(2) - 1 + j_index_min
    base%jmax_owned = base%hi_owned(2) - 1 + j_index_min
    
    base%imin_used = base%lo_used(1) - 1 + i_index_min
    base%imax_used = base%hi_used(1) - 1 + i_index_min
    base%jmin_used = base%lo_used(2) - 1 + j_index_min
    base%jmax_used = base%hi_used(2) - 1 + j_index_min

    RETURN

  END FUNCTION block_var_base_allocate

  ! ----------------------------------------------------------------
  ! SUBROUTINE block_var_base_deallocate
  ! ----------------------------------------------------------------
  SUBROUTINE block_var_base_deallocate(base)

    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    TYPE (block_var_base), POINTER, INTENT(INOUT) :: base

    LOGICAL :: ok

    ok = ga_destroy(base%ga_handle)
    DEALLOCATE(base)
    NULLIFY(base)
    RETURN

  END SUBROUTINE block_var_base_deallocate

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_owns_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_owns_i(base, i)

    IMPLICIT NONE

    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: i

    block_var_base_owns_i = (base%imin_owned .LE. i .AND. i .LE. base%imax_owned)
  END FUNCTION block_var_base_owns_i

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_owns_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_owns_j(base, j)

    IMPLICIT NONE

    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: j

    block_var_base_owns_j = (base%jmin_owned .LE. j .AND. j .LE. base%jmax_owned)
  END FUNCTION block_var_base_owns_j

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_owns
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_owns(base, i, j)

    IMPLICIT NONE

    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: i, j

    block_var_base_owns = (&
         &block_var_base_owns_i(base, i) .AND. &
         &block_var_base_owns_j(base, j))
  END FUNCTION block_var_base_owns

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_uses_i
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_uses_i(base, i)

    IMPLICIT NONE

    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: i

    block_var_base_uses_i = (base%imin_used .LE. i .AND. i .LE. base%imax_used)
  END FUNCTION block_var_base_uses_i

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_uses_j
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_uses_j(base, j)

    IMPLICIT NONE

    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: j

    block_var_base_uses_j = (base%jmin_used .LE. j .AND. j .LE. base%jmax_used)
  END FUNCTION block_var_base_uses_j

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION block_var_base_uses
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION block_var_base_uses(base, i, j)

    IMPLICIT NONE
    TYPE (block_var_base), INTENT(IN) :: base
    INTEGER, INTENT(IN) :: i, j

    block_var_base_uses = (&
         &block_var_base_uses_i(base, i) .AND. &
         &block_var_base_uses_j(base, j))

  END FUNCTION block_var_base_uses


END MODULE block_variable_base
  
