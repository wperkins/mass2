!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 scalars module file
!
! VERSION and DATE: 0.23 4-24-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!
!
!***************************************************************
!
MODULE scalars

  USE utility
  USE misc_vars, ONLY: i_index_min, i_index_extra, j_index_min, j_index_extra

IMPLICIT NONE

INTEGER  :: max_species

                                ! a list of cell types

INTEGER, PUBLIC, PARAMETER :: &
     &SCALAR_NORMAL_TYPE = 1, &
     &SCALAR_BOUNDARY_TYPE = 2
                                ! a list of scalar boundary conditions
                                ! types
INTEGER, PUBLIC, PARAMETER :: &
     &SCALBC_ZG = 1, &
     &SCALBC_CONC = 2, &
     &SCALBC_BLOCK = 3

TYPE scalar_cell_type_struct
   INTEGER :: type
   ! the rest applies only for BOUNDARY type cells
   INTEGER :: bctype 
END TYPE scalar_cell_type_struct

TYPE scalar_struct
	DOUBLE PRECISION, POINTER :: conc(:,:)				! c depth-ave concentration
	DOUBLE PRECISION, POINTER :: concold(:,:)			! c old depth-ave concentration
    TYPE (scalar_cell_type_struct), POINTER :: cell(:,:)
END TYPE scalar_struct

TYPE species_struct

	TYPE(scalar_struct), POINTER :: scalar(:) ! need one for each block up to max_blocks

END TYPE species_struct


TYPE(species_struct), ALLOCATABLE :: species(:)

CHARACTER (LEN=1024), PRIVATE :: msg


CONTAINS
!#####################################################################################################

SUBROUTINE allocate_scalarblock_components(i, block, xmax, ymax)
  ! this routine allocates each component in the array of blocks
  ! allows minimal memory use for each block
  IMPLICIT NONE
  INTEGER :: block, i, alloc_stat	
  INTEGER :: xmax, ymax, imin, imax, jmin, jmax

  imin = i_index_min
  imax = xmax + i_index_extra
  jmin = j_index_min
  jmax = ymax + j_index_extra

  WRITE(msg,*)'starting component allocation for scalars block number - ', block
  CALL status_message(msg)
  WRITE(msg,*)'         maximum number of i elements = ', imax
  CALL status_message(msg)
  WRITE(msg,*)'         maximum number of j elements = ', jmax
  CALL status_message(msg)

  ALLOCATE(species(i)%scalar(block)%conc(imin:imax,jmin:jmax), STAT = alloc_stat)		! c depth-ave concentration
  IF(alloc_stat /= 0)THEN
     CALL error_message('allocation failed for the concentration', fatal=.TRUE.)
  ELSE
     CALL status_message('allocation successful for concentration')
  ENDIF
  species(i)%scalar(block)%conc = 0.0
  
  ALLOCATE(species(i)%scalar(block)%concold(imin:imax,jmin:jmax), STAT = alloc_stat)	! c old depth-ave concentration
  IF(alloc_stat /= 0)THEN
     CALL error_message('allocation failed for the old concentration', fatal=.TRUE.)
  ELSE
     CALL status_message('allocation successful for old concentration')
  ENDIF
  species(i)%scalar(block)%concold = 0.0

  ALLOCATE(species(i)%scalar(block)%cell(imin:imax,jmin:jmax), STAT = alloc_stat)	! c old depth-ave concentration
  IF(alloc_stat /= 0)THEN
     CALL error_message('allocation failed for cell type', fatal=.TRUE.)
  ELSE
     CALL status_message('allocation successful for cell type')
  ENDIF

  WRITE(msg,*)'completed component allocation for scalars block number - ', block
  CALL status_message(msg)

END SUBROUTINE allocate_scalarblock_components

!#####################################################################################################

SUBROUTINE allocate_species(error_iounit,status_iounit)

	IMPLICIT NONE
	INTEGER :: alloc_stat,error_iounit,status_iounit

	ALLOCATE(species(max_species), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of species - max_species=', max_species
		CALL EXIT(1)
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of species - max_species=', max_species
	ENDIF

END SUBROUTINE allocate_species

!####################################################################################################

SUBROUTINE allocate_scalar(max_blocks, n, error_iounit,status_iounit)

	IMPLICIT NONE
	INTEGER :: alloc_stat,error_iounit,status_iounit, n, max_blocks

	ALLOCATE(species(n)%scalar(max_blocks), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of scalars'
		CALL EXIT(1)
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of scalars'
	ENDIF

END SUBROUTINE allocate_scalar

END MODULE scalars
