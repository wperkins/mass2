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

IMPLICIT NONE

INTEGER  :: max_species


TYPE scalar_struct
	DOUBLE PRECISION, POINTER :: conc(:,:)				! c depth-ave concentration
	DOUBLE PRECISION, POINTER :: concold(:,:)			! c old depth-ave concentration
END TYPE scalar_struct

TYPE species_struct

	TYPE(scalar_struct), POINTER :: scalar(:) ! need one for each block up to max_blocks

END TYPE species_struct


TYPE(species_struct), ALLOCATABLE :: species(:)



CONTAINS
!#####################################################################################################

SUBROUTINE allocate_scalarblock_components(i, block , imax, jmax, status_iounit, error_iounit)
! this routine allocates each component in the array of blocks
! allows minimal memory use for each block
IMPLICIT NONE
INTEGER :: block, i, imax, jmax, status_iounit, error_iounit, alloc_stat	! block number, max i elements, max j elements

WRITE(status_iounit,*)'starting component allocation for scalars block number - ', block
WRITE(status_iounit,*)'         maximum number of i elements = ', imax
WRITE(status_iounit,*)'         maximum number of j elements = ', jmax

ALLOCATE(species(i)%scalar(block)%conc(imax,jmax), STAT = alloc_stat)		! c depth-ave concentration
IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the concentration'
		CALL EXIT(1)
	ELSE
		WRITE(status_iounit,*)'allocation successful for concentration'
ENDIF

ALLOCATE(species(i)%scalar(block)%concold(imax,jmax), STAT = alloc_stat)	! c old depth-ave concentration
IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the old concentration'
		CALL EXIT(1)
	ELSE
		WRITE(status_iounit,*)'allocation successful for old concentration'
ENDIF

WRITE(status_iounit,*)'completed component allocation for scalars block number - ', block

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
