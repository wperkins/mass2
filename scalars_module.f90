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
  USE globals
  USE config, ONLY: max_blocks, max_species
  USE block_variable

  IMPLICIT NONE

  ! a list of cell types

  INTEGER, PUBLIC, PARAMETER :: &
       &SCALAR_NORMAL_TYPE = 1, &
       &SCALAR_BOUNDARY_TYPE = 2

  ! a list of scalar boundary conditions
  ! types
  INTEGER, PUBLIC, PARAMETER :: &
       &SCALBC_NONE = 0, &
       &SCALBC_ZG = 1, &
       &SCALBC_CONC = 2, &
       &SCALBC_BLOCK = 3

  TYPE scalar_cell_type_struct
     INTEGER :: xtype, ytype
     ! the rest applies only for BOUNDARY type cells
     INTEGER :: xbctype, ybctype
  END TYPE scalar_cell_type_struct

  TYPE scalar_struct
     TYPE (block_var), POINTER :: concvar
     DOUBLE PRECISION, POINTER :: conc(:,:)
     DOUBLE PRECISION, POINTER :: concold(:,:)
     DOUBLE PRECISION, POINTER :: concoldold(:,:)
     DOUBLE PRECISION, POINTER :: srcterm(:,:)

     TYPE (scalar_cell_type_struct), POINTER :: cell(:,:)

     ! keep track of the mass of the
     ! species in the water column and in
     ! the bed
     DOUBLE PRECISION :: mass, massold
     DOUBLE PRECISION :: bedmass, bedmassold

     ! a place to record the accumulated
     ! flux of the species into the box
     ! (negative means outflow)
     DOUBLE PRECISION :: influx, outflux, netflux
  END TYPE scalar_struct

  TYPE species_struct
     TYPE(scalar_struct), POINTER :: scalar(:) ! need one for each block up to max_blocks
  END TYPE species_struct

  TYPE(species_struct), POINTER :: species(:)

  CHARACTER (LEN=1024), PRIVATE :: msg


CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE allocate_species
  ! ----------------------------------------------------------------
  SUBROUTINE allocate_species()

    IMPLICIT NONE
    INTEGER :: n, alloc_stat
    CHARACTER (LEN=1024) :: msg

    ALLOCATE(species(max_species), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       WRITE(msg,*)'allocation failed for array of species - max_species=', max_species
       CALL error_message(msg)
    ELSE
       WRITE(msg,*)'allocation successful for array of species - max_species=', max_species
       CALL status_message(msg)
    ENDIF

    DO n = 1, max_species
       ALLOCATE(species(n)%scalar(max_blocks), STAT = alloc_stat)
       IF(alloc_stat /= 0)THEN
          CALL error_message('allocation failed for the array of scalars', fatal=.TRUE.)
       ELSE
          CALL status_message('allocation successful for array of scalars')
       ENDIF
    END DO

  END SUBROUTINE allocate_species


  ! ----------------------------------------------------------------
  ! SUBROUTINE allocate_scalarblock_components
  !
  ! this routine allocates each component in the array of blocks
  ! allows minimal memory use for each block
  ! ----------------------------------------------------------------
  SUBROUTINE allocate_scalarblock_components(iblock, vbase)
    IMPLICIT NONE

#include "mafdecls.fh"
#include "global.fh"

    INTEGER, INTENT(IN) :: iblock
    TYPE (block_var_base), POINTER, INTENT(INOUT) :: vbase

    
    INTEGER :: ispecies, alloc_stat, ierr(1)
    INTEGER :: imin, imax, jmin, jmax

    WRITE(msg,*)'starting component allocation for scalars block number - ', iblock
    CALL status_message(msg)

    ierr(1) = 0

    DO ispecies = 1, max_species
       species(ispecies)%scalar(iblock)%concvar => &
            &block_var_allocate("conc", vbase, const=.FALSE.)
       species(ispecies)%scalar(iblock)%conc => &
            &species(ispecies)%scalar(iblock)%concvar%current
       species(ispecies)%scalar(iblock)%concold => &
            &species(ispecies)%scalar(iblock)%concvar%old
       species(ispecies)%scalar(iblock)%concoldold => &
            &species(ispecies)%scalar(iblock)%concvar%oldold
       
       imin = vbase%imin_owned
       imax = vbase%imax_owned
       jmin = vbase%jmin_owned
       jmax = vbase%jmax_owned

       ALLOCATE(species(ispecies)%scalar(iblock)%srcterm(imin:imax,jmin:jmax), STAT = alloc_stat)
       IF (alloc_stat .NE. 0) ierr(1) = ierr(1) + 1

       ALLOCATE(species(ispecies)%scalar(iblock)%cell(imin:imax,jmin:jmax), STAT = alloc_stat)
       IF (alloc_stat .NE. 0) ierr(1) = ierr(1) + 1

       CALL ga_sync()

       IF (ierr(1) .EQ. 0) THEN
          species(ispecies)%scalar(iblock)%conc = 0.0
          species(ispecies)%scalar(iblock)%concold = 0.0
          species(ispecies)%scalar(iblock)%concoldold = 0.0
          
          species(ispecies)%scalar(iblock)%cell(:,:)%xtype = SCALAR_NORMAL_TYPE
          species(ispecies)%scalar(iblock)%cell(:,:)%xbctype = SCALBC_NONE
          species(ispecies)%scalar(iblock)%cell(:,:)%ytype = SCALAR_NORMAL_TYPE
          species(ispecies)%scalar(iblock)%cell(:,:)%ybctype = SCALBC_NONE
          
          species(ispecies)%scalar(iblock)%influx = 0.0
          species(ispecies)%scalar(iblock)%outflux = 0.0
          species(ispecies)%scalar(iblock)%netflux = 0.0
          species(ispecies)%scalar(iblock)%mass = 0.0
          species(ispecies)%scalar(iblock)%bedmass = 0.0
          species(ispecies)%scalar(iblock)%massold = 0.0
          species(ispecies)%scalar(iblock)%bedmassold = 0.0
       END IF
          

    END DO

    IF (ierr(1) .GT. 0) THEN
       WRITE(msg,*)'scalars memory allocation error for block ', &
            &iblock, ' on process ', ga_nodeid()
       CALL error_message(msg)
    END IF

    CALL ga_igop(MT_F_INT, ierr, 1, '+');

    IF (ierr(1) .GT. 0) THEN
       CALL error_message("cannot continue", fatal=.TRUE.)
    END IF
    
    WRITE(msg,*)'completed component allocation for scalars block number - ', iblock
    CALL status_message(msg)

  END SUBROUTINE allocate_scalarblock_components

  ! ----------------------------------------------------------------
  ! SUBROUTINE deallocate_species
  ! ----------------------------------------------------------------
  SUBROUTINE deallocate_species()

    IMPLICIT NONE

    INTEGER :: ispecies, iblock

    DO ispecies = 1, max_species
       DO iblock = 1, max_blocks
          CALL block_var_deallocate(species(ispecies)%scalar(iblock)%concvar)
          DEALLOCATE(species(ispecies)%scalar(iblock)%srcterm)
          DEALLOCATE(species(ispecies)%scalar(iblock)%cell)
       END DO
       DEALLOCATE(species(ispecies)%scalar)
    END DO
    DEALLOCATE(species)

  END SUBROUTINE deallocate_species


END MODULE scalars
