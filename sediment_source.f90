! ----------------------------------------------------------------
! file: sediment_source.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August 23, 2000 by William A. Perkins
! Last Change: 2014-06-09 15:49:14 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE sediment_source
! ----------------------------------------------------------------
MODULE sediment_source

  USE utility
  USE config
  USE block_module
  USE scalars, ONLY: max_species

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE sediment_source_block_rec
                                ! these are rates, units: (mass)/ft^2/s

     TYPE (block_var), POINTER :: bv_deposition
     DOUBLE PRECISION, POINTER :: deposition(:,:)
     TYPE (block_var), POINTER :: bv_erosion
     DOUBLE PRECISION, POINTER :: erosion(:,:)
  END TYPE sediment_source_block_rec

  TYPE sediment_source_rec
     INTEGER :: ifract          ! sediment fraction index
     DOUBLE PRECISION :: erode, eshear
     DOUBLE PRECISION :: setvel, dshear
     DOUBLE PRECISION :: pdens, d50
     TYPE (sediment_source_block_rec), POINTER :: block(:)
  END TYPE sediment_source_rec

                                ! total number of sediment fractions
                                ! simulated (may be needed by bed more
                                ! than here)

  INTEGER, PUBLIC, SAVE :: sediment_fractions = 0

                                ! map sediment fraction index to
                                ! scalar index

  INTEGER, PUBLIC, ALLOCATABLE :: sediment_scalar_index(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE sediment_source_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE sediment_source_initialize()

    IMPLICIT NONE

    INTEGER :: iblk

    ALLOCATE(sediment_scalar_index(sediment_fractions))
  END SUBROUTINE sediment_source_initialize


  ! ----------------------------------------------------------------
  ! TYPE (SEDIMENT_SOURCE_REC) FUNCTION sediment_parse_options
  ! ----------------------------------------------------------------
  TYPE (SEDIMENT_SOURCE_REC) FUNCTION sediment_parse_options(options)

    IMPLICIT NONE
    POINTER sediment_parse_options
    CHARACTER (LEN=*) :: options(:)
    CHARACTER (LEN=1024) :: msg
    INTEGER :: nopt
    INTEGER :: i, iblk

    i = 1
    nopt = UBOUND(options, 1)

    ALLOCATE(sediment_parse_options) 
    sediment_parse_options%erode = 0.0
    sediment_parse_options%eshear = -1.0
    sediment_parse_options%setvel = 0.0
    sediment_parse_options%dshear = 0.0
    sediment_parse_options%pdens = 0.0
    sediment_parse_options%d50 = 0.0

                                ! allocate storage for erosion and
                                ! deposition rates

    ALLOCATE(sediment_parse_options%block(max_blocks))
    DO iblk = 1, max_blocks
       sediment_parse_options%block(iblk)%bv_deposition => &
            &block_var_allocate("deposition", block(iblk)%varbase, .TRUE.)
       sediment_parse_options%block(iblk)%deposition => &
            &sediment_parse_options%block(iblk)%bv_deposition%current

       sediment_parse_options%block(iblk)%bv_erosion => &
            &block_var_allocate("erosion", block(iblk)%varbase, .TRUE.)
       sediment_parse_options%block(iblk)%erosion => &
            &sediment_parse_options%block(iblk)%bv_erosion%current

       sediment_parse_options%block(iblk)%deposition = 0
       CALL block_var_put(sediment_parse_options%block(iblk)%bv_deposition)
       sediment_parse_options%block(iblk)%erosion = 0
       CALL block_var_put(sediment_parse_options%block(iblk)%bv_erosion)
    END DO
    
    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))

                                ! median partical diameter, feet

       CASE ('D50')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'D50'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%d50
          i = i + 1

                                ! erodibility coefficient, (mass)/ft^2/s

       CASE ('ERODIBILTY')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'ERODIBILTY'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%erode
          i = i + 1

                                ! critical shear stress for erosion, lbf/ft^2

       CASE ('ESHEAR')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'ESHEAR'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%eshear
          i = i + 1

                                ! critical shear stress for deposition, lbf/ft^2

       CASE ('DSHEAR')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'DSHEAR'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%dshear
          i = i + 1

                                ! settling velocity for deposition, ft/s

       CASE ('SETVEL')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'SETVEL'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%setvel
          i = i + 1

                                ! solids density, mass/ft^3

       CASE ('DENSITY')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(msg, 100) 'SETVEL'
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          READ(options(i+1), *) sediment_parse_options%pdens
          i = i + 1

       CASE DEFAULT
          WRITE(msg, *) 'GEN scalar option "', &
               &TRIM(options(i)), '" not understood and ignored'
          CALL error_message(msg, fatal=.TRUE.)
       END SELECT
       i = i + 1
    END DO

                                ! in order for a sediment species to
                                ! be meaningful, all of the parameters
                                ! should be non zero

    IF (sediment_parse_options%setvel .LE. 0.0) THEN
       WRITE (msg, *) 'Invalid or unspecified value for SETVEL'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    IF (sediment_parse_options%eshear .LE. 0.0) THEN
       WRITE (msg, *) 'Invalid or unspecified value for ESHEAR'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    IF (sediment_parse_options%dshear .LE. 0.0) THEN
       WRITE (msg, *) 'Invalid or unspecified value for DSHEAR'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    IF (sediment_parse_options%erode .LT. 0.0) THEN
       WRITE (msg, *) 'Invalid or unspecified value for ERODIBILTY'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    IF (sediment_parse_options%pdens .LE. 0.0) THEN
       WRITE (msg, *) 'Invalid or unspecified value for DENSITY'
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    IF (sediment_parse_options%d50 .LE. 0.0) THEN 
       WRITE (msg,*) 'Bad D50 value for SED species: ', sediment_parse_options%d50
       CALL error_message(msg, fatal=.TRUE.)
    END IF
100 FORMAT('additional argument missing for ', A10, ' keyword')
  END FUNCTION sediment_parse_options

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION sediment_erosion
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION sediment_erosion(rec, iblk, i, j)

    IMPLICIT NONE
    INCLUDE 'bed_functions.inc'
    TYPE (sediment_source_rec) :: rec
    INTEGER, INTENT(IN) :: iblk, i, j
    DOUBLE PRECISION :: shear
    
    sediment_erosion = 0.0
    shear = block(iblk)%shear(i,j)

    IF (shear > rec%eshear) THEN
       sediment_erosion = &
            &MIN((shear/rec%eshear - 1.0)*rec%erode,&
            &bed_max_erosion(rec%ifract, iblk, i, j, delta_t))
    END IF
  END FUNCTION sediment_erosion


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION sediment_deposition
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION sediment_deposition(rec, iblk, i, j, sconc)

    IMPLICIT NONE
    TYPE (sediment_source_rec) :: rec
    INTEGER, INTENT(IN) :: iblk, i, j
    DOUBLE PRECISION, INTENT(IN) :: sconc
    DOUBLE PRECISION :: shear
    
    sediment_deposition = 0.0
    shear = block(iblk)%shear(i,j)
  
    IF (shear < rec%dshear) THEN
       sediment_deposition = (1.0 - shear/rec%dshear)*rec%setvel*sconc
       sediment_deposition = MAX(sediment_deposition, 0.0d0)
    END IF
  END FUNCTION sediment_deposition

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION sediment_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION sediment_source_term(rec, iblk, i, j, sconc)

    IMPLICIT NONE

    TYPE(sediment_source_rec), INTENT(IN) :: rec
    INTEGER, INTENT(IN) :: iblk, i, j
    DOUBLE PRECISION :: sconc

    sediment_source_term = &
         &rec%block(iblk)%erosion(i, j) - &
         &rec%block(iblk)%deposition(i, j)
  END FUNCTION sediment_source_term


END MODULE sediment_source

