! ----------------------------------------------------------------
! file: particulate_source.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August 28, 2000 by William A. Perkins
! Last Change: Tue Jul 24 13:43:08 2001 by William A. Perkins <perk@dora.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE particulate_source
! ----------------------------------------------------------------
MODULE particulate_source

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE part_source_block_rec

                                ! particulate exchange with the bed
                                ! via erosion and deposition; if
                                ! positive particulate is raised
                                ! (i.e. erosion) and material is
                                ! removed from the bed

     DOUBLE PRECISION, POINTER :: bedexch(:,:)

  END TYPE part_source_block_rec

  TYPE part_source_rec
     DOUBLE PRECISION :: lamda
     DOUBLE PRECISION :: kd, rate, bedkd
     INTEGER :: disidx          ! dissolved species index
     INTEGER :: sedidx          ! sediment species index
     TYPE (part_source_block_rec), POINTER :: block(:)
  END TYPE part_source_rec

  INTEGER, PUBLIC :: particulates = 0

CONTAINS

  
  ! ----------------------------------------------------------------
  ! TYPE(PART_SOURCE_REC) FUNCTION part_parse_options
  ! ----------------------------------------------------------------
  TYPE(PART_SOURCE_REC) FUNCTION part_parse_options(options)
    
    USE scalars, ONLY: max_species
    USE misc_vars, ONLY: error_iounit
    USE globals

    IMPLICIT NONE

    POINTER part_parse_options
    CHARACTER (LEN=*) :: options(:)
    INTEGER :: nopt
    INTEGER :: i, iblk

    i = 1
    nopt = UBOUND(options, 1)

    ALLOCATE(part_parse_options)
    part_parse_options%disidx = 0
    part_parse_options%sedidx = 0
    part_parse_options%kd = 0.0
    part_parse_options%bedkd = 0.0
    part_parse_options%rate = 0.0

    DO WHILE ((LEN_TRIM(options(i)) .GT. 0) .AND. (i .LE. nopt))
       SELECT CASE (options(i))
       CASE ('KD')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(*, 100) 'KD'
             WRITE(error_iounit, 100) 'KD'
             CALL EXIT(8)
          END IF
          READ(options(i+1), *) part_parse_options%kd
          i = i + 1
       CASE ('BEDKD')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(*, 100) 'BEDKD'
             WRITE(error_iounit, 100) 'BEDKD'
             CALL EXIT(8)
          END IF
          READ(options(i+1), *) part_parse_options%bedkd
          i = i + 1
       CASE ('RATE')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(*, 100) 'RATE'
             WRITE(error_iounit, 100) 'RATE'
             CALL EXIT(8)
          END IF
          READ(options(i+1), *) part_parse_options%rate
          i = i + 1
       CASE ('DISSOLVED')
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(*, 100) 'DISSOLVED'
             WRITE(error_iounit, 100) 'DISSOLVED'
             CALL EXIT(8)
          END IF
          READ(options(i+1), *) part_parse_options%disidx
          i = i + 1
       CASE ('SEDIMENT')   
          IF ((i + 1 .GT. nopt) .OR. (LEN_TRIM(options(i+1)) .LE. 0)) THEN
             WRITE(*, 100) 'SEDIMENT'
             WRITE(error_iounit, 100) 'SEDIMENT'
             CALL EXIT(8)
          END IF
          READ(options(i+1), *) part_parse_options%sedidx
          i = i + 1
      END SELECT
      i = i + 1
    END DO

                                ! A particulate species must have a
                                ! corresponding dissolved phase and a
                                ! sediment fraction to attach to

    IF ((part_parse_options%disidx .EQ. 0) .OR. &
         &(part_parse_options%sedidx .EQ. 0)) THEN
       WRITE (*,*) 'FATAL ERROR: DISSOLVED or SEDIMENT not specified for PART species'
       WRITE (error_iounit,*) 'FATAL ERROR: DISSOLVED or SEDIMENT not specified for PART species'
       CALL EXIT(10)
    END IF
    IF ((part_parse_options%disidx .LE. 0) .OR. (part_parse_options%disidx .GT. max_species)) THEN
       WRITE (*,*) 'FATAL ERROR: Invalid DISSOLVED index for PART species: ', part_parse_options%disidx
       WRITE (error_iounit,*) 'FATAL ERROR: Invalid DISSOLVED index for PART species: ', part_parse_options%disidx
       CALL EXIT(10)
    END IF
    IF ((part_parse_options%sedidx .LE. 0) .OR. (part_parse_options%sedidx .GT. max_species)) THEN
       WRITE (*,*) 'FATAL ERROR: Invalid SEDIMENT index for PART species: ', part_parse_options%disidx
       WRITE (error_iounit,*) 'FATAL ERROR: Invalid SEDIMENT index for PART species: ', part_parse_options%disidx
       CALL EXIT(10)
    END IF

                                ! range check the Kd and rate values

    IF (part_parse_options%kd .LE. 0.0) THEN 
       WRITE (*,*) 'FATAL ERROR: Bad KD value for PART species: ', part_parse_options%kd
       WRITE (error_iounit,*) 'FATAL ERROR: Bad KD value for PART species: ', part_parse_options%kd
       CALL EXIT(10)
    END IF
    IF (part_parse_options%rate .LT. 0.0) THEN 
       WRITE (*,*) 'FATAL ERROR: Bad RATE value for PART species: ', part_parse_options%rate
       WRITE (error_iounit,*) 'FATAL ERROR: Bad RATE value for PART species: ', part_parse_options%rate
       CALL EXIT(10)
    END IF
    IF (part_parse_options%bedkd .LE. 0.0) THEN 
       WRITE (*,*) 'FATAL ERROR: Bad BEDKD value for PART species: ', part_parse_options%bedkd
       WRITE (error_iounit,*) 'FATAL ERROR: Bad BEDKD value for PART species: ', part_parse_options%bedkd
       CALL EXIT(10)
    END IF

                                ! allocate space for the exchange values

    ALLOCATE(part_parse_options%block(max_blocks))
    DO iblk = 1, max_blocks
       ALLOCATE(part_parse_options%block(iblk)%bedexch(block(iblk)%xmax + 1, block(iblk)%ymax + 1))
       part_parse_options%block(iblk)%bedexch = 0.0
    END DO
100 FORMAT('FATAL ERROR: additional argument missing for ', A10, ' keyword')
  END FUNCTION part_parse_options
  
  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION part_bed_exch
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION part_bed_exch(partrec, ispecies, sedrec, iblk, i, j, &
       &pconc, sconc, bconc)

    USE sediment_source
    USE misc_vars, ONLY: delta_t

    IMPLICIT NONE
    TYPE(part_source_rec) :: partrec
    TYPE(sediment_source_rec) :: sedrec
    INTEGER, INTENT(IN) :: ispecies, iblk, i, j
    DOUBLE PRECISION, INTENT(IN) :: pconc, sconc, bconc
    DOUBLE PRECISION :: d, e, emax

    INCLUDE 'bed_functions.inc'

    part_bed_exch = 0.0

    d = sediment_deposition(sedrec, iblk, i, j, sconc)
    IF (d .GT. 0.0) THEN
       part_bed_exch = part_bed_exch - d*pconc/sconc
    END IF

    e = sediment_erosion(sedrec, iblk, i, j)
    IF (e .GT. 0.0) THEN
       emax = bed_max_part_erosion(ispecies, iblk, i, j, delta_t)
       part_bed_exch = part_bed_exch + e*bconc
    END IF
  END FUNCTION part_bed_exch


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION part_dissolve_exch
  ! positive means particulate concentrations are increasing and
  ! dissolved concentrations are decreasing
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION part_dissolve_exch(rec, dconc, pconc, sconc, depth)

    IMPLICIT NONE
    TYPE (part_source_rec) :: rec
    DOUBLE PRECISION, INTENT(IN) :: pconc, dconc, sconc, depth

    part_dissolve_exch = depth*rec%rate*(&
         &rec%kd*sconc*dconc - pconc)

  END FUNCTION part_dissolve_exch

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION part_dissolve_bed_exch
  ! 
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION part_dissolve_bed_exch(partrec, sedrec, iblk, i, j, dconc, bconc)

    USE scalars
    USE sediment_source
    USE misc_vars, ONLY: delta_t

    IMPLICIT NONE
    TYPE (part_source_rec) :: partrec
    TYPE(sediment_source_rec) :: sedrec
    INTEGER, INTENT(IN):: iblk, i, j
    DOUBLE PRECISION, INTENT(IN) :: dconc, bconc
    DOUBLE PRECISION :: tmp, maxexch

    INCLUDE 'bed_functions.inc'

     part_dissolve_bed_exch = 0.0

     tmp = dconc
     IF ((bed_depth(iblk, i, j) .GT. sedrec%d50) .AND. &
          &bed_sediment_mass(sedrec%ifract, iblk, i, j) .GT. 0.0) THEN
        IF (tmp .LT. 0.0) tmp = 0.0
        part_dissolve_bed_exch = &
             &(partrec%bedkd*tmp - bconc)*partrec%rate*&
             &sedrec%pdens*sedrec%d50*(1.0 - bed_porosity(iblk, i, j))

                                ! limit the rate of contaminant
                                ! leaving the bed to that contained in
                                ! the affected bed volume, this is
                                ! d50/depth*(available mass)

        IF (part_dissolve_bed_exch .LT. 0.0) THEN
           maxexch = bed_sediment_mass(sedrec%ifract, iblk, i, j)*bconc
           maxexch = maxexch*sedrec%d50/bed_depth(iblk, i, j)
           IF (part_dissolve_bed_exch < 0.0 .AND. &
                &ABS(part_dissolve_bed_exch)*delta_t > maxexch) THEN
              part_dissolve_bed_exch = -maxexch/delta_t
           END IF
        END IF
     END IF
  END FUNCTION part_dissolve_bed_exch

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION part_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION part_source_term(rec, iblk, i, j, conc, depth)

    IMPLICIT NONE
    TYPE (part_source_rec) :: rec
    INTEGER :: iblk, i, j
    DOUBLE PRECISION :: conc, depth

    part_source_term = 0.0

    IF (rec%lamda .GT. 0.0) THEN
       part_source_term = part_source_term - rec%lamda*depth*conc
    END IF

  END FUNCTION part_source_term

END MODULE particulate_source
