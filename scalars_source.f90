!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 scalars source module file
!
! VERSION and DATE: 0.23 4-24-98
!
! PURPOSE: Enscapsulates the scalar transport source term for various
! kinds of transportable species
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: Created July 21, 2000 by William A. Perkins
! Last Change: Thu Feb 17 12:03:03 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
!
!***************************************************************
! $Id$

! ----------------------------------------------------------------
! MODULE scalars_source
! ----------------------------------------------------------------
MODULE scalars_source

  USE utility
  USE globals, ONLY: nghost
  USE config, ONLY: max_blocks,  max_species
  USE generic_source
  USE temperature_source
  USE tdg_source
  USE sediment_source
  USE particulate_source

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=80), PARAMETER, PRIVATE :: source_filename = "scalar_source.dat"
  INTEGER, PARAMETER, PRIVATE :: source_iounit = 21
  INTEGER, PARAMETER :: source_max_option = 20

                                ! these are the types of scalar
                                ! species to transport

  INTEGER, PARAMETER :: TDG = 1, TEMP = 2, GEN = 3, BIO = 4, BIOTA = 5, SED = 6, PART = 7
  CHARACTER (LEN=10), PARAMETER, PRIVATE :: source_type(7) = &
       &(/ 'TDG  ', 'TEMP ', 'GEN  ', 'BIO  ', 'BIOTA' , 'SED  ', 'PART '/)

  TYPE scalar_source_rec
     INTEGER :: srctype, id
     CHARACTER (LEN=15) :: name
     CHARACTER (LEN=80) :: description
     CHARACTER (LEN=25) :: units

                                ! conversion from supplied volume
                                ! units to cubic feet:
                                ! e.g. kg/l*(28.317 l/ft^3) = kg/ft^3

     DOUBLE PRECISION :: conversion
     DOUBLE PRECISION :: relax

     INTEGER :: scheme
     DOUBLE PRECISION :: cds_blend

     TYPE(generic_source_rec), POINTER :: generic_param
     TYPE(temperature_source_rec), POINTER :: temp_param
     TYPE(tdg_source_rec), POINTER :: tdg_param
     TYPE(sediment_source_rec), POINTER :: sediment_param
     TYPE(part_source_rec), POINTER :: part_param
  END TYPE scalar_source_rec

  TYPE(scalar_source_rec), ALLOCATABLE :: scalar_source(:)
  LOGICAL, PUBLIC, SAVE :: source_need_met = .FALSE.
  LOGICAL, PUBLIC, SAVE :: source_doing_temp = .FALSE.
  INTEGER, PUBLIC, SAVE :: source_temp_idx = 0
  LOGICAL, PUBLIC, SAVE :: source_doing_tdg = .FALSE.
  LOGICAL, PUBLIC, SAVE :: source_doing_sed = .FALSE.
  LOGICAL, PUBLIC, SAVE :: source_doing_part = .FALSE.


CONTAINS

  ! ----------------------------------------------------------------
  ! FUNCTION scalar_source_type
  ! ----------------------------------------------------------------
  INTEGER FUNCTION scalar_source_type(s)
    
    IMPLICIT NONE

    INTEGER :: n
    CHARACTER (LEN=*) :: s

    n = UBOUND(source_type, 1)

    DO scalar_source_type = 1, n
       IF (s .EQ. source_type(scalar_source_type)) EXIT
    END DO
    IF (scalar_source_type .GT. n) scalar_source_type = 0
    RETURN
  END FUNCTION scalar_source_type

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_source_read
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_source_read()

    USE differencing

    IMPLICIT NONE

    INTEGER :: i, istat, id, disidx, iblk, iopt, jopt

    CHARACTER (LEN=20) :: type_name, short_name, units, schemebuf
    CHARACTER (LEN=256) :: long_name, options(source_max_option), &
         &alloptions(source_max_option), buffer
    
    CALL open_existing(source_filename, source_iounit)
    CALL status_message("Reading scalar source information from " // &
         &TRIM(source_filename))

    ALLOCATE(scalar_source(max_species))
       
    DO I = 1, max_species
       options = ''
       alloptions = ''
       READ(source_iounit, *, END=1000) &
            &id, type_name, short_name, long_name, units, alloptions
       IF (id .GT. max_species) THEN
          WRITE(buffer, *) 'scalar species ', TRIM(short_name), &
               &" has ID greater than max (", max_species, ")"
          CALL error_message(buffer, fatal=.TRUE.)
       END IF
       scalar_source(id)%id = id
       scalar_source(id)%srctype = scalar_source_type(type_name)
       scalar_source(id)%name = short_name
       scalar_source(id)%description = long_name
       scalar_source(id)%units = units
       scalar_source(id)%conversion = 1.0
       scalar_source(id)%scheme = diff_upwind
       scalar_source(id)%cds_blend = 0.0
       scalar_source(id)%relax = 1.0
       NULLIFY(scalar_source(id)%generic_param)
       NULLIFY(scalar_source(id)%temp_param)
       NULLIFY(scalar_source(id)%tdg_param)
       NULLIFY(scalar_source(id)%sediment_param)
       NULLIFY(scalar_source(id)%part_param)


                                ! look for generic options

       iopt = 1
       jopt = 1
       DO WHILE ((LEN_TRIM(alloptions(iopt)) .GT. 0) .AND. (iopt .LE. source_max_option))
          SELECT CASE (alloptions(iopt))
          CASE ('SCHEME') 
             IF ((iopt + 1 .GT. source_max_option) .OR. (LEN_TRIM(alloptions(iopt+1)) .LE. 0)) THEN
                WRITE(buffer, *) 'additional argument missing for SCHEME keyword'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             READ(alloptions(iopt+1),*) schemebuf
             scalar_source(id)%scheme = differ_method_from_string(schemebuf)
             IF (scalar_source(id)%scheme .LE. 0) THEN
                WRITE(buffer, *) 'Unknown differencing scheme: (', TRIM(schemebuf), ')'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF

             IF (nghost .LT. 2) THEN
                SELECT CASE (diff_uv)
                CASE (DIFF_SOU, DIFF_MSOU, DIFF_MUSCL)
                   WRITE (buffer, *) 'differencing method "', TRIM(schemebuf), &
                        &'" unavailable - too few ghost cells'
                   CALL error_message(buffer, fatal=.TRUE.)
                END SELECT
             END IF
             
             iopt = iopt + 1
          CASE ('BLEND')
             IF ((iopt + 1 .GT. source_max_option) .OR. (LEN_TRIM(alloptions(iopt+1)) .LE. 0)) THEN
                WRITE(buffer, *) 'additional argument missing for BLEND keyword'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             READ(alloptions(iopt+1),*) scalar_source(id)%cds_blend
             IF (scalar_source(id)%cds_blend < 0.0 .OR. scalar_source(id)%cds_blend > 1.0) THEN
                WRITE(buffer, *) 'CDS Blend factor out of range'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             iopt = iopt + 1
          CASE ('RELAX')
             IF ((iopt + 1 .GT. source_max_option) .OR. (LEN_TRIM(alloptions(iopt+1)) .LE. 0)) THEN
                WRITE(buffer, *) 'additional argument missing for RELAX keyword'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             READ(alloptions(iopt+1),*) scalar_source(id)%relax
             IF (scalar_source(id)%relax <= 0.0 .OR. scalar_source(id)%relax > 1.0) THEN
                WRITE(buffer, *) 'under relaxation out of range'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             iopt = iopt + 1
          CASE ('CONVERT')
             IF ((iopt + 1 .GT. source_max_option) .OR. (LEN_TRIM(alloptions(iopt+1)) .LE. 0)) THEN
                WRITE(buffer, *) 'additional argument missing for CONVERT keyword'
                CALL error_message(buffer, fatal=.TRUE.)
             END IF
             READ(alloptions(iopt+1),*) scalar_source(id)%conversion
             iopt = iopt + 1
          CASE DEFAULT
             options(jopt) = alloptions(iopt)
             jopt = jopt + 1
          END SELECT
          iopt = iopt + 1
       END DO

       SELECT CASE (scalar_source(id)%scheme) 
       CASE (diff_upwind)
          scalar_source(id)%cds_blend = 0.0
       END SELECT
       
                                ! deal with scalar type specific options

       SELECT CASE (scalar_source(id)%srctype)
       CASE (TEMP)
          IF (source_doing_temp) THEN
             WRITE(buffer,*) 'only one TEMP scalar allowed'
             CALL error_message(buffer, fatal=.TRUE.)
          END IF
          scalar_source(id)%temp_param => &
               &temperature_parse_options(options)
          source_doing_temp = .TRUE.
          source_temp_idx = id
          source_need_met = (scalar_source(id)%temp_param%doexchange .OR.&
               &source_need_met)
       CASE (TDG)
          scalar_source(id)%tdg_param => &
               &tdg_parse_options(options)
          source_need_met = (scalar_source(id)%tdg_param%doexchange .OR.&
               &source_need_met)
       CASE (GEN)
          scalar_source(id)%generic_param => &
               &generic_parse_options(options)
       CASE (SED)
          scalar_source(id)%sediment_param => &
               &sediment_parse_options(options)
          scalar_source(id)%sediment_param%pdens = scalar_source(id)%sediment_param%pdens*&
               &scalar_source(id)%conversion
          sediment_fractions = sediment_fractions + 1
          scalar_source(id)%sediment_param%ifract = sediment_fractions
          source_doing_sed = .TRUE.
       CASE (PART)
          scalar_source(id)%part_param => &
               &part_parse_options(options)

                                ! Kd is expected to be in
                                ! (volume)/(mass) units (e.g. m^3/kg)
                                ! -- the same mass and volume units
                                ! used for BC and output
                                ! concentrations -- so we need to
                                ! convert this

          scalar_source(id)%part_param%kd = &
               &scalar_source(id)%part_param%kd/scalar_source(id)%conversion
          scalar_source(id)%part_param%bedkd = &
               &scalar_source(id)%part_param%bedkd/scalar_source(id)%conversion
          particulates = particulates + 1
          source_doing_part = .TRUE.
       CASE DEFAULT
          WRITE(buffer, 300) TRIM(type_name), TRIM(short_name)
          CALL error_message(buffer, fatal=.TRUE.)
       END SELECT

       WRITE(buffer, *) 'Species ', id, ' source specification read'
       CALL status_message(buffer)
       WRITE(buffer, *) 'Species ', id, ' is type ', TRIM(type_name), ' (', &
            &TRIM(scalar_source(id)%description), ')'
       CALL status_message(buffer)
    END DO
1000 CONTINUE

                                ! do some error checking

    IF (source_doing_tdg .AND. (.NOT. source_doing_temp)) THEN
       WRITE(buffer, 400) 
       CALL error_message(buffer, fatal=.TRUE.)
    END IF

                                ! set up to do sediment/particulate transport

    IF (source_doing_sed) THEN
       CALL sediment_source_initialize()
       
       DO i = 1, max_species
          SELECT CASE (scalar_source(i)%srctype) 
          CASE (SED)
             sediment_scalar_index(scalar_source(i)%sediment_param%ifract) = i
          CASE (PART)
             disidx = scalar_source(i)%part_param%disidx
             scalar_source(disidx)%generic_param%issorbed = .TRUE.
             scalar_source(i)%part_param%lamda = scalar_source(disidx)%generic_param%lamda
          END SELECT
       END DO
    END IF

    CALL status_message("Done reading scalar source information")

    RETURN

! 100 FORMAT('FATAL ERROR: unable to open file "', A, '" (', I2.1,')')
! 200 FORMAT('FATAL ERROR: scalar species "', A, '" has ID greater than max (', I6.1,')')
300 FORMAT('scalar type "', A, '" for "', A, '" not known')
400 FORMAT('TEMP species must be included for TDG to be simulated')
  END SUBROUTINE scalar_source_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_source_timestep
  ! Various things needed to be done once each TIME STEP for any
  ! particular scalar source term.  This is primarily limited to
  ! interactions with the bed.
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_source_timestep(time, delta_t)
    USE met_data_module
    IMPLICIT NONE
    DOUBLE PRECISION :: time, delta_t

    INTEGER :: i, didx, sidx

                                ! update meteorologic data, if used

    IF (source_need_met) CALL update_met_data(time)

                                ! necessary activities to prepare
                                ! individual scalar species

!!$    DO i = 1, max_species
!!$       SELECT CASE (scalar_source(i)%srctype)
!!$       CASE (GEN)
!!$          IF (scalar_source(i)%generic_param%hasbedsrc) THEN
!!$             CALL bedsrc_interp(time, delta_t, scalar_source(i)%generic_param%bedsrc)
!!$             IF (ASSOCIATED(bedflowsrc)) CALL bedsrc_interp(time, delta_t, bedflowsrc)
!!$          END IF
!!$       END SELECT
!!$    END DO

    RETURN
  END SUBROUTINE scalar_source_timestep


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION scalar_source_term
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION scalar_source_term(iblock, i, j, ispecies, &
       &conc, depth, area, t_water, sal)

    USE scalars

    IMPLICIT NONE
    
    INTEGER :: iblock, i, j
    INTEGER :: ispecies
    INTEGER :: dphase, bphase, sphase
    DOUBLE PRECISION :: conc, pconc, dconc, sconc, bconc
    DOUBLE PRECISION :: depth, area, t_water, sal
    INTEGER :: partspec

!!$    INCLUDE 'bed_functions.inc'

    scalar_source_term = 0.0

    SELECT CASE (scalar_source(ispecies)%srctype)
    CASE (TEMP)
       scalar_source_term = scalar_source_term + &
            &temperature_source_term(scalar_source(ispecies)%temp_param, conc)
    CASE (TDG)
       scalar_source_term = scalar_source_term + &
            &tdg_source_term(scalar_source(ispecies)%tdg_param, conc, t_water, sal)
    CASE (GEN)
       scalar_source_term = scalar_source_term + &
            &generic_source_term(scalar_source(ispecies)%generic_param, iblock, i, j, conc, depth, area)

                                ! if there is a bed, exchange
                                ! dissolved with the bed pore space

!!$       IF (source_doing_sed) THEN
!!$          scalar_source_term = scalar_source_term + &
!!$               &bed_pore_flux(iblock, ispecies, i, j)
!!$       END IF

                                ! add in exchange with all
                                ! cooresponding particulate species
                                ! and with bed

       IF (scalar_source(ispecies)%generic_param%issorbed) THEN
          DO partspec = 1, max_species
             SELECT CASE(scalar_source(partspec)%srctype)
             CASE (PART)
                IF (scalar_source(partspec)%part_param%disidx .EQ. ispecies) THEN
                   pconc = species(partspec)%scalar(iblock)%concold(i, j)
                   sphase = scalar_source(partspec)%part_param%sedidx
                   sconc = species(sphase)%scalar(iblock)%concold(i, j)
!!$                   bconc = bed_part_conc(partspec, &
!!$                        &scalar_source(sphase)%sediment_param%ifract, iblock, i, j)
                   scalar_source_term = scalar_source_term - &
                        &part_dissolve_exch(scalar_source(partspec)%part_param, &
                        &   conc, pconc, sconc, block(iblock)%depth(i,j))
!!$ - &
!!$                        &part_dissolve_bed_exch(scalar_source(partspec)%part_param, &
!!$                        &   scalar_source(sphase)%sediment_param, iblock, i, j, conc, bconc)
                END IF
             END SELECT
          END DO
       END IF

    CASE (SED)
       scalar_source_term = scalar_source_term +&
            &sediment_source_term(scalar_source(ispecies)%sediment_param, iblock, i, j, conc)

    CASE (PART)
       scalar_source_term = scalar_source_term +&
            &part_source_term(scalar_source(ispecies)%part_param, iblock, i, j, conc, depth)

                                ! include exchange with dissolved and with bed

       sphase = scalar_source(ispecies)%part_param%sedidx
       sconc = species(sphase)%scalar(iblock)%concold(i, j)
       dphase = scalar_source(ispecies)%part_param%disidx
       dconc = species(dphase)%scalar(iblock)%concold(i, j)
!!$       bconc = bed_part_conc(ispecies, scalar_source(sphase)%sediment_param%ifract, &
!!$            &iblock, i, j)
       scalar_source_term = scalar_source_term + &
            &part_dissolve_exch(scalar_source(ispecies)%part_param, &
            &   dconc, conc, sconc, block(iblock)%depth(i,j))
!!$ + &
!!$            &part_bed_exch(scalar_source(ispecies)%part_param, ispecies, &
!!$            &   scalar_source(sphase)%sediment_param, &
!!$            &   iblock, i, j, conc, sconc, bconc)
    CASE DEFAULT
       scalar_source_term = 0.0
    END SELECT
  END FUNCTION scalar_source_term

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_source_post_timestep
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_source_post_timestep()

    IMPLICIT NONE

    INTEGER :: i, iblock

    DO i = 1, max_species
       DO iblock = 1, max_blocks
          SELECT CASE (scalar_source(i)%srctype)
          CASE (SED)
             CALL block_var_put(scalar_source(i)%sediment_param%block(iblock)%bv_deposition)
             CALL block_var_put(scalar_source(i)%sediment_param%block(iblock)%bv_erosion)
          CASE DEFAULT
          END SELECT
       END DO
    END DO
  END SUBROUTINE scalar_source_post_timestep

END MODULE scalars_source


