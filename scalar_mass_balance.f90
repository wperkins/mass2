! ----------------------------------------------------------------
! file: scalar_mass_balance.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 17, 2003 by William A. Perkins
! Last Change: Tue May 20 08:48:39 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE scalar_mass_balance
! ----------------------------------------------------------------
MODULE scalar_mass

  USE utility
  USE globals
  USE scalars_source
  USE bed_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  CHARACTER (LEN=1024), PRIVATE, ALLOCATABLE :: filename(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_mass_init
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_mass_init()

    IMPLICIT NONE

    INTEGER :: iblock, ispecies, iphase, phases
    CHARACTER (LEN=15) :: name
    CHARACTER (LEN=1024) :: buffer
    INTEGER, PARAMETER :: iounit = 15

    ALLOCATE(filename(max_species))

                                ! open one file for each GEN and SED
                                ! scalar
    DO ispecies = 1, max_species
       name = scalar_source(ispecies)%name
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN, SED)
          WRITE(filename(ispecies), *) TRIM(name), '_balance.out'
          CALL open_new(filename(ispecies), iounit)
          WRITE(iounit, 100, ADVANCE='NO') '', '', ''
          WRITE(iounit, 20, ADVANCE='NO') scalar_source(ispecies)%description
       CASE DEFAULT
       END SELECT

       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN)
          phases = 1
          DO iphase = ispecies + 1, max_species
             SELECT CASE (scalar_source(iphase)%srctype)
             CASE (PART)
                IF (scalar_source(iphase)%part_param%disidx .EQ. ispecies) THEN
                   phases = phases + 1
                   WRITE(iounit, 20, ADVANCE='NO') scalar_source(iphase)%description
                END IF
             END SELECT
          END DO

          IF (phases .GT. 1) THEN
             WRITE(iounit, 20, ADVANCE='NO') 'Total for All Phases'
          END IF
          WRITE(iounit, *)
          
          WRITE(iounit, 101, ADVANCE='NO')
          WRITE(iounit, 111, ADVANCE='NO')
          IF (phases .GT. 1) THEN
             DO iphase = 1, phases
                WRITE(iounit, 111, ADVANCE='NO')
             END DO
          END IF
          WRITE(iounit, *)
       CASE (SED)
          WRITE(iounit, *)
          WRITE(iounit, 101, ADVANCE='NO')
          ! WRITE(iounit, 110, ADVANCE='NO') 'WC Mass', 'Bed Mass', 'Flux', 'Error'
          WRITE(iounit, 111, ADVANCE='NO') 
          WRITE(iounit, *)
       END SELECT
       CLOSE(iounit)
    END DO

! 10  FORMAT(1H#, 15X)
20  FORMAT(' ', A43)
100 FORMAT(1H#, A24, A10, ' ', A5)
101 FORMAT('#Date/Time                Scalar     Block')
! 110 FORMAT(4(' ', A10))
111 FORMAT(' ', 'WC Mass   ', ' ', 'Bed Mass  ', ' ', 'Flux      ', ' ', 'Error     ')
  END SUBROUTINE scalar_mass_init

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_mass_balance
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_mass_balance(deltat, domass)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: deltat ! sec
    LOGICAL, INTENT(IN), OPTIONAL :: domass

    LOGICAL :: mydomass
    DOUBLE PRECISION :: flux

    INTEGER :: iblock, ispecies
    
                                ! we do not need to compute the block
                                ! mass every time; only do it when
                                ! called for (right before print)
    mydomass = .FALSE.
    IF (PRESENT(domass)) mydomass = domass

                                ! accumulate the flux of stuff into the
                                ! block
    DO iblock = 1, max_blocks
       DO ispecies = 1, max_species

                                ! if called for, compute current mass
                                ! within the block and bed
          IF (mydomass) THEN
             species(ispecies)%scalar(iblock)%massold = species(ispecies)%scalar(iblock)%mass
             species(ispecies)%scalar(iblock)%mass = &
                  &scalar_wc_mass(block(iblock), species(ispecies)%scalar(iblock))
             IF (source_doing_sed) THEN
                species(ispecies)%scalar(iblock)%bedmassold = species(ispecies)%scalar(iblock)%bedmass
                species(ispecies)%scalar(iblock)%bedmass = &
                     &scalar_bed_mass(ispecies, block(iblock), bed(iblock), &
                     &scalar_source(ispecies)%srctype)
             ELSE
                species(ispecies)%scalar(iblock)%bedmassold = 0.0
                species(ispecies)%scalar(iblock)%bedmass = 0.0
             END IF
          END IF
          flux = scalar_mass_flux(block(iblock), species(ispecies)%scalar(iblock))

          SELECT CASE (scalar_source(ispecies)%srctype)
          CASE (GEN)
             IF (scalar_source(ispecies)%generic_param%hasbedsrc) THEN
                flux = flux + scalar_bed_flux(iblock, block(iblock), &
                     &scalar_source(ispecies)%generic_param%bedsrc)/deltat
             END IF
          END SELECT
          species(ispecies)%scalar(iblock)%netflux = &
               &species(ispecies)%scalar(iblock)%netflux + flux*deltat
       END DO
    END DO
  END SUBROUTINE scalar_mass_balance


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION scalar_mass_flux
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION scalar_mass_flux(blk, spec)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (scalar_struct), INTENT(IN) :: spec
    DOUBLE PRECISION :: cflux, dflux, area, conc, mflux, d, k, sign, u
    INTEGER :: i, j, ioff
    INTEGER :: x_beg, x_end
    INTEGER :: y_beg, y_end

                                ! scalar cell indices
    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    scalar_mass_flux = 0.0

    DO i = x_beg, x_end, x_end - x_beg
       DO j = y_beg, y_end
          IF (i .EQ. x_beg) THEN
             ioff = i-1
             d = blk%depth_w(i,j)
             k = blk%k_w(i,j)
             u = blk%uvel(ioff,j)
             sign = 1.0
          ELSE IF (i .EQ. x_end) THEN
             ioff = i+1
             d = blk%depth_e(i,j)
             k = blk%k_e(i,j)
             u = blk%uvel(i,j)
             sign = -1.0
          END IF
          area = blk%hu2(ioff,j)*d
          cflux = sign*u*area
          dflux = sign*k*area
          SELECT CASE (spec%cell(i,j)%type)
          CASE (SCALAR_BOUNDARY_TYPE)
             SELECT CASE (spec%cell(i,j)%bctype)
             CASE (SCALBC_ZG)
                conc = spec%conc(i,j)
                cflux = cflux*conc
                dflux = 0.0
             CASE (SCALBC_CONC)
                conc = spec%conc(ioff,j)
                cflux = cflux*conc
                dflux = dflux*(spec%conc(ioff,j) - spec%conc(i,j))/&
                     &(0.5*blk%hu1(ioff,j))
             END SELECT
          CASE DEFAULT
             conc = 0.5*(spec%conc(i,j) + spec%conc(ioff,j))
             cflux = cflux*conc
             dflux = dflux*(spec%conc(ioff,j) - spec%conc(i,j))/blk%hu1(ioff,j)
          END SELECT
          scalar_mass_flux = scalar_mass_flux + cflux + dflux
       END DO
    END DO
    
  END FUNCTION scalar_mass_flux

  ! ----------------------------------------------------------------
  !  DOUBLE PRECISION FUNCTION scalar_bed_flux
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION scalar_bed_flux(iblock, blk, bedsrc)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblock
    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (bedsrc_rec), INTENT(IN) :: bedsrc

    INTEGER :: i, j
    INTEGER :: x_beg, x_end
    INTEGER :: y_beg, y_end


    scalar_bed_flux = 0.0

                                ! scalar cell indices
    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    DO i = x_beg, x_end
       DO j = y_beg, y_end
          scalar_bed_flux = scalar_bed_flux + bedsrc%map(iblock)%srcmass(i,j)
       END DO
    END DO
    

  END FUNCTION scalar_bed_flux


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION scalar_wc_mass
  ! Computes the mass of the specified scalar in the water column of
  ! the specified block
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION scalar_wc_mass(blk, spec)

    IMPLICIT NONE

    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (scalar_struct), INTENT(IN) :: spec
    DOUBLE PRECISION :: vol, conc
    INTEGER :: i, j, x_beg, x_end, y_beg, y_end

    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    scalar_wc_mass = 0.0

    DO i = x_beg, x_end
       DO j = y_beg, y_end
          vol = blk%depth(i,j)*blk%hp1(i,j)*blk%hp2(i,j)
          conc = spec%conc(i,j)
          scalar_wc_mass = scalar_wc_mass + conc*vol
       END DO
    END DO

  END FUNCTION scalar_wc_mass

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION scalar_bed_mass
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION scalar_bed_mass(ispecies, blk, bed, srctype)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ispecies, srctype
    TYPE (block_struct), INTENT(IN) :: blk
    TYPE (bed_block_rec), INTENT(IN) :: bed
    
    DOUBLE PRECISION :: mass
    INTEGER :: i, j, x_beg, x_end, y_beg, y_end

    scalar_bed_mass = 0.0

    x_beg = 2
    x_end = blk%xmax
    y_beg = 2
    y_end = blk%ymax

    DO i = x_beg, x_end
       DO j = y_beg, y_end
          
                                ! based on the species type choose
                                ! where the values are stored.
          SELECT CASE (srctype)
          CASE (GEN)
             scalar_bed_mass = scalar_bed_mass + &
                  &bed%pore(ispecies, i, j)*blk%hp1(i,j)*blk%hp2(i,j)
          CASE (PART)
             scalar_bed_mass = scalar_bed_mass + &
                  &bed%particulate(ispecies, i, j)*blk%hp1(i,j)*blk%hp2(i,j)
          CASE (SED)
             scalar_bed_mass = scalar_bed_mass + &
                  &bed%sediment(ispecies, i, j)*blk%hp1(i,j)*blk%hp2(i,j)
          END SELECT
       END DO
    END DO
  END FUNCTION scalar_bed_mass

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_mass_print
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_mass_print(now)

    IMPLICIT NONE

    TYPE (datetime_struct), INTENT(IN) :: now

    INTEGER :: iblock, ispecies, phases, iphase
    INTEGER, PARAMETER :: iounit = 15
    CHARACTER (LEN=1024) :: buffer, timestr

                                ! these variables hold the sum of all
                                ! phases in a single block
    DOUBLE PRECISION :: tmass, tbedmass, tflux, terr, merr

                                ! these variables hold the sum of
                                ! masses for all blocks
    DOUBLE PRECISION, DIMENSION(max_species) :: allmass, allbedmass, allflux, allerr

                                ! compute only block masses,
                                ! additional fluxes are zero
    CALL scalar_mass_balance(0.0d0, domass=.TRUE.)
    
    CALL date_format(now%time, timestr)

    allmass = 0.0
    allbedmass = 0.0
    allflux = 0.0
    allerr = 0.0

    DO ispecies = 1, max_species

                                ! open a file for each generic or
                                ! sediment scalar
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN, SED)
          OPEN(unit=iounit, FILE=filename(ispecies), ACTION='WRITE', POSITION='APPEND')
       CASE DEFAULT
       END SELECT

       DO iblock = 1, max_blocks
          SELECT CASE (scalar_source(ispecies)%srctype)
          CASE (GEN, SED)
             WRITE(iounit, 120, ADVANCE='NO') timestr, &
                  &scalar_source(ispecies)%name, iblock
             merr = species(ispecies)%scalar(iblock)%netflux - &
                  &(species(ispecies)%scalar(iblock)%mass - &
                  &species(ispecies)%scalar(iblock)%massold + &
                  &species(ispecies)%scalar(iblock)%bedmass - &
                  &species(ispecies)%scalar(iblock)%bedmassold)
             WRITE(iounit, 130, ADVANCE='NO') &
                  &species(ispecies)%scalar(iblock)%mass, &
                  &species(ispecies)%scalar(iblock)%bedmass, &
                  &species(ispecies)%scalar(iblock)%netflux, merr

                                ! add to sum of all phases in block
             tmass = species(ispecies)%scalar(iblock)%mass
             tbedmass = species(ispecies)%scalar(iblock)%bedmass
             tflux = species(ispecies)%scalar(iblock)%netflux
             terr = merr

                                 ! increment sum of all blocks
             allmass(ispecies) = allmass(ispecies) + &
                  &species(ispecies)%scalar(iblock)%mass
             allbedmass(ispecies) = allbedmass(ispecies) + &
                  &species(ispecies)%scalar(iblock)%bedmass
             allflux(ispecies) = allflux(ispecies) + &
                  &species(ispecies)%scalar(iblock)%netflux
             allerr(ispecies) = allerr(ispecies) + merr
                  
                                ! reset the flux accumulator
             species(ispecies)%scalar(iblock)%netflux = 0.0
       CASE DEFAULT
       END SELECT

       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN)
          phases = 1
          DO iphase = ispecies + 1, max_species
             SELECT CASE (scalar_source(iphase)%srctype)
             CASE (PART)
                IF (scalar_source(iphase)%part_param%disidx .EQ. ispecies) THEN
                   phases = phases + 1
                   merr = species(iphase)%scalar(iblock)%netflux - &
                        &(species(iphase)%scalar(iblock)%mass - &
                        &species(iphase)%scalar(iblock)%massold + &
                        &species(iphase)%scalar(iblock)%bedmass - &
                        &species(iphase)%scalar(iblock)%bedmassold)
                   WRITE(iounit, 130, ADVANCE='NO') &
                        &species(iphase)%scalar(iblock)%mass, &
                        &species(iphase)%scalar(iblock)%bedmass, &
                        &species(iphase)%scalar(iblock)%netflux, merr
                   tmass = tmass + species(iphase)%scalar(iblock)%mass
                   tbedmass = tbedmass + species(iphase)%scalar(iblock)%bedmass
                   tflux = tflux + species(iphase)%scalar(iblock)%netflux
                   terr = terr + merr
                                 ! increment sum of all blocks
                   allmass(iphase) = allmass(iphase) + &
                        &species(iphase)%scalar(iblock)%mass
                   allbedmass(iphase) = allbedmass(iphase) + &
                        &species(iphase)%scalar(iblock)%bedmass
                   allflux(iphase) = allflux(iphase) + &
                        &species(iphase)%scalar(iblock)%netflux
                   allerr(iphase) = allerr(iphase) + merr

                                ! reset the flux accumulator
                   species(iphase)%scalar(iblock)%netflux = 0.0
                END IF
             END SELECT
          END DO

          IF (phases .GT. 1) THEN
             WRITE(iounit, 130, ADVANCE='NO') tmass, tbedmass, tflux, terr
          END IF
       END SELECT
       WRITE(iounit, *)
    END DO

                                ! write a summary for all blocks
    SELECT CASE (scalar_source(ispecies)%srctype)
    CASE (GEN, SED)
       WRITE (iounit, 121, ADVANCE='NO') timestr, scalar_source(ispecies)%name, '_ALL_'
       WRITE (iounit, 130, ADVANCE='NO') allmass(ispecies), &
            &allbedmass(ispecies), allflux(ispecies), allerr(ispecies)
       tmass = allmass(ispecies)
       tbedmass = allbedmass(ispecies)
       tflux = allflux(ispecies)
       terr = allerr(ispecies)
    CASE DEFAULT
    END SELECT
       
    SELECT CASE (scalar_source(ispecies)%srctype)
    CASE (GEN)
       phases = 1
       DO iphase = ispecies + 1, max_species
          SELECT CASE (scalar_source(iphase)%srctype)
          CASE (PART)
             phases = phases + 1
             IF (scalar_source(iphase)%part_param%disidx .EQ. ispecies) THEN
                WRITE (iounit, 130, ADVANCE='NO') allmass(iphase), &
                     &allbedmass(iphase), allflux(iphase), allerr(iphase)
                tmass = allmass(iphase)
                tbedmass = allbedmass(iphase)
                tflux = allflux(iphase)
                terr = allerr(iphase)
             END IF
          END SELECT
       END DO
    END SELECT
       
    IF (phases .GT. 1) THEN
       WRITE(iounit, 130, ADVANCE='NO') tmass, tbedmass, tflux, terr
    END IF
    WRITE(iounit, *)
    CLOSE(iounit)
 END DO
    

  
120 FORMAT(A25, ' ', A10, ' ', I5)
121 FORMAT(A25, ' ', A10, ' ', A5)
130 FORMAT(4(' ', E10.4))
  END SUBROUTINE scalar_mass_print

END MODULE scalar_mass
