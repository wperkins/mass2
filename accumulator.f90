! ----------------------------------------------------------------
! file: accumulator.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 25, 2000 by William A. Perkins
! Last Change: Fri Dec 17 09:44:56 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE accumulator
! ----------------------------------------------------------------
MODULE accumulator

  USE globals
  USE scalars
  USE scalars_source
  USE bed_module
  USE misc_vars, ONLY: do_flow, do_flow_output, do_transport, salinity, delta_t, &
       &i_index_min, i_index_extra, j_index_min, j_index_extra
  USE date_time

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

                                ! the accumulation module can
                                ! accumulate several statistics.
                                ! Currently only one is output, and
                                ! that choice is made at compile time,
                                ! here.

  INTEGER, PRIVATE, PARAMETER :: ACCUMAVG = 1, ACCUMMIN = 2, ACCUMMAX = 3
  INTEGER, PRIVATE, PARAMETER :: accum_mode = ACCUMAVG
  
  TYPE accum_var_rec
     DOUBLE PRECISION, POINTER :: max(:, :)
     DOUBLE PRECISION, POINTER :: min(:, :)
     DOUBLE PRECISION, POINTER :: sum(:, :)
  END TYPE accum_var_rec

  TYPE accum_temp_rec
     TYPE (accum_var_rec) :: evaporation
  END TYPE accum_temp_rec

  TYPE accum_tdg_rec
     TYPE (accum_var_rec) :: press
     TYPE (accum_var_rec) :: deltap
     TYPE (accum_var_rec) :: sat
  END TYPE accum_tdg_rec

  TYPE accum_hydro_rec
     TYPE (accum_var_rec) :: uvelp
     TYPE (accum_var_rec) :: vvelp
     TYPE (accum_var_rec) :: ucart
     TYPE (accum_var_rec) :: vcart
     TYPE (accum_var_rec) :: uflux
     TYPE (accum_var_rec) :: vflux
     TYPE (accum_var_rec) :: vmag
     TYPE (accum_var_rec) :: depth
     TYPE (accum_var_rec) :: wsel
     TYPE (accum_var_rec) :: shear
     TYPE (accum_var_rec) :: froude
     TYPE (accum_var_rec) :: courant
     TYPE (accum_var_rec) :: eddyvisc
  END TYPE accum_hydro_rec

  TYPE accum_bed_rec
     TYPE (accum_var_rec), POINTER :: mass(:)
     TYPE (accum_var_rec), POINTER :: erosion(:)
     TYPE (accum_var_rec), POINTER :: deposit(:)
     TYPE (accum_var_rec), POINTER :: conc(:)
     TYPE (accum_var_rec), POINTER :: pore(:)
     TYPE (accum_var_rec) :: depth 
  END TYPE accum_bed_rec

  TYPE accum_block_rec
     TYPE (accum_hydro_rec) :: hydro
     TYPE (accum_var_rec), POINTER :: conc(:)
     TYPE (accum_tdg_rec) :: tdg
     TYPE (accum_bed_rec) :: bed 
     TYPE (accum_temp_rec) :: temp
  END TYPE accum_block_rec

  TYPE (accum_block_rec), ALLOCATABLE :: accum_block(:)
  DOUBLE PRECISION, SAVE :: accum_start_time,  accum_end_time
  TYPE (datetime_struct) :: accum_time
  DOUBLE PRECISION, SAVE, ALLOCATABLE, PRIVATE :: accum_tmp(:,:)
  INTEGER, SAVE :: accum_count

  DOUBLE PRECISION, PARAMETER, PRIVATE :: realbig = 1.0d50, realsmall = - 1.0d50

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_init_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_init_var(mx, my, accum)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: mx, my
    TYPE (accum_var_rec) :: accum
    INTEGER :: nx, ny

    nx = mx + i_index_extra
    ny = my + j_index_extra

    ALLOCATE(accum%max(i_index_min:nx, j_index_min:ny))
    ALLOCATE(accum%min(i_index_min:nx, j_index_min:ny))
    ALLOCATE(accum%sum(i_index_min:nx, j_index_min:ny))

    accum%min = 1e20
    accum%max = -1e20
    accum%sum = 0.0


  END SUBROUTINE accum_init_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_init_bed
  ! ----------------------------------------------------------------
  SUBROUTINE accum_init_bed(bed, nx, ny)

    IMPLICIT NONE
    TYPE (accum_bed_rec) :: bed
    INTEGER, INTENT(IN) :: nx, ny
    INTEGER :: ispecies

    ALLOCATE(bed%mass(max_species), bed%conc(max_species), &
         &bed%erosion(max_species), bed%deposit(max_species), &
         &bed%pore(max_species))
    
    DO ispecies = 1, max_species
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN)
          CALL accum_init_var(nx, ny, bed%pore(ispecies))
       END SELECT
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN,SED,PART)
          CALL accum_init_var(nx, ny, bed%mass(ispecies))
          CALL accum_init_var(nx, ny, bed%conc(ispecies))
          CALL accum_init_var(nx, ny, bed%erosion(ispecies))
          CALL accum_init_var(nx, ny, bed%deposit(ispecies))
       END SELECT
    END DO
    CALL accum_init_var(nx, ny, bed%depth)

  END SUBROUTINE accum_init_bed


  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE accum_initialize()

    IMPLICIT NONE

    INTEGER :: iblk, ispec
    INTEGER :: mx, my

    ALLOCATE(accum_block(max_blocks))
   
    DO iblk = 1, max_blocks

       IF (do_flow .OR. do_flow_output) THEN
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%uvelp)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%vvelp)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%ucart)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%vcart)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%uflux)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%vflux)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%vmag)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%depth)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%wsel)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%shear)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%froude)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%courant)
          CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%hydro%eddyvisc)
       END IF
       IF (do_transport) THEN
          ALLOCATE(accum_block(iblk)%conc(max_species))
          DO ispec = 1, max_species
             CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%conc(ispec))
             SELECT CASE (scalar_source(ispec)%srctype)
             CASE (TEMP)
                CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%temp%evaporation)
             CASE (TDG)
                CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%tdg%press)
                CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%tdg%deltap)
                CALL accum_init_var(block(iblk)%xmax, block(iblk)%ymax, accum_block(iblk)%tdg%sat)
             END SELECT
             IF (source_doing_sed) THEN
                CALL accum_init_bed(accum_block(iblk)%bed, block(iblk)%xmax, block(iblk)%ymax)
             END IF
          END DO
       END IF
    END DO

                                ! allocate some temporary work space

    mx = MAXVAL(block(:)%xmax) + i_index_extra
    my = MAXVAL(block(:)%ymax) + j_index_extra

    ALLOCATE(accum_tmp(i_index_min:mx, j_index_min:my))

                                ! zero out the accumulators

    CALL accum_reset()

  END SUBROUTINE accum_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset_var(accum)

    IMPLICIT NONE
    TYPE (accum_var_rec) :: accum

    accum%max = realsmall
    accum%min = realbig
    accum%sum = 0.0

  END SUBROUTINE accum_reset_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset_bed
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset_bed(bed)

    IMPLICIT NONE
    TYPE (accum_bed_rec) :: bed
    INTEGER :: ispecies

    DO ispecies = 1, max_species
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN)
          CALL accum_reset_var(bed%pore(ispecies))
       END SELECT
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN,SED,PART)
          CALL accum_reset_var(bed%mass(ispecies))
          CALL accum_reset_var(bed%conc(ispecies))
          CALL accum_reset_var(bed%erosion(ispecies))
          CALL accum_reset_var(bed%deposit(ispecies))
       END SELECT
    END DO
    CALL accum_reset_var(bed%depth)


  END SUBROUTINE accum_reset_bed


  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset()

    IMPLICIT NONE

    INTEGER :: iblk, ispec

    DO iblk = 1, max_blocks
       IF (do_flow .OR. do_flow_output) THEN
          CALL accum_reset_var(accum_block(iblk)%hydro%uvelp)
          CALL accum_reset_var(accum_block(iblk)%hydro%vvelp)
          CALL accum_reset_var(accum_block(iblk)%hydro%ucart)
          CALL accum_reset_var(accum_block(iblk)%hydro%vcart)
          CALL accum_reset_var(accum_block(iblk)%hydro%uflux)
          CALL accum_reset_var(accum_block(iblk)%hydro%vflux)
          CALL accum_reset_var(accum_block(iblk)%hydro%vmag)
          CALL accum_reset_var(accum_block(iblk)%hydro%depth)
          CALL accum_reset_var(accum_block(iblk)%hydro%wsel)
          CALL accum_reset_var(accum_block(iblk)%hydro%shear)
          CALL accum_reset_var(accum_block(iblk)%hydro%froude)
          CALL accum_reset_var(accum_block(iblk)%hydro%courant)
          CALL accum_reset_var(accum_block(iblk)%hydro%eddyvisc)
       END IF
       IF (do_transport) THEN
          DO ispec = 1, max_species
             CALL accum_reset_var(accum_block(iblk)%conc(ispec))
             SELECT CASE (scalar_source(ispec)%srctype)
             CASE (TEMP)
                CALL accum_reset_var(accum_block(iblk)%temp%evaporation)
             CASE (TDG)
                CALL accum_reset_var(accum_block(iblk)%tdg%press)
                CALL accum_reset_var(accum_block(iblk)%tdg%deltap)
                CALL accum_reset_var(accum_block(iblk)%tdg%sat)
             END SELECT
          END DO
          IF (source_doing_sed) CALL accum_reset_bed(accum_block(iblk)%bed)
       END IF
    END DO
    accum_count = 0.0
    accum_start_time = 0.0
    accum_end_time = 0.0
    accum_time%date_string = '01-01-1900'
    accum_time%time_string = '00:00:00'
  END SUBROUTINE accum_reset

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_var(var, accum)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: var(i_index_min:,j_index_min:)
    TYPE (accum_var_rec) :: accum

    SELECT CASE (accum_mode)
    CASE (ACCUMAVG)
       accum%sum = accum%sum + var
    CASE (ACCUMMIN)
       WHERE (var .LT. accum%min) accum%min = var
    CASE (ACCUMMAX)
       WHERE (var .GT. accum%max) accum%max = var
    END SELECT
  END SUBROUTINE accum_var
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate_tdg
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate_tdg(iblk, ispec)

    USE gas_functions
    USE met_zone

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: iblk, ispec

    INTEGER :: i, j

    DOUBLE PRECISION :: baro_press

    IF (ALLOCATED(met_zones)) THEN
       baro_press = met_zones(1)%current(MET_BARO)
    ELSE 
       baro_press = 760.0
    END IF

    accum_tmp = 0.0
    DO i = i_index_min, block(iblk)%xmax + i_index_extra
       DO j = j_index_min, block(iblk)%ymax + j_index_extra
          accum_tmp(i, j) = &
               &TDGasPress(species(ispec)%scalar(iblk)%conc(i,j), &
               &species(source_temp_idx)%scalar(iblk)%conc(i,j), salinity)
       END DO
    END DO
    CALL accum_var(accum_tmp(&
         &i_index_min:block(iblk)%xmax + i_index_extra,&
         &j_index_min:block(iblk)%ymax + j_index_extra), &
         &accum_block(iblk)%tdg%press)

    accum_tmp = 0.0
    DO i = i_index_min, block(iblk)%xmax + i_index_extra
       DO j = j_index_min, block(iblk)%ymax + j_index_extra
          accum_tmp(i, j) = TDGasDP(species(ispec)%scalar(iblk)%conc(i,j), &
               &species(source_temp_idx)%scalar(iblk)%conc(i,j), salinity, baro_press)
       END DO
    END DO
    CALL accum_var(accum_tmp(&
         &i_index_min:block(iblk)%xmax + i_index_extra,&
         &j_index_min:block(iblk)%ymax + j_index_extra), &
         &accum_block(iblk)%tdg%deltap)

    accum_tmp = 0.0
    DO i = i_index_min, block(iblk)%xmax + i_index_extra
       DO j = j_index_min, block(iblk)%ymax + j_index_extra
          accum_tmp(i, j) = TDGasSaturation(species(ispec)%scalar(iblk)%conc(i,j), &
               &species(source_temp_idx)%scalar(iblk)%conc(i,j), salinity, baro_press)
       END DO
    END DO
    CALL accum_var(accum_tmp(&
         &i_index_min:block(iblk)%xmax + i_index_extra,&
         &j_index_min:block(iblk)%ymax + j_index_extra), &
         &accum_block(iblk)%tdg%sat)

  END SUBROUTINE accumulate_tdg

  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate_bed
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate_bed(iblk)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iblk
    INTEGER :: ispec, ifract, i, j 

    DO ispec = 1, max_species
       SELECT CASE (scalar_source(ispec)%srctype)
       CASE (SED)
          ifract = scalar_source(ispec)%sediment_param%ifract
          CALL accum_var(bed(iblk)%sediment(ifract, :, :), &
               &accum_block(iblk)%bed%conc(ispec))
          accum_tmp = 0.0
          accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra) = &
               &bed(iblk)%sediment(ifract, :, :)*&
               &block(iblk)%hp1*block(iblk)%hp2
          CALL accum_var(accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra), &
               &accum_block(iblk)%bed%mass(ispec))
          CALL accum_var(scalar_source(ispec)%sediment_param%block(iblk)%deposition,&
               &accum_block(iblk)%bed%deposit(ispec))
          CALL accum_var(scalar_source(ispec)%sediment_param%block(iblk)%erosion,&
               &accum_block(iblk)%bed%erosion(ispec))
       CASE (PART)
          CALL accum_var(bed(iblk)%particulate(ispec, :, :), &
               &accum_block(iblk)%bed%conc(ispec))
          accum_tmp = 0.0
          accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra) = &
               &bed(iblk)%particulate(ispec, :, :)* &
               &block(iblk)%hp1*block(iblk)%hp2
          CALL accum_var(accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra), &
               &accum_block(iblk)%bed%mass(ispec))
          CALL accum_var(scalar_source(ispec)%part_param%block(iblk)%bedexch,&
               &accum_block(iblk)%bed%deposit(ispec))
       CASE (GEN)
          CALL accum_var(bed(iblk)%pore(ispec, :, :), &
               &accum_block(iblk)%bed%conc(ispec))
          accum_tmp = 0.0
          accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra) = &
               &bed(iblk)%pore(ispec, :, :)* &
               &block(iblk)%hp1*block(iblk)%hp2
          CALL accum_var(accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra), &
               &accum_block(iblk)%bed%mass(ispec))
          accum_tmp = 0.0
          DO i = 1, block(iblk)%xmax + i_index_extra
             DO j = 1, block(iblk)%ymax + j_index_extra
                IF (REAL(bed(iblk)%depth(i,j)) .GT. 1e-8) &
                     &accum_tmp(i,j) = &
                     &bed(iblk)%pore(ispec, i, j)/ &
                     &(bed(iblk)%depth(i,j)* bed(iblk)%porosity(i,j))
             END DO
          END DO
          CALL accum_var(accum_tmp(&
               &i_index_min:block(iblk)%xmax + i_index_extra,&
               &j_index_min:block(iblk)%ymax + j_index_extra), &
               &accum_block(iblk)%bed%pore(ispec))
       END SELECT       
    END DO
    CALL accum_var(bed(iblk)%depth, accum_block(iblk)%bed%depth)

  END SUBROUTINE accumulate_bed


  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate(time)

    IMPLICIT NONE
  
    DOUBLE PRECISION, INTENT(IN) :: time
    INTEGER :: iblk, ispec, i, j

    IF (accum_count .EQ. 0) accum_start_time = time
    accum_end_time = time

    CALL velocity_shift()

    DO iblk = 1, max_blocks

                                ! cell centered velocities

       IF (do_flow .OR. do_flow_output) THEN
          CALL accum_var(block(iblk)%uvel_p, accum_block(iblk)%hydro%uvelp)
          CALL accum_var(block(iblk)%vvel_p, accum_block(iblk)%hydro%vvelp)
          CALL accum_var(block(iblk)%u_cart, accum_block(iblk)%hydro%ucart)
          CALL accum_var(block(iblk)%v_cart, accum_block(iblk)%hydro%vcart)
          CALL accum_var(block(iblk)%uflux, accum_block(iblk)%hydro%uflux)
          CALL accum_var(block(iblk)%vflux, accum_block(iblk)%hydro%vflux)

                                ! velocity magnitudes

          accum_tmp = 0.0
          DO i = i_index_min, block(iblk)%xmax + i_index_extra
             DO j = j_index_min, block(iblk)%ymax + j_index_extra
                ! FORALL (i = i_index_min:block(iblk)%xmax + i_index_extra, j = j_index_min:block(iblk)%ymax + j_index_extra)
                accum_tmp(i, j) = &
                     &SQRT(block(iblk)%vvel_p(i,j)*block(iblk)%vvel_p(i,j) + &
                     &block(iblk)%uvel_p(i,j)*block(iblk)%uvel_p(i,j))
                ! END FORALL
             END DO
          END DO
          CALL accum_var(accum_tmp(i_index_min:block(iblk)%xmax + i_index_extra, &
               &j_index_min:block(iblk)%ymax + j_index_extra), &
               &accum_block(iblk)%hydro%vmag)

                                ! remaining hydrodynamics variables

          CALL accum_var(block(iblk)%depth, accum_block(iblk)%hydro%depth)
          CALL accum_var(block(iblk)%wsel, accum_block(iblk)%hydro%wsel)
          CALL accum_var(block(iblk)%shear, accum_block(iblk)%hydro%shear)
          CALL accum_var(block(iblk)%froude_num, accum_block(iblk)%hydro%froude)
          CALL accum_var(block(iblk)%courant_num, accum_block(iblk)%hydro%courant)
          CALL accum_var(block(iblk)%eddy, accum_block(iblk)%hydro%eddyvisc)
       END IF
       IF (do_transport) THEN
          DO ispec = 1, max_species
             CALL accum_var(species(ispec)%scalar(iblk)%conc, &
                  &accum_block(iblk)%conc(ispec))
             SELECT CASE (scalar_source(ispec)%srctype)
             CASE (TEMP)
                CALL accum_var(scalar_source(ispec)%temp_param%block(iblk)%evaporation,&
                     &accum_block(iblk)%temp%evaporation)
             CASE (TDG)
                CALL accumulate_tdg(iblk, ispec)
             END SELECT
          END DO
          IF (source_doing_sed) CALL accumulate_bed(iblk)
       END IF
    END DO

    accum_count = accum_count + 1
  
  END SUBROUTINE accumulate

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc_var(accum)

    IMPLICIT NONE
    TYPE (accum_var_rec) :: accum

    SELECT CASE (accum_mode)
    CASE (ACCUMAVG)
       accum%sum = accum%sum/DBLE(accum_count)
    CASE (ACCUMMIN)
       accum%sum = accum%min
    CASE (ACCUMMAX) 
       accum%sum = accum%max
    END SELECT
  
  END SUBROUTINE accum_calc_var
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc_bed
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc_bed(bed)

    IMPLICIT NONE
  
    TYPE (accum_bed_rec) :: bed
    INTEGER :: ispecies

    DO ispecies = 1, max_species
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN)
          CALL accum_calc_var(bed%pore(ispecies))
       END SELECT
       SELECT CASE (scalar_source(ispecies)%srctype)
       CASE (GEN,SED,PART)
          CALL accum_calc_var(bed%mass(ispecies))
          CALL accum_calc_var(bed%conc(ispecies))
          CALL accum_calc_var(bed%erosion(ispecies))
          CALL accum_calc_var(bed%deposit(ispecies))
       END SELECT
    END DO
    CALL accum_calc_var(bed%depth)
   

  END SUBROUTINE accum_calc_bed

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc
  ! be sure to call accum_reset after calling this
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc()

    IMPLICIT NONE

    INTEGER :: iblk, ispec

    IF (accum_count .LE. 0) accum_count = 1

    DO iblk = 1, max_blocks

       IF (do_flow .OR. do_flow_output) THEN
          CALL accum_calc_var(accum_block(iblk)%hydro%uvelp)
          CALL accum_calc_var(accum_block(iblk)%hydro%vvelp)
          CALL accum_calc_var(accum_block(iblk)%hydro%ucart)
          CALL accum_calc_var(accum_block(iblk)%hydro%vcart)
          CALL accum_calc_var(accum_block(iblk)%hydro%uflux)
          CALL accum_calc_var(accum_block(iblk)%hydro%vflux)
          CALL accum_calc_var(accum_block(iblk)%hydro%vmag)
          CALL accum_calc_var(accum_block(iblk)%hydro%depth)
          CALL accum_calc_var(accum_block(iblk)%hydro%shear)
          CALL accum_calc_var(accum_block(iblk)%hydro%froude)
          CALL accum_calc_var(accum_block(iblk)%hydro%courant)
          CALL accum_calc_var(accum_block(iblk)%hydro%eddyvisc)
       END IF
       IF (do_transport) THEN
          DO ispec = 1, max_species
             CALL accum_calc_var(accum_block(iblk)%conc(ispec))
             SELECT CASE (scalar_source(ispec)%srctype)
             CASE (TEMP)
                CALL accum_calc_var(accum_block(iblk)%temp%evaporation)
             CASE (TDG)
                CALL accum_calc_var(accum_block(iblk)%tdg%press)
                CALL accum_calc_var(accum_block(iblk)%tdg%deltap)
                CALL accum_calc_var(accum_block(iblk)%tdg%sat)
             END SELECT
          END DO
          IF (source_doing_sed) CALL accum_calc_bed(accum_block(iblk)%bed)
       END IF
    END DO

    accum_time%time = (accum_start_time + accum_end_time)/2.0
    CALL decimal_to_date(accum_time%time, accum_time%date_string, accum_time%time_string)

  END SUBROUTINE accum_calc

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_done
  ! ----------------------------------------------------------------
  SUBROUTINE accum_done()

    IMPLICIT NONE

    INTEGER :: iblk, ispec

    DO iblk = 1, max_blocks
       IF (do_flow .OR. do_flow_output) THEN
          DEALLOCATE(accum_block(iblk)%hydro%uvelp%max)
          DEALLOCATE(accum_block(iblk)%hydro%uvelp%min)
          DEALLOCATE(accum_block(iblk)%hydro%uvelp%sum)
          DEALLOCATE(accum_block(iblk)%hydro%vvelp%max)
          DEALLOCATE(accum_block(iblk)%hydro%vvelp%min)
          DEALLOCATE(accum_block(iblk)%hydro%vvelp%sum)
          DEALLOCATE(accum_block(iblk)%hydro%depth%max)
          DEALLOCATE(accum_block(iblk)%hydro%depth%min)
          DEALLOCATE(accum_block(iblk)%hydro%depth%sum)
          DEALLOCATE(accum_block(iblk)%hydro%wsel%max)
          DEALLOCATE(accum_block(iblk)%hydro%wsel%min)
          DEALLOCATE(accum_block(iblk)%hydro%wsel%sum)
          DEALLOCATE(accum_block(iblk)%hydro%shear%max)
          DEALLOCATE(accum_block(iblk)%hydro%shear%min)
          DEALLOCATE(accum_block(iblk)%hydro%shear%sum)
       END IF
       IF (do_transport) THEN
          DO ispec = 1, max_species
             DEALLOCATE(accum_block(iblk)%conc(ispec)%max)
             DEALLOCATE(accum_block(iblk)%conc(ispec)%min)
             DEALLOCATE(accum_block(iblk)%conc(ispec)%sum)
          END DO
          DEALLOCATE(accum_block(iblk)%conc)
       END IF
    END DO
    DEALLOCATE(accum_block)

  END SUBROUTINE accum_done

END MODULE accumulator
