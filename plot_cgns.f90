! ----------------------------------------------------------------
! file: plot_cgns.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 11, 2003 by William A. Perkins
! Last Change: Wed Apr 23 12:43:31 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE plot_cgns
! ----------------------------------------------------------------
MODULE plot_cgns

  USE accumulator

  IMPLICIT NONE
!!$  INTERFACE 
!!$    SUBROUTINE cg_goto_f
!!$	  !DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_goto_f
!!$	END SUBROUTINE cg_goto_f
!!$	SUBROUTINE cg_array_write_f
!!$	  !DEC$ ATTRIBUTES REFERENCE, C, VARYING :: cg_array_write_f
!!$	END SUBROUTINE cg_array_write_f
!!$  END INTERFACE
  INCLUDE 'cgnslib_f.h'

  CHARACTER (LEN=80), PRIVATE, PARAMETER :: grid_file_name = "grid.cgns"
  INTEGER, PRIVATE :: pfileidx
  INTEGER, PRIVATE :: pbaseidx
  CHARACTER (LEN=1024), PRIVATE :: plot_file_name

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, ALLOCATABLE, PRIVATE :: tmp_int(:)
  REAL, ALLOCATABLE, PRIVATE :: tmp_real(:)
  DOUBLE PRECISION, ALLOCATABLE, PRIVATE :: tmp_double(:)

                                ! this flag is used to determine the
                                ! kind of grid stored in CGNS. If
                                ! .FALSE., the CGNS output mimics that
                                ! to netcdf. Otherwise, the CGNS
                                ! output uses the grid read in by
                                ! mass2 and all values are cell
                                ! centered.

  LOGICAL, PUBLIC :: plot_cgns_docell = .FALSE.

                                ! this flag is used to determine if
                                ! descriptor nodes are written along
                                ! with the data.

  LOGICAL, PUBLIC :: plot_cgns_dodesc = .TRUE.

                                ! since the CGNS format becomes very
                                ! unwieldy with large numbers of
                                ! nodes, the plot output can be split
                                ! into several files containing a
                                ! fixed number of output time steps

  INTEGER, PRIVATE :: plot_cgns_outstep = 0
  INTEGER, PUBLIC :: plot_cgns_maxtime = 10

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_error
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_error(routine, message, fatal)

    USE utility

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: message
    CHARACTER (LEN=*), INTENT(IN) :: routine
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    
    CHARACTER (LEN=1024) :: buffer, errmsg
    LOGICAL :: myfatal = .FALSE.

    IF (PRESENT(fatal)) myfatal = fatal

    CALL cg_get_error_f(errmsg)

    WRITE (buffer, *) 'CGNS: ', TRIM(routine), ': ', TRIM(message), &
            &': ', TRIM(errmsg)

    CALL error_message(buffer, myfatal)
    RETURN
  END SUBROUTINE plot_cgns_error

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_make_name
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_make_name(s)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(INOUT) :: s
    INTEGER :: fnum

    fnum = plot_cgns_outstep/plot_cgns_maxtime
    
    WRITE(s, '("plot", I3.3, ".cgns")') fnum

  END SUBROUTINE plot_cgns_make_name

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_file_setup(filename, docoord, fileidx, baseidx)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_file_setup"
    CHARACTER (LEN=*) :: filename
    LOGICAL, INTENT(IN) :: docoord
    INTEGER, INTENT(INOUT) :: fileidx, baseidx

    INTEGER :: iblock, zoneidx, ierr

                                ! open the CGNS file for writing

    CALL cg_open_f(filename, MODE_WRITE, fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
         &"cannot open " // TRIM(filename), fatal=.TRUE.)

  
                                ! create a single base

    CALL cg_base_write_f(fileidx, "MASS2", 2, 2, baseidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write base in " //&
         &TRIM(filename), fatal=.TRUE.)

                                ! create a zone for each block, and write the grid

    DO iblock = 1, max_blocks
       CALL plot_cgns_write_grid(fileidx, baseidx, block(iblock), iblock, docoord, zoneidx)
    END DO


  END SUBROUTINE plot_cgns_file_setup


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_setup()

    IMPLICIT NONE

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_setup"
    INTEGER :: ierr, zoneidx, max_x, max_y
    INTEGER :: iblock

                                 ! make a buffer to hold data going in and out
 
    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1
    ALLOCATE(tmp_int(2*max_x*max_y), &
         &tmp_real(2*max_x*max_y), &
         &tmp_double(2*max_x*max_y))

                                ! make a grid file with coordinates in it

    CALL plot_cgns_file_setup(grid_file_name, .TRUE., pfileidx, pbaseidx)
    CALL cg_close_f(pfileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
         &"cannot close grid file", fatal=.TRUE.)

                                ! make another file to hold the data
                                ! (but leave this one open)

    CALL plot_cgns_make_name(plot_file_name)
    CALL plot_cgns_file_setup(plot_file_name, .FALSE., pfileidx, pbaseidx)

  END SUBROUTINE plot_cgns_setup

  

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_grid
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_grid(fileidx, baseidx, blk, id, docoord, zoneidx)

    USE globals, ONLY: block_struct

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, baseidx
    TYPE (block_struct), INTENT(IN) :: blk
    INTEGER, INTENT(IN) :: id
    LOGICAL, INTENT(IN) :: docoord
    INTEGER, INTENT(OUT) :: zoneidx

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_grid"
    CHARACTER (LEN=80) :: buffer
    INTEGER :: size(9)
    INTEGER :: ddataidx, ierr

    WRITE(buffer, '("Block", I2.2)') id
    size = 0

    IF (plot_cgns_docell) THEN
       size(1) = blk%xmax       ! vertices
       size(2) = blk%ymax
    ELSE
       size(1) = blk%xmax + 1   ! vertices
       size(2) = blk%ymax + 1
    END IF

    size(3) = size(1) - 1       ! cells
    size(4) = size(2) - 1

    CALL cg_zone_write_f(fileidx, baseidx, buffer, size, Structured, zoneidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write zone", fatal=.TRUE.)

    IF (docoord) THEN
       IF (plot_cgns_docell) THEN
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateX', &
               & blk%x_grid(1:size(1), 1:size(2)))
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateY', &
               & blk%y_grid(1:size(1), 1:size(2)))
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateZ', &
               & blk%zbot_grid(1:size(1), 1:size(2)))
       ELSE 
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateX', &
               & blk%x_out(1:size(1), 1:size(2)))
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateY', &
               & blk%y_out(1:size(1), 1:size(2)))
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateZ', &
               & blk%zbot_out(1:size(1), 1:size(2)))
       END IF

                                ! include the grid metric coefficient
                                ! in with the grid coordinates
                                ! (because it's easier than using a
                                ! DiscreteData node).  I don't know 

       ! CALL plot_cgns_write_coord(zoneidx, size, "hp1", &
       !      &blk%hp1(1:size(1), 1:size(2)))
       ! CALL plot_cgns_write_coord(zoneidx, size, "hp2", &
       !      &blk%hp2(1:size(1), 1:size(2)))
       ! CALL plot_cgns_write_coord(zoneidx, size, "gp12", &
       !      &blk%gp12(1:size(1), 1:size(2)))

       CALL cg_discrete_write_f(fileidx, baseidx, zoneidx, "GridMetrics", ddataidx, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write descrete data", fatal=.TRUE.)
    
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "hp1", &
            &blk%hp1(1:size(1), 1:size(2)))
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "hp2", &
            &blk%hp2(1:size(1), 1:size(2)))
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "gp12", &
            &blk%gp12(1:size(1), 1:size(2)))
    END IF
    RETURN

  END SUBROUTINE plot_cgns_write_grid

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_coord
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, name, coord)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, baseidx, zoneidx, size(:)
    CHARACTER (LEN=*) :: name
    DOUBLE PRECISION, INTENT(IN) :: coord(1:size(1), 1:size(2))
    
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_coord"
    CHARACTER (LEN=80) :: buffer
    INTEGER :: i, j, pos, coordidx, ierr

                                ! dump 2D coordinates

    DO j = 1, size(2)
       DO i = 1, size(1)
          pos = i + (j-1)*size(1)
          tmp_double(pos) = coord(i,j)
       END DO
    END DO

    CALL cg_coord_write_f(fileidx, baseidx, zoneidx, RealDouble, &
         &name, tmp_double, coordidx, ierr)
    IF (ierr .EQ. ERROR) THEN 
       WRITE(buffer, *) 'cannot write coord "', name, '"'
       CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
    END IF

  END SUBROUTINE plot_cgns_write_coord

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_metric
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, name, metric)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, baseidx, zoneidx, ddataidx, size(:)
    CHARACTER (LEN=*) :: name
    DOUBLE PRECISION, INTENT(IN) :: metric(1:size(1), 1:size(2))
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_metric"
    CHARACTER (LEN=1024) :: buffer
    INTEGER :: i, j, pos, ierr

    CALL cg_goto_f(fileidx, baseidx, ierr, &
         &'Zone_t', zoneidx, &
         &'DiscreteData_t', ddataidx, &
         &'end')
    IF (ierr .EQ. ERROR) THEN
       WRITE(buffer, '("cannot find discrete data ", I2, " for zone ", I2)') &
            & ddataidx, zoneidx
       CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
    END IF

    DO j = 1, size(2)
       DO i = 1, size(1)
          pos = i + (j-1)*size(1)
          tmp_double(pos) = metric(i,j)
       END DO
    END DO

    CALL cg_array_write_f(name, RealDouble, 2, size, tmp_double, ierr)
    IF (ierr .EQ. ERROR) THEN
       WRITE(buffer, *) "cannot write ", TRIM(name), " in zone ", zoneidx
       CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
    END IF

  END SUBROUTINE plot_cgns_write_metric


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_cgns
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write(date_string, time_string, salinity, baro_press)

    USE globals
    USE misc_vars, ONLY: do_flow, do_flow_output, do_flow_diag, do_transport, do_wetdry, do_rptdead
    USE scalars
    USE scalars_source
    USE date_time
    USE gas_functions
    USE bed_module

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: date_string
    CHARACTER (LEN=*), INTENT(IN) :: time_string
    DOUBLE PRECISION, INTENT(IN) :: salinity, baro_press
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write"

    INTEGER :: iblock, ispecies, solidx, xmax, ymax, ierr, ifract
    CHARACTER (LEN=32) :: timestamp

    timestamp = TRIM(accum_time%date_string) // " " // &
         &TRIM(accum_time%time_string)
    
    DO iblock = 1, max_blocks

       xmax =  block(iblock)%xmax
       ymax =  block(iblock)%ymax
       IF (plot_cgns_docell) THEN
          CALL cg_sol_write_f(pfileidx, pbaseidx, iblock, timestamp, CellCenter, &
               &solidx, ierr)
       ELSE
          CALL cg_sol_write_f(pfileidx, pbaseidx, iblock, timestamp, Vertex, &
               &solidx, ierr)
       END IF
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write solution", fatal=.TRUE.)

       ! Solution Field Names: I have stuck to the traditional MASS2
       ! short names here.  They do not follow the CGNS rules for
       ! field names, which are rather limited.

       IF (do_flow .OR. do_flow_output) THEN

                                ! average u_cart

          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &accum_block(iblock)%hydro%ucart%sum, &
               &'ucart', 'Eastward Velocity', 'feet/second')

                                ! average v_cart

          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &accum_block(iblock)%hydro%vcart%sum, &
               &'vcart', 'Northward Velocity', 'feet/second')

                                ! average depth

          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &accum_block(iblock)%hydro%depth%sum, &
               &'depth', 'Water Depth', 'feet')

          IF (do_flow_diag) THEN

                                ! average u

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%uvelp%sum, &
                  &'uvel', 'Longitudinal Velocity', 'feet/second')

                                ! average v

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%vvelp%sum, &
                  &'vvel', 'Lateral Velocity', 'feet/second')

                                ! average wsel

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%wsel%sum, &
                  &'wsel', 'Water Surface Elevation', 'feet')

                                ! average velocity magnitude

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%vmag%sum, &
                  &'vmag', 'Velocity Magnitude', 'feet/second')

                                ! average bed shear

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%shear%sum, &
                  &'shear', 'Bed Shear Stress', 'pound/foot^2')

                                ! Froude number

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%froude%sum, &
                  &'froude', 'Froude Number', 'none')

                                ! Courant number

             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%hydro%courant%sum, &
                  &'courant', 'Courant Number', 'none')

          END IF

                                ! Dry Cell Flag

          IF (do_wetdry) THEN
             
             CALL plot_cgns_write_flag(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%isdry, 'isdry', 'Dry Cell Flag')

          END IF
                                ! Dead Cell Flag

          IF (do_rptdead) THEN

             CALL plot_cgns_write_flag(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%isdead(:,:)%p, 'isdead', 'Dead Cell Flag')

          END IF

       END IF

       IF (do_transport) THEN
          DO ispecies = 1, max_species
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &accum_block(iblock)%conc(ispecies)%sum, &
                  &scalar_source(ispecies)%name,  &
                  &scalar_source(ispecies)%description, &
                  &scalar_source(ispecies)%units,&
                  &scalar_source(ispecies)%conversion) 

             SELECT CASE(scalar_source(ispecies)%srctype)
             CASE (TDG)
                                ! TDG pressure

                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                     &accum_block(iblock)%tdg%press%sum, &
                     &"tdgpress",  'Total Dissolved Gas Pressure', "millimeters of Hg") 

                                ! TDG delta P

                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                     &accum_block(iblock)%tdg%deltap%sum, &
                     &"tdgdeltap",  'Total Dissolved Gas Pressure Delta', "millimeters of Hg") 

                                ! TDG percent saturation
                
                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                     &accum_block(iblock)%tdg%sat%sum, &
                     &"tdgsat",  'Total Dissolved Gas Saturation', "percent") 


             CASE (GEN)
                                ! if we have a bed, report the amounts
                                ! of dissolved scalars in the bed pore
                                ! water

                IF (source_doing_sed) THEN

                   ! dissolved contaminant mass in bed pores

                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                        &accum_block(iblock)%bed%mass(ispecies)%sum, &
                        &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                        &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                        &"mass")
                   
                   ! dissolved contaminant mass per unit bed area

                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                        &accum_block(iblock)%bed%mass(ispecies)%sum, &
                        &TRIM(scalar_source(ispecies)%name) // '-bed', &
                        &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                        &"mass/foot^2")
                   
                   ! dissolved contaminant mass per unit volume bed pore water

                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                        &accum_block(iblock)%bed%pore(ispecies)%sum, &
                        &TRIM(scalar_source(ispecies)%name) // '-pore', &
                        &"Concentration of " // TRIM(scalar_source(ispecies)%description) // " in Bed Pores", &
                        &scalar_source(ispecies)%units, &
                        &scalar_source(ispecies)%conversion)


                END IF
                   
             CASE (SED)
                                ! If we are doing sediment, output
                                ! sediment erosion and deposition
                                ! rates
                ifract = scalar_source(ispecies)%sediment_param%ifract

                                ! deposition rate

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%deposit(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-depos',&
                     &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description),&
                     &"mass/foot^2/second")

                                ! erosion rate

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%erosion(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-erode', &
                     &"Rate of Erosion of " // TRIM(scalar_source(ispecies)%description),&
                     &"mass/foot^2/second")

                                ! bed total mass

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%mass(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass")
                
                                ! bed mass per unit area

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%conc(ispecies)%sum,  &
                     &TRIM(scalar_source(ispecies)%name) // '-bed', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass/foot^2")

             CASE (PART)
                                ! If we are doing particulate phases,
                                ! output erosion and deposition rates

                                ! particulate deposition rate

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%deposit(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-depos', &
                     &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description), &
                     &"mass/foot^2/second") 

                                ! particulate bed mass

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%mass(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass")

                                ! particulate bed mass per unit area

                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                     &accum_block(iblock)%bed%conc(ispecies)%sum, &
                     &TRIM(scalar_source(ispecies)%name) // '-bed', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass/foot^2")

             END SELECT
          END DO
       END IF

                                ! bed depth if called for
       IF (source_doing_sed) THEN
          CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
               &accum_block(iblock)%bed%depth%sum, &
               &"beddepth", "Depth of Bed Sediments", "feet")
       END IF

    END DO

                                ! if we have written the specified
                                ! number of time slices, make a new
                                ! file

    plot_cgns_outstep = plot_cgns_outstep + 1
    IF (MOD(plot_cgns_outstep, plot_cgns_maxtime) .EQ. 0) THEN
       CALL plot_cgns_file_close(pfileidx)
       CALL plot_cgns_make_name(plot_file_name)
       CALL plot_cgns_file_setup(plot_file_name, .FALSE., pfileidx, pbaseidx)
    END IF

  END SUBROUTINE plot_cgns_write

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_var
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_var(zoneidx, solidx, xmax, ymax, var, name, desc, units, conv)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: zoneidx, solidx
    INTEGER, INTENT(IN) :: xmax, ymax
    DOUBLE PRECISION, INTENT(IN) :: var(i_index_min:,j_index_min:)
    CHARACTER (LEN=*), INTENT(IN) :: name, desc, units
    DOUBLE PRECISION, INTENT(IN), OPTIONAL :: conv
    INTEGER :: i, j, pos, fldidx, ierr
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_var"
    CHARACTER (LEN=32) :: buffer
    DOUBLE PRECISION :: myconv, junk

    myconv = 1.0
    IF (PRESENT(conv)) myconv = conv
    
    tmp_real = 0.0
    IF (plot_cgns_docell) THEN
       DO j = 2, ymax
          DO i = 2, xmax
             pos = (i-1) + (j-2)*(xmax-1)
             IF (DABS(var(i,j)) .LT. 1.0d-37) THEN
                tmp_real(pos) = 0.0
             ELSE
                tmp_real(pos) = SNGL(var(i,j)/myconv)
             END IF
          END DO
       END DO
    ELSE
       DO j = 1, ymax + 1
          DO i = 1, xmax + 1
             pos = (i) + (j-1)*(xmax + 1)

             IF (i .EQ. 1) THEN
                junk = 0.5*(var(i,j) + var(i+1,j))
             ELSE IF (i .EQ. xmax+1) THEN
                junk = 0.5*(var(i,j) + var(i-1,j))
             ELSE 
                junk = var(i,j)
             END IF
             IF (DABS(junk) .LT. 1.0d-37) THEN
                tmp_real(pos) = 0.0
             ELSE
                tmp_real(pos) = SNGL(junk/myconv)
             END IF
          END DO
       END DO
    END IF

    buffer = name
    CALL cg_field_write_f(pfileidx, pbaseidx, zoneidx, solidx, &
         &RealSingle, buffer, tmp_real, fldidx, ierr)
    IF (ierr .EQ. ERROR) THEN
       WRITE(buffer, *) 'cannot write variable "', TRIM(name), '"'
       CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
    END IF
    
                                ! put a description for the field

    IF (plot_cgns_dodesc) THEN
       CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
            &'Zone_t', zoneidx, &
            &'FlowSolution_t', solidx, &
            &'DataArray_t', fldidx,&
            &'end')
       IF (ierr .EQ. ERROR) THEN
          WRITE(buffer, *) 'cannot write description for "', TRIM(name), '"'
          CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
       END IF
       
       CALL cg_descriptor_write_f('Description', desc, ierr)
       CALL cg_descriptor_write_f('Units', units, ierr)
    END IF


  END SUBROUTINE plot_cgns_write_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_flag
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_flag(zoneidx, solidx, xmax, ymax, flag, name, desc)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: zoneidx, solidx
    INTEGER, INTENT(IN) :: xmax, ymax
    LOGICAL, INTENT(IN) :: flag(i_index_min:,j_index_min:)
    CHARACTER (LEN=*), INTENT(IN) :: name, desc
    INTEGER :: i, j, pos, fldidx, ierr
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_flag"
    CHARACTER (LEN=32) :: buffer
    
    tmp_int = 0

    IF (plot_cgns_docell) THEN

       DO j = 2, ymax
          DO i = 2, xmax
             pos = (i-1) + (j-2)*(xmax-1)
             IF (flag(i,j)) tmp_int(pos) = 1
          END DO
       END DO
       
    ELSE

       DO j = 1, ymax+1
          DO i = 1, xmax+1
             pos = (i) + (j-1)*(xmax + 1)
             IF (flag(i,j)) tmp_int(pos) = 1
          END DO
       END DO

    END IF

    buffer = name
    CALL cg_field_write_f(pfileidx, pbaseidx, zoneidx, solidx, &
         &Integer, buffer, tmp_int, fldidx, ierr)

    IF (ierr .EQ. ERROR) THEN
       WRITE(buffer, *) 'cannot write flag "', TRIM(name), '"'
       CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
    END IF

    IF (plot_cgns_dodesc) THEN 
                                ! put a description for the field

       CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
            &'Zone_t', zoneidx, &
            &'FlowSolution_t', solidx, &
            &'DataArray_t', fldidx,&
            &'end')
       IF (ierr .EQ. ERROR) THEN
          WRITE(buffer, *) 'cannot write description for "', TRIM(name), '"'
          CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
       END IF
       
       CALL cg_descriptor_write_f('Description', desc, ierr)
    END IF

  END SUBROUTINE plot_cgns_write_flag

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_file_close
  ! This closes the plot file and adds links to the grid
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_file_close(fileidx)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_file_close"
    CHARACTER (LEN=1024) :: buffer
    INTEGER :: baseidx, nzones, zoneidx, ierr

    baseidx = 1

                                ! close the file that is now open

    CALL cg_close_f(fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot close file", fatal=.TRUE.)

                                ! reopen the CGNS file for modification

    CALL cg_open_f(plot_file_name, MODE_MODIFY, fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot reopen", fatal=.TRUE.)

                                ! read the number of zones

    CALL cg_nzones_f(fileidx, baseidx, nzones, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot reread number of zones", fatal=.TRUE.)

    DO zoneidx = 1, nzones

                                ! make links to the coordinates and
                                ! metrics in the grid file

       CALL cg_goto_f(fileidx, baseidx, ierr, &
            &'Zone_t', zoneidx, "end")
       WRITE(buffer, '("MASS2/Block", I2.2, "/GridCoordinates")') zoneidx
       CALL cg_link_write_f('GridCoordinates', grid_file_name, buffer, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
            &"cannot link to grid coordinates path: " // buffer, fatal=.TRUE.)
       WRITE(buffer, '("MASS2/Block", I2.2, "/GridMetrics")') zoneidx
       CALL cg_link_write_f('GridMetrics', grid_file_name, buffer, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
            &"cannot link to grid metrics path: " // buffer, fatal=.TRUE.)

!!$       IF (plot_cgns_doiter) THEN
!!$
!!$          CALL cg_nsols_f(pfileidx, pbaseidx, zoneidx, nsols, ierr)
!!$          IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot reread number of solutions", fatal=.TRUE.)
!!$       
!!$          IF (nsols .GT. 0) THEN
!!$
!!$             ALLOCATE(names(nsols))
!!$             IF (zoneidx .EQ. 1) ALLOCATE(times(nsols))
!!$             
!!$             DO solidx = 1, nsols
!!$
!!$                CALL cg_sol_info_f(pfileidx, pbaseidx, zoneidx, solidx, solname, solloc, ierr)
!!$                IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot read solution info", fatal=.TRUE.)
!!$       
!!$                READ(solname, *) datestr, timestr
!!$          
!!$                IF (zoneidx .EQ. 1) times(solidx) = date_to_decimal(datestr, timestr)
!!$                names(solidx) = solname
!!$             END DO
!!$
!!$             CALL cg_ziter_write_f(pfileidx, pbaseidx, zoneidx, 'ZoneSolutions', ierr)
!!$             IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write zone iterative data", fatal=.TRUE.)
!!$
!!$             CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
!!$                  &'Zone_t', zoneidx, &
!!$                  &'ZoneIterativeData_t', 1, 'end')
!!$             IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot find zone iterative data", fatal=.TRUE.)
!!$
!!$             idx(1) = 32
!!$             idx(2) = nsols
!!$             CALL cg_array_write_f('ZoneSolutionPointers', Character, 2, idx, names, ierr)
!!$             IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write zone solution pointers", fatal=.TRUE.)
!!$             
!!$             IF (zoneidx .EQ. 1) THEN
!!$                
!!$                CALL cg_biter_write_f(pfileidx, pbaseidx, 'Solution Time Planes', nsols, ierr)
!!$                IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write base iterative data", fatal=.TRUE.)
!!$
!!$                CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
!!$                     &'BaseIterativeData_t', 1, 'end')
!!$                IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot find base iterative data", fatal=.TRUE.)
!!$
!!$                idx(1) = nsols
!!$             
!!$                CALL cg_array_write_f('TimeValues', RealDouble, 1, idx, times, ierr)
!!$                IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write base solution data", fatal=.TRUE.)
!!$
!!$                DEALLOCATE(times)
!!$             END IF
!!$
!!$             DEALLOCATE(names)
!!$          END IF
!!$       END IF


    END DO
    CALL cg_close_f(fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot reclose file", fatal=.TRUE.)
    

  END SUBROUTINE plot_cgns_file_close

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_close()

    USE date_time

    IMPLICIT NONE

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_close"
    INTEGER :: nzones, zoneidx, nsols, solidx, solloc, ierr, idx(2)
    CHARACTER (LEN=1024) :: buffer
    CHARACTER (LEN=80) :: solname
    CHARACTER (LEN=20) :: datestr, timestr
    CHARACTER (LEN=32), ALLOCATABLE :: names(:)
    DOUBLE PRECISION, ALLOCATABLE :: times(:)

    CALL plot_cgns_file_close(pfileidx)

    DEALLOCATE(tmp_real, tmp_double)


  END SUBROUTINE plot_cgns_close


END MODULE plot_cgns
