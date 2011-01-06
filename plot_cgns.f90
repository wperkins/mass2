! ----------------------------------------------------------------
! file: plot_cgns.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 11, 2003 by William A. Perkins
! Last Change: Wed Jan  5 18:35:59 2011 by William A. Perkins <d3g096@PE10588.local>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE plot_cgns
! ----------------------------------------------------------------
MODULE plot_cgns

  USE date_time
  USE utility
  USE config, ONLY: do_flow, do_flow_output, do_flow_diag, &
       &max_blocks, plot_cgns_docell, plot_cgns_dodesc, plot_cgns_maxtime
  USE block_grid
  USE block_hydro

  IMPLICIT NONE

  ! Some crap to make the Visual Fortran compiler for Windoze happy.
  ! It has trouble with the variable argument lists these two CGNS
  ! routines have. 

  EXTERNAL cg_goto_f
  !DEC!$ ATTRIBUTES REFERENCE, C, VARYING :: cg_goto_f
  EXTERNAL cg_array_write_f
  !DEC!$ ATTRIBUTES REFERENCE, C, VARYING :: cg_array_write_f

  INCLUDE 'cgnslib_f.h'

  CHARACTER (LEN=80), PRIVATE, PARAMETER :: grid_cgns_name = "grid.cgns"
  LOGICAL, PRIVATE, PARAMETER :: do2D = .TRUE.
  INTEGER, PRIVATE :: pfileidx
  INTEGER, PRIVATE :: pbaseidx
  CHARACTER (LEN=1024), PRIVATE :: plot_file_name

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, ALLOCATABLE, PRIVATE :: tmp_int(:)
  REAL, ALLOCATABLE, PRIVATE :: tmp_real(:)
  DOUBLE PRECISION, ALLOCATABLE, PRIVATE :: tmp_double(:)
  REAL, ALLOCATABLE, PRIVATE :: tmp_times(:)

  INTEGER, PRIVATE :: plot_cgns_outstep = 0
  DOUBLE PRECISION, PRIVATE :: plot_cgns_start_time = -1

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_setup()

    IMPLICIT NONE

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_setup"
    INTEGER :: ierr, max_x, max_y

                                 ! make a buffer to hold data going in and out
 
    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1
    ALLOCATE(tmp_int(2*max_x*max_y), &
         &tmp_real(2*max_x*max_y), &
         &tmp_double(2*max_x*max_y))
    ALLOCATE(tmp_times(plot_cgns_maxtime))
    plot_cgns_start_time = -1.0
    tmp_times = 0.0
    

                                ! make a grid file with coordinates in it

    CALL plot_cgns_file_setup(grid_cgns_name, .TRUE., pfileidx, pbaseidx)
    CALL cg_close_f(pfileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
         &"cannot close grid file", fatal=.TRUE.)

                                ! make another file to hold the data
                                ! (but leave this one open)

    CALL plot_cgns_make_name(plot_file_name)
    CALL plot_cgns_file_setup(plot_file_name, .FALSE., pfileidx, pbaseidx)
    CALL plot_cgns_link_coord(pfileidx, grid_cgns_name, max_blocks)

  END SUBROUTINE plot_cgns_setup

  
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

    INTEGER :: iblock, zoneidx, ierr, physdim

                                ! open the CGNS file for writing

    CALL cg_open_f(filename, MODE_WRITE, fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
         &"cannot open " // TRIM(filename), fatal=.TRUE.)

  
                                ! create a single base

    IF (do2D) THEN
       physdim = 2
    ELSE 
       physdim = 3
    END IF
    CALL cg_base_write_f(fileidx, "MASS2", 2, 2, baseidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write base in " //&
         &TRIM(filename), fatal=.TRUE.)

                                ! create a zone for each block, and write the grid

    DO iblock = 1, max_blocks
       CALL plot_cgns_write_grid(fileidx, baseidx, block(iblock), iblock, docoord, zoneidx)
    END DO


  END SUBROUTINE plot_cgns_file_setup



  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_grid
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write_grid(fileidx, baseidx, blk, id, docoord, zoneidx)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, baseidx
    TYPE (block_struct), INTENT(INOUT) :: blk
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
          CALL block_collect(blk, blk%bv_x_grid)
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateX', &
               & blk%buffer(1:size(1), 1:size(2)))
          CALL block_collect(blk, blk%bv_y_grid)
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateY', &
               & blk%buffer(1:size(1), 1:size(2)))
          IF (.NOT. do2D) THEN
             CALL block_collect(blk, blk%bv_zbot_grid)
             CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateZ', &
                  & blk%buffer(1:size(1), 1:size(2)))
          END IF
       ELSE 
          CALL block_collect(blk, blk%bv_x_out)
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateX', &
               & blk%buffer(1:size(1), 1:size(2)))
          CALL block_collect(blk, blk%bv_y_out)
          CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateY', &
               & blk%buffer(1:size(1), 1:size(2)))
          IF (.NOT. do2D) THEN
             CALL block_collect(blk, blk%bv_zbot_out)
             CALL plot_cgns_write_coord(fileidx, baseidx, zoneidx, size, 'CoordinateZ', &
                  & blk%buffer(1:size(1), 1:size(2)))
          END IF
       END IF

       CALL cg_discrete_write_f(fileidx, baseidx, zoneidx, "GridMetrics", ddataidx, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot write descrete data", fatal=.TRUE.)
    
       CALL block_collect(blk, blk%bv_hp1)
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "hp1", &
            &blk%buffer(1:size(1), 1:size(2)))
       CALL block_collect(blk, blk%bv_hp2)
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "hp2", &
            &blk%buffer(1:size(1), 1:size(2)))
       CALL block_collect(blk, blk%bv_gp12)
       CALL plot_cgns_write_metric(fileidx, baseidx, zoneidx, ddataidx, size, "gp12", &
            &blk%buffer(1:size(1), 1:size(2)))
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
  ! SUBROUTINE plot_cgns_link_coord
  ! make links to the coordinates and
  ! metrics in the grid file
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_link_coord(fileidx, grid_cgns_name, nzones)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, nzones
    CHARACTER (LEN=*), INTENT(IN) :: grid_cgns_name
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_link_coord"
    INTEGER :: baseidx, zoneidx, ierr
    CHARACTER (LEN=1024) :: buffer
    
    baseidx = 1
  
    DO zoneidx = 1, nzones
       CALL cg_goto_f(fileidx, baseidx, ierr, 'Zone_t', zoneidx, "end")
       WRITE(buffer, '("MASS2/Block", I2.2, "/GridCoordinates")') zoneidx
       CALL cg_link_write_f('GridCoordinates', grid_cgns_name, buffer, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
            &"cannot link to grid coordinates path: " // buffer, fatal=.TRUE.)
       WRITE(buffer, '("MASS2/Block", I2.2, "/GridMetrics")') zoneidx
       CALL cg_link_write_f('GridMetrics', grid_cgns_name, buffer, ierr)
       IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, &
            &"cannot link to grid metrics path: " // buffer, fatal=.TRUE.)
       
    END DO
  END SUBROUTINE plot_cgns_link_coord


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_cgns
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_write(date_string, time_string, salinity, baro_press)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: date_string
    CHARACTER (LEN=*), INTENT(IN) :: time_string
    DOUBLE PRECISION, INTENT(IN) :: salinity, baro_press
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write"

    INTEGER :: iblock, ispecies, solidx, xmax, ymax, ierr, ifract, timeidx
    CHARACTER (LEN=32) :: timestamp
    DOUBLE PRECISION :: elapsed

    TYPE (block_struct), POINTER :: blk

    timestamp = TRIM(date_string) // " " // &
         &TRIM(time_string)

    elapsed = date_to_decimal(date_string, time_string)
    elapsed = elapsed/SECONDS

    IF (plot_cgns_start_time .LT. 0.0) THEN
       plot_cgns_start_time = elapsed
    END IF
    
    elapsed = elapsed - plot_cgns_start_time
    timeidx = MOD(plot_cgns_outstep, plot_cgns_maxtime) + 1
    tmp_times(timeidx) = elapsed;

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

          CALL block_collect(block(iblock), block(iblock)%bv_u_cart)
          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &block(iblock)%buffer, &
               &'VelocityX', 'Eastward Velocity', 'feet/second')

                                ! average v_cart

          CALL block_collect(block(iblock), block(iblock)%bv_v_cart)
          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &block(iblock)%buffer, &
               &'VelocityY', 'Northward Velocity', 'feet/second')

                                ! average depth

          CALL block_collect(block(iblock), block(iblock)%bv_depth)
          CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
               &block(iblock)%buffer, &
               &'depth', 'Water Depth', 'feet')

          IF (do_flow_diag) THEN

                                ! average u

             CALL block_collect(block(iblock), block(iblock)%bv_uvel_p)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'GridVelocityXi', 'Longitudinal Velocity', 'feet/second')

                                ! average v

             CALL block_collect(block(iblock), block(iblock)%bv_vvel_p)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'GridVelocityEta', 'Lateral Velocity', 'feet/second')

                                ! average wsel

             CALL block_collect(block(iblock), block(iblock)%bv_wsel)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'wsel', 'Water Surface Elevation', 'feet')


                                ! average velocity magnitude

             CALL block_collect(block(iblock), block(iblock)%bv_vmag)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'VelocityMagnitude', 'Velocity Magnitude', 'feet/second')

                                ! average bed shear

!!$             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                  &accum_block(iblock)%hydro%shear%sum, &
!!$                  &'shear', 'Bed Shear Stress', 'pound/foot^2')

                                ! Froude number

             CALL block_collect(block(iblock), block(iblock)%bv_froude_num)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'FroudeNumber', 'Froude Number', 'none')

                                ! Courant number

             CALL block_collect(block(iblock), block(iblock)%bv_courant_num)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'CourantNumber', 'Courant Number', 'none')

                                ! Eddy Viscosity

             CALL block_collect(block(iblock), block(iblock)%bv_eddy)
             CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
                  &block(iblock)%buffer, &
                  &'KinematicEddyViscosity', 'Kinematic Eddy Viscosity', 'feet^2/second')


                                ! average u

             CALL block_collect(block(iblock), block(iblock)%bv_uflux)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'GridFluxXi', 'Longitudinal Flux', 'feet^3/second')

                                ! average v

             CALL block_collect(block(iblock), block(iblock)%bv_vflux)
             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
                  &block(iblock)%buffer, &
                  &'GridFluxEta', 'Lateral Flux', 'feet^3/second')

          END IF

!!$                                ! Dry Cell Flag
!!$
!!$          IF (do_wetdry) THEN
!!$             
!!$             CALL plot_cgns_write_flag(iblock, solidx, xmax,  ymax, &
!!$                  &block(iblock)%isdry, 'isdry', 'Dry Cell Flag')
!!$
!!$          END IF
!!$                                ! Dead Cell Flag
!!$
!!$          IF (do_rptdead) THEN
!!$
!!$             CALL plot_cgns_write_flag(iblock, solidx, xmax,  ymax, &
!!$                  &block(iblock)%isdead(:,:)%p, 'isdead', 'Dead Cell Flag')
!!$
!!$          END IF

       END IF

!!$       IF (do_transport) THEN
!!$          DO ispecies = 1, max_species
!!$             CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                  &accum_block(iblock)%conc(ispecies)%sum, &
!!$                  &scalar_source(ispecies)%name,  &
!!$                  &scalar_source(ispecies)%description, &
!!$                  &scalar_source(ispecies)%units,&
!!$                  &scalar_source(ispecies)%conversion) 
!!$
!!$             SELECT CASE(scalar_source(ispecies)%srctype)
!!$             CASE (TDG)
!!$                                ! TDG pressure
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                     &accum_block(iblock)%tdg%press%sum, &
!!$                     &"tdgpress",  'Total Dissolved Gas Pressure', "millimeters of Hg") 
!!$
!!$                                ! TDG delta P
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                     &accum_block(iblock)%tdg%deltap%sum, &
!!$                     &"tdgdeltap",  'Total Dissolved Gas Pressure Delta', "millimeters of Hg") 
!!$
!!$                                ! TDG percent saturation
!!$                
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                     &accum_block(iblock)%tdg%sat%sum, &
!!$                     &"tdgsat",  'Total Dissolved Gas Saturation', "percent") 
!!$
!!$
!!$             CASE (GEN)
!!$                                ! if we have a bed, report the amounts
!!$                                ! of dissolved scalars in the bed pore
!!$                                ! water
!!$
!!$                IF (source_doing_sed) THEN
!!$
!!$                   ! dissolved contaminant mass in bed pores
!!$
!!$                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                        &accum_block(iblock)%bed%mass(ispecies)%sum, &
!!$                        &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
!!$                        &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                        &"mass")
!!$                   
!!$                   ! dissolved contaminant mass per unit bed area
!!$
!!$                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                        &accum_block(iblock)%bed%mass(ispecies)%sum, &
!!$                        &TRIM(scalar_source(ispecies)%name) // '-bed', &
!!$                        &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                        &"mass/foot^2")
!!$                   
!!$                   ! dissolved contaminant mass per unit volume bed pore water
!!$
!!$                   CALL plot_cgns_write_var(iblock, solidx, xmax,  ymax, &
!!$                        &accum_block(iblock)%bed%pore(ispecies)%sum, &
!!$                        &TRIM(scalar_source(ispecies)%name) // '-pore', &
!!$                        &"Concentration of " // TRIM(scalar_source(ispecies)%description) // " in Bed Pores", &
!!$                        &scalar_source(ispecies)%units, &
!!$                        &scalar_source(ispecies)%conversion)
!!$
!!$
!!$                END IF
!!$                   
!!$             CASE (SED)
!!$                                ! If we are doing sediment, output
!!$                                ! sediment erosion and deposition
!!$                                ! rates
!!$                ifract = scalar_source(ispecies)%sediment_param%ifract
!!$
!!$                                ! deposition rate
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%deposit(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-depos',&
!!$                     &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description),&
!!$                     &"mass/foot^2/second")
!!$
!!$                                ! erosion rate
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%erosion(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-erode', &
!!$                     &"Rate of Erosion of " // TRIM(scalar_source(ispecies)%description),&
!!$                     &"mass/foot^2/second")
!!$
!!$                                ! bed total mass
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%mass(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
!!$                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                     &"mass")
!!$                
!!$                                ! bed mass per unit area
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%conc(ispecies)%sum,  &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-bed', &
!!$                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                     &"mass/foot^2")
!!$
!!$             CASE (PART)
!!$                                ! If we are doing particulate phases,
!!$                                ! output erosion and deposition rates
!!$
!!$                                ! particulate deposition rate
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%deposit(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-depos', &
!!$                     &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description), &
!!$                     &"mass/foot^2/second") 
!!$
!!$                                ! particulate bed mass
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%mass(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
!!$                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                     &"mass")
!!$
!!$                                ! particulate bed mass per unit area
!!$
!!$                CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$                     &accum_block(iblock)%bed%conc(ispecies)%sum, &
!!$                     &TRIM(scalar_source(ispecies)%name) // '-bed', &
!!$                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
!!$                     &"mass/foot^2")
!!$
!!$             END SELECT
!!$          END DO
!!$       END IF

                                ! bed depth if called for
!!$       IF (source_doing_sed) THEN
!!$          CALL plot_cgns_write_var(iblock, solidx, xmax, ymax, &
!!$               &accum_block(iblock)%bed%depth%sum, &
!!$               &"beddepth", "Depth of Bed Sediments", "feet")
!!$       END IF

    END DO

                                ! if we have written the specified
                                ! number of time slices, make a new
                                ! file

    plot_cgns_outstep = plot_cgns_outstep + 1
    IF (MOD(plot_cgns_outstep, plot_cgns_maxtime) .EQ. 0) THEN
       CALL plot_cgns_file_close(pfileidx, pbaseidx)
       CALL plot_cgns_make_name(plot_file_name)
       CALL plot_cgns_file_setup(plot_file_name, .FALSE., pfileidx, pbaseidx)
       CALL plot_cgns_link_coord(pfileidx, grid_cgns_name, max_blocks)
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
    CHARACTER (LEN=1024) :: buffer
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

  ! ! ----------------------------------------------------------------
  ! ! SUBROUTINE plot_cgns_write_flag
  ! ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_write_flag(zoneidx, solidx, xmax, ymax, flag, name, desc)

  !   IMPLICIT NONE

  !   INTEGER, INTENT(IN) :: zoneidx, solidx
  !   INTEGER, INTENT(IN) :: xmax, ymax
  !   LOGICAL, INTENT(IN) :: flag(i_index_min:,j_index_min:)
  !   CHARACTER (LEN=*), INTENT(IN) :: name, desc
  !   INTEGER :: i, j, pos, fldidx, ierr
  !   CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_write_flag"
  !   CHARACTER (LEN=32) :: buffer
    
  !   tmp_int = 0

  !   IF (plot_cgns_docell) THEN

  !      DO j = 2, ymax
  !         DO i = 2, xmax
  !            pos = (i-1) + (j-2)*(xmax-1)
  !            IF (flag(i,j)) tmp_int(pos) = 1
  !         END DO
  !      END DO
       
  !   ELSE

  !      DO j = 1, ymax+1
  !         DO i = 1, xmax+1
  !            pos = (i) + (j-1)*(xmax + 1)
  !            IF (flag(i,j)) tmp_int(pos) = 1
  !         END DO
  !      END DO

  !   END IF

  !   buffer = name
  !   CALL cg_field_write_f(pfileidx, pbaseidx, zoneidx, solidx, &
  !        &Integer, buffer, tmp_int, fldidx, ierr)

  !   IF (ierr .EQ. ERROR) THEN
  !      WRITE(buffer, *) 'cannot write flag "', TRIM(name), '"'
  !      CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
  !   END IF

  !   IF (plot_cgns_dodesc) THEN 
  !                               ! put a description for the field

  !      CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
  !           &'Zone_t', zoneidx, &
  !           &'FlowSolution_t', solidx, &
  !           &'DataArray_t', fldidx,&
  !           &'end')
  !      IF (ierr .EQ. ERROR) THEN
  !         WRITE(buffer, *) 'cannot write description for "', TRIM(name), '"'
  !         CALL plot_cgns_error(func, buffer, fatal=.TRUE.)
  !      END IF
       
  !      CALL cg_descriptor_write_f('Description', desc, ierr)
  !   END IF

  ! END SUBROUTINE plot_cgns_write_flag

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_file_close
  ! This closes the plot file and adds links to the grid
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_file_close(fileidx, baseidx)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: fileidx, baseidx
    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_file_close"
    INTEGER :: ierr, times(2)

                                ! figure out how many time planes are
                                ! stored in this file

    times(2) = 0

    IF (plot_cgns_outstep .GT. 0) THEN
       times(1) = MOD(plot_cgns_outstep, plot_cgns_maxtime) 
       IF (times(1) .EQ. 0) THEN
          times = plot_cgns_maxtime
       END IF
       CALL cg_simulation_type_write_f(fileidx, baseidx, TimeAccurate, ierr)
       IF (ierr .EQ. CG_ERROR) &
            &CALL plot_cgns_error(func, "cannot write simulation type", fatal=.FALSE.)
       CALL cg_biter_write_f(fileidx, baseidx, "TransientBase", times, ierr)
       IF (ierr .EQ. CG_ERROR) &
            &CALL plot_cgns_error(func, "cannot write base iterative data", fatal=.FALSE.)
       CALL cg_goto_f(pfileidx, pbaseidx, ierr, &
            &'BaseIterativeData_t', 1, 'end')
       CALL cg_array_write_f('TimeValues', RealSingle, 1, times, tmp_times, ierr)
       IF (ierr .EQ. ERROR) THEN
          CALL plot_cgns_error(func, "cannot write TimeValues in BaseIterativeData node", fatal=.TRUE.)
       END IF
    END IF

                                ! close the file that is now open

    CALL cg_close_f(fileidx, ierr)
    IF (ierr .EQ. ERROR) CALL plot_cgns_error(func, "cannot close file", fatal=.TRUE.)

  END SUBROUTINE plot_cgns_file_close

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_cgns_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_cgns_close()

    IMPLICIT NONE

    CHARACTER (LEN=20), PARAMETER :: func = "plot_cgns_close"

    CALL plot_cgns_file_close(pfileidx, pbaseidx)

    DEALLOCATE(tmp_real, tmp_double, tmp_times)


  END SUBROUTINE plot_cgns_close


END MODULE plot_cgns
