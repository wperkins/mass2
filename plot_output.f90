! ----------------------------------------------------------------
! file: plot_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 21, 1999 by William A. Perkins
! Last Change: Fri Jan 10 13:17:25 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE plot_output
! ----------------------------------------------------------------
MODULE plot_output

  USE accumulator
  USE misc_vars, ONLY: do_accumulate, &
       &i_index_min, i_index_extra, j_index_min, j_index_extra

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

                                ! module constants

  INTEGER, PARAMETER, PUBLIC :: plot_iounit=12
  LOGICAL, PARAMETER, PUBLIC :: plot_do_tecplot = .false.
  LOGICAL, PARAMETER, PUBLIC :: plot_do_netcdf = (.not. plot_do_tecplot)
  CHARACTER (LEN=80), PARAMETER :: plot_ioname = 'plot.dat'

  INTEGER, PRIVATE :: plot_ncid
  CHARACTER (LEN=80), PARAMETER :: plot_ncname = 'plot.nc'
  INTEGER, PRIVATE :: tslen = 20
  INTEGER, PRIVATE :: block_dimid, eta_dimid, xi_dimid, time_dimid, tslen_dimid
  INTEGER, PRIVATE :: etamax_varid, ximax_varid
  INTEGER, PRIVATE :: x_varid, y_varid, zbot_varid, time_varid, ts_varid
  INTEGER, PRIVATE :: hp1_varid, hp2_varid, gp12_varid, area_varid
  INTEGER, PRIVATE :: u_varid, v_varid, ucart_varid, vcart_varid, vmag_varid, shear_varid
  INTEGER, PRIVATE :: depth_varid, wsel_varid
  INTEGER, PRIVATE, POINTER :: scalar_varid(:)
  INTEGER, PRIVATE :: press_varid, dp_varid, sat_varid
  INTEGER, PRIVATE :: courant_varid, froude_varid, isdry_varid, isdead_varid
  INTEGER, PRIVATE, POINTER :: part_depos_varid(:), depos_varid(:), erode_varid(:)
  INTEGER, PRIVATE, POINTER :: bedsed_varid(:), bedmass_varid(:)
  INTEGER, PRIVATE, POINTER :: beddis_varid(:), bedpore_varid(:), bedporemass_varid(:)
  INTEGER, PRIVATE, POINTER :: bedpart_varid(:), bedpartmass_varid(:)
  INTEGER, PRIVATE :: beddepth_varid
  REAL, ALLOCATABLE, PRIVATE :: nctmp_real(:,:,:)
  DOUBLE PRECISION, ALLOCATABLE, PRIVATE :: nctmp_double(:,:,:), blktmp_double(:,:)

  INTEGER, PARAMETER, PRIVATE :: diag_plot_iounit=20
  CHARACTER (LEN=80), PARAMETER :: diag_plot_ioname = 'diagnostic_plot.dat'

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_zone_name
  ! ----------------------------------------------------------------
  SUBROUTINE plot_zone_name(date_string, time_string, zone_name)
    
    IMPLICIT NONE
    CHARACTER*(*) :: date_string, time_string, zone_name

    zone_name(1:10) = date_string
    zone_name(11:11) = ' '
    zone_name(12:19) = time_string
    zone_name(20:20) = ' '
    zone_name(21:26) = 'block '

  END SUBROUTINE plot_zone_name

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup_tecplot()

    IMPLICIT NONE

    OPEN(plot_iounit,file=plot_ioname)
    WRITE(plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
    WRITE(plot_iounit,100)
100 FORMAT("variables=""x"" ""y"" ""u cart"" ""v cart"" ""depth"" ""zbot"" ""wsel"" &
           &            ""TDG con"" ""Temp"" ""TGP"" ""TDG delP"" ""%Sat""")
    

  END SUBROUTINE plot_file_setup_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_tecplot(date_string, time_string, salinity, baro_press)

    USE globals
    USE scalars
    USE gas_functions
    
    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: salinity, baro_press, ccstar, conc_TDG, t_water
    CHARACTER*26 :: zone_name
    INTEGER :: i, j, iblock

    CALL plot_zone_name(date_string, time_string, zone_name)

    DO iblock=1,max_blocks
       WRITE(plot_iounit,*) "zone f=block", " t=""", zone_name, iblock , """",&
            &" i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
       WRITE(plot_iounit,*)block(iblock)%x
       WRITE(plot_iounit,*)block(iblock)%y
       ! WRITE(plot_iounit,*)block(iblock)%uvel_p
       ! WRITE(plot_iounit,*)block(iblock)%vvel_p
       WRITE(plot_iounit,*)block(iblock)%u_cart
       WRITE(plot_iounit,*)block(iblock)%v_cart
       WRITE(plot_iounit,*)block(iblock)%depth
       WRITE(plot_iounit,*)block(iblock)%zbot
       WRITE(plot_iounit,*)block(iblock)%wsel

                                ! need to fix for transport

!        WRITE(plot_iounit,*)species(1)%scalar(iblock)%conc
!        WRITE(plot_iounit,*)species(2)%scalar(iblock)%conc

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasPress(conc_TDG,  t_water,  salinity)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasDP(conc_TDG, t_water,  salinity,  baro_press)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

!        DO i=1,block(iblock)%xmax+1
!           DO j=1,block(iblock)%ymax+1
!              conc_TDG = species(1)%scalar(iblock)%conc(i,j)
!              t_water = species(2)%scalar(iblock)%conc(i,j)
!              block(iblock)%TDG_stuff(i,j) = &
!                   &TDGasSaturation( conc_TDG, t_water,  salinity, baro_press)
!           END DO
!        END DO
!        WRITE(plot_iounit,*)block(iblock)%TDG_stuff

    END DO
  END SUBROUTINE plot_print_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_set_var_attributes
  ! General attributes used for (floating point) spatial and temporal
  ! variables in the plot file
  ! ----------------------------------------------------------------
  SUBROUTINE plot_set_var_attributes(plot_ncid, varid, desc, units, isbed)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    INTEGER, INTENT(IN) :: plot_ncid, varid
    CHARACTER (LEN=*), INTENT(IN) :: desc, units
    LOGICAL, INTENT(IN) :: isbed

    INTEGER :: xtype, ncstat

    ncstat = nf_put_att_text (plot_ncid, varid, "Units", LEN_TRIM(units), units)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, varid, "Description", LEN_TRIM(desc), desc)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! these fit the attribute conventions
                                ! in the NetCDF manual

    ncstat = nf_put_att_text (plot_ncid, varid, "units", LEN_TRIM(units), units)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text (plot_ncid, varid, "long_name", LEN_TRIM(desc), desc)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! choose a fill value based on the
                                ! variable type

    ncstat = nf_inq_vartype(plot_ncid, varid, xtype)
    SELECT CASE (xtype)
    CASE (NF_FLOAT)
       ncstat = nf_put_att_real (plot_ncid, varid, "_FillValue", NF_FLOAT, 1, nf_fill_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    CASE (NF_DOUBLE)
       ncstat = nf_put_att_double (plot_ncid, varid, "_FillValue", NF_DOUBLE, 1, nf_fill_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    END SELECT

                                ! mark bed variables 

    IF (isbed) THEN
       ncstat = nf_put_att_text (plot_ncid, varid, "isbed", 4, "true")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    END IF
    
  END SUBROUTINE plot_set_var_attributes

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION plot_add_space_var
  ! Adds a variable to the plot file that is not time-varying, but is
  ! spatially-varying
  ! ----------------------------------------------------------------
  INTEGER FUNCTION plot_add_space_var(name, desc, units)

    IMPLICIT NONE
    CHARACTER (LEN=*) :: name, desc, units
    INTEGER :: varid, dimids(3), ncstat

    INCLUDE 'netcdf.inc'

    dimids(1) = block_dimid
    dimids(2) = eta_dimid
    dimids(3) = xi_dimid

    ncstat = nf_def_var (plot_ncid, name, nf_float, 3, dimids, varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    CALL plot_set_var_attributes(plot_ncid, varid, desc, units, .FALSE.)

    plot_add_space_var = varid

  END FUNCTION plot_add_space_var

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION plot_add_tim_var
  ! Adds a variable to the plot file that is time- and spatially-varying
  ! ----------------------------------------------------------------
  INTEGER FUNCTION plot_add_time_var(name, desc, units, flag)

    IMPLICIT NONE
    CHARACTER (LEN=*) :: name, desc, units
    LOGICAL, OPTIONAL :: flag
    INTEGER :: varid, dimids(4), ncstat
    LOGICAL :: isbed

    INCLUDE 'netcdf.inc'

    IF (PRESENT(flag)) THEN
       isbed = flag
    ELSE
       isbed = .FALSE.
    END IF

    dimids(1) = block_dimid
    dimids(2) = eta_dimid
    dimids(3) = xi_dimid
    dimids(4) = time_dimid

    ncstat = nf_def_var (plot_ncid, name, nf_float, 4, dimids, varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    CALL plot_set_var_attributes(plot_ncid, varid, desc, units, isbed)

    plot_add_time_var = varid

  END FUNCTION plot_add_time_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup_netcdf()

    USE globals
    USE misc_vars, ONLY: do_flow, do_flow_output, do_flow_diag, do_transport, do_wetdry, do_rptdead
    USE scalars, ONLY: max_species
    USE scalars_source

    IMPLICIT NONE

    INTEGER :: ncstat, dimids(10)
    INTEGER :: iblock, index(10), len(10)
    INTEGER :: i, j, ispecies, max_x, max_y, ifract
    CHARACTER (LEN=128) :: buffer

    INCLUDE 'netcdf.inc'

    ncstat = nf_create (plot_ncname, IOR(IOR(nf_write, nf_clobber), nf_fill), plot_ncid)
    ncstat = nf_def_dim (plot_ncid, "block", max_blocks, block_dimid)

                                ! limits of individual block dimensions

    dimids(1) = block_dimid
    ncstat = nf_def_var (plot_ncid, "etamax", nf_int, 1, dimids, etamax_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_var (plot_ncid, "ximax", nf_int, 1, dimids, ximax_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! limits for data

    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1
    ncstat = nf_def_dim (plot_ncid, "eta", max_x, eta_dimid) 
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "xi", max_y, xi_dimid) 
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "time", nf_unlimited, time_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_def_dim (plot_ncid, "tslen", tslen, tslen_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! time variables

    dimids(1) = time_dimid
    ncstat = nf_def_var (plot_ncid, "time", nf_double, 1, dimids, time_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    ncstat = nf_put_att_text(plot_ncid, time_varid, "Units", &
         &32, "days since 1900-01-01 00:00:00")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    dimids(1) = tslen_dimid
    dimids(2) = time_dimid
    ncstat = nf_def_var (plot_ncid, "timestamp", nf_char, 2, dimids, ts_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! non-time-dependant data variables

    dimids(1) = block_dimid
    dimids(2) = eta_dimid
    dimids(3) = xi_dimid

                                ! easting (x) and northing (y) have to
                                ! be double precision

    ncstat = nf_def_var (plot_ncid, "x", nf_double, 3, dimids, x_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    CALL plot_set_var_attributes(plot_ncid, x_varid, "Easting", "feet", .FALSE.)

    ncstat = nf_def_var (plot_ncid, "y", nf_double, 3, dimids, y_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
    CALL plot_set_var_attributes(plot_ncid, y_varid, "Northing", "feet", .FALSE.)

    zbot_varid = plot_add_space_var("zbot", "Bottom Elevation", "feet")
    hp1_varid = plot_add_space_var("hp1", "Grid Metric hp1", "feet")
    hp2_varid = plot_add_space_var("hp2", "Grid Metric hp2", "feet")
    gp12_varid = plot_add_space_var("gp12", "Grid Metric gp12", "feet")

                                ! time-dependant data variables

    dimids(4) = time_dimid

    IF (do_flow .OR. do_flow_output) THEN

       ucart_varid = plot_add_time_var("ucart", "Eastward Velocity", "feet/second")
       vcart_varid = plot_add_time_var("vcart", "Northward Velocity", "feet/second")
       depth_varid = plot_add_time_var("depth", "Water Depth", "feet")

       
                                ! diagnostic values

       IF (do_flow_diag) THEN
          u_varid = plot_add_time_var("uvel", "Longitudinal Velocity", "feet/second")
          v_varid = plot_add_time_var("vvel", "Lateral Velocity", "feet/second")
          vmag_varid = plot_add_time_var("vmag", "Velocity Magnitude", "feet/second")
          wsel_varid = plot_add_time_var("wsel", "Water Surface Elevation", "feet")
          shear_varid = plot_add_time_var("shear", "Bed Shear Stress", "pound/foot^2")
          courant_varid = plot_add_time_var("courant", "Courant Number", "none")
          froude_varid = plot_add_time_var("froude", "Froude Number", "none")
       END IF

       IF (do_wetdry) THEN
          isdry_varid = plot_add_time_var("isdry", "Dry Cell Flag", "none")
       END IF

       IF (do_rptdead) THEN
          isdead_varid = plot_add_time_var("isdead", "Dead Cell Flag", "none")
       END IF
    END IF

                                ! water quality variables

    IF (do_transport) THEN
       ALLOCATE(scalar_varid(max_species))

       IF (source_doing_sed) THEN
          ALLOCATE(depos_varid(sediment_fractions))
          ALLOCATE(erode_varid(sediment_fractions))
          ALLOCATE(part_depos_varid(max_species))
          ALLOCATE(bedsed_varid(sediment_fractions))
          ALLOCATE(bedmass_varid(sediment_fractions))
          ALLOCATE(bedpart_varid(max_species))
          ALLOCATE(bedpartmass_varid(max_species))
          ALLOCATE(bedpore_varid(max_species))
          ALLOCATE(bedporemass_varid(max_species))
          ALLOCATE(beddis_varid(max_species))
       END IF

       DO ispecies = 1, max_species

          scalar_varid(ispecies) = plot_add_time_var(scalar_source(ispecies)%name, &
               &scalar_source(ispecies)%description, scalar_source(ispecies)%units)
          ncstat = nf_put_att_double (plot_ncid, scalar_varid(ispecies), &
               &"Conversion", NF_DOUBLE, 1,scalar_source(ispecies)%conversion)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

          SELECT CASE(scalar_source(ispecies)%srctype)
          CASE (TDG)

                                ! TDG pressure

             press_varid = plot_add_time_var("tdgpress", &
                  &"Total Dissolved Gas Pressure", "millimeters of Hg")

                                ! TDG delta P

             dp_varid = plot_add_time_var("tdgdeltap", &
                  &"Total Dissolved Gas Pressure Delta", "millimeters of Hg")

                                ! TDG percent saturation

             sat_varid = plot_add_time_var("tdgsat", &
                  &"Total Dissolved Gas Saturation", "percent")

          CASE (GEN)
             IF (source_doing_sed) THEN
                
                                ! dissolved mass in bed pores

                bedporemass_varid(ispecies) = plot_add_time_var(&
                     &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass", .TRUE.)

                                ! dissolved mass per unit area in bed

                bedpore_varid(ispecies) = plot_add_time_var(&
                     &TRIM(scalar_source(ispecies)%name) // '-bed', &
                     &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                     &"mass/foot^2", .TRUE.)

                                ! dissolved mass per unit volume in bed pores

                beddis_varid(ispecies) = plot_add_time_var(&
                     &TRIM(scalar_source(ispecies)%name) // '-pore', &
                     &"Concentration of " // TRIM(scalar_source(ispecies)%description) // " in Bed Pores", &
                     &scalar_source(ispecies)%units, .TRUE.)
                ncstat = nf_put_att_double (plot_ncid, beddis_varid(ispecies), &
                     &"Conversion", NF_DOUBLE, 1,scalar_source(ispecies)%conversion)
                IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
                
             END IF


          CASE (SED)

             ifract = scalar_source(ispecies)%sediment_param%ifract

                                ! sediment deposition rate

             depos_varid(ifract) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-depos',&
                  &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description),&
                  &"mass/foot^2/second", .TRUE.)

                                ! sediment erosion rate

             erode_varid(ifract) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-erode', &
                  &"Rate of Erosion of " // TRIM(scalar_source(ispecies)%description),&
                  &"mass/foot^2/second", .TRUE.)

                                ! sediment mass in bed

             bedmass_varid(ifract) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                  &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                  &"mass", .TRUE.)

                                ! sediment mass per unit area in bed

             bedsed_varid(ifract) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-bed', &
                  &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                  &"mass/foot^2", .TRUE.)

          CASE (PART)

             buffer = scalar_source(scalar_source(ispecies)%part_param%disidx)%name

             ncstat = nf_put_att_text (plot_ncid, scalar_varid(ispecies), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! particulate deposition rate

             part_depos_varid(ispecies) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-depos', &
                  &"Rate of Deposition of " // TRIM(scalar_source(ispecies)%description), &
                  &"mass/foot^2/second", .TRUE.)
             ncstat = nf_put_att_text (plot_ncid, part_depos_varid(ispecies), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! sediment mass in bed

             bedpartmass_varid(ispecies) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-bedmass', &
                  &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                  &"mass", .TRUE.)
             ncstat = nf_put_att_text (plot_ncid, bedpartmass_varid(ispecies), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! sediment mass per unit area in bed

             bedpart_varid(ispecies) = plot_add_time_var(&
                  &TRIM(scalar_source(ispecies)%name) // '-bed', &
                  &"Mass of " // TRIM(scalar_source(ispecies)%description) // " in Bed", &
                  &"mass/foot^2", .TRUE.)
             ncstat = nf_put_att_text (plot_ncid, bedpart_varid(ispecies), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

          END SELECT
       END DO
    END IF

                                ! bed depth if called for

    IF (source_doing_sed) THEN
       beddepth_varid = plot_add_time_var("beddepth", "Depth of Bed Sediments", "feet", .TRUE.)
    END IF
                                ! done w/ file definition

    ncstat = nf_enddef(plot_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! write block sizes to data 

    ALLOCATE(nctmp_real(1, max_x, max_y))
    ALLOCATE(nctmp_double(1, max_x, max_y))
    ALLOCATE(blktmp_double(i_index_min:(max_x - 1 + i_index_extra), &
         &j_index_min:(max_y - 1 + j_index_extra)))

    DO iblock = 1, max_blocks

       index(1) = iblock

       ncstat = nf_put_var1_int (plot_ncid, etamax_varid, index, block(iblock)%xmax + 1)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       ncstat = nf_put_var1_int (plot_ncid, ximax_varid, index, block(iblock)%ymax + 1)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       index(1) = iblock
       index(2) = 1
       index(3) = 1
       len(1) = 1
       len(2) = max_x
       len(3) = max_y
       ! len(2) = block(iblock)%xmax + 1
       ! len(3) = block(iblock)%ymax + 1

                                ! grid coordinates: x

       nctmp_double = nf_fill_double
       nctmp_double(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%x_out(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_double(plot_ncid, x_varid, index, len, nctmp_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! grid coordinates: x

       nctmp_double = nf_fill_double
       nctmp_double(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%y_out(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_double(plot_ncid, y_varid, index, len, nctmp_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! bottom elevation

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%zbot_out(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, zbot_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%hp1(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, hp1_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%hp2(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, hp2_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

       nctmp_real = nf_fill_real
       nctmp_real(1,1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1) = &
            &block(iblock)%gp12(1:block(iblock)%xmax + 1,1:block(iblock)%ymax + 1)
       ncstat = nf_put_vara_real(plot_ncid, gp12_varid, index, len, nctmp_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    END DO

    CALL accum_initialize()

  END SUBROUTINE plot_file_setup_netcdf

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_time_var
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_time_var(varid, start, length, xmax, ymax, var, convert)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    INTEGER, INTENT(IN) :: varid, start(:), length(:), xmax, ymax
    DOUBLE PRECISION, INTENT(IN) :: &
         &var(i_index_min:,j_index_min:)
    DOUBLE PRECISION, INTENT(IN), OPTIONAL :: convert
    INTEGER :: ncstat, i, j
    DOUBLE PRECISION :: conversion

    IF (.NOT. PRESENT(convert)) THEN
       conversion = 1.0
    ELSE
       conversion = convert
    END IF

    nctmp_real = nf_fill_real
    DO i = 1, xmax + 1
       DO j = 1, ymax + 1
          IF (DABS(var(i,j)) .LT. 1.0d-37) THEN
              nctmp_real(1, i, j) = 0.0
           ELSE
              nctmp_real(1, i, j) = SNGL(var(i, j)/conversion)
           END IF
        END DO
     END DO
     i = 1
     nctmp_real(1, i, 1:ymax+1) = &
          &0.5*SNGL((var(i,1:ymax+1) + var(i+1,1:ymax+1))/conversion)
     i = xmax + 1
     nctmp_real(1, i, 1:ymax+1) = &
          &0.5*SNGL((var(i,1:ymax+1) + var(i-1,1:ymax+1))/conversion)

     ncstat = nf_put_vara_real(plot_ncid, varid, start, length, nctmp_real)
     IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

  END SUBROUTINE plot_print_time_var


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print_netcdf(date_string, time_string, salinity, baro_press)

    USE globals
    USE misc_vars, ONLY: do_flow, do_flow_output, do_flow_diag, do_transport, do_wetdry, do_rptdead
    USE scalars
    USE scalars_source
    USE date_time
    USE gas_functions
    USE bed_module

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    CHARACTER (LEN=tslen) :: timestamp
    DOUBLE PRECISION :: salinity, baro_press
    INTEGER :: ncstat
    INTEGER :: start(4), length(4)
    INTEGER :: iblock, i, j, ispecies, max_x, max_y, trec, max_blkx, max_blky, ifract
    DOUBLE PRECISION :: conc_TDG, t_water

    max_x = MAXVAL(block(:)%xmax) + 1
    max_y = MAXVAL(block(:)%ymax) + 1

    ncstat = nf_inq_dimlen(plot_ncid, time_dimid, trec)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

                                ! do the time stamp

    timestamp(1:10) = accum_time%date_string
    timestamp(11:11) = ' '
    timestamp(12:19) = accum_time%time_string
    timestamp(tslen:tslen) = CHAR(0)

    start(1) = 1
    start(2) = trec + 1
    length(1) = tslen
    length(2) = 1
    ncstat = nf_put_vara_text(plot_ncid, ts_varid, start, length, timestamp)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

    start(1) = trec + 1
    ncstat = nf_put_var1_double(plot_ncid, time_varid, start, &
         &date_to_decimal(date_string, time_string))
    
    start(2) = 1
    start(3) = 1
    start(4) = trec + 1

    length(1) = 1
    length(2) = max_x
    length(3) = max_y
    length(4) = 1

    DO iblock = 1, max_blocks

       max_blkx = block(iblock)%xmax + 1
       max_blky = block(iblock)%ymax + 1
       ! length(2) = max_blkx
       ! length(3) = max_blky

       start(1) = iblock

       IF (do_flow .OR. do_flow_output) THEN

                                ! average u_cart

          CALL plot_print_time_var(ucart_varid, start, length, &
               &block(iblock)%xmax, block(iblock)%ymax,&
               &accum_block(iblock)%hydro%ucart%sum)
          
                                ! average v_cart

          CALL plot_print_time_var(vcart_varid, start, length, &
               &block(iblock)%xmax, block(iblock)%ymax,&
               &accum_block(iblock)%hydro%vcart%sum)

                                ! average depth

          CALL plot_print_time_var(depth_varid, start, length, &
               &block(iblock)%xmax, block(iblock)%ymax,&
               &accum_block(iblock)%hydro%depth%sum)

          IF (do_flow_diag) THEN

                                ! average u

             CALL plot_print_time_var(u_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%uvelp%sum)
             
                                ! average v

             CALL plot_print_time_var(v_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%vvelp%sum)

                                ! average wsel

             CALL plot_print_time_var(wsel_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%wsel%sum)

                                ! average vmag

             CALL plot_print_time_var(vmag_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%vmag%sum)

                                ! average bed shear

             CALL plot_print_time_var(shear_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%shear%sum)

                                ! Froude number

             CALL plot_print_time_var(froude_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%froude%sum)

                                ! Courant number

             CALL plot_print_time_var(courant_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%hydro%courant%sum)

          END IF

                                ! Dry Cell Flag

          IF (do_wetdry) THEN
!!$             blktmp_double = 0.0
!!$             WHERE (block(iblock)%isdry) blktmp_double = 1.0
             blktmp_double = 0.0
             DO i = 1, block(iblock)%xmax + 1
                DO j = 1, block(iblock)%ymax + 1
                   IF (block(iblock)%isdry(i,j)) blktmp_double(i,j) = 1.0
                END DO
             END DO
             CALL plot_print_time_var(isdry_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &blktmp_double)
          END IF

          IF (do_rptdead) THEN
             blktmp_double = 0.0
             DO i = 1, block(iblock)%xmax + 1
                DO j = 1, block(iblock)%ymax + 1
                   IF (block(iblock)%isdead(i,j)%p) blktmp_double(i,j) = 1.0
                END DO
             END DO
             CALL plot_print_time_var(isdead_varid, start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &blktmp_double)
          END IF

       END IF

                                ! extra water quality variables

       IF (do_transport) THEN
          DO ispecies = 1, max_species
             CALL plot_print_time_var(scalar_varid(ispecies), start, length, &
                  &block(iblock)%xmax, block(iblock)%ymax,&
                  &accum_block(iblock)%conc(ispecies)%sum, &
                  &scalar_source(ispecies)%conversion)

             SELECT CASE (scalar_source(ispecies)%srctype)

                                ! If we are doing TDG we need to
                                ! output several variations of gas
                                ! "concentration"

             CASE (TDG)
                                ! computed: TDG pressure

                CALL plot_print_time_var(press_varid, start, length, &
                     &block(iblock)%xmax, block(iblock)%ymax,&
                     &accum_block(iblock)%tdg%press%sum)

                                ! computed: TDG delta P

                CALL plot_print_time_var(dp_varid, start, length, &
                     &block(iblock)%xmax, block(iblock)%ymax,&
                     &accum_block(iblock)%tdg%deltap%sum)

                                ! computed: TDG saturation

                CALL plot_print_time_var(sat_varid, start, length, &
                     &block(iblock)%xmax, block(iblock)%ymax,&
                     &accum_block(iblock)%tdg%sat%sum)
                   
                CASE (GEN)
                   IF (source_doing_sed) THEN

                                ! dissolved contaminant mass in bed pores
                   
                      CALL plot_print_time_var(bedporemass_varid(ispecies), start, length, &
                           &block(iblock)%xmax, block(iblock)%ymax,&
                           &accum_block(iblock)%bed%mass(ispecies)%sum)
                      
                      ! dissolved contaminant mass per unit bed area

                      CALL plot_print_time_var(bedpore_varid(ispecies), start, length, &
                           &block(iblock)%xmax, block(iblock)%ymax,&
                           &accum_block(iblock)%bed%conc(ispecies)%sum)

                                ! dissolved contaminant mass per unit volume bed pore water
                   
                      CALL plot_print_time_var(beddis_varid(ispecies), start, length, &
                           &block(iblock)%xmax, block(iblock)%ymax,&
                           &accum_block(iblock)%bed%pore(ispecies)%sum, &
                           &scalar_source(ispecies)%conversion)

                   END IF


                                ! If we are doing sediment, output
                                ! sediment erosion and deposition
                                ! rates

                CASE (SED)
                   ifract = scalar_source(ispecies)%sediment_param%ifract

                                ! deposition rate

                   CALL plot_print_time_var(depos_varid(ifract), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%deposit(ispecies)%sum)

                   
                                ! erosion rate

                   CALL plot_print_time_var(erode_varid(ifract), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%erosion(ispecies)%sum)

                                ! bed total mass

                   CALL plot_print_time_var(bedmass_varid(ifract), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%mass(ispecies)%sum)
                   
                                ! bed mass per unit area

                   CALL plot_print_time_var(bedsed_varid(ifract), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%conc(ispecies)%sum)

                CASE (PART)

                                ! particulate deposition rate

                   CALL plot_print_time_var(part_depos_varid(ispecies), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%deposit(ispecies)%sum)

                                ! particulate bed mass

                   CALL plot_print_time_var(bedpartmass_varid(ispecies), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%mass(ispecies)%sum)

                                ! particulate bed mass per unit area

                   CALL plot_print_time_var(bedpart_varid(ispecies), start, length, &
                        &block(iblock)%xmax, block(iblock)%ymax,&
                        &accum_block(iblock)%bed%conc(ispecies)%sum)

                END SELECT
             END DO
             IF (source_doing_sed) THEN
                CALL plot_print_time_var(beddepth_varid, start, length, &
                     &block(iblock)%xmax, block(iblock)%ymax,&
                     &accum_block(iblock)%bed%depth%sum)
             END IF
             
          END IF
    END DO


    ncstat = nf_sync(plot_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)

  END SUBROUTINE plot_print_netcdf

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_geometry
  ! This routine is used to compute the geometry of the output.  It is
  ! intended to be the ONLY place where the coordinates of output data
  ! are computed.  This routine exists so that output grids can remain
  ! understandable even when the computation grid is complicated with
  ! ghost cells, half cells, etc.
  ! ----------------------------------------------------------------
  SUBROUTINE plot_geometry()

    USE globals

    IMPLICIT NONE

    INTEGER :: iblock, i, j

    DO iblock = 1, max_blocks

                                ! without ghost cells, the output
                                ! locations are the same as the
                                ! computation locations

       block(iblock)%x_out = block(iblock)%x
       block(iblock)%y_out = block(iblock)%y
       block(iblock)%zbot_out = block(iblock)%zbot

                                ! with ghost cells at the up/down
                                ! stream end, the output locations
                                ! need to be interpolated at the block
                                ! ends

       i = 1
       block(iblock)%x_out(i,1) = block(iblock)%x_grid(i,1)
       block(iblock)%y_out(i,1) = block(iblock)%y_grid(i,1)
       block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i,1)
       DO j = 2, block(iblock)%ymax
          block(iblock)%x_out(i,j) = &
               &0.5*(block(iblock)%x_grid(i,j-1) + block(iblock)%x_grid(i,j))
          block(iblock)%y_out(i,j) = &
               &0.5*(block(iblock)%y_grid(i,j-1) + block(iblock)%y_grid(i,j))
          block(iblock)%zbot_out(i,j) = &
               &0.5*(block(iblock)%zbot_grid(i,j-1) + block(iblock)%zbot_grid(i,j))
       END DO
       j = block(iblock)%ymax + 1
       block(iblock)%x_out(i,j) = block(iblock)%x_grid(i,j-1)
       block(iblock)%y_out(i,j) = block(iblock)%y_grid(i,j-1)
       block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i,j-1)

       i =  block(iblock)%xmax + 1
       block(iblock)%x_out(i,1) = block(iblock)%x_grid(i-1,1)
       block(iblock)%y_out(i,1) = block(iblock)%y_grid(i-1,1)
       block(iblock)%zbot_out(i,1) = block(iblock)%zbot_grid(i-1,1)
       DO j = 2, block(iblock)%ymax
          block(iblock)%x_out(i,j) = &
               &0.5*(block(iblock)%x_grid(i-1,j-1) + block(iblock)%x_grid(i-1,j))
          block(iblock)%y_out(i,j) = &
               &0.5*(block(iblock)%y_grid(i-1,j-1) + block(iblock)%y_grid(i-1,j))
          block(iblock)%zbot_out(i,j) = &
               &0.5*(block(iblock)%zbot_grid(i-1,j-1) + block(iblock)%zbot_grid(i-1,j))
       END DO
       j = block(iblock)%ymax + 1
       block(iblock)%x_out(i,j) = block(iblock)%x_grid(i-1,j-1)
       block(iblock)%y_out(i,j) = block(iblock)%y_grid(i-1,j-1)
       block(iblock)%zbot_out(i,j) = block(iblock)%zbot_grid(i-1,j-1)
       
                                ! deal with the lateral ghost cells later

    END DO
  END SUBROUTINE plot_geometry


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_setup()

    IMPLICIT NONE

    CALL plot_geometry()
    IF (plot_do_tecplot) &
         &CALL plot_file_setup_tecplot()
    IF (plot_do_netcdf) &
         &CALL plot_file_setup_netcdf()

  END SUBROUTINE plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE plot_print(date_string, time_string, salinity, baro_press)

    USE scalars_source, ONLY: source_doing_sed
    USE bed_module
    USE globals
    
    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t, salinity, baro_press

    CALL accum_calc()
    CALL velocity_shift()

    IF (plot_do_tecplot) &
         &CALL plot_print_tecplot(date_string, time_string, salinity, baro_press)
    IF (plot_do_netcdf) &
         &CALL plot_print_netcdf(date_string, time_string, salinity, baro_press)

    CALL accum_reset()

  END SUBROUTINE plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE plot_file_close()

    IMPLICIT NONE

    INTEGER :: ncstat

    INCLUDE 'netcdf.inc'

    IF (plot_do_netcdf) THEN
       ncstat = nf_close(plot_ncid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(plot_ncname, ncstat)
       DEALLOCATE(nctmp_real)
       DEALLOCATE(nctmp_double)
    ELSE
       CLOSE(plot_iounit)
    END IF

    CALL accum_done()
    IF (ASSOCIATED(scalar_varid)) DEALLOCATE(scalar_varid)
    IF (ASSOCIATED(depos_varid)) DEALLOCATE(depos_varid)
    IF (ASSOCIATED(erode_varid)) DEALLOCATE(erode_varid)
    IF (ASSOCIATED(part_depos_varid)) DEALLOCATE(part_depos_varid)
    IF (ASSOCIATED(bedsed_varid)) DEALLOCATE(bedsed_varid)
    IF (ASSOCIATED(bedmass_varid)) DEALLOCATE(bedmass_varid)
    IF (ASSOCIATED(bedpart_varid)) DEALLOCATE(bedpart_varid)
    IF (ASSOCIATED(bedpartmass_varid)) DEALLOCATE(bedpartmass_varid)
    IF (ASSOCIATED(beddis_varid)) DEALLOCATE(beddis_varid)
    IF (ASSOCIATED(bedpore_varid)) DEALLOCATE(bedpore_varid)
    IF (ASSOCIATED(bedporemass_varid)) DEALLOCATE(bedporemass_varid)

  END SUBROUTINE plot_file_close

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_setup
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_setup()

    IF (plot_do_tecplot) THEN
       CALL diag_plot_file_setup_tecplot()
    END IF

    ! IF (plot_do_netcdf): already taken care of in plot_file_setup_netcdf

  END SUBROUTINE diag_plot_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_file_setup_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_setup_tecplot()

    IMPLICIT NONE

    OPEN(diag_plot_iounit,file=diag_plot_ioname)
    WRITE(diag_plot_iounit,*)"title=""2d Depth-Averaged Flow MASS2 Code"""
    WRITE(diag_plot_iounit,100)
100 FORMAT("variables=""x"" ""y"" ""Froude No."" ""Courant No.""")

  END SUBROUTINE diag_plot_file_setup_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_print
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_print(date_string, time_string, delta_t)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t
    INTEGER :: i, iblock

    DO iblock = 1, max_blocks

       WHERE (block(iblock)%depth .GT. 0.0 .AND. &
            &block(iblock)%hp1 .NE. 0.0 .AND. &
            &block(iblock)%hp2 .NE. 0.0)
          block(iblock)%froude_num = &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)/ &
               &SQRT(grav*block(iblock)%depth)
          block(iblock)%courant_num = &
               &delta_t*(2.0*SQRT(grav*block(iblock)%depth) + &
               &SQRT(block(iblock)%uvel_p**2 + block(iblock)%vvel_p**2)) * &
               &SQRT(1/block(iblock)%hp1**2 + 1/block(iblock)%hp2**2)
       ELSEWHERE 
          block(iblock)%froude_num = 0.0
          block(iblock)%courant_num = 0.0
       END WHERE


    END DO

    IF (plot_do_tecplot) &
         &CALL diag_plot_print_tecplot(date_string, time_string, delta_t)

  END SUBROUTINE diag_plot_print


  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_print_tecplot
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_print_tecplot(date_string, time_string, delta_t)

    USE globals

    IMPLICIT NONE

    CHARACTER (LEN=*) :: date_string
    CHARACTER (LEN=*) :: time_string
    DOUBLE PRECISION :: delta_t
    CHARACTER*26 :: zone_name
    INTEGER :: i, iblock

    CALL plot_zone_name(date_string, time_string, zone_name)

    DO iblock=1,max_blocks

       !---------------------------------------------------------------
       ! diagnostic plot file output; tecplot format
       WRITE(diag_plot_iounit,*) "zone f=block"," t=""",zone_name,iblock,"""",&
            &" i=", block(iblock)%xmax+1, " j= ",block(iblock)%ymax+1
       WRITE(diag_plot_iounit,*)block(iblock)%x
       WRITE(diag_plot_iounit,*)block(iblock)%y

       WRITE(diag_plot_iounit,*)block(iblock)%froude_num
       
       WRITE(diag_plot_iounit,*)block(iblock)%courant_num
    END DO
  END SUBROUTINE diag_plot_print_tecplot

  ! ----------------------------------------------------------------
  ! SUBROUTINE diag_plot_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE diag_plot_file_close()

    IF (plot_do_tecplot) THEN
       CLOSE(diag_plot_iounit)
    END IF

  END SUBROUTINE diag_plot_file_close


END MODULE plot_output
