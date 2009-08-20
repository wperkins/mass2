!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	gage_output
!
! VERSION and DATE: MASS2 v0.241 9/28/98
!
! PURPOSE: manages time-series files for gage locations
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY: fixed-non F90 derived type initialization; mcr 9/28/98
!
!
!***************************************************************
!

MODULE gage_output

  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE gage_specs_struct
     !CHARACTER (LEN=80) :: filename = ''  ! fails for non F95 compilers
     CHARACTER (LEN=80) :: filename
     CHARACTER (LEN=40) :: ident
     INTEGER :: block
     INTEGER :: i_cell, j_cell
  END TYPE gage_specs_struct

  TYPE(gage_specs_struct), POINTER :: gage_specs(:)

  INTEGER, PARAMETER :: gage_iounit = 50
  INTEGER :: num_gages
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: gage_control = 'gage_control.dat'

  LOGICAL, PARAMETER, PRIVATE :: gage_do_text = .TRUE.
  LOGICAL, PARAMETER, PRIVATE :: gage_do_netcdf = (.NOT. gage_do_text)

  INTEGER, PRIVATE :: gage_ncid
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: gage_ncname = 'gage.nc'
  INTEGER, PRIVATE :: tslen = 20, idlen = 40
  INTEGER, PRIVATE :: gage_dimid, time_dimid, tslen_dimid, idlen_dimid
  INTEGER, PRIVATE :: block_varid, eta_varid, xi_varid, gname_varid
  INTEGER, PRIVATE :: time_varid,  ts_varid, id_varid, elapsed_varid

  INTEGER, PRIVATE :: wselev_varid, depth_varid, vmag_varid, uvel_varid, vvel_varid, isdry_varid
  INTEGER, PRIVATE, POINTER :: scalar_varid(:)
  INTEGER, PRIVATE :: press_varid, deltap_varid, sat_varid

  INTEGER, PARAMETER, PRIVATE :: mass_source_iounit = 19
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: mass_source_ioname = 'mass_source_monitor.out'

  INTEGER, PRIVATE, POINTER :: depos_varid(:), erode_varid(:), bedsed_varid(:), bedmass_varid(:)
  INTEGER, PRIVATE, POINTER :: bedporemass_varid(:), bedpore_varid(:), beddis_varid(:)
  INTEGER, PRIVATE, POINTER :: part_depos_varid(:), bedpartmass_varid(:), bedpart_varid(:)
  INTEGER, PRIVATE :: beddepth_varid
  INTEGER, PRIVATE :: evap_varid

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_make_ident
  ! ----------------------------------------------------------------
  SUBROUTINE gage_make_ident(gage_rec)

    IMPLICIT NONE
    TYPE(gage_specs_struct) :: gage_rec
    CHARACTER (LEN=40) :: buffer
    INTEGER :: i

    IF (LEN_TRIM(gage_rec%ident) .LE. 0) THEN
       WRITE(buffer, 100) &
            &gage_rec%block, gage_rec%i_cell, gage_rec%j_cell
       gage_rec%ident = buffer
    END IF

100 FORMAT('block=', I2.2, ',i=', I3.3, ',j=', I3.3)
  END SUBROUTINE gage_make_ident
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_read_control
  ! ----------------------------------------------------------------
  SUBROUTINE gage_read_control()

    USE globals, ONLY: max_blocks

    IMPLICIT NONE

    INTEGER :: dum, alloc_stat, i, ierr
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    ! count up the number of gages and allocate the structure        
    num_gages = 0    
    CALL open_existing(gage_control, 50)
	DO WHILE(.TRUE.)
       READ(50,*,END=100)dum
       num_gages = num_gages + 1	
	END DO
100	CLOSE(50)

	ALLOCATE(gage_specs(num_gages), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the array of gage specs ')
	ENDIF

    CALL open_existing(gage_control, 50)
	DO i=1,num_gages
       gage_specs(i)%ident = ' '
       READ(50,*)gage_specs(i)%block,gage_specs(i)%i_cell,gage_specs(i)%j_cell,gage_specs(i)%ident
       IF (gage_specs(i)%block .LE. 0 .OR. gage_specs(i)%block .GT. max_blocks) THEN
          WRITE (msg, *) 'gage location ', i, ': invalid block number: ', gage_specs(i)%block
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
       CALL gage_make_ident(gage_specs(i))
	END DO
	CLOSE(50)

    IF (ierr .GT. 0) THEN
       CALL error_message('errors in "' // TRIM(gage_control) // '" input file', fatal=.TRUE.)
    END IF

    CALL status_message('allocation successful for array of gage specs')

  END SUBROUTINE gage_read_control

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_header
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_header(iounit)

    USE globals
    USE scalars, ONLY: max_species
    USE scalars_source
    USE bed_module
    USE misc_vars, ONLY: do_transport

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
    INTEGER :: i

    WRITE(iounit, '(A)', ADVANCE='NO') "#"
    WRITE(iounit, 101, ADVANCE='NO') "timestamp"
    WRITE(iounit, 100, ADVANCE='NO') "elapsed"
    
    ! time-dependent data variables

    WRITE(iounit, 100, ADVANCE='NO') "wsel"
    WRITE(iounit, 100, ADVANCE='NO') "depth"
    WRITE(iounit, 100, ADVANCE='NO') "vmag"
    WRITE(iounit, 100, ADVANCE='NO') "uvel"
    WRITE(iounit, 100, ADVANCE='NO') "vvel"
    WRITE(iounit, 100, ADVANCE='NO') "isdry"

    IF (do_transport) THEN
       DO i = 1, max_species
          WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name)
          
          SELECT CASE (scalar_source(i)%srctype)
          CASE (GEN)
             IF (source_doing_sed) THEN
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-pore'
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
                WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             END IF
          CASE (PART)
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-depos'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
          CASE (TDG)                             
             WRITE(iounit, 100, ADVANCE='NO') "tdgpress"
             WRITE(iounit, 100, ADVANCE='NO') "tdgdeltap"
             WRITE(iounit, 100, ADVANCE='NO') "tdgsat"
          CASE (SED)                             
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-depos'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-erode'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bedmass'
             WRITE(iounit, 100, ADVANCE='NO') TRIM(scalar_source(i)%name) // '-bed'
          CASE (TEMP)
             WRITE(iounit, 100, ADVANCE='NO') "evap"
          END SELECT
       END DO
       IF (source_doing_sed) THEN
          WRITE(iounit, 100, ADVANCE='NO') "beddepth"
       END IF
    END IF

    WRITE(iounit, *)
  
100 FORMAT(A15, ' ')
101 FORMAT(A19, ' ')
  END SUBROUTINE gage_file_header



  !##################################################################################
  SUBROUTINE gage_file_setup_text(do_transport)

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: do_transport
    INTEGER :: i

    ! construct the generic file name for the gage output files
    ! file = "gage_block=1_icell=11_jcell=2.out"
    DO i=1,num_gages
       gage_specs(i)%filename = "gage_" // TRIM(gage_specs(i).ident) // ".out"
       OPEN(gage_iounit, file=gage_specs(i)%filename)
       CALL gage_file_header(gage_iounit)
       CLOSE(50)
    END DO
  END SUBROUTINE gage_file_setup_text
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print_text
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_text(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    USE globals
    USE scalars
    USE date_time
    USE gas_functions
    USE scalars_source
    USE bed_module
   
    IMPLICIT NONE
    CHARACTER*(*), INTENT(IN) :: date_string, time_string
    DOUBLE PRECISION, INTENT(IN) :: elapsed, salinity, baro_press
    LOGICAL, INTENT(IN) :: do_transport

    INTEGER :: i, j, iblock, icell, jcell, ifract
    DOUBLE PRECISION :: conc_TDG, t_water
    DOUBLE PRECISION :: value
    CHARACTER (LEN=tslen) :: timestamp

    timestamp = TRIM(date_string) // ' ' // TRIM(time_string)

    DO i=1,num_gages
       OPEN(gage_iounit, file=gage_specs(i)%filename, POSITION="APPEND")

       WRITE(gage_iounit, 101, ADVANCE='NO') timestamp
       WRITE(gage_iounit, 100, ADVANCE='NO') elapsed

       iblock = gage_specs(i)%block
       icell = gage_specs(i)%i_cell + 1 ! convert from cell to i,j
       jcell = gage_specs(i)%j_cell + 1

       
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%wsel(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%depth(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') &
            &SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%uvel(icell,jcell)
       WRITE (gage_iounit, 102, ADVANCE='NO') block(iblock)%vvel(icell,jcell)

       IF (block(iblock)%isdry(icell,jcell)) THEN
          value = 1.0
       ELSE
          value = 0.0
       END IF
       WRITE (gage_iounit, 102, ADVANCE='NO') value

       IF(do_transport)THEN

          IF (source_doing_temp) THEN
             t_water = species(source_temp_idx)%scalar(iblock)%conc(icell,jcell)
          END IF
          DO j = 1, max_species
             WRITE (gage_iounit, 102, ADVANCE='NO') &
                  &species(j)%scalar(iblock)%conc(icell,jcell)/&
                  &scalar_source(j)%conversion
             SELECT CASE(scalar_source(j)%srctype)
             CASE (GEN)
                IF (scalar_source(j)%generic_param%issorbed) THEN
                   WRITE(gage_iounit, 102, ADVANCE='NO') bed(iblock)%pore(j, icell, jcell)
                   value = 0.0
                   IF (bed(iblock)%depth(icell, jcell) .GT. 0.0) value= &
                        &bed(iblock)%pore(j, icell, jcell)/ &
                        &(bed(iblock)%depth(icell, jcell)* &
                        &bed(iblock)%porosity(icell, jcell))
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') &
                        &bed(iblock)%pore(j, icell, jcell)*&
                        &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                ELSE
                   value = 0.0
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                   WRITE(gage_iounit, 102, ADVANCE='NO') value
                END IF
                
             CASE (TDG)                             
                conc_TDG = species(j)%scalar(iblock)%conc(icell,jcell)
                value = TDGasPress(conc_TDG,  t_water,  salinity)
                WRITE(gage_iounit, 102, ADVANCE='NO') value
                
                value = TDGasDP(conc_TDG,  t_water,  salinity, baro_press)
                WRITE(gage_iounit, 102, ADVANCE='NO') value
          
                value = TDGasSaturation(conc_TDG,  t_water,  salinity, baro_press)
                WRITE(gage_iounit, 102, ADVANCE='NO') value

             CASE (SED)                             
                ifract = scalar_source(j)%sediment_param%ifract
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%sediment_param%block(iblock)%deposition(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%sediment_param%block(iblock)%erosion(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%sediment(ifract, icell, jcell)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%sediment(ifract, icell, jcell)

             CASE (PART)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &scalar_source(j)%part_param%block(iblock)%bedexch(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%sediment(j, icell, jcell)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     &bed(iblock)%particulate(j, icell, jcell)

             CASE (TEMP)
                WRITE(gage_iounit, 102, ADVANCE='NO')&
                     scalar_source(j)%temp_param%block(iblock)%evaporation(icell, jcell)
             END SELECT

          END DO
       END IF
       CLOSE(50)
    END DO
100 FORMAT(F15.6, ' ')
101 FORMAT(A20, ' ')
102 FORMAT(E15.8, ' ')

  END SUBROUTINE gage_print_text


  ! ----------------------------------------------------------------
  ! SUBROUTINE plot_set_var_attributes
  ! General attributes used for (floating point) spatial and temporal
  ! variables in the plot file
  ! ----------------------------------------------------------------
  SUBROUTINE gage_set_var_attributes(gage_ncid, varid, desc, units, isbed)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    INTEGER, INTENT(IN) :: gage_ncid, varid
    CHARACTER (LEN=*), INTENT(IN) :: desc, units
    LOGICAL, INTENT(IN) :: isbed

    INTEGER :: xtype, ncstat

    ncstat = nf_put_att_text (gage_ncid, varid, "Units", LEN_TRIM(units), units)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, varid, "Description", LEN_TRIM(desc), desc)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! these fit the attribute conventions
                                ! in the NetCDF manual

    ncstat = nf_put_att_text (gage_ncid, varid, "units", LEN_TRIM(units), units)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, varid, "long_name", LEN_TRIM(desc), desc)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! choose a fill value based on the
                                ! variable type

    ncstat = nf_inq_vartype(gage_ncid, varid, xtype)
    SELECT CASE (xtype)
    CASE (NF_FLOAT)
       ncstat = nf_put_att_real (gage_ncid, varid, "_FillValue", NF_FLOAT, 1, nf_fill_real)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    CASE (NF_DOUBLE)
       ncstat = nf_put_att_double (gage_ncid, varid, "_FillValue", NF_DOUBLE, 1, nf_fill_double)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    END SELECT

                                ! mark bed variables 

    IF (isbed) THEN
       ncstat = nf_put_att_text (gage_ncid, varid, "isbed", 4, "true")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    END IF
    
  END SUBROUTINE gage_set_var_attributes

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION gage_add_tim_var
  ! Adds a variable to the gage file that is time- and spatially-varying
  ! ----------------------------------------------------------------
  INTEGER FUNCTION gage_add_time_var(name, desc, units, flag)

    IMPLICIT NONE
    CHARACTER (LEN=*) :: name, desc, units
    LOGICAL, OPTIONAL :: flag
    INTEGER :: varid, dimids(2), ncstat
    LOGICAL :: isbed

    INCLUDE 'netcdf.inc'

    IF (PRESENT(flag)) THEN
       isbed = flag
    ELSE
       isbed = .FALSE.
    END IF

    dimids(1) = gage_dimid
    dimids(2) = time_dimid

    ncstat = nf_def_var (gage_ncid, name, nf_real, 2, dimids, varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    CALL gage_set_var_attributes(gage_ncid, varid, desc, units, isbed)

    gage_add_time_var = varid

  END FUNCTION gage_add_time_var


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_setup_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_setup_netcdf(do_transport)

    USE globals
    USE scalars, ONLY: max_species
    USE scalars_source
    USE bed_module

    IMPLICIT NONE

    INTEGER :: ncstat, dimids(10), index(10), i, length(10), l, ifract
    LOGICAL :: do_transport
    CHARACTER (LEN=80) :: buffer

    INCLUDE 'netcdf.inc'

    ncstat = nf_create (gage_ncname, IOR(IOR(nf_write, nf_clobber), nf_fill), gage_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! dimensions

    ncstat = nf_def_dim (gage_ncid, "gage", num_gages, gage_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_def_dim (gage_ncid, "time", nf_unlimited, time_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_def_dim (gage_ncid, "tslen", tslen, tslen_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_def_dim (gage_ncid, "idlen", idlen, idlen_dimid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! gage identifier variables

    dimids(1) = gage_dimid
    ncstat = nf_def_var (gage_ncid, "block", nf_int, 1, dimids, block_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
    ncstat = nf_def_var (gage_ncid, "eta", nf_int, 1, dimids, eta_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_def_var (gage_ncid, "xi", nf_int, 1, dimids, xi_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    dimids(1) = idlen_dimid
    dimids(2) = gage_dimid
    ncstat = nf_def_var (gage_ncid, "gage_name", nf_char, 2, dimids, id_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! time (only) variables

    dimids(1) = time_dimid
    ncstat = nf_def_var (gage_ncid, "time", nf_double, 1, dimids, time_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text(gage_ncid, time_varid, "Units", &
         &32, "days since 1900-01-01 00:00:00")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_def_var (gage_ncid, "elapsed", nf_real, 1, dimids, elapsed_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text(gage_ncid, elapsed_varid, "Units", 5, "hours")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, elapsed_varid, "Description", &
         &23, "Elapsed Simulation Time")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    dimids(1) = tslen_dimid
    dimids(2) = time_dimid
    ncstat = nf_def_var (gage_ncid, "timestamp", nf_char, 2, dimids, ts_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! time-dependant data variables

    wselev_varid = gage_add_time_var("wsel", "Water Surface Elevation", "feet")
    depth_varid = gage_add_time_var("depth", "Depth", "feet")
    vmag_varid = gage_add_time_var("vmag", "Velocity Magnitude", "feet/second")
    uvel_varid = gage_add_time_var("uvel", "Longitudinal Velocity", "feet/second")
    vvel_varid = gage_add_time_var("vvel", "Lateral Velocity", "feet/second")
    isdry_varid = gage_add_time_var("isdry", "Dry Cell Flag", "none")


!!$    ncstat = nf_def_var (gage_ncid, "uvel", nf_real, 2, dimids, uvel_varid)
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$    ncstat = nf_put_att_text (gage_ncid, uvel_varid, "Units", 11, "feet/second")
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$    ncstat = nf_put_att_text (gage_ncid, uvel_varid, "Description", &
!!$         &21, "Longitudinal Velocity")
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
!!$    ncstat = nf_def_var (gage_ncid, "vvel", nf_real, 2, dimids, vvel_varid)
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$    ncstat = nf_put_att_text (gage_ncid, vvel_varid, "Units", 11, "feet/second")
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$    ncstat = nf_put_att_text (gage_ncid, vvel_varid, "Description", &
!!$         &16, "Lateral Velocity")
!!$    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    IF (do_transport) THEN

       ALLOCATE(scalar_varid(max_species))

       IF (source_doing_sed) THEN
          ALLOCATE(depos_varid(sediment_fractions))
          ALLOCATE(erode_varid(sediment_fractions))
          ALLOCATE(bedsed_varid(sediment_fractions))
          ALLOCATE(bedmass_varid(sediment_fractions))
          ALLOCATE(part_depos_varid(max_species))
          ALLOCATE(bedpartmass_varid(max_species))
          ALLOCATE(bedpart_varid(max_species))
          ALLOCATE(bedporemass_varid(max_species))
          ALLOCATE(bedpore_varid(max_species))
          ALLOCATE(beddis_varid(max_species))
       END IF

       DO i = 1, max_species

          scalar_varid(i) = gage_add_time_var(scalar_source(i)%name, &
               &scalar_source(i)%description, scalar_source(i)%units)

!!$          ncstat = nf_def_var (gage_ncid, TRIM(scalar_source(i)%name), &
!!$               &nf_real, 2, dimids, scalar_varid(i))
!!$          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$          buffer = TRIM(scalar_source(i)%units)
!!$          ncstat = nf_put_att_text (gage_ncid, scalar_varid(i), "Units", &
!!$               &LEN_TRIM(buffer), buffer)
!!$          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
!!$          buffer = scalar_source(i)%description
!!$          ncstat = nf_put_att_text (gage_ncid, scalar_varid(i), "Description", &
!!$               &LEN_TRIM(buffer), buffer)
!!$          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

          ncstat = nf_put_att_double(gage_ncid, scalar_varid(i), "Conversion", &
               &NF_DOUBLE, 1, scalar_source(i)%conversion)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
          
          SELECT CASE (scalar_source(i)%srctype)

          CASE (GEN)
             IF (source_doing_sed) THEN
                                ! dissolved mass in bed pores

                bedporemass_varid(i) = gage_add_time_var(&
                     &TRIM(scalar_source(i)%name) // '-bedmass', &
                     &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                     &"mass", .TRUE.)

                                ! dissolved mass per unit area in bed

                bedpore_varid(i) = gage_add_time_var(&
                     &TRIM(scalar_source(i)%name) // '-bed', &
                     &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                     &"mass/foot^2", .TRUE.)

                                ! dissolved mass per unit volume in bed pores

                beddis_varid(i) = gage_add_time_var(&
                     &TRIM(scalar_source(i)%name) // '-pore', &
                     &"Concentration of " // TRIM(scalar_source(i)%description) // " in Bed Pores", &
                     &"mass/foot^3", .TRUE.)

             END IF

          CASE (PART)

             part_depos_varid(i) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-depos', &
                  &"Rate of Deposition of " // TRIM(scalar_source(i)%description), &
                  &"mass/foot^2/second", .TRUE.)
             ncstat = nf_put_att_text (gage_ncid, part_depos_varid(i), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! sediment mass in bed

             bedpartmass_varid(i) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-bedmass', &
                  &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                  &"mass", .TRUE.)
             ncstat = nf_put_att_text (gage_ncid, bedpartmass_varid(i), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! sediment mass per unit area in bed

             bedpart_varid(i) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-bed', &
                  &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                  &"mass/foot^2", .TRUE.)
             ncstat = nf_put_att_text (gage_ncid, bedpart_varid(i), "dissolved", &
                  &LEN_TRIM(buffer), buffer)
             IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

          CASE (TDG)

             press_varid = gage_add_time_var("tdgpress", &
                  &"Total Dissolved Gas Pressure", "millibars")
       
             deltap_varid = gage_add_time_var("tdgdeltap", &
                  &"Total Dissolved Gas Pressure above Atmospheric", "millibars")

             sat_varid =  gage_add_time_var("tdgsat", &
                  &"Total Dissolved Gas Pressure Saturation", "percent")

          CASE (SED)

             ifract = scalar_source(i)%sediment_param%ifract

                                ! sediment deposition rate
             
             depos_varid(ifract) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-depos', &
                  &"Deposition Rate of " // TRIM(scalar_source(i)%description), &
                  &"mass/foot^2/second", .TRUE.)

                                ! sediment erosion rate

             erode_varid(ifract) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-erode', &
                  &"Deposition Rate of " // TRIM(scalar_source(i)%description), &
                  &"mass/foot^2/second", .TRUE.)
             
                                ! sediment mass in bed

             bedmass_varid(ifract) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-bedmass', &
                  &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                  &"mass", .TRUE.)

                                ! sediment mass per unit area in bed

             bedsed_varid(ifract) = gage_add_time_var(&
                  &TRIM(scalar_source(i)%name) // '-bed', &
                  &"Mass of " // TRIM(scalar_source(i)%description) // " in Bed", &
                  &"mass/foot^2", .TRUE.)

             CASE (TEMP)

                evap_varid = gage_add_time_var(&
                  &"evaporation", &
                  &"Evaporation Rate", &
                  &"in/day", .TRUE.)

          END SELECT
       END DO

                                ! bed depth, if called for

       IF (source_doing_sed) THEN
          beddepth_varid = gage_add_time_var("beddepth", "Bed Depth", "feet", .TRUE.)
       END IF

       
    END IF

                                ! done w/ file definition

    ncstat = nf_enddef(gage_ncid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! fill up the known gages

    DO i = 1, num_gages
       index(1) = i
       ncstat = nf_put_var1_int (gage_ncid, block_varid, index, gage_specs(i)%block)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_var1_int (gage_ncid, eta_varid, index, gage_specs(i)%i_cell)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_var1_int (gage_ncid, xi_varid, index, gage_specs(i)%j_cell)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

       buffer = gage_specs(i)%ident
       l = LEN_TRIM(buffer)
       l = MIN(l, idlen - 1)
       buffer(l+1:l+1) = CHAR(0)
       index(1) = 1
       index(2) = i
       length(1) = l + 1
       length(2) = 1
       ncstat = nf_put_vara_text(gage_ncid, id_varid, index, length, buffer)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       
    END DO

    ncstat = nf_sync(gage_ncid)

  END SUBROUTINE gage_file_setup_netcdf

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print_time_var
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_time_var(varid, start, var, convert)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    INTEGER, INTENT(IN) :: varid, start(:)
    DOUBLE PRECISION, INTENT(IN) :: var
    DOUBLE PRECISION, INTENT(IN), OPTIONAL :: convert
    INTEGER :: ncstat
    DOUBLE PRECISION :: conversion
    REAL :: value

    IF (.NOT. PRESENT(convert)) THEN
       conversion = 1.0
    ELSE
       conversion = convert
    END IF

    value = var/conversion
    ncstat = nf_put_var1_real(gage_ncid, varid, start, value)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

  END SUBROUTINE gage_print_time_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_netcdf(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    USE globals
    USE scalars
    USE date_time
    USE gas_functions
    USE scalars_source
    USE bed_module
   
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

    CHARACTER*(*) :: date_string, time_string
    DOUBLE PRECISION :: elapsed, salinity, baro_press
    LOGICAL :: do_transport

    INTEGER :: i, j, iblock, icell, jcell, ifract
    DOUBLE PRECISION :: conc_TDG, t_water
    DOUBLE PRECISION :: value
    INTEGER :: ncstat, trec, dimid(2), index(2), length(2)
    CHARACTER (LEN=tslen) :: timestamp

    IF (num_gages .LE. 0) RETURN

                                ! find the last time index

    ncstat = nf_inq_dimlen(gage_ncid, time_dimid, trec)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! do the time and time stamp

    timestamp = TRIM(date_string) // ' ' // TRIM(time_string) // CHAR(0)

    dimid(1) = tslen_dimid
    dimid(2) = time_dimid
    index(1) = 1
    index(2) = trec + 1
    length(1) = tslen
    length(2) = 1
    ncstat = nf_put_vara_text(gage_ncid, ts_varid, index, length, timestamp)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    dimid(1) = time_dimid
    index(1) = trec + 1
    ncstat = nf_put_var1_double(gage_ncid, time_varid, index, &
         &date_to_decimal(date_string, time_string))
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    ncstat = nf_put_var1_real(gage_ncid, elapsed_varid, index, REAL(elapsed))
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! loop thru the gage locations and
                                ! write to file

    dimid(1) = gage_dimid
    dimid(2) = time_dimid
    index(2) = trec + 1

    DO i=1,num_gages
       iblock = gage_specs(i)%block
       icell = gage_specs(i)%i_cell + 1 ! convert from cell to i,j
       jcell = gage_specs(i)%j_cell + 1
       
       index(1) = i

                                ! put the hydrodynamic variables
       
       CALL gage_print_time_var(wselev_varid, index, block(iblock)%wsel(icell,jcell))
       CALL gage_print_time_var(depth_varid, index, block(iblock)%depth(icell,jcell))
       CALL gage_print_time_var(vmag_varid, index, &
            &SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2))
       CALL gage_print_time_var(uvel_varid, index, block(iblock)%uvel(icell,jcell))
       CALL gage_print_time_var(vvel_varid, index, block(iblock)%vvel(icell,jcell))
       IF (block(iblock)%isdry(icell,jcell)) THEN
          value = 1.0
       ELSE
          value = 0.0
       END IF
       CALL gage_print_time_var(isdry_varid, index, value)

       IF(do_transport)THEN

          IF (source_doing_temp) THEN
             t_water = species(source_temp_idx)%scalar(iblock)%conc(icell,jcell)
          END IF
          DO j = 1, max_species

             CALL gage_print_time_var(scalar_varid(j), index, &
                  &species(j)%scalar(iblock)%conc(icell,jcell), &
                  &scalar_source(j)%conversion)

             SELECT CASE(scalar_source(j)%srctype)
             CASE (GEN)
                
                IF (scalar_source(j)%generic_param%issorbed) THEN
                   CALL gage_print_time_var(bedpore_varid(j), index, &
                        &bed(iblock)%pore(j, icell, jcell))
                   CALL gage_print_time_var(bedporemass_varid(j), index, &
                        &bed(iblock)%pore(j, icell, jcell)*&
                        &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell))
                   value = 0.0
                   IF (bed(iblock)%depth(icell, jcell) .GT. 0.0) value= &
                        &bed(iblock)%pore(j, icell, jcell)/ &
                        &(bed(iblock)%depth(icell, jcell)* &
                        &bed(iblock)%porosity(icell, jcell))
                   CALL gage_print_time_var(beddis_varid(j), index, value)
                END IF

             CASE (TEMP)

                CALL gage_print_time_var(evap_varid, index, &
                     &scalar_source(j)%temp_param%block(iblock)%evaporation(icell,jcell))

             CASE (TDG)
                conc_TDG = species(j)%scalar(iblock)%conc(icell,jcell)
                value = TDGasPress(conc_TDG,  t_water,  salinity)
                CALL gage_print_time_var(press_varid, index, value)
                
                value = TDGasDP(conc_TDG,  t_water,  salinity, baro_press)
                CALL gage_print_time_var(deltap_varid, index, value)
          
                value = TDGasSaturation(conc_TDG,  t_water,  salinity, baro_press)
                CALL gage_print_time_var(sat_varid, index, value)

             CASE (SED)
                ifract = scalar_source(j)%sediment_param%ifract
                
                CALL gage_print_time_var(depos_varid(ifract), index, &
                     &scalar_source(j)%sediment_param%block(iblock)%deposition(icell, jcell))
                CALL gage_print_time_var(erode_varid(ifract), index,&
                     &scalar_source(j)%sediment_param%block(iblock)%erosion(icell, jcell))
                CALL  gage_print_time_var(bedsed_varid(ifract), index, &
                     &bed(iblock)%sediment(ifract, icell, jcell))
                CALL gage_print_time_var(bedmass_varid(ifract), index, &
                     &bed(iblock)%sediment(ifract, icell, jcell)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell))

             CASE (PART)
                CALL  gage_print_time_var(part_depos_varid(j), index, &
                     &scalar_source(j)%part_param%block(iblock)%bedexch(icell, jcell))
                CALL  gage_print_time_var(bedpart_varid(j), index, &
                     &bed(iblock)%particulate(j, icell, jcell))
                CALL gage_print_time_var(bedpartmass_varid(j), index, &
                     &bed(iblock)%sediment(j, icell, jcell)* &
                     &block(iblock)%hp1(icell, jcell)*block(iblock)%hp2(icell, jcell))
                
             END SELECT
          END DO
          IF (source_doing_sed) THEN
             CALL gage_print_time_var(beddepth_varid, index, &
                  &bed(iblock)%depth(icell, jcell))
          END IF
       END IF
    END DO

    ncstat = nf_sync(gage_ncid)

  END SUBROUTINE gage_print_netcdf


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_setup(do_transport, error_iounit, status_iounit)

    IMPLICIT NONE
    LOGICAL :: do_transport
    INTEGER :: error_iounit, status_iounit

    NULLIFY(gage_specs)
    NULLIFY(scalar_varid)
    NULLIFY(depos_varid)
    NULLIFY(erode_varid)
    NULLIFY(bedsed_varid)

    CALL gage_read_control()

    IF (num_gages .gt. 0) THEN
       IF (gage_do_text) THEN
          CALL gage_file_setup_text(do_transport)
       END IF
       IF (gage_do_netcdf) THEN
          CALL gage_file_setup_netcdf(do_transport)
       END IF
    END IF

  END SUBROUTINE gage_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    IMPLICIT NONE
    CHARACTER*(*) :: date_string, time_string
    DOUBLE PRECISION :: elapsed, salinity, baro_press
    LOGICAL :: do_transport

    IF (gage_do_text) THEN
       CALL gage_print_text(date_string, time_string, elapsed, &
            &do_transport, salinity, baro_press)
    END IF
    IF (gage_do_netcdf) THEN
       CALL gage_print_netcdf(date_string, time_string, elapsed, &
            &do_transport, salinity, baro_press)
    END IF

  END SUBROUTINE gage_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_close()

    IMPLICIT NONE
    INCLUDE 'netcdf.inc'
    INTEGER ncstat

    IF (gage_do_text) THEN
       CLOSE(gage_iounit)
    END IF
    IF (gage_do_netcdf) THEN
       ncstat = nf_close(gage_ncid)
    END IF
    IF (ASSOCIATED(gage_specs)) DEALLOCATE(gage_specs)
    IF (ASSOCIATED(scalar_varid)) DEALLOCATE(scalar_varid)
    IF (ASSOCIATED(depos_varid)) DEALLOCATE(depos_varid)
    IF (ASSOCIATED(erode_varid)) DEALLOCATE(erode_varid)
    IF (ASSOCIATED(bedsed_varid)) DEALLOCATE(bedsed_varid)

  END SUBROUTINE gage_file_close
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_setup()

    USE globals

    IMPLICIT NONE

    INTEGER iblock

    CALL open_new(mass_source_ioname, mass_source_iounit)
    WRITE(mass_source_iounit,*)"# mass source history - summation of the mass source in each block "
    WRITE(mass_source_iounit,*)"#      total mass imbalance for each block in ft3/sec"
    WRITE(mass_source_iounit,100,advance='no')
    DO iblock = 1, max_blocks 
       WRITE(mass_source_iounit,200, advance='no') iblock
    END DO
    WRITE(mass_source_iounit,*)
    CALL FLUSH(mass_source_iounit)

100 FORMAT('#date',8x,'time',5x)
200 FORMAT(i5,5x)

  END SUBROUTINE mass_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_print
  ! ----------------------------------------------------------------
  SUBROUTINE mass_print(date_string, time_string)

    USE globals
    USE misc_vars, ONLY: iteration

    IMPLICIT NONE

    INTEGER :: iblock, j
    CHARACTER*(*) :: date_string, time_string

    j = 1
    DO iblock = 1, max_blocks
       IF (j == 1) &
            &WRITE(mass_source_iounit,3013, advance='no')date_string,time_string,iteration
       WRITE(mass_source_iounit,3012, advance='no')SUM(ABS(block(iblock)%mass_source))
       IF (j >= 20) THEN
          j = 1
          IF (iblock .ne. max_blocks) WRITE(mass_source_iounit,*)
       ELSE
          j = j + 1
       END IF
    END DO
    WRITE(mass_source_iounit,*)
    CALL FLUSH(mass_source_iounit)

3013 FORMAT(a10,2x,a12,1x,I3,1X)
3012 FORMAT((g12.4,1x))
    

  END SUBROUTINE mass_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_close()

    CLOSE(mass_source_iounit)    

  END SUBROUTINE mass_file_close



END MODULE gage_output
