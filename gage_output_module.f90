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

  TYPE gage_specs_struct
     !CHARACTER (LEN=80) :: filename = ''  ! fails for non F95 compilers
     CHARACTER (LEN=80) :: filename
     CHARACTER (LEN=40) :: ident
     INTEGER :: block
     INTEGER :: i_cell, j_cell
  END TYPE gage_specs_struct

  TYPE(gage_specs_struct), ALLOCATABLE :: gage_specs(:)

  INTEGER, PARAMETER :: gage_iounit = 50
  INTEGER :: num_gages
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: gage_control = 'gage_control.dat'

  LOGICAL, PARAMETER, PRIVATE :: gage_do_text = .FALSE.
  LOGICAL, PARAMETER, PRIVATE :: gage_do_netcdf = (.NOT. gage_do_text)

  INTEGER, PRIVATE :: gage_ncid
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: gage_ncname = 'gage.nc'
  INTEGER, PRIVATE :: tslen = 20, idlen = 40
  INTEGER, PRIVATE :: gage_dimid, time_dimid, tslen_dimid, idlen_dimid
  INTEGER, PRIVATE :: block_varid, eta_varid, xi_varid, gname_varid
  INTEGER, PRIVATE :: time_varid,  ts_varid, id_varid, elapsed_varid

  INTEGER, PRIVATE :: wselev_varid, depth_varid, vmag_varid, uvel_varid, vvel_varid
  INTEGER, PRIVATE :: temp_varid, conc_varid, press_varid, deltap_varid, sat_varid

  INTEGER, PARAMETER, PRIVATE :: mass_source_iounit = 19
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: mass_source_ioname = 'mass_source_monitor.out'

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

100 FORMAT('block=', I2.2, ' i=', I3.3, ' j=', I3.3)
  END SUBROUTINE gage_make_ident
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_read_control
  ! ----------------------------------------------------------------
  SUBROUTINE gage_read_control(error_iounit, status_iounit)

    IMPLICIT NONE

    INTEGER :: error_iounit, status_iounit
    INTEGER :: dum, alloc_stat, i

    ! count up the number of gages and allocate the structure        
    num_gages = 0    
	OPEN(50,file=gage_control)
	DO WHILE(.TRUE.)
       READ(50,*,END=100)dum
       num_gages = num_gages + 1	
	END DO
100	CLOSE(50)

	ALLOCATE(gage_specs(num_gages), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
       WRITE(error_iounit,*)'allocation failed for the array of gage specs '
       CALL EXIT(1)
	ENDIF

	OPEN(50,file=gage_control)
	DO i=1,num_gages
       gage_specs(i)%ident = ' '
       READ(50,*)gage_specs(i)%block,gage_specs(i)%i_cell,gage_specs(i)%j_cell,gage_specs(i)%ident
       CALL gage_make_ident(gage_specs(i))
	END DO
	CLOSE(50)

    WRITE(status_iounit,*)'allocation successful for array of gage specs'

  END SUBROUTINE gage_read_control


  !##################################################################################
  SUBROUTINE gage_file_setup_text(error_iounit, status_iounit)

    IMPLICIT NONE

    INTEGER :: i, error_iounit, status_iounit, alloc_stat
    INTEGER :: len1,len2,len3,spot1,spot2,spot3
    CHARACTER*20 string1,string2,string3
    CHARACTER*80 stringall

    gage_specs%filename = ''

    ! construct the generic file name for the gage output files
    ! file = "gage_block=1_icell=11_jcell=2.out"
    DO i=1,num_gages
       stringall = ''
       ! gage_specs(i)%filename(1:11) = "gage_block="
       stringall(1:11) = "gage_block="
       WRITE(string1,*)gage_specs(i)%block
       WRITE(string2,*)gage_specs(i)%i_cell
       WRITE(string3,*)gage_specs(i)%j_cell
       !READ(gage_point(i),*)string2
       string1 = ADJUSTL(string1)
       string2 = ADJUSTL(string2)
       string3 = ADJUSTL(string3)
       len1 = LEN_TRIM(string1)
       len2 = LEN_TRIM(string2)
       len3 = LEN_TRIM(string3)
       spot1 =12 + len1 - 1
       ! gage_specs(i)%filename(12:spot1) = string1(1:len1)
       stringall(12:spot1) = string1(1:len1)
       spot1 = spot1 + 1
       spot2 = spot1 + LEN_TRIM("_icell=") - 1
       ! gage_specs(i)%filename(spot1:spot2) = "_icell="
       stringall(spot1:spot2) = "_icell="
       spot1 = spot2 + 1
       spot2 = spot1 + len2 - 1
       ! gage_specs(i)%filename(spot1:spot2) = string2(1:len2)
       stringall(spot1:spot2) = string2(1:len2)
       spot1 = spot2 + 1
       spot2 = spot1 + LEN_TRIM("_jcell=") - 1
       ! gage_specs(i)%filename(spot1:spot2) = "_jcell="
       stringall(spot1:spot2) = "_jcell="
       spot1 = spot2 + 1
       spot2 = spot1 + len3 - 1
       ! gage_specs(i)%filename(spot1:spot2) = string3(1:len3)
       stringall(spot1:spot2) = string3(1:len3)
       spot1 = spot2 + 1
       spot2 = spot1 + LEN_TRIM(".out")
       ! gage_specs(i)%filename(spot1:spot2) = '.out'
       stringall(spot1:spot2) = '.out'
       gage_specs(i)%filename = stringall
    END DO
    
    ! open the files and write out the header info
    DO i=1,num_gages
       OPEN(50,file=gage_specs(i)%filename)
       WRITE(50,1015)gage_specs(i)%block, gage_specs(i)%i_cell, gage_specs(i)%j_cell
       WRITE(50,1020)
       WRITE(50,1005)
1005   FORMAT('#date',8x,'time',5x,'hours from start',5x,'water elev',2x,'depth',5x,'vel mag',5x,'u-cart vel',4x,'v-cart vel', &
            7x,'Block Mass Source',2x,'species 1 - TDG (mg/l) ',2x,'species 2 - Temp (C)', &
            2x,'Total Diss. Gas Press. (mmHg)',2x,'TDG deltaP (mmHg)',2x,'TDG %Sat')
       WRITE(50,1020)
       CLOSE(50)
       
    END DO
1020 FORMAT('#',80('-'))
1015 FORMAT('#gage output for Block ',i5,' i cell =',i5,' j cell=',i5)
  END SUBROUTINE gage_file_setup_text
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_print_text
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_text(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    USE globals
    USE scalars
    USE gas_functions
   
    IMPLICIT NONE
    CHARACTER*(*) :: date_string, time_string
    DOUBLE PRECISION :: elapsed, salinity, baro_press
    LOGICAL :: do_transport
    INTEGER i, ispecies, iblock, icell, jcell
    DOUBLE PRECISION :: species_io_vec(100)
    DOUBLE PRECISION :: conc_TDG, t_water

    DO i=1,num_gages
       OPEN(50,file=gage_specs(i)%filename, POSITION="APPEND")
       iblock = gage_specs(i)%block
       icell = gage_specs(i)%i_cell + 1 ! convert from cell to i,j
       jcell = gage_specs(i)%j_cell + 1

       IF(do_transport)THEN
          DO ispecies=1,max_species
             species_io_vec(ispecies) = &
                  &species(ispecies)%scalar(iblock)%conc(icell,jcell)
          END DO
          conc_TDG = species(1)%scalar(iblock)%conc(icell,jcell)
          t_water = species(2)%scalar(iblock)%conc(icell,jcell)

          WRITE(50,100)date_string,time_string, elapsed, &
               block(iblock)%wsel(icell,jcell),block(iblock)%depth(icell,jcell), &
               SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2), &
               block(iblock)%uvel(icell,jcell), block(iblock)%vvel(icell,jcell), &
               SUM(ABS(block(iblock)%mass_source)), &
               species_io_vec(1:max_species), &
               TDGasPress(conc_TDG,  t_water,  salinity), &
               TDGasDP(conc_TDG, t_water,  salinity,  baro_press), &
               TDGasSaturation( conc_TDG, t_water,  salinity, baro_press)
       ELSE 
          species_io_vec(1:max_species) = 0
          WRITE(50,100)date_string,time_string, elapsed, &
               block(iblock)%wsel(icell,jcell),block(iblock)%depth(icell,jcell), &
               SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2), &
               block(iblock)%uvel(icell,jcell), block(iblock)%vvel(icell,jcell), &
               SUM(ABS(block(iblock)%mass_source)), &
               species_io_vec(1:max_species), &
               0.0, &
               0.0, &
               0.0
       END IF
       

       CLOSE(50)
    END DO
100 FORMAT(a10,2x,a8,2x,50(f12.2,2x))

  END SUBROUTINE gage_print_text


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_file_setup_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE gage_file_setup_netcdf(do_transport)

    IMPLICIT NONE

    INTEGER :: ncstat, dimids(10), index(10), i, length(10), l
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

    dimids(1) = gage_dimid
    dimids(2) = time_dimid
    ncstat = nf_def_var (gage_ncid, "wsel", nf_real, 2, dimids, wselev_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, wselev_varid, "Units", 4, "feet")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, wselev_varid, "Description", &
         &23, "Water Surface Elevation")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
    ncstat = nf_def_var (gage_ncid, "depth", nf_real, 2, dimids, depth_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, depth_varid, "Units", 4, "feet")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, depth_varid, "Description", &
         &5, "Depth")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
    ncstat = nf_def_var (gage_ncid, "vmag", nf_real, 2, dimids, vmag_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, vmag_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, vmag_varid, "Description", &
         &18, "Velocity Magnitude")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
    ncstat = nf_def_var (gage_ncid, "uvel", nf_real, 2, dimids, uvel_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, uvel_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, uvel_varid, "Description", &
         &21, "Longitudinal Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    
    ncstat = nf_def_var (gage_ncid, "vvel", nf_real, 2, dimids, vvel_varid)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, vvel_varid, "Units", 11, "feet/second")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
    ncstat = nf_put_att_text (gage_ncid, vvel_varid, "Description", &
         &16, "Lateral Velocity")
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

    IF (do_transport) THEN

       ncstat = nf_def_var (gage_ncid, "temperature", nf_real, 2, dimids, temp_varid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, temp_varid, "Units", 10, "Centigrade")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, temp_varid, "Description", &
            &17, "Water Temperature")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       
       ncstat = nf_def_var (gage_ncid, "tdgconc", nf_real, 2, dimids, conc_varid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, conc_varid, "Units", 15, "milligram/liter")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, conc_varid, "Description", &
            &33, "Total Dissolved Gas Concentration")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       
       ncstat = nf_def_var (gage_ncid, "tdgpress", nf_real, 2, dimids, press_varid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, press_varid, "Units", 9, "millibars")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, press_varid, "Description", &
            &32, "Total Dissolved Gas Pressure")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       
       ncstat = nf_def_var (gage_ncid, "tdgdeltap", nf_real, 2, dimids, deltap_varid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, deltap_varid, "Units", 9, "millibars")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, deltap_varid, "Description", &
            &48, "Total Dissolved Gas Pressure above Atmospheric")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       
       ncstat = nf_def_var (gage_ncid, "tdgsat", nf_real, 2, dimids, sat_varid)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, sat_varid, "Units", 7, "percent")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
       ncstat = nf_put_att_text (gage_ncid, sat_varid, "Description", &
            &39, "Total Dissolved Gas Pressure Saturation")
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
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
  ! SUBROUTINE gage_print_netcdf
  ! ----------------------------------------------------------------
  SUBROUTINE gage_print_netcdf(date_string, time_string, elapsed, &
       &do_transport, salinity, baro_press)

    USE globals
    USE scalars
    USE date_time
    USE gas_functions
   
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

    CHARACTER*(*) :: date_string, time_string
    DOUBLE PRECISION :: elapsed, salinity, baro_press
    LOGICAL :: do_transport

    INTEGER :: i, iblock, icell, jcell
    DOUBLE PRECISION :: conc_TDG, t_water
    REAL :: value
    INTEGER :: ncstat, trec, dimid(2), index(2), length(2)
    CHARACTER (LEN=tslen) :: timestamp

                                ! find the last time index

    ncstat = nf_inq_dimlen(gage_ncid, time_dimid, trec)
    IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

                                ! do the time and time stamp

    timestamp(1:10) = date_string
    timestamp(11:11) = ' '
    timestamp(12:19) = time_string
    timestamp(tslen:tslen) = CHAR(0)

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

       value = block(iblock)%wsel(icell,jcell)
       ncstat = nf_put_var1_real(gage_ncid, wselev_varid, index, value)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

       value = block(iblock)%depth(icell,jcell)
       ncstat = nf_put_var1_real(gage_ncid, depth_varid, index, value)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

       value = SQRT(block(iblock)%uvel(icell,jcell)**2 + block(iblock)%vvel(icell,jcell)**2)
       ncstat = nf_put_var1_real(gage_ncid, vmag_varid, index, value)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

       value = block(iblock)%uvel(icell,jcell)
       ncstat = nf_put_var1_real(gage_ncid, uvel_varid, index, value)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

       value = block(iblock)%vvel(icell,jcell)
       ncstat = nf_put_var1_real(gage_ncid, vvel_varid, index, value)
       IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)


       IF(do_transport)THEN

                                ! assume we know what the species are

          t_water = species(2)%scalar(iblock)%conc(icell,jcell)
          value = t_water
          ncstat = nf_put_var1_real(gage_ncid, temp_varid, index, value)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

          conc_TDG = species(1)%scalar(iblock)%conc(icell,jcell)
          value = conc_TDG
          ncstat = nf_put_var1_real(gage_ncid, conc_varid, index, value)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)

          value = TDGasPress(conc_TDG,  t_water,  salinity)
          ncstat = nf_put_var1_real(gage_ncid, press_varid, index, value)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
          
          value = TDGasDP(conc_TDG,  t_water,  salinity, baro_press)
          ncstat = nf_put_var1_real(gage_ncid, deltap_varid, index, value)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
          
          value = TDGasSaturation(conc_TDG,  t_water,  salinity, baro_press)
          ncstat = nf_put_var1_real(gage_ncid, sat_varid, index, value)
          IF (ncstat .ne. nf_noerr) CALL netcdferror(gage_ncname, ncstat)
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

    CALL gage_read_control(error_iounit, status_iounit)

    IF (num_gages .gt. 0) THEN
       IF (gage_do_text) THEN
          CALL gage_file_setup_text(error_iounit, status_iounit)
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
  END SUBROUTINE gage_file_close
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_setup
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_setup()

    USE globals

    IMPLICIT NONE

    INTEGER iblock

    OPEN(mass_source_iounit,file=mass_source_ioname)
    WRITE(mass_source_iounit,*)"# mass source history - summation of the mass source in each block "
    WRITE(mass_source_iounit,*)"#      total mass imbalance for each block in ft3/sec"
    WRITE(mass_source_iounit,100,advance='no')
    DO iblock = 1, max_blocks 
       WRITE(mass_source_iounit,200, advance='no') iblock
    END DO
    WRITE(mass_source_iounit,*)

100 FORMAT('#date',8x,'time',5x)
200 FORMAT(i5,5x)

  END SUBROUTINE mass_file_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_print
  ! ----------------------------------------------------------------
  SUBROUTINE mass_print(date_string, time_string)

    USE globals

    IMPLICIT NONE

    INTEGER :: iblock, j
    CHARACTER*(*) :: date_string, time_string

    j = 1
    DO iblock = 1, max_blocks
       IF (j == 1) &
            &WRITE(mass_source_iounit,3013, advance='no')date_string,time_string
       WRITE(mass_source_iounit,3012, advance='no')SUM(ABS(block(iblock)%mass_source))
       IF (j >= 20) THEN
          j = 1
          IF (iblock .ne. max_blocks) WRITE(mass_source_iounit,*)
       ELSE
          j = j + 1
       END IF
    END DO
    WRITE(mass_source_iounit,*)

3013 FORMAT(a10,2x,a8,2x)
3012 FORMAT((f12.2,1x))
    

  END SUBROUTINE mass_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass_file_close
  ! ----------------------------------------------------------------
  SUBROUTINE mass_file_close()

    CLOSE(mass_source_iounit)    

  END SUBROUTINE mass_file_close



END MODULE gage_output
